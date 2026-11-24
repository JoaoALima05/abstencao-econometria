# =============================================================================
# 02_construir_base.R
# Econometria I – Abstenção eleitoral x nível socioeconômico municipal
# João Andrade Lima e Thomas Miguellone Lucio Martins
#
# Lê os brutos salvos por 01_baixar_dados.R, calcula a taxa de abstenção,
# constrói as variáveis do modelo e junta tudo na base de análise.
# =============================================================================

# install.packages(c("data.table", "dplyr", "stringr", "tidyr", "stringi"))

library(data.table)
library(dplyr)
library(stringr)
library(tidyr)

dir.create("dados/intermed", recursive = TRUE, showWarnings = FALSE)

ano_eleicao   <- 2026
tse           <- readRDS(paste0("dados/brutos/tse_bruto_", ano_eleicao, ".rds"))
dep_tse_ibge  <- fread("dados/brutos/correspondencia_tse_ibge.csv")
populacao     <- readRDS("dados/brutos/sidra_populacao.rds")
pib           <- readRDS("dados/brutos/sidra_pib.rds")
alfabetizacao <- readRDS("dados/brutos/sidra_alfabetizacao.rds")
urbanizacao   <- readRDS("dados/brutos/sidra_urbanizacao.rds")
idade         <- readRDS("dados/brutos/sidra_idade.rds")
domicilios    <- readRDS("dados/brutos/sidra_domicilios.rds")

# Para o script se algum código de município aparecer duas vezes antes de uma junção
checar_unico <- function(df, nome) {
  n_dup <- sum(duplicated(df$cod_ibge))
  message(sprintf("%-14s %5d municípios | %d duplicatas", nome, nrow(df), n_dup))
  stopifnot(n_dup == 0)
  invisible(df)
}

add_cod_ibge <- function(df) mutate(df, cod_ibge = as.integer(`Município (Código)`))


# ---- 1. Taxa de abstenção ----------------------------------------------------
# Presidente, 1º turno, sem o exterior (ZZ). Fixamos um cargo porque aptos e
# abstenções se repetem para cada cargo do mesmo turno.
pres_1t <- tse |> filter(CD_CARGO == 1, NR_TURNO == 1, SG_UF != "ZZ")
stopifnot(n_distinct(pres_1t$CD_ELEICAO) == 1)   # só a eleição geral, sem suplementares

abstencao <- pres_1t |>
  group_by(cod_tse = as.integer(CD_MUNICIPIO)) |>
  summarise(aptos          = sum(QT_APTOS),
            comparecimento = sum(QT_COMPARECIMENTO),
            abstencoes     = sum(QT_ABSTENCOES),
            .groups = "drop") |>
  mutate(taxa_abstencao = abstencoes / aptos * 100)

stopifnot(all(abstencao$aptos == abstencao$comparecimento + abstencao$abstencoes))


# ---- 2. Código TSE -> código IBGE --------------------------------------------
# A tabela do betafcc tem alguns códigos trocados. Conferimos pelo nome: quando
# o nome do município no TSE corresponde a outro município do IBGE na mesma UF,
# usamos o código desse município.
normalizar <- function(x) {
  stringi::stri_trans_general(x, "Latin-ASCII") |>
    toupper() |>
    str_remove_all("[^A-Z]")
}

nomes_ibge <- populacao |>
  as_tibble() |>
  transmute(cod_ibge_nome = as.integer(`Município (Código)`),
            uf            = str_extract(Município, "[A-Z]{2}$"),
            chave         = normalizar(str_remove(Município, " - [A-Z]{2}$")))

nomes_tse <- pres_1t |>
  distinct(CD_MUNICIPIO, NM_MUNICIPIO, SG_UF) |>
  as_tibble() |>
  transmute(cod_tse = as.integer(CD_MUNICIPIO),
            uf      = SG_UF,
            chave   = normalizar(NM_MUNICIPIO))

corresp <- dep_tse_ibge |>
  as_tibble() |>
  transmute(cod_tse  = as.integer(codigo_tse),
            cod_ibge = as.integer(codigo_ibge)) |>
  distinct() |>
  inner_join(nomes_tse, by = "cod_tse") |>
  left_join(nomes_ibge, by = c("uf", "chave")) |>
  mutate(corrigido = !is.na(cod_ibge_nome) & cod_ibge_nome != cod_ibge,
         cod_ibge  = if_else(!is.na(cod_ibge_nome), cod_ibge_nome, cod_ibge))

# Número citado no texto
message("Códigos IBGE corrigidos pela conferência de nomes: ", sum(corresp$corrigido))

corresp <- select(corresp, cod_tse, cod_ibge)
stopifnot(!anyDuplicated(corresp$cod_tse), !anyDuplicated(corresp$cod_ibge))

abstencao <- left_join(abstencao, corresp, by = "cod_tse")

# Número citado no texto
message("Municípios do TSE sem código IBGE (descartados): ", sum(is.na(abstencao$cod_ibge)))

abstencao <- abstencao |>
  filter(!is.na(cod_ibge)) |>
  checar_unico("abstencao")


# ---- 3. População, PIB per capita e urbanização ------------------------------
pop <- populacao |>
  add_cod_ibge() |>
  transmute(cod_ibge, pop = Valor, log_pop = log(Valor)) |>
  checar_unico("pop")

# PIB de 2022 (mil reais) / população do Censo 2022. Em log pela assimetria.
pib_pc <- pib |>
  add_cod_ibge() |>
  select(cod_ibge, pib = Valor) |>
  inner_join(select(pop, cod_ibge, pop), by = "cod_ibge") |>
  mutate(pib_pc     = pib * 1000 / pop,
         log_pib_pc = log(pib_pc)) |>
  select(cod_ibge, pib_pc, log_pib_pc) |>
  checar_unico("pib")

urbana <- urbanizacao |>
  add_cod_ibge() |>
  filter(`Situação do domicílio` %in% c("Total", "Urbana")) |>
  select(cod_ibge, situacao = `Situação do domicílio`, Valor) |>
  pivot_wider(names_from = situacao, values_from = Valor)

# No SIDRA, "-" significa zero absoluto e vira NA na conversão para número.
# Verificação: listamos os municípios afetados. Se a população total existe e
# só a urbana está ausente, o município não tem população urbana: o NA vira 0
# em vez de descartar o município.
sem_urbana <- filter(urbana, is.na(Urbana) | is.na(Total))
message("Municípios com população urbana ou total ausente: ", nrow(sem_urbana))
if (nrow(sem_urbana) > 0) print(sem_urbana)

urbana <- urbana |>
  mutate(Urbana     = if_else(is.na(Urbana) & !is.na(Total), 0, Urbana),
         pct_urbana = Urbana / Total * 100) |>
  select(cod_ibge, pct_urbana) |>
  checar_unico("urbana")


# ---- 4. Estrutura etária -----------------------------------------------------
# Usamos só as idades simples. Defasagem 2022 -> 2026 (+4 anos):
#   16+ em 2026 -> 12+ em 2022 | 16–17 -> 12–13 | 70+ -> 66+
idade_simples <- idade |>
  add_cod_ibge() |>
  mutate(idade_n = case_when(
    Idade == "Menos de 1 ano"         ~ 0L,
    str_detect(Idade, "^\\d+ anos?$") ~ as.integer(str_extract(Idade, "^\\d+")),
    Idade == "100 anos ou mais"       ~ 100L
  ))

# Checagem: as idades simples devem ser exatamente 0..100 e somar o Total
# (garante que ninguém é contado duas vezes com as faixas da tabela)
conferencia <- idade_simples |>
  group_by(cod_ibge) |>
  summarise(total        = sum(Valor[Idade == "Total"], na.rm = TRUE),
            soma_simples = sum(Valor[!is.na(idade_n)],  na.rm = TRUE),
            n_idades     = n_distinct(idade_n, na.rm = TRUE),
            .groups = "drop")
stopifnot(all(conferencia$n_idades == 101), all(conferencia$soma_simples == conferencia$total))

etaria <- idade_simples |>
  filter(!is.na(idade_n)) |>
  group_by(cod_ibge) |>
  summarise(pop_12mais = sum(Valor[idade_n >= 12],     na.rm = TRUE),
            jovens     = sum(Valor[idade_n %in% 12:13], na.rm = TRUE),
            idosos_70  = sum(Valor[idade_n >= 66],     na.rm = TRUE),
            .groups = "drop") |>
  mutate(pct_jovens    = jovens    / pop_12mais * 100,
         pct_idosos_70 = idosos_70 / pop_12mais * 100) |>
  select(cod_ibge, pop_12mais, pct_jovens, pct_idosos_70) |>
  checar_unico("etaria")


# ---- 5. Escolaridade ---------------------------------------------------------
escolaridade <- alfabetizacao |>
  add_cod_ibge() |>
  filter(str_detect(Variável, regex("taxa de alfabetiza", ignore_case = TRUE))) |>
  select(cod_ibge, taxa_alfab = Valor) |>
  checar_unico("escolaridade")


# ---- 6. Domicílios coletivos (frase sobre presídios na seção de dados) -------
# População em domicílios coletivos = população total - moradores em domicílios
# particulares permanentes ocupados. Inclui presídios, asilos e quartéis.
coletivos <- domicilios |>
  add_cod_ibge() |>
  filter(str_detect(Variável, "^Moradores")) |>
  select(cod_ibge, pop_dppo = Valor) |>
  inner_join(select(pop, cod_ibge, pop), by = "cod_ibge") |>
  mutate(pct_coletiva = (pop - pop_dppo) / pop * 100) |>
  select(cod_ibge, pct_coletiva) |>
  checar_unico("coletivos")

for (corte in c(1, 5)) {
  message("Municípios com mais de ", corte, "% da população em domicílios coletivos: ",
          sum(coletivos$pct_coletiva > corte))
}


# ---- 7. Junção ---------------------------------------------------------------
n_antes <- nrow(abstencao)

base_final <- abstencao |>
  left_join(pop,          by = "cod_ibge") |>
  left_join(pib_pc,       by = "cod_ibge") |>
  left_join(urbana,       by = "cod_ibge") |>
  left_join(etaria,       by = "cod_ibge") |>
  left_join(escolaridade, by = "cod_ibge") |>
  left_join(coletivos,    by = "cod_ibge") |>
  mutate(
    # Eleitores aptos / população de 16+ (aproximada pelos 12+ do Censo):
    # mede o quanto o cadastro está inflado
    razao_aptos_pop = aptos / pop_12mais,
    regiao = factor(substr(cod_ibge, 1, 1), levels = 1:5,
                    labels = c("Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")),
    regiao = relevel(regiao, ref = "Sudeste")
  )

stopifnot(nrow(base_final) == n_antes)

# Mesma amostra em todos os modelos
vars_modelo <- c("taxa_abstencao", "log_pib_pc", "log_pop", "pct_urbana",
                 "pct_jovens", "pct_idosos_70", "taxa_alfab", "razao_aptos_pop")

base_analise <- base_final |> filter(if_all(all_of(vars_modelo), ~ !is.na(.x)))

# Descartados por falta de dados: lista e contagem por região, para ver se a
# exclusão se concentra em algum grupo (números citados no texto)
descartados <- anti_join(base_final, base_analise, by = "cod_ibge")
if (nrow(descartados) > 0) {
  descartados |>
    select(cod_ibge, regiao, pop, aptos, all_of(vars_modelo)) |>
    print(n = Inf)
  print(count(descartados, regiao))
}

message("Municípios na base de análise: ", nrow(base_analise),
        " (descartados por falta de dados: ", nrow(base_final) - nrow(base_analise), ")")
message("Municípios com razão aptos/população > 1: ",
        sum(base_analise$razao_aptos_pop > 1))

saveRDS(base_analise, "dados/intermed/base_analise.rds")
summary(base_analise)
