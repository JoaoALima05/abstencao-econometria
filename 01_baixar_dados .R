# =============================================================================
# 01_baixar_dados.R
# Econometria I – Abstenção eleitoral x nível socioeconômico municipal
# João Andrade Lima e Thomas Miguellone Lucio Martins
#
# Este script só baixa os dados brutos e os salva em dados/brutos/.
# Cada download é feito apenas se o arquivo ainda não existir, de modo que o
# script roda sem internet quando os brutos já estão no repositório.
# A limpeza e a construção das variáveis ficam em 02_construir_base.R.
# =============================================================================


# ---- 0. Pacotes --------------------------------------------------------------
# install.packages(c("data.table", "sidrar", "dplyr"))

library(data.table)
library(sidrar)
library(dplyr)

options(timeout = 900)  # os arquivos do TSE são grandes


# ---- 1. Pastas e funções auxiliares ------------------------------------------
dir.create("dados/brutos", recursive = TRUE, showWarnings = FALSE)

# Lê o .rds se ele já existir; se não, executa a função de download e salva.
em_cache <- function(arquivo, baixar) {
  if (file.exists(arquivo)) {
    message("Lendo do disco: ", arquivo)
    return(readRDS(arquivo))
  }
  message("Baixando: ", arquivo)
  dados <- baixar()
  saveRDS(dados, arquivo)
  dados
}

# Para o script se a tabela não trouxer o número esperado de municípios.
conferir_municipios <- function(df, nome, esperado = 5570) {
  n <- n_distinct(df$`Município (Código)`)
  if (n != esperado) {
    stop(sprintf("Tabela %s: %d municípios (esperado: %d).", nome, n, esperado))
  }
  message(sprintf("%-14s %d municípios", nome, n))
  invisible(df)
}


# ---- 2. TSE: detalhe da apuração por município e zona ------------------------
# Arquivo "Detalhe da apuração por município e zona" de 2026,
# do Portal de Dados Abertos do TSE.
ano_eleicao <- 2026

url_tse <- paste0(
  "https://cdn.tse.jus.br/estatistica/sead/odsele/detalhe_votacao_munzona/",
  "detalhe_votacao_munzona_", ano_eleicao, ".zip"
)
zip_tse <- file.path("dados/brutos", basename(url_tse))

tse <- em_cache(
  file.path("dados/brutos", paste0("tse_bruto_", ano_eleicao, ".rds")),
  function() {
    if (!file.exists(zip_tse)) download.file(url_tse, destfile = zip_tse, mode = "wb")
    
    pasta_tse <- file.path("dados/brutos", paste0("tse_", ano_eleicao))
    unzip(zip_tse, exdir = pasta_tse)
    
    csvs_tse   <- list.files(pasta_tse, pattern = "\\.csv$", full.names = TRUE)
    csv_brasil <- csvs_tse[grepl("BRASIL", csvs_tse)]
    
    # Lemos só o consolidado BRASIL quando ele existe, para não duplicar linhas
    ler_tse <- function(arq) fread(arq, sep = ";", encoding = "Latin-1", dec = ",")
    if (length(csv_brasil) == 1) ler_tse(csv_brasil)
    else rbindlist(lapply(csvs_tse, ler_tse), fill = TRUE)
  }
)

# Pelo leiame, cada linha é município x zona x cargo x turno
dim(tse)

# Colunas usadas no 02; o layout do arquivo pode mudar entre eleições
colunas_tse <- c("CD_ELEICAO", "DS_ELEICAO", "CD_TIPO_ELEICAO", "SG_UF",
                 "CD_MUNICIPIO", "NM_MUNICIPIO", "NR_TURNO", "CD_CARGO",
                 "QT_APTOS", "QT_COMPARECIMENTO", "QT_ABSTENCOES")
faltando <- setdiff(colunas_tse, names(tse))
if (length(faltando) > 0) {
  stop("Colunas ausentes no arquivo do TSE: ", paste(faltando, collapse = ", "))
}


# ---- 3. Correspondência código TSE -> código IBGE ----------------------------
# O TSE tem um código de município próprio, diferente do IBGE. Usamos a tabela
# do repositório betafcc/Municipios-Brasileiros-TSE.
arq_dep <- "dados/brutos/correspondencia_tse_ibge.csv"
if (!file.exists(arq_dep)) {
  url_dep <- paste0(
    "https://raw.githubusercontent.com/betafcc/Municipios-Brasileiros-TSE/",
    "master/municipios_brasileiros_tse.csv"
  )
  fwrite(fread(url_dep, encoding = "UTF-8"), arq_dep)
}


# ---- 4. IBGE via SIDRA -------------------------------------------------------
# Tabelas usadas:
#   4709 – População residente (Censo 2022)
#   5938 – PIB dos Municípios (2022)
#   9543 – Taxa de alfabetização, 15 anos ou mais (Censo 2022)
#   9923 – População por situação do domicílio (Censo 2022)
#   4712 – Moradores em domicílios particulares permanentes ocupados (Censo 2022)
#   9514 – População por idade (Censo 2022)

populacao <- em_cache("dados/brutos/sidra_populacao.rds",
                      function() get_sidra(api = "/t/4709/n6/all/v/93/p/2022"))

# PIB de 2022, para coincidir com o ano do Censo
pib <- em_cache("dados/brutos/sidra_pib.rds",
                function() get_sidra(api = "/t/5938/n6/all/v/37/p/2022"))

alfabetizacao <- em_cache("dados/brutos/sidra_alfabetizacao.rds",
                          function() get_sidra(api = "/t/9543/n6/all/v/all/p/2022"))

# Pedimos só a variável 93 (população residente, em pessoas). Sem isso,
# get_sidra() traz todas as variáveis não percentuais da tabela.
urbanizacao <- em_cache("dados/brutos/sidra_urbanizacao.rds",
                        function() get_sidra(x = 9923, variable = 93,
                                             period = "2022", geo = "City"))

# Moradores em domicílios particulares permanentes ocupados (Censo 2022).
# A diferença em relação à população total é a população em domicílios
# coletivos (presídios, asilos, quartéis) e improvisados.
domicilios <- em_cache("dados/brutos/sidra_domicilios.rds",
                       function() get_sidra(api = "/t/4712/n6/all/v/all/p/2022"))

# A API do SIDRA limita o número de valores por consulta. Com as idades simples,
# a consulta por UF fica abaixo do limite em todas as UFs, exceto em MG, que tem
# o maior número de municípios (853). Por isso, MG é baixada em dois grupos de
# mesorregiões.
idade <- em_cache("dados/brutos/sidra_idade.rds", function() {
  ufs <- c(11:17, 21:29, 32:33, 35, 41:43, 50:53)  # todas as UFs exceto MG
  sem_mg <- lapply(ufs, function(uf) {
    Sys.sleep(1)
    get_sidra(x = 9514, variable = 93, period = "2022",
              geo = "City", geo.filter = list("State" = uf),
              classific = "c287", category = "all")
  })
  mg_1 <- get_sidra(api = "/t/9514/n6/in%20n8%203101,3102,3103,3104,3105,3106/v/93/p/2022/c287/all")
  Sys.sleep(1)
  mg_2 <- get_sidra(api = "/t/9514/n6/in%20n8%203107,3108,3109,3110,3111,3112/v/93/p/2022/c287/all")
  bind_rows(sem_mg, mg_1, mg_2)
})


# ---- 5. Conferência das tabelas baixadas -------------------------------------
# Em 9543, 4712 e 9923 o conteúdo depende do comportamento padrão da API.
# Imprimimos as variáveis e categorias que vieram e paramos o script se faltar
# alguma que o 02 usa.
print(unique(alfabetizacao$Variável))
stopifnot("Tabela 9543 sem a taxa de alfabetização" =
            any(grepl("taxa de alfabetiza", alfabetizacao$Variável, ignore.case = TRUE)))

print(unique(urbanizacao$Variável))
print(unique(urbanizacao$`Situação do domicílio`))
stopifnot("Tabela 9923 sem as categorias Total e Urbana" =
            all(c("Total", "Urbana") %in% urbanizacao$`Situação do domicílio`))

print(unique(domicilios$Variável))
stopifnot("Tabela 4712 sem a variável de moradores" =
            any(grepl("^Moradores", domicilios$Variável)))

# Todas as tabelas do Censo 2022 devem cobrir os 5.570 municípios
conferir_municipios(populacao,     "4709 (pop.)")
conferir_municipios(pib,           "5938 (PIB)")
conferir_municipios(alfabetizacao, "9543 (alfab.)")
conferir_municipios(urbanizacao,   "9923 (urb.)")
conferir_municipios(domicilios,    "4712 (dom.)")
conferir_municipios(idade,         "9514 (idade)")