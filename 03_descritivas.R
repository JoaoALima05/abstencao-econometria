# =============================================================================
# 03_descritivas.R
# Econometria I – Abstenção eleitoral x nível socioeconômico municipal
# João Andrade Lima e Thomas Miguellone Lucio Martins
#
# Tabelas descritivas (Word + CSV + LaTeX) e as Figuras 1 e 2 do texto (PNG),
# a partir da base do 02 (ou do base_analise.csv, ver R/ler_base.R).
# A Tabela 1 e as Figuras 1 e 2 têm a numeração do texto. As tabelas 2 e 3
# daqui (médias por região e correlações) são auxiliares e não aparecem no PDF.
# =============================================================================

# install.packages(c("data.table", "dplyr", "tidyr", "ggplot2", "flextable"))

library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(flextable)

dir.create("resultados/tabelas", recursive = TRUE, showWarnings = FALSE)
dir.create("resultados/figuras", recursive = TRUE, showWarnings = FALSE)

source("R/ler_base.R")
base <- ler_base()
theme_set(theme_minimal(base_size = 11))

# Números no padrão brasileiro
fmt <- function(x, digitos = 2) {
  formatC(x, format = "f", digits = digitos, big.mark = ".", decimal.mark = ",")
}

salvar_csv <- function(tab, nome) {
  fwrite(tab, file.path("resultados/tabelas", paste0(nome, ".csv")),
         sep = ";", dec = ",", bom = TRUE)
}

salvar_figura <- function(grafico, nome, largura = 7, altura = 4.5) {
  print(grafico)
  ggsave(file.path("resultados/figuras", paste0(nome, ".png")),
         grafico, width = largura, height = altura, dpi = 300, bg = "white")
}

formatar_tabela <- function(df, nota) {
  flextable(df) |>
    theme_booktabs() |>
    align(j = 2:ncol(df), align = "right", part = "all") |>
    add_footer_lines(nota) |>
    fontsize(size = 9, part = "all") |>
    autofit()
}

# Tabela em LaTeX (booktabs) para colar no Overleaf: \input{arquivo.tex}
# Só o "tabular"; título, fonte e nota vão no texto, como pede a ABNT.
salvar_tex <- function(df, nome) {
  esc <- function(x) {
    x <- gsub("([%$&_#])", "\\\\\\1", x)
    x <- gsub("R²", "R\\\\textsuperscript{2}", x)
    x <- gsub("<", "$<$", x)
    # sinal de menos tipográfico nos números (ex.: -0,227 -> $-$0,227)
    gsub("(^|\\()-([0-9])", "\\1$-$\\2", x)
  }
  corpo <- as.data.frame(lapply(df, \(col) esc(as.character(col))))
  linha <- function(v) paste0(paste(v, collapse = " & "), " \\\\")
  linhas <- apply(corpo, 1, linha)
  # Linha separando coeficientes das estatísticas de ajuste (tabelas de regressão)
  i_obs <- which(corpo[[1]] == "Observações")
  if (length(i_obs) == 1) linhas <- append(linhas, "\\midrule", after = i_obs - 1)
  tex <- c(paste0("\\begin{tabular}{l", strrep("c", ncol(df) - 1), "}"),
           "\\toprule", linha(esc(names(df))), "\\midrule",
           linhas, "\\bottomrule", "\\end{tabular}")
  con <- file(file.path("resultados/tabelas", paste0(nome, ".tex")), "w", encoding = "UTF-8")
  writeLines(tex, con)
  close(con)
}

rotulos <- c(
  taxa_abstencao  = "Taxa de abstenção (% dos aptos)",
  log_pib_pc      = "Log do PIB per capita",
  log_pop         = "Log da população",
  pct_urbana      = "População urbana (% da pop. total)",
  pct_jovens      = "Jovens de 16–17 anos (% da pop. de 16+)",
  pct_idosos_70   = "Idosos de 70+ anos (% da pop. de 16+)",
  taxa_alfab      = "Taxa de alfabetização (% da pop. de 15+)",
  razao_aptos_pop = "Eleitores aptos / pop. de 16+"
)

nota_16mais <- paste("População de 16 anos ou mais em 2026 aproximada pela",
                     "população de 12 anos ou mais no Censo 2022.")


# ---- Tabela 1: estatísticas descritivas --------------------------------------
vars_tab1 <- c("taxa_abstencao", "pib_pc", "log_pib_pc", "pop", "log_pop",
               "pct_urbana", "pct_jovens", "pct_idosos_70", "taxa_alfab",
               "razao_aptos_pop")
rotulos_tab1 <- c(rotulos,
                  pib_pc = "PIB per capita (R$)",
                  pop    = "População (habitantes)")[vars_tab1]

tab1_num <- base |>
  select(all_of(vars_tab1)) |>
  pivot_longer(everything(), names_to = "var", values_to = "valor") |>
  group_by(var) |>
  summarise(Média           = mean(valor),
            `Desvio-padrão` = sd(valor),
            Mínimo          = min(valor),
            Mediana         = median(valor),
            Máximo          = max(valor),
            .groups = "drop") |>
  mutate(var = factor(var, levels = vars_tab1)) |>
  arrange(var)

salvar_csv(tab1_num, "tab1_estatisticas_descritivas")

tab1 <- tab1_num |>
  mutate(across(Média:Máximo,
                ~ if_else(var %in% c("pib_pc", "pop"), fmt(.x, 0), fmt(.x, 2))),
         Variável = rotulos_tab1[as.character(var)]) |>
  select(Variável, Média:Máximo)

ft1 <- formatar_tabela(tab1, paste(
  paste0("Nota: unidade de observação é o município (N = ", fmt(nrow(base), 0), ")."),
  nota_16mais, "Fonte: TSE e IBGE."))


# ---- Municípios com maior PIB per capita (para a seção de limitações) --------
# O PIB per capita mede produção, não renda dos moradores.
# Os nomes vêm do bruto do SIDRA; sem ele (rodando só pelo CSV), mostra o código.
arq_pop <- "dados/brutos/sidra_populacao.rds"
top_pib <- base |> arrange(desc(pib_pc)) |> head(10)
if (file.exists(arq_pop)) {
  nomes_mun <- readRDS(arq_pop) |>
    transmute(cod_ibge = as.integer(`Município (Código)`), municipio = Município)
  top_pib <- left_join(top_pib, nomes_mun, by = "cod_ibge")
} else {
  top_pib <- mutate(top_pib, municipio = as.character(cod_ibge))
}
top_pib |> select(municipio, pib_pc, pop, taxa_abstencao) |> print()


# ---- Tabela 2: médias por região ---------------------------------------------
tab2_num <- bind_rows(
  mutate(base, Região = as.character(regiao)),
  mutate(base, Região = "Brasil")
) |>
  group_by(Região) |>
  summarise(Municípios = n(),
            across(all_of(names(rotulos)), mean),
            .groups = "drop") |>
  mutate(Região = factor(Região, levels = c("Norte", "Nordeste", "Sudeste", "Sul",
                                            "Centro-Oeste", "Brasil"))) |>
  arrange(Região)

salvar_csv(tab2_num, "tab2_por_regiao")

tab2 <- tab2_num |>
  pivot_longer(-Região, names_to = "var", values_to = "valor") |>
  mutate(valor = if_else(var == "Municípios", fmt(valor, 0), fmt(valor, 2)),
         var   = factor(var, levels = c("Municípios", names(rotulos)))) |>
  pivot_wider(names_from = Região, values_from = valor) |>
  arrange(var) |>
  mutate(Variável = c(Municípios = "Número de municípios", rotulos)[as.character(var)],
         .before = 1) |>
  select(-var)

ft2 <- formatar_tabela(tab2, paste(
  "Nota: médias simples entre municípios.", nota_16mais, "Fonte: TSE e IBGE."))


# ---- Teto da alfabetização por região (motivação da interação) --------------
# Auxiliar (não aparece no texto): mediana e parcela de municípios acima de 95%.
alfab_teto <- base |>
  group_by(regiao) |>
  summarise(municipios   = n(),
            mediana      = median(taxa_alfab),
            pct_acima_95 = mean(taxa_alfab > 95) * 100,
            .groups = "drop")
print(alfab_teto)
salvar_csv(alfab_teto, "alfab_teto_por_regiao")


# ---- Tabela 3: correlações (multicolinearidade renda x escolaridade) ---------
vars_cor <- names(rotulos)
k        <- length(vars_cor)
correl   <- cor(base[vars_cor])

salvar_csv(data.table(Variável = vars_cor, correl), "tab3_correlacoes")

tri <- matrix("", k, k)
tri[lower.tri(tri)] <- fmt(correl[lower.tri(correl)], 2)
diag(tri) <- "—"

tab3 <- data.frame(paste0("(", 1:k, ") ", rotulos), tri, check.names = FALSE)
names(tab3) <- c("Variável", paste0("(", 1:k, ")"))

ft3 <- formatar_tabela(tab3, "Nota: correlações de Pearson entre municípios. Fonte: TSE e IBGE.")

salvar_tex(tab1, "tab1_estatisticas_descritivas")
salvar_tex(tab2, "tab2_por_regiao")

save_as_docx(
  "Tabela 1 – Estatísticas descritivas"     = ft1,
  "Tabela 2 – Médias por região"            = ft2,
  "Tabela 3 – Matriz de correlação"         = ft3,
  path = "resultados/tabelas/tabelas_descritivas.docx"
)
save_as_html(
  "Tabela 1 – Estatísticas descritivas"     = ft1,
  "Tabela 2 – Médias por região"            = ft2,
  "Tabela 3 – Matriz de correlação"         = ft3,
  path = "resultados/tabelas/tabelas_descritivas.html"
)


# ---- Figura 1 do texto: abstenção contra cada explicativa ------------------
# Dispersão com a reta de MQO da regressão simples em cada painel.
# No painel da razão, só P1–P99 para os extremos não achatarem o eixo.
lim_razao <- quantile(base$razao_aptos_pop, c(0.01, 0.99))

fig1 <- base |>
  select(all_of(names(rotulos))) |>
  pivot_longer(-taxa_abstencao, names_to = "var", values_to = "valor") |>
  filter(!(var == "razao_aptos_pop" & (valor < lim_razao[1] | valor > lim_razao[2]))) |>
  mutate(var = factor(var, levels = names(rotulos)[-1], labels = rotulos[-1])) |>
  ggplot(aes(valor, taxa_abstencao)) +
  geom_point(alpha = 0.15, size = 0.5) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = "firebrick") +
  facet_wrap(~ var, scales = "free_x", ncol = 3, labeller = label_wrap_gen(width = 28)) +
  labs(x = NULL, y = rotulos[["taxa_abstencao"]])
salvar_figura(fig1, "fig1_dispersao", largura = 9, altura = 7.5)


# ---- Figura 2 do texto: razão aptos / população (problema do cadastro) -------
fig2 <- ggplot(base, aes(razao_aptos_pop)) +
  geom_histogram(bins = 60, fill = "grey50", color = "white") +
  geom_vline(xintercept = 1, color = "firebrick", linetype = "dashed") +
  labs(x = rotulos[["razao_aptos_pop"]], y = "Número de municípios")
salvar_figura(fig2, "fig2_razao_aptos_pop")

# Abre as tabelas no navegador (só em sessão interativa, como no RStudio)
if (interactive()) browseURL(normalizePath("resultados/tabelas/tabelas_descritivas.html"))
