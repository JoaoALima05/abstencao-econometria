# =============================================================================
# 00_rodar_tudo.R
# Econometria I – Abstenção eleitoral x nível socioeconômico municipal
# João Andrade Lima e Thomas Miguellone Lucio Martins
#
# Roda o projeto inteiro, na ordem. Execute a partir da raiz do repositório
# (abrindo o .Rproj ou com setwd() na pasta do projeto).
#
# Para reproduzir só as descritivas e as regressões, sem baixar os dados do
# TSE e do IBGE, mude baixar_dados para FALSE: o 03 e o 04 leem então o
# base_analise.csv do repositório.
# =============================================================================

baixar_dados <- TRUE

pacotes <- c("data.table", "dplyr", "tidyr", "stringr", "stringi", "sidrar",
             "ggplot2", "flextable", "fixest", "lmtest")
faltando <- setdiff(pacotes, rownames(installed.packages()))
if (length(faltando) > 0) install.packages(faltando)

if (baixar_dados) {
  source("01_baixar_dados.R",   encoding = "UTF-8", echo = TRUE)
  source("02_construir_base.R", encoding = "UTF-8", echo = TRUE)
}
source("03_descritivas.R", encoding = "UTF-8", echo = TRUE)
source("04_regressoes.R",  encoding = "UTF-8", echo = TRUE)

writeLines(capture.output(sessionInfo()), "resultados/sessionInfo.txt")
message("Pronto. Tabelas em resultados/tabelas/ e figuras em resultados/figuras/.")
