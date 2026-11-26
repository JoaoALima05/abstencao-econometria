# =============================================================================
# R/ler_base.R
# Lê a base de análise usada pelo 03 e pelo 04.
#
# Se o 02 já rodou, usa dados/intermed/base_analise.rds. Se não, lê o
# base_analise.csv que está no repositório. Assim dá para reproduzir as
# descritivas e as regressões sem baixar de novo os dados do TSE e do IBGE.
# =============================================================================

ler_base <- function() {
  arq_rds <- "dados/intermed/base_analise.rds"
  if (file.exists(arq_rds)) {
    message("Lendo a base de ", arq_rds)
    return(readRDS(arq_rds))
  }

  message("Base .rds não encontrada; lendo base_analise.csv")
  base <- data.table::fread("base_analise.csv", sep = ";", dec = ",",
                            encoding = "UTF-8")
  base <- as.data.frame(base)

  # No CSV a região volta como texto: refazemos o fator com o Sudeste
  # como categoria de referência, como no 02.
  base$regiao <- factor(base$regiao,
                        levels = c("Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste"))
  base$regiao <- relevel(base$regiao, ref = "Sudeste")

  stopifnot(nrow(base) == 5570, !anyNA(base$regiao))
  base
}
