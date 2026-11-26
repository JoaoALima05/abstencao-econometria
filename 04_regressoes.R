# =============================================================================
# 04_regressoes.R
# Econometria I – Abstenção eleitoral x nível socioeconômico municipal
# João Andrade Lima e Thomas Miguellone Lucio Martins
#
# Roteiro: escada de modelos por MQO -> Breusch-Pagan -> erros robustos (HC1)
# -> testes t e F -> não-linearidade -> interação -> robustez (sem razão e sem
# extremos).
# Tabelas em resultados/tabelas/ (Word, CSV e LaTeX). A Tabela 2 do texto é
# tab_texto_modelos_robustez.tex; as tabelas 4, 5 e 6 daqui são auxiliares.
# =============================================================================

# install.packages(c("data.table", "dplyr", "fixest", "flextable", "lmtest"))

library(data.table)
library(dplyr)
library(fixest)
library(flextable)
library(lmtest)

dir.create("resultados/tabelas", recursive = TRUE, showWarnings = FALSE)

source("R/ler_base.R")
base <- ler_base()

# Erro-padrão usado em todas as tabelas e testes: robusto à heterocedasticidade
ep <- "hetero"   # HC1 (White)

fmt   <- function(x, digitos = 3) {
  formatC(x, format = "f", digits = digitos, big.mark = ".", decimal.mark = ",")
}
fmt_p <- function(p) ifelse(p < 0.001, "< 0,001", fmt(p, 3))

rotulos_coef <- c(
  log_pib_pc           = "Log do PIB per capita",
  taxa_alfab           = "Taxa de alfabetização (% da pop. de 15+)",
  log_pop              = "Log da população",
  pct_urbana           = "População urbana (% da pop. total)",
  pct_jovens           = "Jovens de 16–17 anos (% da pop. de 16+)",
  pct_idosos_70        = "Idosos de 70+ anos (% da pop. de 16+)",
  razao_aptos_pop      = "Eleitores aptos / pop. de 16+",
  regiaoNorte          = "Norte",
  regiaoNordeste       = "Nordeste",
  regiaoSul            = "Sul",
  `regiaoCentro-Oeste` = "Centro-Oeste",
  `(Intercept)`        = "Constante"
)


# ---- 1. Escada de modelos ----------------------------------------------------
# (1) só renda; (2) + escolaridade; (3) + controles demográficos e de cadastro;
# (4) + dummies de região (referência: Sudeste)
formula_4 <- taxa_abstencao ~ log_pib_pc + taxa_alfab + log_pop + pct_urbana +
  pct_jovens + pct_idosos_70 + razao_aptos_pop + regiao

modelos <- list(
  "(1)" = feols(taxa_abstencao ~ log_pib_pc, data = base),
  "(2)" = feols(taxa_abstencao ~ log_pib_pc + taxa_alfab, data = base),
  "(3)" = feols(taxa_abstencao ~ log_pib_pc + taxa_alfab + log_pop + pct_urbana +
                  pct_jovens + pct_idosos_70 + razao_aptos_pop, data = base),
  "(4)" = feols(formula_4, data = base)
)
m4 <- modelos[["(4)"]]


# ---- 2. Heterocedasticidade --------------------------------------------------
# Breusch-Pagan (versão de Koenker) no modelo (4). H0: homocedasticidade.
m4_lm <- lm(formula_4, data = base)
bp    <- bptest(m4_lm)
print(bp)

# Multicolinearidade imperfeita (SW 6.7): correlação entre renda e
# alfabetização, citada no texto. Quanto mais perto de 1, maior a variância dos
# dois coeficientes (fator 1 - r² na variância do estimador).
message(sprintf("Correlação entre log do PIB pc e alfabetização: %.2f",
                cor(base$log_pib_pc, base$taxa_alfab)))

# Efeito prático da correlação: coeficiente e erro-padrão da renda antes e
# depois de a alfabetização entrar, modelos (1) e (2)
for (m in c("(1)", "(2)")) {
  ct <- coeftable(modelos[[m]], vcov = ep)
  message(sprintf("Modelo %s – log do PIB pc: coef. %.3f | erro-padrão %.3f",
                  m, ct["log_pib_pc", 1], ct["log_pib_pc", 2]))
}


# ---- 3. Testes F conjuntos ---------------------------------------------------
# Renda e alfabetização juntas: com as duas muito correlacionadas, cada uma pode
# não ser significativa sozinha e as duas serem conjuntamente.
f_ses <- wald(m4, keep = "^log_pib_pc$|^taxa_alfab$", vcov = ep, print = FALSE)

# Dummies de região juntas: dado o resto do modelo, a abstenção média difere
# entre as regiões? (H0: as quatro dummies são zero, ou seja, nenhuma região
# difere do Sudeste)
f_reg <- wald(m4, keep = "^regiao", vcov = ep, print = FALSE)


# ---- 4. Não-linearidade: termos quadráticos ----------------------------------
# Renda e escolaridade centralizadas na média, para o coeficiente linear ser o
# efeito no ponto médio (comparável ao modelo 4). Citado no texto.
base_c <- base |>
  mutate(pib_c   = log_pib_pc - mean(log_pib_pc),
         alfab_c = taxa_alfab - mean(taxa_alfab))

m_quad <- feols(taxa_abstencao ~ pib_c + I(pib_c^2) + alfab_c + I(alfab_c^2) +
                  log_pop + pct_urbana + pct_jovens + pct_idosos_70 +
                  razao_aptos_pop + regiao, data = base_c)
etable(m4, m_quad, vcov = ep)

f_quad <- wald(m_quad, keep = "^I\\(", vcov = ep, print = FALSE)

# Pontos de virada dos termos quadráticos (números citados no texto, seção 4.2)
b      <- coef(m_quad)
b_pib2 <- unname(b[grep("pib_c\\^2", names(b))])
b_alf2 <- unname(b[grep("alfab_c\\^2", names(b))])

corte_pib <- exp(mean(base$log_pib_pc) - unname(b["pib_c"]) / (2 * b_pib2))
corte_alf <- mean(base$taxa_alfab) - unname(b["alfab_c"]) / (2 * b_alf2)

message(sprintf("Renda: inclinação se anula em PIB per capita de R$ %.0f", corte_pib))
message(sprintf("Municípios acima desse valor: %.1f%%", mean(base$pib_pc > corte_pib) * 100))
message(sprintf("Alfabetização: inclinação se anula em %.1f%%", corte_alf))
message(sprintf("Municípios abaixo desse nível: %d (%.1f%% da amostra)",
                sum(base$taxa_alfab < corte_alf), mean(base$taxa_alfab < corte_alf) * 100))

# ---- 5. Interação: escolaridade x região -------------------------------------
# Modelo (4) + alfabetização x Nordeste. A dummy de Nordeste já está no modelo
# (via regiao), então só o produto entra. Resultado citado no texto, não em tabela.
#   coeficiente de taxa_alfab            -> efeito da alfabetização fora do Nordeste
#   coeficiente de taxa_alfab:nordeste   -> quanto esse efeito é diferente no Nordeste
#   soma dos dois                        -> efeito da alfabetização no Nordeste
# O teste t da interação responde se o efeito é o mesmo no Nordeste e no resto do país.
base_int <- mutate(base, nordeste = as.integer(regiao == "Nordeste"))
m_int    <- feols(update(formula_4, . ~ . + taxa_alfab:nordeste), data = base_int)

ct_int <- coeftable(m_int, vcov = ep)
print(ct_int[c("taxa_alfab", "taxa_alfab:nordeste"), ])

# A inclinação no Nordeste é diferente de zero? É um teste sobre a soma de dois
# coeficientes (H0: b_alfab + b_interação = 0). Usamos a transformação da
# regressão do SW 7.3: o mesmo modelo, escrito com uma inclinação para cada
# grupo, em vez de inclinação de referência + diferença.
#   alfab_fora = alfabetização x (1 - nordeste)  -> inclinação fora do Nordeste
#   alfab_ne   = alfabetização x nordeste        -> inclinação no Nordeste (= a soma)
# O ajuste é idêntico ao de m_int; o teste t de alfab_ne testa a soma.
base_int <- mutate(base_int,
                   alfab_fora = taxa_alfab * (1 - nordeste),
                   alfab_ne   = taxa_alfab * nordeste)
m_int_ne <- feols(update(formula_4, . ~ . - taxa_alfab + alfab_fora + alfab_ne),
                  data = base_int)
print(coeftable(m_int_ne, vcov = ep)[c("alfab_fora", "alfab_ne"), ])


# ---- 6. Robustez -------------------------------------------------------------
# Sem razão: compara com o (4) para ver o viés de omitir o cadastro inflado.
# Sem extremos: exclui o 1% mais baixo e o 1% mais alto da razão aptos/população
# (corte num regressor, não na variável dependente).
lim_razao    <- quantile(base$razao_aptos_pop, c(0.01, 0.99))
base_sem_ext <- filter(base, between(razao_aptos_pop, lim_razao[1], lim_razao[2]))
message("Municípios excluídos (extremos da razão): ", nrow(base) - nrow(base_sem_ext))

robustez <- list(
  "Modelo (4)"   = m4,
  "Sem razão"    = feols(update(formula_4, . ~ . - razao_aptos_pop), data = base),
  "Sem extremos" = feols(formula_4, data = base_sem_ext)
)

etable(modelos,  vcov = ep)
etable(robustez, vcov = ep)

# ---- 7. Tabelas --------------------------------------------------------------
estrelas <- function(p) ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.1, "*", "")))

# Coeficiente com estrelas e erro-padrão robusto entre parênteses
montar_tabela <- function(modelos, coef_map) {
  coefs  <- lapply(modelos, coeftable, vcov = ep)
  termos <- intersect(names(coef_map), unique(unlist(lapply(coefs, rownames))))
  
  linhas <- lapply(termos, function(t) {
    est <- sapply(coefs, \(ct) if (t %in% rownames(ct))
      paste0(fmt(ct[t, 1]), estrelas(ct[t, 4])) else "")
    se  <- sapply(coefs, \(ct) if (t %in% rownames(ct))
      paste0("(", fmt(ct[t, 2]), ")") else "")
    rbind(c(coef_map[[t]], est), c("", se))
  })
  
  ajuste <- rbind(
    c("Observações", sapply(modelos, \(m) fmt(nobs(m), 0))),
    c("R²",          sapply(modelos, \(m) fmt(r2(m, "r2")))),
    c("R² ajustado", sapply(modelos, \(m) fmt(r2(m, "ar2"))))
  )
  
  tab <- as.data.frame(rbind(do.call(rbind, linhas), ajuste))
  names(tab) <- c("Variável", names(modelos))
  tab
}

formatar_tabela <- function(df, nota) {
  flextable(df) |>
    theme_booktabs() |>
    align(j = 2:ncol(df), align = "center", part = "all") |>
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

nota_base <- paste(
  "Nota: variável dependente é a taxa de abstenção (% dos aptos) no 1º turno da",
  "eleição presidencial de 2026. Erros-padrão robustos à heterocedasticidade (HC1)",
  "entre parênteses. *** p < 0,01; ** p < 0,05; * p < 0,1. Fonte: TSE e IBGE."
)

# Tabela 4: modelos principais
tab4 <- montar_tabela(modelos, rotulos_coef)
fwrite(tab4, "resultados/tabelas/tab4_regressoes.csv", sep = ";", bom = TRUE)
ft4 <- formatar_tabela(tab4, paste(nota_base, "Categoria de referência da região: Sudeste."))

# Tabela 5: robustez
tab5 <- montar_tabela(robustez, rotulos_coef)
fwrite(tab5, "resultados/tabelas/tab5_robustez.csv", sep = ";", bom = TRUE)
ft5 <- formatar_tabela(tab5, paste(
  nota_base,
  "Na coluna sem razão, a razão aptos/população sai do modelo. Na coluna sem",
  "extremos, excluem-se os municípios abaixo do percentil 1 e acima do 99 da razão."))

# Tabela 6: testes
testes <- tibble(
  Teste           = c("Breusch-Pagan (Koenker)",
                      "F conjunto: renda e alfabetização",
                      "F conjunto: dummies de região",
                      "F conjunto: termos quadráticos"),
  `Hipótese nula` = c("Homocedasticidade",
                      "Coeficientes de renda e alfabetização iguais a zero",
                      "Nenhuma região difere do Sudeste (quatro dummies iguais a zero)",
                      "Coeficientes dos quadráticos iguais a zero"),
  Estatística     = fmt(c(bp$statistic, f_ses$stat, f_reg$stat, f_quad$stat)),
  `Valor-p`       = fmt_p(c(bp$p.value, f_ses$p, f_reg$p, f_quad$p))
)
print(testes)
fwrite(testes, "resultados/tabelas/tab6_testes.csv", sep = ";", bom = TRUE)
ft6 <- formatar_tabela(testes, paste(
  "Nota: testes no modelo (4). Os testes F usam erros-padrão robustos (HC1).",
  "Fonte: TSE e IBGE."))

salvar_tex(tab4,   "tab4_regressoes")
# Tabela 2 do texto: modelos (1)-(4) e as duas robustezes na mesma tabela
tab_texto <- montar_tabela(c(modelos, list("(5) Sem razão"    = robustez[["Sem razão"]],
                                           "(6) Sem extremos" = robustez[["Sem extremos"]])),
                           rotulos_coef)
fwrite(tab_texto, "resultados/tabelas/tab_texto_modelos_robustez.csv", sep = ";", bom = TRUE)
salvar_tex(tab_texto, "tab_texto_modelos_robustez")
salvar_tex(tab5,   "tab5_robustez")
salvar_tex(testes, "tab6_testes")

save_as_docx(
  "Tabela 4 – Determinantes da abstenção municipal" = ft4,
  "Tabela 5 – Testes de robustez"                   = ft5,
  "Tabela 6 – Testes de hipótese e diagnóstico"     = ft6,
  path = "resultados/tabelas/tabelas_regressao.docx"
)
save_as_html(
  "Tabela 4 – Determinantes da abstenção municipal" = ft4,
  "Tabela 5 – Testes de robustez"                   = ft5,
  "Tabela 6 – Testes de hipótese e diagnóstico"     = ft6,
  path = "resultados/tabelas/tabelas_regressao.html"
)

# Abre as tabelas no navegador (só em sessão interativa, como no RStudio)
if (interactive()) browseURL(normalizePath("resultados/tabelas/tabelas_regressao.html"))
