# Alfabetização, renda e abstenção eleitoral nos municípios brasileiros

Trabalho de Econometria I (FGV EESP, 2026) de João Andrade Lima e Thomas Miguellone Lucio Martins.

Investigamos se municípios com maior renda e alfabetização têm menor abstenção no 1º turno da eleição presidencial de 2026, sob voto obrigatório. Usamos dados do TSE e do IBGE para os 5.570 municípios e estimamos modelos por MQO com erros-padrão robustos (HC1), controlando por população, urbanização, estrutura etária, região e inflação do cadastro eleitoral.

O texto completo está em [`trabalho.pdf`](trabalho.pdf).

## Como reproduzir

Abra `abstencao-econometria.Rproj` no RStudio (ou rode `setwd()` na pasta do repositório) e rode os scripts na ordem: 01, 02, 03 e 04. Na primeira execução, o download dos dados do TSE e do SIDRA leva alguns minutos.

**Atalho sem download.** Para conferir só as descritivas e as regressões, rode apenas o 03 e o 04. Eles leem o `base_analise.csv` que está no repositório.

## Estrutura

| Arquivo | O que faz |
|---|---|
| `01_baixar_dados.R` | Baixa os brutos do TSE e do SIDRA para `dados/brutos/` (com cache) |
| `02_construir_base.R` | Calcula a abstenção, compatibiliza os códigos TSE e IBGE, constrói as variáveis e salva a base |
| `03_descritivas.R` | Tabela 1 e Figuras 1 e 2 do texto, mais tabelas auxiliares |
| `04_regressoes.R` | Modelos (1) a (4), testes, não linearidade, interação e robustez (Tabela 2 do texto) |
| `R/ler_base.R` | Lê a base de análise (do `.rds` do 02 ou do CSV) |
| `base_analise.csv` | Base final, uma linha por município (separador `;`, decimal `,`) |
| `resultados/` | Tabelas (`.tex`, `.csv`, `.docx`) e figuras geradas pelos scripts |

A Tabela 2 do texto corresponde a `resultados/tabelas/tab_texto_modelos_robustez.tex`.

## Dados

* **TSE**: "Detalhe da apuração por município e zona", eleições gerais de 2026, [Portal de Dados Abertos](https://dadosabertos.tse.jus.br). Baixado em nov. 2026. O TSE pode atualizar o arquivo depois dessa data.
* **IBGE (SIDRA)**: Censo Demográfico 2022 (tabelas 4709, 4712, 9514, 9543 e 9923) e PIB dos Municípios 2022 (tabela 5938).
* **Correspondência TSE e IBGE**: [betafcc/Municipios-Brasileiros-TSE](https://github.com/betafcc/Municipios-Brasileiros-TSE), conferida pelo nome do município dentro de cada UF (7 códigos corrigidos).

### Variáveis de `base_analise.csv`

| Variável | Descrição |
|---|---|
| `cod_tse`, `cod_ibge` | Códigos do município no TSE e no IBGE |
| `aptos`, `comparecimento`, `abstencoes` | Totais do 1º turno para presidente (sem exterior) |
| `taxa_abstencao` | Abstenções / aptos × 100 |
| `pop`, `log_pop` | População residente (Censo 2022) |
| `pib_pc`, `log_pib_pc` | PIB 2022 / população, em reais |
| `pct_urbana` | % da população em situação urbana |
| `pop_12mais` | População de 12+ no Censo, proxy da população de 16+ em 2026 |
| `pct_jovens` | 12 e 13 anos no Censo (16 e 17 em 2026), % da pop. de 12+ |
| `pct_idosos_70` | 66+ no Censo (70+ em 2026), % da pop. de 12+ |
| `taxa_alfab` | Taxa de alfabetização da população de 15+ |
| `pct_coletiva` | % da população fora de domicílios particulares permanentes |
| `razao_aptos_pop` | Aptos / pop. de 12+ (inflação do cadastro) |
| `regiao` | Grande região (referência nas regressões: Sudeste) |

## Uso de inteligência artificial

Usamos o Claude (Anthropic) para dúvidas de programação em R, discussão da metodologia, revisão do texto e formatação em LaTeX. As decisões e a interpretação são dos autores; a seção 3.6 do texto descreve o que foi verificado e corrigido. Conversas: [1](https://claude.ai/share/5711ed2f-173b-44f7-ba9a-43572d230e33) e [2](https://claude.ai/share/e4636d35-67f1-45ed-87eb-493fccad3045).
