# Dengue em Curitiba

Cinco gráficos sobre notificações de dengue em Curitiba.

| Script | Gráfico |
|---|---|
| `mapa.R` | Coroplético: casos por 10 mil hab. por bairro |
| `pirulito.R` | Top 7 bairros por total absoluto |
| `histograma.R` | Casos por semana |
| `piramide.R` | Pirâmide etária por sexo |
| `waffle.R` | Origem de infecção (autóctone × importado) |

## Dados

`2026-09-02_Casos_Dengue_Base_de_Dados.csv` no mesmo diretório. Separador `;`, UTF-8.

Colunas usadas: `DATA DA NOTIFICAÇÃO`, `DATA DOS PRIMEIROS SINTOMAS`,
`BAIRRO DE RESIDÊNCIA`, `SEXO`, `IDADE (anos)`, `LOCAL DE INFECÇÃO`.
Filtro comum: notificações a partir de 2025-01-01.

O mapa usa a malha de bairros do `geobr` (Censo 2022, cacheada em
`bairros_curitiba_geobr.rds` na primeira execução) e população por bairro
embutida como `tribble`.

## Dependências

install.packages(c("readr","dplyr","ggplot2","stringr","stringi",
                   "lubridate","sf","geobr","scales","showtext","purrr"))

Fontes Space Mono e Anton baixadas via sysfonts::font_add_google() — precisa
de internet na primeira execução.

## Rodar

Abra o script no RStudio, Ctrl+Shift+Enter. O gráfico aparece no painel Plots.
A primeira execução do mapa.R baixa ~50 MB do geobr; depois usa o cache.
