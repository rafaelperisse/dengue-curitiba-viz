# =====================================================================
# Pirâmide etária por sexo
# =====================================================================

library(readr); library(dplyr); library(ggplot2); library(showtext)

sysfonts::font_add_google("Space Mono", "mono2")
sysfonts::font_add_google("Anton", "anton")
showtext_auto(); showtext_opts(dpi = 300)

p <- read_delim("2026-09-02_Casos_Dengue_Base_de_Dados.csv", ";") |>
  transmute(dt    = as.Date(`DATA DA NOTIFICAÇÃO`, "%d/%m/%Y"),
            sexo  = `SEXO`,
            idade = as.numeric(`IDADE (anos)`)) |>
  filter(dt >= "2025-01-01", !is.na(idade), idade >= 0, idade <= 110) |>
  mutate(faixa = cut(idade, c(0,10,20,30,40,50,60,Inf), right = FALSE,
                     labels = c("0 a 9","10 a 19","20 a 29","30 a 39",
                                "40 a 49","50 a 59","60+"))) |>
  summarise(F = sum(sexo == "F"), M = sum(sexo == "M"), .by = faixa) |>
  arrange(faixa) |>
  mutate(i = row_number())

pf  <- sum(p$F) / (sum(p$F) + sum(p$M))
u   <- max(p$F, p$M)
gap <- 0.32 * u; off <- 0.04 * u; mb <- 0.46
mm  <- function(x) x / ggplot2::.pt
pct <- sub("\\.", ",", sprintf("%.1f%%", c(pf, 1 - pf) * 100))

ggplot() +
  annotate("rect", xmin = -(gap + p$F), xmax = -gap,
           ymin = p$i - mb, ymax = p$i + mb, fill = "#D9778F") +
  annotate("rect", xmin = gap, xmax = gap + p$M,
           ymin = p$i - mb, ymax = p$i + mb, fill = "#5B86B0") +
  annotate("text", x = -(gap + p$F) - off, y = p$i, label = p$F, hjust = 1,
           family = "mono2", fontface = "bold", size = mm(9)) +
  annotate("text", x =  gap + p$M + off, y = p$i, label = p$M, hjust = 0,
           family = "mono2", fontface = "bold", size = mm(9)) +
  annotate("text", x = 0, y = p$i, label = p$faixa,
           family = "mono2", fontface = "bold", size = mm(9.5)) +
  annotate("text", x = c(-gap, gap), y = 8.80, label = c("MULHERES","HOMENS"),
           hjust = c(1, 0), family = "mono2", fontface = "bold",
           size = mm(8), color = c("#B8486A", "#2F5C85")) +
  annotate("text", x = c(-gap, gap), y = 8.10, label = pct,
           hjust = c(1, 0), family = "anton",
           size = mm(18), color = c("#B8486A", "#2F5C85")) +
  scale_x_continuous(limits = c(-1.65 * u, 1.65 * u), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0.10, 9.10), expand = c(0, 0)) +
  theme_void()