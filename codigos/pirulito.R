# =====================================================================
# Pirulito: 7 bairros com mais casos
# =====================================================================

library(readr); library(dplyr); library(ggplot2)
library(stringr); library(showtext)

sysfonts::font_add_google("Space Mono", "mono2")
showtext_auto(); showtext_opts(dpi = 300)

top7 <- read_delim("2026-09-02_Casos_Dengue_Base_de_Dados.csv", ";") |>
  transmute(dt     = as.Date(`DATA DA NOTIFICAÇÃO`, "%d/%m/%Y"),
            bairro = str_squish(str_remove(`BAIRRO DE RESIDÊNCIA`, "\\s*\\(.*\\)$"))) |>
  filter(dt >= "2025-01-01", !is.na(bairro)) |>
  count(bairro, sort = TRUE) |>
  slice_head(n = 7) |>
  mutate(bairro = factor(bairro, levels = rev(bairro)),
         cor    = if_else(n == max(n), "#8E1B10", "#E8412F"))

ggplot(top7, aes(n, bairro)) +
  geom_segment(aes(x = 0, xend = n, yend = bairro), color = top7$cor,
               linewidth = 0.6) +
  geom_point(color = top7$cor, size = 3) +
  geom_text(aes(label = n), hjust = -0.4, family = "mono2",
            fontface = "bold", size = 9.5 / .pt) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme_void(base_family = "mono2") +
  theme(axis.text.y = element_text(hjust = 1, size = 7.5, color = "#1B1B1B"),
        plot.margin = margin(6, 24, 6, 6))