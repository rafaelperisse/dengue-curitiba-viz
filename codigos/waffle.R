# =====================================================================
# Waffle: origem de infecção (1 quadrado = 1%)
# =====================================================================

library(readr); library(dplyr); library(ggplot2); library(showtext)

sysfonts::font_add_google("Space Mono", "mono2")
showtext_auto(); showtext_opts(dpi = 300)

d <- read_delim("2026-09-02_Casos_Dengue_Base_de_Dados.csv", ";") |>
  transmute(dt   = as.Date(`DATA DA NOTIFICAÇÃO`, "%d/%m/%Y"),
            tipo = `LOCAL DE INFECÇÃO`) |>
  filter(!is.na(dt), dt >= "2025-01-01",
         tipo %in% c("Autóctone", "Importado")) |>
  count(tipo)

pct_auto <- round(100 * d$n[d$tipo == "Autóctone"] / sum(d$n))

cel <- tibble(k = 0:99, row = k %/% 10, col = k %% 10, aut = k < pct_auto)

ggplot(cel, aes(col, row, fill = aut)) +
  geom_tile(width = 0.86, height = 0.86) +
  scale_fill_manual(
    values = c(`TRUE` = "#E8412F", `FALSE` = "#8C8C8C"),
    breaks = c(TRUE, FALSE),
    labels = c(paste0(pct_auto, "% AUTÓCTONES"),
               paste0(100 - pct_auto, "% IMPORTADOS"))) +
  coord_equal(expand = FALSE) +
  theme_void(base_family = "mono2") +
  theme(legend.position  = "top",
        legend.direction = "vertical",
        legend.title     = element_blank(),
        legend.text      = element_text(face = "bold", size = 10))