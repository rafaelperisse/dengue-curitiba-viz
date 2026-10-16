# =====================================================================
# Barras semanais: casos por semana (data dos primeiros sintomas)
# =====================================================================

library(readr); library(dplyr); library(ggplot2)
library(lubridate); library(showtext)

sysfonts::font_add_google("Space Mono", "mono2")
showtext_auto(); showtext_opts(dpi = 300)

sem <- read_delim("2026-09-02_Casos_Dengue_Base_de_Dados.csv", ";") |>
  transmute(dt = as.Date(`DATA DOS PRIMEIROS SINTOMAS`, "%d/%m/%Y")) |>
  filter(dt >= "2025-01-01") |>
  count(semana = floor_date(dt, "week", week_start = 7))

pico <- slice_max(sem, n, n = 1, with_ties = FALSE)

ggplot(sem, aes(semana, n)) +
  geom_col(fill = "#E8412F", alpha = 0.9, width = 6) +
  geom_col(data = pico, fill = "#8E1B10", width = 6) +
  annotate("text", x = pico$semana, y = pico$n,
           label = pico$n, vjust = -0.5, hjust = 0.5,
           family = "mono2", fontface = "bold", color = "#8E1B10", size = 3.5) +
  annotate("text", x = pico$semana, y = pico$n,
           label = paste0("semana de ", format(pico$semana, "%d/%m")),
           vjust = -0.5, hjust = -0.1,
           family = "mono2", color = "#8E1B10", size = 3) +
  scale_x_date(date_labels = "%b/%y", date_breaks = "4 months") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  theme_minimal(base_family = "mono2") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted",
                                          color = "#1B1B1B", linewidth = 0.25),
        axis.title = element_blank(),
        axis.text  = element_text(color = "#1B1B1B"))