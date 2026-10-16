# =====================================================================
# Mapa: casos por 10 mil hab. por bairro de Curitiba
# =====================================================================

library(readr); library(dplyr); library(sf); library(geobr)
library(ggplot2); library(scales); library(showtext); library(stringi)
library(stringr); library(purrr)

sysfonts::font_add_google("Space Mono", "mono2")
showtext_auto(); showtext_opts(dpi = 300)

arquivo      <- "2026-09-02_Casos_Dengue_Base_de_Dados.csv"
cache_mapa   <- "bairros_curitiba_geobr.rds"
cod_curitiba <- 4106902
data_corte   <- as.Date("2025-01-01")
inc_max      <- 90
pop_minima   <- 10

cor_tinta <- "#1B1B1B"; cor_cinza <- "#8C8C8C"; cor_nota <- "#2A2620"
ramp_cont <- colour_ramp(c("#F4BE7E","#F09A5A","#E8412F","#B83E20","#540D05"))

pt2mm <- function(pt) pt / ggplot2::.pt
num0  <- function(x) number(x, accuracy = 1, big.mark = ".", decimal.mark = ",")
limpa <- function(x) str_squish(toupper(stri_trans_general(x, "Latin-ASCII")))
chave <- function(x) limpa(str_remove(x, "\\s*\\(.*\\)\\s*$"))

anes <- function(s) map_dfr(seq_len(nrow(s)), function(i) {
  co <- st_coordinates(s[i, ]); cn <- colnames(co)
  L2 <- if ("L2" %in% cn) co[,"L2"] else 1
  L3 <- if ("L3" %in% cn) co[,"L3"] else 1
  data.frame(x = co[,"X"], y = co[,"Y"], feat = i, ring = co[,"L1"],
             poly = paste(i, L2, L3, sep = "_")) })

resolve <- function(chaves, alvo, cand) {
  out <- setNames(rep(NA_character_, length(chaves)), chaves)
  for (k in chaves) {
    if (k %in% alvo) { out[k] <- k; next }
    c2 <- cand[[k]]; ok <- c2[c2 %in% alvo]
    if (length(ok) > 0) { out[k] <- ok[1]; next }
    d <- adist(k, alvo)[1, ] / pmax(nchar(k), nchar(alvo))
    if (min(d) <= 0.2) out[k] <- alvo[which.min(d)] }
  out }

casos <- read_delim(arquivo, ";") |>
  transmute(dt = as.Date(`DATA DA NOTIFICAÇÃO`, "%d/%m/%Y"),
            bk = chave(`BAIRRO DE RESIDÊNCIA`)) |>
  filter(!is.na(dt), dt >= data_corte, bk != "") |> count(bk, name = "n")

pop_2022 <- tribble(
  ~bairro, ~pop,
  "Abranches",13991,"Água Verde",49573,"Ahú",11859,"Alto Boqueirão",51178,
  "Alto da Glória",6242,"Alto da Rua XV",8102,"Atuba",19958,"Augusta",7399,
  "Bacacheri",23989,"Bairro Alto",42767,"Barreirinha",15986,"Batel",9913,
  "Bigorrilho",25590,"Boa Vista",31983,"Bom Retiro",5001,"Boqueirão",65618,
  "Butiatuvinha",13501,"Cabral",13723,"Cachoeira",11462,"Cajuru",90002,
  "Campina do Siqueira",6887,"Campo Comprido",32395,"Campo de Santana",40633,
  "Capão da Imbuia",18147,"Capão Raso",37852,"Cascatinha",2160,"Caximba",7288,
  "Centro",38671,"Centro Cívico",4473,"Cidade Industrial de Curitiba",172510,
  "Cristo Rei",17604,"Fanny",8397,"Fazendinha",25380,"Ganchinho",19045,
  "Guabirotuba",10650,"Guaíra",14791,"Hauer",11846,"Hugo Lange",4177,
  "Jardim Botânico",5442,"Jardim das Américas",15059,"Jardim Social",4902,
  "Juvevê",12001,"Lamenha Pequena",1146,"Lindóia",8336,"Mercês",12290,
  "Mossunguê",12246,"Novo Mundo",42073,"Orleans",8261,"Parolin",9925,
  "Pilarzinho",27044,"Pinheirinho",56440,"Portão",49180,"Prado Velho",4907,
  "Rebouças",13799,"Riviera",442,"Santa Cândida",40966,"Santa Felicidade",32414,
  "Santa Quitéria",10970,"Santo Inácio",6781,"São Braz",23547,
  "São Francisco",5008,"São João",3277,"São Lourenço",6034,"São Miguel",6689,
  "Seminário",5720,"Sítio Cercado",102293,"Taboão",3470,"Tarumã",7073,
  "Tatuquara",56054,"Tingui",12203,"Uberaba",73241,"Umbará",21969,
  "Vila Izabel",12859,"Vista Alegre",10790,"Xaxim",58124)

if (file.exists(cache_mapa)) geo_cwb <- readRDS(cache_mapa) else {
  g <- tryCatch(read_neighborhood(year = 2022, showProgress = FALSE),
                error = function(e) read_neighborhood(year = 2010, showProgress = FALSE))
  geo_cwb <- g |> filter(code_muni == cod_curitiba); saveRDS(geo_cwb, cache_mapa) }

mapa <- geo_cwb |> st_transform(31982) |> st_make_valid() |>
  mutate(bk = limpa(name_neighborhood)) |>
  group_by(bk) |> summarise(.groups = "drop") |> st_make_valid()

cand_cic <- list(
  "CIC" = c("CIDADE INDUSTRIAL","CIDADE INDUSTRIAL DE CURITIBA"),
  "CIDADE INDUSTRIAL DE CURITIBA" = c("CIDADE INDUSTRIAL","CIC"),
  "CIDADE INDUSTRIAL" = c("CIC","CIDADE INDUSTRIAL DE CURITIBA"))
cwb <- unique(mapa$bk)
dp  <- resolve(unique(casos$bk), cwb, cand_cic)
casos <- casos |> mutate(bk = coalesce(unname(dp[bk]), bk))

ch_pop <- chave(pop_2022$bairro)
mp     <- resolve(unique(ch_pop), cwb, cand_cic)
pop_df <- tibble(bk = coalesce(unname(mp[ch_pop]), ch_pop), pop = pop_2022$pop) |>
  group_by(bk) |> summarise(pop = sum(pop), .groups = "drop")

centros <- tibble(bk = mapa$bk) |>
  left_join(casos, by = "bk") |> left_join(pop_df, by = "bk") |>
  mutate(n = coalesce(n, 0L),
         inc = if_else(!is.na(pop) & pop >= pop_minima, n / pop * 1e4, NA_real_),
         p = sqrt(pmax(0, pmin(inc, inc_max)) / inc_max),
         cor_fill = if_else(is.na(inc), cor_cinza, ramp_cont(p)))

w_L <- 6.0; h_map <- 3.6
cx_d <- w_L/2; cy_d <- h_map/2

mapa_df <- anes(mapa); mapa_df$bk <- mapa$bk[mapa_df$feat]
mapa_df <- left_join(mapa_df, select(centros, bk, cor_fill), by = "bk")

x_mid <- mean(range(mapa_df$x)); y_mid <- mean(range(mapa_df$y))
esc   <- min((w_L - 0.12)/diff(range(mapa_df$x)),
             (h_map - 0.12)/diff(range(mapa_df$y)))

mapa_df <- mapa_df |> mutate(xi = cx_d + (x - x_mid)*esc,
                             yi = cy_d + (y - y_mid)*esc)
vx <- mapa_df$xi; vy <- mapa_df$yi

bar_w <- 0.18; bar_h <- 1.5
w_head <- nchar("casos/10 mil hab.") * 0.61 * 7.5 / 72 + 0.04
chips  <- ramp_cont(((0:39)+0.5)/40)
tick_v <- c(0,30,60,90); tick_p <- sqrt(tick_v/inc_max)

livre <- function(cl) {
  r <- c(cl[1]-0.02, cl[1]+bar_w+0.32, cl[2]-0.08, cl[2]+bar_h+0.50)
  !any(vx >= r[1] & vx <= r[2] & vy >= r[3] & vy <= r[4]) }
cantos <- list(c(0.10, 0.18), c(w_L-0.12-w_head, 0.18),
               c(w_L-0.12-w_head, h_map-1.98), c(0.10, h_map-1.98))
lx <- Filter(livre, cantos)[1][[1]]
if (is.null(lx)) lx <- cantos[[1]]

ggplot() + list(
  geom_polygon(data = mapa_df,
               aes(xi, yi, group = poly, subgroup = ring, fill = cor_fill),
               color = "white", linewidth = 0.30),
  scale_fill_identity(),
  annotate("rect", xmin = lx[1], xmax = lx[1]+bar_w,
           ymin = lx[2] + (0:39)*bar_h/40, ymax = lx[2] + (1:40)*bar_h/40,
           fill = chips, color = NA),
  annotate("text", x = lx[1]+bar_w+0.06, y = lx[2] + tick_p*bar_h,
           label = num0(tick_v), hjust = 0, vjust = 0.5,
           family = "mono2", size = pt2mm(9), color = cor_tinta),
  annotate("text", x = lx[1], y = lx[2]+bar_h+0.34, label = "casos/10 mil hab.",
           hjust = 0, vjust = 0, family = "mono2", size = pt2mm(7.5), color = cor_nota),
  annotate("text", x = lx[1], y = lx[2]+bar_h+0.10, label = "escala: raiz quadrada.",
           hjust = 0, vjust = 0, family = "mono2", size = pt2mm(7.5), color = cor_nota)) +
  scale_x_continuous(limits = c(0, w_L),  expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, h_map), expand = c(0, 0)) +
  theme_void() +
  theme(plot.margin = margin(0.22, 0.22, 0.22, 0.22, "in"),
        legend.position = "none")