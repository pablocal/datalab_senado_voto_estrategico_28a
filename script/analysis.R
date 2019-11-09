# Metadata ----------------------------------------------------------------
# Title: Analysis vote to Senate in 2019 Spanish General Election
# Purpose: Explore strategic vote
# Author(s): @pablocal
# Date Created: 2019-09-20
#
# Comments ----------------------------------------------------------------
# 
# 
# 
#
#
# Options and packages ----------------------------------------------------
rm(list = ls())

library(tidyverse)
library(tmap)

# A) Analyse data ---------------------------------------------------------

# A.1. Get elec data ------------------------------------------------------
df <- read_rds("data/senate_analysis_df.RDS")

# A.2. Summary statistics -------------------------------------------------

alter_df <- df %>% 
  filter(year > 2011) %>% 
  group_by(year, CPROV) %>% 
  summarise(seats_alter_dist = first(seats_alter_dist),
            seats_alter_order = first(seats_alter_order))%>% 
  group_by(year) %>% 
  summarise(seats_alter_dist = sum(seats_alter_dist),
            seats_alter_order = sum(seats_alter_order)) %>% 
  gather(key = "camb", value = "val", seats_alter_dist:seats_alter_order) %>% 
  mutate(camb = fct_relevel(camb, "seats_alter_order"))

# now plot
ann_text <- data.frame(year = c(1.25, .75), 
                       val = c(3.5, 8.5),
                       camb = c("seats_alter_dist", "seats_alter_order"),
                       lab = c("Distribución",  "Orden"))


plot_imp <- ggplot(alter_df, aes(y = val, x = as.character(year), fill = camb)) +
  geom_col(position = "dodge", alpha = .7) +
  geom_text(data = ann_text, aes(x = year, y = val, label = lab), size = 3.5, col = "black", family = "Josefine Sans") +
  scale_y_continuous(limits = c(0, 16)) +
  scale_fill_manual(values = c("gray", "black")) +
  ggtitle("Alteraciones por voto estratégico \nen las elecciones al Senado") +
  theme(plot.title = element_text(family = "Josefine Sans"),
        legend.position = "none",
        axis.line.x = element_line(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major.x  = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey", linetype = "dotted"),
        panel.background = element_rect(fill = "white"),
        strip.placement = "outside",
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(family = "Josefine Sans", colour = "black", face = "bold")
  )


write_rds(plot_imp, "data/plot_imp.RDS")

# Number of provinces in which seats have altered order or distribution has doubled since 2015
# if no strategic vote to senate would expect por_cong = max_sen = min_sen

# disim indicators
df <- df %>%
  filter(year > 2011) %>% # compare only from 2015 onwards
  mutate(dif = por_sen - por_cong, # dif cong and senate
         dif_max = max_sen - por_cong, # dif max sen and cong
         dif_min = min_sen - por_cong) # dif min sen and cong

df_sum <- df %>% 
  group_by(year, CPROV, prov_name, party) %>% 
  summarise( 
           dif = mean(dif, na.rm = T), 
           dif_max = first(dif_max),
           dif_min  = first(dif_min), 
           ratio = first(ratio),
           ) %>% 
  arrange(party, year)

# A.3. Map analysis ---------------------------------------------------------

# load map and merge with data
map_spain <- read_rds("data/gadm36_ESP_2_sp.rds")
data <- map_spain@data

df_sum_long <- df_sum %>%
  ungroup() %>% 
  gather(key = "var", value = "value", dif:ratio) %>% 
  mutate(var = paste(var, party, year, sep = "_")) %>%
  select(CPROV, var, value) %>% 
  spread(key = var, value = value) 

data <- data %>% 
  mutate(CPROV = as.numeric(CC_2)) %>% 
  left_join(df_sum_long, by = "CPROV")

map_spain@data <- data

# exclude from the map circuns with les than 3 seats (islands, ceuta and melilla)
map_spain <- subset(map_spain, !(NAME_2 %in% c("Baleares", "Las Palmas", "Santa Cruz de Tenerife", "Ceuta", "Melilla")))

tmap_mode("plot")
brks <- c(-5, -3, 0 , 3, 10)

pp_max <- tm_shape(map_spain) + 
  tm_polygons(c("dif_max_PP_2015", "dif_max_PP_2016", "dif_max_PP_2019"), palette = "Blues", title = "PP", breaks = brks, midpoint = 0) +
  tm_facets(sync = TRUE, ncol = 3) + 
  tm_layout(bg.color = "white", frame = FALSE, legend.outside = TRUE) +
  tm_layout(legend.format = list(text.separator = "a",
                                 text.less.than = "Menos",
                                 text.or.more = "o más"))

cs_max <- tm_shape(map_spain) + 
  tm_polygons(c("dif_max_Cs_2015", "dif_max_Cs_2016", "dif_max_Cs_2019"), palette = "Oranges", title = "C's", breaks = brks, midpoint = 0) +
  tm_facets(sync = TRUE, ncol = 3)  + 
  tm_layout(bg.color = "white", frame = FALSE, legend.outside = TRUE) +
  tm_layout(legend.format = list(text.separator = "a",
                                 text.less.than = "Menos",
                                 text.or.more = "o más"))

vox_max <- tm_shape(map_spain) + 
  tm_polygons(c("dif_max_Vox_2015", "dif_max_Vox_2016", "dif_max_Vox_2019"), palette = "Greens", breaks = brks, title = "Vox", textNA = "No se presenta", midpoint = 0) +
  tm_facets(sync = TRUE, ncol = 3) + 
  tm_layout(bg.color = "white", frame = FALSE, legend.outside = TRUE) +
  tm_layout(legend.format = list(text.separator = "a",
                                 text.less.than = "Menos",
                                 text.or.more = "o más"))

psoe_max <- tm_shape(map_spain) + 
  tm_polygons(c("dif_max_PSOE_2015", "dif_max_PSOE_2016", "dif_max_PSOE_2019"), palette = "Reds", breaks = brks, title = "PSOE", midpoint = 0) +
  tm_facets(sync = TRUE, ncol = 3) + 
  tm_layout(bg.color = "white", frame = FALSE, legend.outside = TRUE) +
  tm_layout(legend.format = list(text.separator = "a",
                                 text.less.than = "Menos",
                                 text.or.more = "o más"))

up_max <- tm_shape(map_spain) + 
  tm_polygons(c("dif_max_UP_2015", "dif_max_UP_2016", "dif_max_UP_2019"), palette = "Purples", breaks = brks, title = "UP", midpoint = 0) +
  tm_facets(sync = TRUE, ncol = 3) + 
  tm_layout(bg.color = "white", frame = FALSE, legend.outside = TRUE) +
  tm_layout(legend.format = list(text.separator = "a",
                                 text.less.than = "Menos",
                                 text.or.more = "o más"))

dif_max <- tmap_arrange(pp_max, cs_max, vox_max, psoe_max, up_max, nrow = 5)

pp_min <- tm_shape(map_spain) + 
  tm_polygons(c("dif_min_PP_2015", "dif_min_PP_2016", "dif_min_PP_2019"), palette = "Blues", title = "PP", breaks = brks, midpoint = 0) +
  tm_facets(sync = TRUE, ncol = 3) + 
  tm_layout(bg.color = "white", frame = FALSE, legend.outside = TRUE) +
  tm_layout(legend.format = list(text.separator = "a",
                                 text.less.than = "Menos",
                                 text.or.more = "o más"))


cs_min <- tm_shape(map_spain) + 
  tm_polygons(c("dif_min_Cs_2015", "dif_min_Cs_2016", "dif_min_Cs_2019"), palette = "Oranges", title = "C's", breaks = brks, midpoint = 0) +
  tm_facets(sync = TRUE, ncol = 3)  + 
  tm_layout(bg.color = "white", frame = FALSE, legend.outside = TRUE) +
  tm_layout(legend.format = list(text.separator = "a",
                                 text.less.than = "Menos",
                                 text.or.more = "o más"))

vox_min <- tm_shape(map_spain) + 
  tm_polygons(c("dif_min_Vox_2015", "dif_min_Vox_2016", "dif_min_Vox_2019"), palette = "Greens", breaks = brks, title = "Vox", textNA = "No se presenta", midpoint = 0) +
  tm_facets(sync = TRUE, ncol = 3) + 
  tm_layout(bg.color = "white", frame = FALSE, legend.outside = TRUE) +
  tm_layout(legend.format = list(text.separator = "a",
                                 text.less.than = "Menos",
                                 text.or.more = "o más"))

psoe_min <- tm_shape(map_spain) + 
  tm_polygons(c("dif_min_PSOE_2015", "dif_min_PSOE_2016", "dif_min_PSOE_2019"), palette = "Reds", breaks = brks, title = "PSOE", midpoint = 0) +
  tm_facets(sync = TRUE, ncol = 3) + 
  tm_layout(bg.color = "white", frame = FALSE, legend.outside = TRUE) +
  tm_layout(legend.format = list(text.separator = "a",
                                 text.less.than = "Menos",
                                 text.or.more = "o más"))

up_min <- tm_shape(map_spain) + 
  tm_polygons(c("dif_min_UP_2015", "dif_min_UP_2016", "dif_min_UP_2019"), palette = "Purples", breaks = brks, title = "UP", midpoint = 0) +
  tm_facets(sync = TRUE, ncol = 3) + 
  tm_layout(bg.color = "white", frame = FALSE, legend.outside = TRUE) +
  tm_layout(legend.format = list(text.separator = "a",
                                 text.less.than = "Menos",
                                 text.or.more = "o más"))

dif_min <- tmap_arrange(pp_min, cs_min, vox_min, psoe_min, up_min, nrow = 5)

# A.4. Cases of Madrid and Zamora ---------------------------------

# identify cases
df %>% 
  filter(year == 2019 & party == "PP") %>% 
  arrange(-dif_min)
# take Zamora

df %>% 
  filter(year == 2019 & party == "Vox") %>% 
  arrange(-dif_max)
# take Madrid

data_cases <- df %>% 
  filter(prov_name %in% c("Madrid", "Zamora") & year != 2011) %>% 
  group_by(year, CPROV, prov_name, party) %>% 
  summarise(min_sen = first(min_sen),
            max_sen = first(max_sen),
            por_cong = first(por_cong)) %>% 
  mutate(party = recode(party, "Podemos" = "UP", "Cs" = "C's"),
    party = fct_relevel(party, "UP", "PSOE", "Vox", "Cs", "PP"))

cols <- c(PSOE = "red2", PP = "deepskyblue2", `C's` = "darkorange", Vox = "green", UP = "purple")

ann_text <- data.frame(por_cong = c(33, 6), 
                       party = c("Vox", "PP"),
                       lab = c("% mín. y máx. \nvotos Senado",  "% voto \nCongreso"),
                       year = 2015,
                       prov_name = "Madrid")
  
curve <- data.frame(x = c(12, 19), 
                    y = c(5, 3),
                    xend = c(19, 14),
                    yend = c(4.3, 3.7),
                    year = 2015,
                    curvature = 0.2,
                    prov_name = "Madrid")

data_cases$party <- fct_relevel(data_cases$party, "UP", "PSOE", "Vox", "C's", "PP")

plot_cases <- ggplot(data_cases, aes(x = por_cong, y = party, col = party)) +
  geom_errorbarh(aes(y = party, xmin = min_sen, xmax = max_sen), height = .4) +
  geom_point(size = 3, shape = 19, alpha = .5) +
  geom_text(data = ann_text, aes(label = lab), size = 2.7, col = "black", family = "Josefine Sans", fontface = "bold") +
  geom_curve(data = curve, aes(x = x, y = y, xend = xend, yend = yend), curvature = -.4,  color = "black",
              arrow = arrow(length = unit(0.1, "inches"))) +
  scale_color_manual(values = cols) +
  facet_grid(prov_name ~ year, switch = "y") +
  labs(title = "Voto estratégico en las elecciones al Senado",
       subtitle = "Los casos de Madrid (1+1+1) y Zamora (voto útil al PP)") +
  theme(plot.title = element_text(family = "Josefine Sans"),
        legend.position = "none",
        axis.line.x = element_line(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(family = "Josefine Sans", colour = "black"),
        panel.grid.major.x  = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey", linetype = "dotted"),
        panel.background = element_rect(fill = "white"),
        strip.placement = "outside",
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(family = "Josefine Sans", colour = "black", face = "bold")
        )

  

save_tmap(dif_min, "data/dif_min.png")
save_tmap(dif_max, "data/dif_max.png")

write_rds(plot_cases, "data/plot_cases.RDS")

# B) Using CIS data to estima prevalence of strategic vote ----------------


# B.1. Load prost elec CIS data -------------------------------------------
rm(list = ls())

cis <- haven::read_sav("data/3248.sav")
cis <- haven::as_factor(cis)

# B.2. Estimates ------------------------------------------------------------

sjmisc::frq(cis$P23)
sjmisc::frq(cis$P23C)
sjmisc::frq(cis$P23D)


cis %>% 
  filter(P23C == "Votó a los/as 3 del mismo partido" & !is.na(P23) & !is.na(P23C)) %>% 
  sjmisc::flat_table(P23, P23D)

cis <- cis %>% 
  mutate(
    P23_char = as.character(P23),
    P23D_char = as.character(P23D),
    cambio_senado = case_when(
    P23C == "Votó a los/as 3 del mismo partido" &  P23D_char != P23_char & !is.na(P23D) & !is.na(P23) & P23 != "N.C." & P23D != "N.C." & P23 != "No votó." & P23 != "No recuerda" ~ "Cambio de partido en bloque",
    P23C == "Votó a los/as 3 del mismo partido" &  P23D_char == P23_char & !is.na(P23D) & !is.na(P23) & P23 != "N.C." & P23D != "N.C." ~ "Voto paralelo",
    P23 == "En blanco" & P23C == "Votó en blanco" ~ "Voto paralelo",
    P23C == "Votó por varios/as de diferentes partidos" ~ "División de voto",
    P23C == "No votó al Senado" & !(P23 %in% c("N.C.", "No votó", "Voto nulo", "No recuerda")) ~ "Abstención Senado",
    TRUE ~ "NR + Abstención"
  ),
    voto_pp_cs_VOX = ifelse(P23E01 %in% c("PP", "Ciudadanos", "VOX") & (P23E02 %in% c("PP", "Ciudadanos", "VOX") | P23E03 %in% c("PP", "Ciudadanos", "VOX")), 1, 0),
    voto_psoe_up = ifelse(P23E01 %in% c("PSOE", "Unidas UP", "En Comú Podem") & (P23E02 %in% c("PSOE", "Unidas UP", "En Comú Podem") | P23E03 %in% c("PSOE", "Unidas UP", "En Comú Podem")), 1, 0)
)

cis %>% 
  sjmisc::frq(cambio_senado, voto_pp_cs_VOX, voto_psoe_up)

cis %>% 
  filter(cambio_senado !=  "NR + Abstención" & !(CCAA %in% c("Balears (Illes)", "Ceuta (Ciudad Autónoma de)", "Melilla (Ciudad Autónoma de)", "Canarias"))) %>% 
  sjmisc::frq(cambio_senado)

# estimate data 
df <- cis %>% 
  filter(P23 %in% c("PP", "Ciudadanos", "VOX") & cambio_senado != "NR + Abstención" & !(CCAA %in% c("Balears (Illes)", "Ceuta (Ciudad Autónoma de)", "Melilla (Ciudad Autónoma de)", "Canarias"))) %>% 
  sjmisc::frq(cambio_senado)
df <- df[[1]]

df_text <- data.frame(
  x = c(1, 2, 3, 4),
  y = c(10, 14, 20, 94),
  text = c("Abstención", "Otro \npartido", "Varios \npartidos", "Mismo \npartido")
)

plot_cis <- ggplot(filter(df, !is.na(val)), aes(val, raw.prc, label = paste0(round(raw.prc, 0), "%"))) +
  geom_col(width = .7, col = "gray", alpha = .6) +
  geom_text(aes(y = raw.prc+2.5), size = 4, col = "black", family = "Josefine Sans", fontface = "bold") +
  geom_text(data = df_text, aes(x = x, y = y, label = text), size = 4, col = "black", family = "Josefine Sans") +
  ggtitle("Los que votaron a PP, Cs o Vox al Congreso, \nen la papeleta del Senado...") +
  theme(plot.title = element_text(family = "Josefine Sans"),
        legend.position = "none",
        axis.line.x = element_line(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x  = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey", linetype = "dotted"),
        panel.background = element_rect(fill = "white"),
        strip.placement = "outside",
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(family = "Josefine Sans", colour = "black", face = "bold")
  )

write_rds(plot_cis, "data/plot_cis.RDS")
