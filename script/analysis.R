#############################################
##### Analysis voto senado 2019 #############
#############################################
## by PCA on 28th july

rm(list = ls())
library(tidyverse)
library(tmap)
source("source/elections.R")


# 1. Get data -------------------------------------------------------------
cong <- read_rds("data/cong.RDS")
sen <- read_rds("data/sen.RDS")
provs <- read_csv("data/provs.csv", locale = locale(encoding = "Latin1"))

# 2. Data wrangling -------------------------------------------------------

unique(cong$Variable)
unique(sen$Party)
parties <- c("PP", "PSOE", "Cs", "VOX", "PSC_PSC_PSOE__ICV_EUA",
  "PODEMOS", "ENCOMÚ", "PODEMOS_En", "ENMAREA", "ECP", "NA+",
  "PODEMOS_COM","PODEMOS_IU", "PODEMOS_EU", "PODEMOS_EN", "CAMBIO_ALDA", "ECP_GUANYEM", "PODEMOS_ENMAREA_ANOVA_EU")


## cong data
cong_valid <- cong %>% 
  filter(Variable == "Valid") %>% 
  select(year, CPROV, Value) %>% 
  rename(valid = Value)

cong_party <- cong %>% 
  filter(Variable %in% parties) %>% 
  filter(!(Variable == "ENMAREA" & year == 2019)) %>% 
  left_join(cong_valid, by = c("year", "CPROV")) %>% 
  mutate(por_cong = Value/valid*100,
         party = recode(Variable, "PSOE" = "PSOE",
                        "PSC_PSC_PSOE__ICV_EUA" = "PSOE",
                        "NA+" = "PP",
                        "PP" = "PP",
                        "Cs" = "Cs",
                        "VOX" = "VOX",
                        .default = "Podemos")) %>% 
  select(year, CPROV, party, por_cong)

## sen data
sen_seats <- sen %>% 
  mutate(n_seats = case_when(
    CPROV %in% c(38) ~ 3,
    CPROV %in% c(51, 52) ~ 2,
    TRUE ~ 4
  )) %>% 
  group_by(year, CAUT, CPROV) %>% 
  mutate(seat_order = dense_rank(desc(Votes)),
         seat = ifelse(seat_order <= n_seats, 1, 0),
         first_party = first(ifelse(max(Votes) == Votes, Party, "")),
         seats_alter_order = case_when(
          n_seats == 4 & seat_order < 4 & Party != first_party ~ 1,
          n_seats == 3 & seat_order < 3 & Party != first_party ~ 1,
          n_seats == 2 & seat_order < 3 & Party != first_party ~ 1,
          n_seats == 1 & seat_order < 2 & Party != first_party ~ 1,
          TRUE ~ 0
         ),
         seats_alter_order = max(seats_alter_order)) %>% 
  group_by(year, CAUT, CPROV, Party) %>% 
  mutate(seats_party = sum(seat),
         seats_alter_dist = case_when(
           n_seats == 4 & !(seats_party %in% c(0, 3, 1)) ~ 1,
           n_seats == 3 & !(seats_party %in% c(0, 2, 1)) ~ 1,
           n_seats == 2 & !(seats_party %in% c(0, 2)) ~ 1,
           TRUE ~ 0
         )) %>% 
  group_by(year, CAUT, CPROV) %>% 
  mutate(seats_alter_dist = max(seats_alter_dist)) %>% 
 select(year, CAUT, CPROV, Candidate, n_seats, seat_order, seat, seats_party, seats_alter_dist, seats_alter_order)

sen_party <- sen %>% 
  filter(Party %in% parties) %>%
  filter(!(Party == "ENMAREA" & year == 2019)) %>% 
  mutate(por_sen = Votes/Valid*100,
         party = recode(Party, "PSOE" = "PSOE",
                        "PSC_PSC_PSOE__ICV_EUA" = "PSOE",
                        "NA+" = "PP",
                        "PP" = "PP",
                        "Cs" = "Cs",
                        "VOX" = "VOX",
                        .default = "Podemos")) %>% 
  left_join(sen_seats, by = c("year", "CAUT", "CPROV", "Candidate")) %>%
  left_join(cong_party, by = c("year", "CPROV", "party")) %>%
  left_join(provs, by = "CPROV") %>% 
  select(year, CAUT, CPROV, prov_name, party, TurnoutFinal, por_cong, por_sen, n_seats, seat_order, 
         seat, seats_party, seats_alter_dist, seats_alter_order) %>% 
  group_by(year, CAUT, CPROV, party) %>% 
  mutate(
         min_sen = min(por_sen),
         max_sen = max(por_sen),
         ratio = max_sen/min_sen,
         count = n()) %>% 
  group_by(year) %>% 
  mutate(total_votes = sum(TurnoutFinal),
         por_turnout = TurnoutFinal/total_votes)


## subset 2015-2018 and check

df <- sen_party %>% 
          filter(year > 2011) 
sjmisc::frq(df$party)



# 2. Analysis -------------------------------------------------------------

## disim index
df <- df %>% 
  mutate(dif = por_sen - por_cong,
         dif_max = max_sen - por_cong,
         dif_min = min_sen - por_cong)

df %>% group_by(year, party) %>% summarise(dif_1  = mean(dif, na.rm = T), 
                                    dif_wt1 = weighted.mean(dif, por_cong, na.rm = T), 
                                    dif_wt2 = weighted.mean(dif, por_cong/100*por_turnout, na.rm = T),
                                    dif_max_1  = mean(dif_max, na.rm = T), 
                                    dif_max_wt1 = weighted.mean(dif_max, por_cong, na.rm = T), 
                                    dif_max_wt2 = weighted.mean(dif_max, por_cong/100*por_turnout, na.rm = T),
                                    dif_min_1  = mean(dif_min, na.rm = T), 
                                    dif_min_wt1 = weighted.mean(dif_min, por_cong, na.rm = T), 
                                    dif_min_wt2 = weighted.mean(dif_min, por_cong/100*por_turnout, na.rm = T),
                                    ratio_1  = mean(ratio, na.rm = TRUE),
                                    ratio_wt = weighted.mean(ratio, por_cong/100*por_turnout, na.rm = T)) %>% 
  arrange(party, year)

df %>% group_by(year, CPROV,prov_name) %>% summarise(dif_wt2 = weighted.mean(dif, por_cong/100*por_turnout, na.rm = T),
                                           ratio_1  = mean(ratio, na.rm = TRUE),
                                           ratio_wt = weighted.mean(ratio, por_cong/100*por_turnout, na.rm = T)) %>% 
  arrange(CPROV, year)

data_summary <- df %>% 
  group_by(year, CPROV, prov_name, party) %>% 
  summarise( 
           dif = mean(dif, na.rm = T), 
           dif_max = mean(dif_max, na.rm = T),
           dif_min  = mean(dif_min, na.rm = T), 
           ratio = mean(ratio, na.rm = TRUE),
           ) %>% 
  arrange(party, year)



# 3. Map analysis ---------------------------------------------------------

map_spain <- read_rds("data/gadm36_ESP_2_sp.rds")
data <- map_spain@data

data_summary_long <- data_summary %>%
  ungroup() %>% 
  gather(key = "var", value = "value", dif:ratio) %>% 
  mutate(var = paste(var, party, year, sep = "_")) %>%
  select(CPROV, var, value) %>% 
  spread(key = var, value = value) 

data <- data %>% 
  mutate(CPROV = as.numeric(CC_2)) %>% 
  left_join(data_summary_long, by = "CPROV")

map_spain@data <- data

map_spain <- subset(map_spain, !(NAME_2 %in% c("Baleares", "Las Palmas", "Santa Cruz de Tenerife", "Ceuta", "Melilla")))

brks <- c(-Inf, 0 , 3, 6, Inf)

pp_max <- tm_shape(map_spain) + 
  tm_polygons(c("dif_max_PP_2015", "dif_max_PP_2016", "dif_max_PP_2019"), palette = "Blues", title = "PP", breaks = brks) +
  tm_facets(sync = TRUE, ncol = 3) + 
  tm_layout(bg.color = "white", frame = FALSE, legend.outside = TRUE)


cs_max <- tm_shape(map_spain) + 
  tm_polygons(c("dif_max_Cs_2015", "dif_max_Cs_2016", "dif_max_Cs_2019"), palette = "Oranges", title = "C's", breaks = brks) +
  tm_facets(sync = TRUE, ncol = 3)  + 
  tm_layout(bg.color = "white", frame = FALSE, legend.outside = TRUE)

vox_max <- tm_shape(map_spain) + 
  tm_polygons(c("dif_max_VOX_2015", "dif_max_VOX_2016", "dif_max_VOX_2019"), palette = "Greens", breaks = brks, title = "VOX") +
  tm_facets(sync = TRUE, ncol = 3) + 
  tm_layout(bg.color = "white", frame = FALSE, legend.outside = TRUE)

psoe_max <- tm_shape(map_spain) + 
  tm_polygons(c("dif_max_PSOE_2015", "dif_max_PSOE_2016", "dif_max_PSOE_2019"), palette = "Reds", breaks = brks, title = "PSOE") +
  tm_facets(sync = TRUE, ncol = 3) + 
  tm_layout(bg.color = "white", frame = FALSE, legend.outside = TRUE)


up_max <- tm_shape(map_spain) + 
  tm_polygons(c("dif_max_Podemos_2015", "dif_max_Podemos_2016", "dif_max_Podemos_2019"), palette = "Purples", breaks = brks, title = "Podemos") +
  tm_facets(sync = TRUE, ncol = 3) + 
  tm_layout(bg.color = "white", frame = FALSE, legend.outside = TRUE)

dif_max_der <- tmap_arrange(pp_max, cs_max, vox_max, nrow = 3)
dif_max_izq <- tmap_arrange(psoe_max, up_max, nrow = 2)

pp_min <- tm_shape(map_spain) + 
  tm_polygons(c("dif_min_PP_2015", "dif_min_PP_2016", "dif_min_PP_2019"), palette = "Blues", title = "PP", breaks = brks) +
  tm_facets(sync = TRUE, ncol = 3) + 
  tm_layout(bg.color = "white", frame = FALSE, legend.outside = TRUE)


cs_min <- tm_shape(map_spain) + 
  tm_polygons(c("dif_min_Cs_2015", "dif_min_Cs_2016", "dif_min_Cs_2019"), palette = "Oranges", title = "C's", breaks = brks) +
  tm_facets(sync = TRUE, ncol = 3)  + 
  tm_layout(bg.color = "white", frame = FALSE, legend.outside = TRUE)

vox_min <- tm_shape(map_spain) + 
  tm_polygons(c("dif_min_VOX_2015", "dif_min_VOX_2016", "dif_min_VOX_2019"), palette = "Greens", breaks = brks, title = "VOX") +
  tm_facets(sync = TRUE, ncol = 3) + 
  tm_layout(bg.color = "white", frame = FALSE, legend.outside = TRUE)

psoe_min <- tm_shape(map_spain) + 
  tm_polygons(c("dif_max_PSOE_2015", "dif_max_PSOE_2016", "dif_max_PSOE_2019"), palette = "Reds", breaks = brks, title = "PSOE") +
  tm_facets(sync = TRUE, ncol = 3) + 
  tm_layout(bg.color = "white", frame = FALSE, legend.outside = TRUE)

up_min <- tm_shape(map_spain) + 
  tm_polygons(c("dif_min_Podemos_2015", "dif_min_Podemos_2016", "dif_min_Podemos_2019"), palette = "Purples", breaks = brks, title = "Podemos") +
  tm_facets(sync = TRUE, ncol = 3) + 
  tm_layout(bg.color = "white", frame = FALSE, legend.outside = TRUE)

dif_min_der <- tmap_arrange(pp_min, cs_min, vox_min, nrow = 3)
dif_min_izq <- tmap_arrange(psoe_min, up_min, nrow = 2)

dif_max_izq
dif_min_izq
dif_max_der
dif_min_der

# 3. Case analysis Guadalajara and Cuenca ---------------------------------

data_cases <- df %>% 
  filter(prov_name %in% c("Guadalajara", "Castellón") & !(party %in% c("PSOE", "Podemos"))) %>% 
  group_by(year, CPROV, prov_name, party) %>% 
  summarise(min_sen = first(min_sen),
            max_sen = first(max_sen),
            por_cong = first(por_cong)) %>% 
  mutate(party = fct_relevel(party, "Podemos", "PSOE", "VOX", "Cs", "PP"))

cols <- c(PSOE = "red", PP = "blue", Cs = "orange", VOX = "green", Podemos = "purple")

plot <- ggplot(data_cases, aes(x = por_cong, y = party, col = party)) +
  geom_point(size = 3, shape = 19, alpha = .4) +
  geom_errorbarh(aes(y = party, xmin = min_sen, xmax = max_sen), height = .3) +
  scale_color_manual(values = cols) +
  facet_grid(prov_name ~ year) +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(family = "Arial", colour = "black"),
        panel.grid.major.x  = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey", linetype = "dotted"),
        panel.background = element_rect(fill = "white"),
        strip.background = element_rect(fill = "lightblue"),
        strip.text = element_text(family = "Arial", colour = "black", hjust = .1)
        )

plot  

write_rds(dif_max_der, "data/dif_max_der.RDS")
write_rds(dif_min_der, "data/dif_min_der.RDS")
write_rds(plot, "data/plot_cases.RDS")

tmap_mode("plot")
dif_max_der
