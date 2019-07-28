#############################################
##### Analysis voto senado 2019 #############
#############################################
## by PCA on 28th july

rm(list = ls())
library(tidyverse)
#source("source/elections.R")


# 1. Get data -------------------------------------------------------------
cong <- read_rds("data/cong.RDS")
sen <- read_rds("data/sen.RDS")
provs <- read_csv2("data/provs.csv", locale = locale(encoding = "Latin1"))

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
  filter(!(party == "VOX" & year != 2019)) %>% 
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

df %>% group_by(year, CPROV, prov_name, party) %>% summarise(dif_1  = mean(dif, na.rm = T), 
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


## prov with alterations
df %>% 
  group_by(year, CAUT, CPROV) %>% 
  summarise(alter_order = first(seats_alter_order), 
            alter_dist = first(seats_alter_dist)) %>% 
  group_by(year) %>% 
  summarise(alter_order_sum = sum(alter_order),
            alter_dist_sum = sum(alter_dist))


list_provs <- df %>% 
  group_by(year, CAUT, CPROV, prov_name) %>% 
  summarise(alter_order = first(seats_alter_order), 
            alter_dist = first(seats_alter_dist)) %>% 
  filter(alter_order == 1 | alter_dist == 1)


## ggplot
provs_2019 <- list_provs %>% filter(year == 2019) %>% pull(prov_name)

ggplot(filter(df, prov_name %in% provs_2019), aes(x = por_cong, y = party)) +
  geom_point(col = "red") +
  geom_point(aes(x = min_sen, y = party), col = "green") +
  geom_point(aes(x = max_sen, y = party), col = "green") +
  facet_grid(prov_name ~ year) 



a <- filter(df, prov_name %in% provs_2019)


