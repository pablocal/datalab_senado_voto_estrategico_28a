# Metadata ----------------------------------------------------------------
# Title: Tidy data
# Purpose: Tidy data before analysis
# Author(s): @pablocal
# Date Created: 2019-09-22
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


# A) Generate senate dataset ----------------------------------------------

# A.1. Get elec data ------------------------------------------------------
cong <- read_rds("data/cong.RDS")
sen <- read_rds("data/sen.RDS")
provs <- read_csv("data/provs.csv", locale = locale(encoding = "Latin1"))

# A.2. Prepare Congress data  ---------------------------------------------
unique(cong$Variable)
unique(sen$Party)

parties <- c("PP", "PSOE", "Cs", "VOX", "PSC_PSC_PSOE__ICV_EUA",
             "PODEMOS", "ENCOMÚ", "PODEMOS_En", "ENMAREA", "ECP", "NA+",
             "PODEMOS_COM","PODEMOS_IU", "PODEMOS_EU", "PODEMOS_EN", "CAMBIO_ALDA", "ECP_GUANYEM", "PODEMOS_ENMAREA_ANOVA_EU")

# total valid vote for each prov.
cong_valid <- cong %>% 
  filter(Variable == "Valid") %>% 
  select(year, CPROV, Value) %>% 
  rename(valid = Value)

# valid vote per province, election and party
cong_party <- cong %>% 
  filter(Variable %in% parties) %>% 
  filter(!(Variable == "ENMAREA" & year == 2019)) %>% # exclude ENMAREA 2019 bc is not PODEMOS
  left_join(cong_valid, by = c("year", "CPROV")) %>% # join total valid vote to compute percentages
  mutate(por_cong = Value/valid*100,
         party = recode(Variable, "PSOE" = "PSOE",
                        "PSC_PSC_PSOE__ICV_EUA" = "PSOE",
                        "NA+" = "PP",
                        "PP" = "PP",
                        "Cs" = "Cs",
                        "VOX" = "Vox",
                        .default = "UP")) %>% 
  select(year, CPROV, party, por_cong)



# A.3 Prepare senate data (final df) --------------------------------------

# senate seats
sen_seats <- sen %>% 
  mutate(n_seats = case_when(
    CPROV %in% c(35, 38, 7) ~ 3,
    CPROV %in% c(51, 52) ~ 2,
    TRUE ~ 4
  )) %>% 
  group_by(year, CAUT, CPROV) %>% 
  mutate(seat_order = dense_rank(desc(Votes)), # candidates ranked by votes
         seat = ifelse(seat_order <= n_seats, 1, 0), # whether candidate elected
         first_party = first(ifelse(max(Votes) == Votes, Party, "")), # party first candiate in province
         seats_alter_order = case_when( # whether the order 3+1 has been altered
           n_seats == 4 & seat_order < 4 & Party != first_party ~ 1,
           n_seats == 3 & seat_order < 3 & Party != first_party ~ 1,
           n_seats == 2 & seat_order < 3 & Party != first_party ~ 1,
           n_seats == 1 & seat_order < 2 & Party != first_party ~ 1,
           TRUE ~ 0
         ),
         seats_alter_order = max(seats_alter_order)) %>% # whether the order has been alteres (for province)
  group_by(year, CAUT, CPROV, Party) %>% 
  mutate(seats_party = sum(seat),
         seats_alter_dist = case_when( # whether the dist of seats has changed from 3+1
           n_seats == 4 & !(seats_party %in% c(0, 3, 1)) ~ 1,
           n_seats == 3 & !(seats_party %in% c(0, 2, 1)) ~ 1,
           n_seats == 2 & !(seats_party %in% c(0, 2)) ~ 1,
           TRUE ~ 0
         )) %>% 
  group_by(year, CAUT, CPROV) %>% 
  mutate(seats_alter_dist = max(seats_alter_dist)) %>% # whether the seat dist has been altered (for province)
  select(year, CAUT, CPROV, Candidate, n_seats, seat_order, seat, seats_party, seats_alter_dist, seats_alter_order)

sen_party <- sen %>% 
  filter(Party %in% parties) %>% # select only the parties for analysis
  filter(!(Party == "ENMAREA" & year == 2019)) %>% 
  mutate(por_sen = Votes/Valid*100,
         party = recode(Party, "PSOE" = "PSOE",
                        "PSC_PSC_PSOE__ICV_EUA" = "PSOE",
                        "NA+" = "PP",
                        "PP" = "PP",
                        "Cs" = "Cs",
                        "VOX" = "Vox",
                        .default = "UP")) %>% 
  left_join(sen_seats, by = c("year", "CAUT", "CPROV", "Candidate")) %>% # join with information from seats df
  left_join(cong_party, by = c("year", "CPROV", "party")) %>% # join with congress results
  left_join(provs, by = "CPROV") %>% # join with provinces names
  select(year, CAUT, CPROV, prov_name, party, TurnoutFinal, por_cong, por_sen, n_seats, seat_order, 
         seat, seats_party, seats_alter_dist, seats_alter_order) %>% 
  group_by(year, CAUT, CPROV, party) %>% # create some party level indicators
  mutate(
    min_sen = min(por_sen), # min per of valid votes in a province and senate election
    max_sen = max(por_sen), # max same
    ratio = max_sen/min_sen, # ratio max min
    count = n())

# A.4 Save final file ------------------------------------------------------
write_rds(sen_party, "data/senate_analysis_df.RDS")




