###############################################
########      Senado 3x1            ###########
###############################################
## by PCA on 24th july 2019

rm(list = ls())
library(tidyverse)
source("script/elections.R")

## 1. get data

# sen <- elections("sen", c(2011, 2015, 2016, 2019), level = "province", format = "wide")
# sen[[5]] <- NULL
# 
# sen[[1]]$year <- 2011
# sen[[2]]$year <- 2015
# sen[[3]]$year <- 2016
# sen[[4]]$year <- 2019
# 
# sen <- map_df(sen, bind_rows)
# write_rds(sen, "data/sen.RDS")

# cong <- elections("cong", c(2011, 2015, 2016, 2019), level = "province", format = "long")
# cong[[5]] <- NULL
# 
# cong[[1]]$year <- 2011
# cong[[2]]$year <- 2015
# cong[[3]]$year <- 2016
# cong[[4]]$year <- 2019
# 
# cong <- map_df(cong, bind_rows)
# write_rds(cong, "data/cong.RDS")

cong <- read_rds("data/cong.RDS")
sen <- read_rds("data/sen.RDS")

## 2. analysis

# clean cong
unique(cong$Variable)
parties <- c("PP", "PSOE",
             "PODEMOS", "VOX", 
             "ENCOMÚ", "PODEMOS_COM", 
             "PODEMOS_En", "PODEMOS_EU",
             "PODEMOS_IU", "PODEMOS_EN", "ECP", 
             "ECP_GUANYEM", "ENMAREA", "PODEMOS_ENMAREA_ANOVA_EU",
             "PSC_PSC_PSOE__ICV_EUA", "Cs")

cong_val <- cong %>%
  filter(Variable == "Valid") %>% 
  select(year, CAUT, CPROV, Value) %>% 
  rename(Valid = Value) 
  

cong <- cong %>% 
  filter(Variable %in% parties & year != 2011) %>% 
  mutate(Party = recode(Variable,
                        "PSC_PSC_PSOE__ICV_EUA" = "PSOE",
                        "PSOE" = "PSOE",
                        "PP" = "PP",
                        "VOX" = "VOX",
                        "Cs" = "Cs",
                        .default = "Podemos")) %>% 
  filter(!(Party == "VOX" & year != 2019)) %>% 
  rename(votes_cong = Value) %>% 
  left_join(cong_val, by = c("year", "CAUT", "CPROV")) %>% 
  mutate(por_cong = votes_cong/Valid*100) %>% 
  select(year, CAUT, CPROV, Party, por_cong)

sjmisc::frq(cong$Party)

# clean sen

sen <- sen %>% 
  filter(Party %in% parties & year != 2011) %>% 
  mutate(Party = recode(Party,
                        "PSC_PSC_PSOE__ICV_EUA" = "PSOE",
                        "PSOE" = "PSOE",
                        "PP" = "PP",
                        "VOX" = "VOX",
                        "Cs" = "Cs",
                        .default = "Podemos")) %>% 
  filter(!(Party == "VOX" & year != 2019)) %>% 
  left_join(. , cong, by = c("year", "CAUT", "CPROV", "Party")) %>% 
  mutate(por_sen = Votes/Valid*100)
sjmisc::frq(sen$Party)

# ratios

sen_ratio <-  sen %>% 
  group_by(year, CPROV, Party) %>% 
  summarise(min_votes = min(Votes), 
            max_votes = max(Votes), 
            min_por_sen = min(por_sen), 
            max_por_sen = max(por_sen), 
            por_con = first(por_cong),
            ratio = max_votes/min_votes)





ggplot(sen_ratio, aes(x = ratio, y = Party, col = Party)) +
  geom_point() +
  facet_grid(CPROV ~ year) +
  scale_x_continuous(limits = c(1, 2))
