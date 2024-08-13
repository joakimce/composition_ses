
library(tidyverse)
library(data.table)

byr = fread("/ess/p805/data/durable/data/registers/original 2023 04/csv/W19_0634_FASTE_OPPLYSNINGER.csv", na.strings = "", stringsAsFactors = F) %>% 
  select(w19_0634_lnr,foedsels_aar_mnd) %>% rename(birth_year = foedsels_aar_mnd, lnr = w19_0634_lnr) %>%
  mutate(birth_year = as.numeric(substring(birth_year,1,4)))

inf = fread("/ess/p805/data/durable/projects/joakim/SES_composition/data/phenos_prep/inflation_nor_1993_2022.csv") %>% mutate(cpi = idx2015/100) %>% select(-c(idx2015))

inc = fread("/ess/p805/data/durable/data/registers/original 2023 04/csv/W19_0634_INNTEKT.csv") %>% 
  rename(lnr=w19_0634_lnr, year=aargang) %>% 
  select(lnr, year, ies) %>%
  left_join(byr, by = "lnr") %>%
  mutate(ies = as.numeric(ies)) %>% 
  left_join(inf, by="year") %>%
  mutate(ies_adj = ies/cpi) %>%
  dplyr::filter(year-birth_year >= 35 & year-birth_year <= 45) %>%
  group_by(lnr) %>%
  summarise(inc_avg = mean(ies_adj, na.rm=T)) 

write.csv(inc, "/ess/p805/data/durable/projects/joakim/SES_composition/data/phenos/inc.csv", row.names=F)
