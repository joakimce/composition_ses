# libraries ----

library(tidyverse)
library(data.table)

# - import wealth data, inflation and birth year ----

#wealth = fread("N:/durable/data/registers/original 2023 04/csv/W19_0634_FORMUESVARIABLER.csv", nrows = 10000) %>% 
#  mutate(net_wea = as.numeric(ber_bruttoformue)) %>%
#  rename(lnr = w19_0634_lnr, year = aargang) %>% 
#  select (lnr, year, net_wea)


wea = fread("/ess/p805/data/durable/data/registers/original 2023 04/csv/W19_0634_FORMUESVARIABLER.csv") %>% 
  mutate(net_wea = as.numeric(ber_bruttoformue)) %>%
  rename(lnr = w19_0634_lnr, year = aargang) %>% 
  select (lnr, year, net_wea)

inf = fread("/ess/p805/data/durable/projects/joakim/SES_composition/data/phenos_prep/inflation_nor_1993_2022.csv") %>% mutate(cpi = idx2015/100) %>% select(-c(idx2015))

byr = fread("/ess/p805/data/durable/data/registers/original 2023 04/csv/W19_0634_FASTE_OPPLYSNINGER.csv", na.strings = "", stringsAsFactors = F) %>% 
  select(w19_0634_lnr,foedsels_aar_mnd) %>% rename(birth_year = foedsels_aar_mnd, lnr = w19_0634_lnr) %>%
  mutate(birth_year = as.numeric(substring(birth_year,1,4)))

# -- adjust for inflation and create avg. net wealth between age 35 and 45 ----

wea_avg = wea %>% left_join(byr, by = "lnr") %>%
  filter(year-birth_year >= 35 & year-birth_year <= 45) %>%
  left_join(inf, by="year") %>%
  mutate(wea_adj = net_wea/cpi) %>%
  group_by(lnr) %>%
  summarise(wea_avg = mean(wea_adj, na.rm=T)) 

# --- export csv file ----

write.csv(wea_avg, "/ess/p805/data/durable/projects/joakim/SES_composition/data/phenos/wea.csv", row.names=F)
