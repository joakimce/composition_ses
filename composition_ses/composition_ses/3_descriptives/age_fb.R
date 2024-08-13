library(tidyverse)
library(data.table)

path = ("/ess/p805/data/durable/projects/joakim/SES_composition/data/")
dat_pop = fread(paste0(path,"phenos/population_phenos.csv")) %>% select(lnr)
dat_sam = fread(paste0(path,"phenos/sample_phenos.csv")) %>% select(lnr)

pop_par = fread(paste0(path,"phenos_prep/eligible_parents_n883147.csv"), header = T) %>% rename(lnr=w19_0634_lnr)
sam_par = fread("/ess/p805/data/durable/data/moba/linkage/PDB2601_kobling_SSB_v12.csv") %>% filter(rolle != "SU2PT_CHILD") %>% 
  rename(lnr = w19_0634_lnr, sex = rolle) %>% select(lnr, sex) %>% mutate(sex = ifelse(sex=="SU2PT_MOTHER",1,0))   

faste = fread("/ess/p805/data/durable/data/registers/original 2023 04/csv/W19_0634_FASTE_OPPLYSNINGER.csv")

byr = fread("/ess/p805/data/durable/data/registers/original 2023 04/csv/W19_0634_FASTE_OPPLYSNINGER.csv", na.strings = "", stringsAsFactors = F) %>% 
  select(w19_0634_lnr,foedsels_aar_mnd) %>% rename(birth_year = foedsels_aar_mnd, lnr = w19_0634_lnr) %>%
  mutate(birth_year = as.numeric(substring(birth_year,1,4)))


#-- number of children mothers ----

mor = faste %>% select(lopenr_mor, w19_0634_lnr) %>% rename(lnr = w19_0634_lnr)%>%
  left_join(byr, by ="lnr") %>%
  group_by(lopenr_mor) %>%
  filter(birth_year == min(birth_year)) %>%
  select(-c(lnr)) %>%
  rename(byr_child = birth_year, lnr=lopenr_mor) %>%
  left_join(byr, by="lnr") %>%
  rename(byr_mor = birth_year) %>%
  mutate(afb = byr_mor-byr_child)
    
far = faste %>% select(lopenr_far, w19_0634_lnr) %>% rename(lnr = w19_0634_lnr) %>%
  left_join(byr, by ="lnr") %>%
  group_by(lopenr_far) %>%
  filter(birth_year == min(birth_year)) %>%
  select(-c(lnr)) %>%
  rename(byr_child = birth_year, lnr=lopenr_far) %>%
  left_join(byr, by="lnr") %>%
  rename(byr_far = birth_year) %>%
  mutate(afb = byr_far-byr_child)

mor_pop = left_join(pop_par, mor, by="lnr") %>% select(afb)
mor_sam = left_join(sam_par, mor, by="lnr") %>% select(afb)
print("mor")
(cbind(
  rbind(
    mean(mor_pop$afb, na.rm = T),
    sd(mor_pop$afb, na.rm = T)),
  rbind(
    mean(mor_sam$afb, na.rm = T),
    sd(mor_sam$afb, na.rm = T))))

far_pop = left_join(pop_par, far, by="lnr") %>% select(afb)
far_sam = left_join(sam_par, far, by="lnr") %>% select(afb)
print("far")
(cbind(
  rbind(
    mean(far_pop$afb, na.rm = T),
    sd(far_pop$afb, na.rm = T)),
  rbind(
    mean(far_sam$afb, na.rm = T),
    sd(far_sam$afb, na.rm = T))))