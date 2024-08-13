library(tidyverse)
library(data.table)

path = ("/ess/p805/data/durable/projects/joakim/SES_composition/data/")
dat_pop = fread(paste0(path,"phenos/population_phenos.csv"))
dat_sam = fread(paste0(path,"phenos/sample_phenos.csv"))

pop_par = fread(paste0(path,"phenos_prep/eligible_parents_n883147.csv"), header = T) %>% rename(lnr=w19_0634_lnr)
sam_par = fread("/ess/p805/data/durable/data/moba/linkage/PDB2601_kobling_SSB_v12.csv") %>% filter(rolle != "SU2PT_CHILD") %>% 
  rename(lnr = w19_0634_lnr, sex = rolle) %>% select(lnr, sex) %>% mutate(sex = ifelse(sex=="SU2PT_MOTHER",1,0))   

faste = fread("/ess/p805/data/durable/data/registers/original 2023 04/csv/W19_0634_FASTE_OPPLYSNINGER.csv")

#-- number of children mothers ----

mor = faste %>% select(lopenr_mor, w19_0634_lnr) %>% rename(lnr = lopenr_mor) %>% 
  filter(!is.na(lnr)) %>% filter(!is.na(w19_0634_lnr)) %>% 
  filter(lnr != "") %>%
  group_by(lnr) %>% mutate(ant_barn = n()) %>%
  distinct(lnr, .keep_all = T)

mor_pop = left_join(pop_par, mor, by="lnr") %>% select(-c(w19_0634_lnr))
mor_sampl = left_join(sam_par, mor, by="lnr") %>% select(-c(w19_0634_lnr))


cbind(rbind(mean(mor_pop$ant_barn, na.rm=T),mean(mor_sampl$ant_barn, na.rm=T)), 
      rbind(sd(mor_pop$ant_barn, na.rm=T),sd(mor_sampl$ant_barn, na.rm=T)))
print("mor",
mean(mor_pop$ant_barn, na.rm=T),
mean(mor_sampl$ant_barn, na.rm=T),
sd(mor_pop$ant_barn, na.rm=T),
sd(mor_sampl$ant_barn, na.rm=T))


# -- number of children fathers ----

far = faste %>% select(lopenr_far, w19_0634_lnr) %>% rename(lnr = lopenr_far) %>% 
  filter(!is.na(lnr)) %>% filter(!is.na(w19_0634_lnr)) %>% 
  filter(lnr != "") %>%
  group_by(lnr) %>% mutate(ant_barn = n()) %>%
  distinct(lnr, .keep_all = T)

far_pop = left_join(pop_par, far, by="lnr") %>% 
  filter(lnr != "") %>% select(-c(w19_0634_lnr))
far_sampl = left_join(sam_par, far, by="lnr") %>% 
  filter(lnr != " ") %>% select(-c(w19_0634_lnr))
print("far",
mean(far_pop$ant_barn, na.rm=T),
mean(far_sampl$ant_barn, na.rm=T),
sd(far_pop$ant_barn, na.rm=T),
sd(far_sampl$ant_barn, na.rm=T))

