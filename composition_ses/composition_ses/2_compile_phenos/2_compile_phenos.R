
#import libraries and create path object ----

library(tidyverse)
library(data.table)

#- import parents with children born 1999-2008 population (pop) and sample (sam) parents. inlcude sex variable for sample ----

path = ("N:/durable/projects/joakim/SES_composition/data/")
pop_par = fread(paste0(path,"phenos_prep/eligible_parents_n883147.csv"), header = T) %>% rename(lnr=w19_0634_lnr)
sam_par = fread("N:/durable/data/moba/linkage/PDB2601_kobling_SSB_v12.csv") %>% filter(rolle != "SU2PT_CHILD") %>% rename(lnr = w19_0634_lnr) %>% select(lnr)     


# birh year of sample
lnr_byr = fread("N:/durable/data/registers/original 2023 04/csv/W19_0634_FASTE_OPPLYSNINGER.csv", na.strings = "", stringsAsFactors = F) %>% 
  select(w19_0634_lnr,foedsels_aar_mnd) %>% rename(birth_year = foedsels_aar_mnd, lnr = w19_0634_lnr) %>%
  mutate(birth_year = as.numeric(substring(birth_year,1,4))) 

byr = left_join(sam_par, lnr_byr, by = "lnr")
hist(byr$birth_year)

table(byr$birth_year)
mean(byr$birth_year, na.rm=T)
which.max(byr$birth_year)
sd(byr$birth_year, na.rm=T)

#--- import education, occupation, income, wealth ----

edu = fread(paste0(path,"phenos/edu.csv"))

occ = fread(paste0(path,"phenos/occ.csv")) %>% select(-c(V1))

inc = fread(paste0(path,"phenos/inc.csv"))

wea = fread(paste0(path,"phenos/wea.csv"))


#-------- dataframes with all measures population and sample  ----

dat_pop = pop_par %>% left_join(edu, by="lnr") %>% left_join(occ, by="lnr") %>% left_join(inc, by="lnr") %>% left_join(wea, by="lnr") %>% distinct(lnr, .keep_all = T)
dat_sam = sam_par %>% left_join(edu, by="lnr") %>% left_join(occ, by="lnr")  %>% left_join(inc, by="lnr") %>% left_join(wea, by="lnr") %>% distinct(lnr, .keep_all = T)

nrow(dat_pop)-colSums(is.na(dat_pop))
nrow(dat_sam)-colSums(is.na(dat_sam))

write.csv(dat_pop, file = paste0(path,"phenos/population_phenos.csv"), row.names=F)
write.csv(dat_sam, file = paste0(path,"phenos/sample_phenos.csv"), row.names=F)

