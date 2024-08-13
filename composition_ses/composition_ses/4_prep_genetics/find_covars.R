library(data.table)
library(tidyverse)

covar = fread("N:/durable/data/genetics/MoBaPsychGen_v1/MoBaPsychGen_v1-ec-eur-batch-basic-qc-cov-noMoBaIDs.txt")
batch = covar %>% select(c(IID,genotyping_batch))
pcs = covar %>% select(c(3,14:33))

gen = fread("N:/durable/projects/joakim/SES_composition/data/phenos/phenos_gen_lnr.csv") %>% select(FID,IID,lnr)

faste = fread("N:/durable/data/registers/original 2023 04/csv/W19_0634_FASTE_OPPLYSNINGER.csv", na.strings = "", stringsAsFactors = F)
byr = faste %>% select(w19_0634_lnr,foedsels_aar_mnd) %>% rename(birth_year = foedsels_aar_mnd, lnr = w19_0634_lnr) %>%
  mutate(birth_year = as.numeric(substring(birth_year,1,4)))
sex = faste %>% select(w19_0634_lnr, kjoenn) %>% rename(lnr = w19_0634_lnr, sex=kjoenn) %>%  mutate(sex=if_else(sex==2,1,0))

q_covars = left_join(gen, pcs, by="IID") %>% left_join(byr, by="lnr") %>%
  mutate(age=2023-birth_year) %>%
  select(-c(lnr,birth_year))

n_covars = left_join(gen, batch, by="IID") %>% left_join(sex, by="lnr") %>% select(-c(lnr))


write.table(q_covars, file = "N:/durable/projects/joakim/SES_composition/data/gcta/q_covars.txt", row.names = F, col.names = F, quote=F)
write.table(n_covars, file = "N:/durable/projects/joakim/SES_composition/data/gcta/n_covars.txt", row.names = F, col.names = F, quote=F)
