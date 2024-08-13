library(tidyverse)
library(dat_sama.table)

path = ("N:/durable/projects/joakim/SES_composition/data/")
dat_pop = fread(paste0(path,"phenos/population_phenos.csv"))
dat_sam = fread(paste0(path,"phenos/sample_phenos.csv"))

faste = fread("N:/durable/data/registers/original 2023 04/csv/W19_0634_FASTE_OPPLYSNINGER.csv", na.strings = "", stringsAsFactors = F)
sex = faste %>% select(w19_0634_lnr, kjoenn) %>% rename(lnr = w19_0634_lnr, sex=kjoenn) %>% mutate(sex=if_else(sex==2,1,0))

dat_pop = left_join(dat_pop, sex, by = "lnr")
dat_sam = left_join(dat_sam, sex, by = "lnr")

colSums(dat_pop == 1, na.rm=T)
colSums(dat_pop == 0, na.rm=T)
colSums(dat_sam == 1, na.rm=T)
colSums(dat_sam == 0, na.rm=T)
