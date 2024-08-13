library(tidyverse)
library(data.table)

path = ("N:/durable/projects/joakim/SES_composition/data/")
dat_pop = fread(paste0(path,"phenos/population_phenos.csv")) 
dat_sam = fread(paste0(path,"phenos/sample_phenos.csv"))

sex = fread("N:/durable/data/registers/original 2023 04/csv/W19_0634_FASTE_OPPLYSNINGER.csv", na.strings = "", stringsAsFactors = F) %>% 
  select(w19_0634_lnr, kjoenn) %>% rename(lnr = w19_0634_lnr, sex=kjoenn) %>%  mutate(sex=if_else(sex==2,1,0))

dat_pop_m = dat_pop %>% left_join(sex, by="lnr") %>% filter(sex == 0)
dat_pop_f = dat_pop %>% left_join(sex, by="lnr") %>% filter(sex == 1)
dat_sam_m = dat_sam %>% left_join(sex, by="lnr") %>% filter(sex == 0)
dat_sam_f = dat_sam %>% left_join(sex, by="lnr") %>% filter(sex == 1)


sum(dat_pop_m$edu_yrs == 7, na.rm=T)
sum(dat_pop_m$edu_yrs == 7, na.rm=T)/nrow(dat_pop_m)*100
sum(dat_sam_m$edu_yrs == 7, na.rm=T)
sum(dat_sam_m$edu_yrs == 7, na.rm=T)/nrow(dat_sam_m)*100

sum(dat_pop_m$edu_yrs == 10, na.rm=T)
sum(dat_pop_m$edu_yrs == 10, na.rm=T)/nrow(dat_pop_m)*100
sum(dat_sam_m$edu_yrs == 10, na.rm=T)
sum(dat_sam_m$edu_yrs == 10, na.rm=T)/nrow(dat_sam_m)*100

sum(dat_pop_m$edu_yrs == 13, na.rm=T)
sum(dat_pop_m$edu_yrs == 13, na.rm=T)/nrow(dat_pop_m)*100
sum(dat_sam_m$edu_yrs == 13, na.rm=T)
sum(dat_sam_m$edu_yrs == 13, na.rm=T)/nrow(dat_sam_m)*100

sum(dat_pop_m$edu_yrs == 15, na.rm=T)
sum(dat_pop_m$edu_yrs == 15, na.rm=T)/nrow(dat_pop_m)*100
sum(dat_sam_m$edu_yrs == 15, na.rm=T)
sum(dat_sam_m$edu_yrs == 15, na.rm=T)/nrow(dat_sam_m)*100

sum(dat_pop_m$edu_yrs == 19, na.rm=T)
sum(dat_pop_m$edu_yrs == 19, na.rm=T)/nrow(dat_pop_m)*100
sum(dat_sam_m$edu_yrs == 19, na.rm=T)
sum(dat_sam_m$edu_yrs == 19, na.rm=T)/nrow(dat_sam_m)*100

sum(dat_pop_m$edu_yrs == 20, na.rm=T)
sum(dat_pop_m$edu_yrs == 20, na.rm=T)/nrow(dat_pop_m)*100
sum(dat_sam_m$edu_yrs == 20, na.rm=T)
sum(dat_sam_m$edu_yrs == 20, na.rm=T)/nrow(dat_sam_m)*100

sum(dat_pop_m$edu_yrs == 22, na.rm=T)
sum(dat_pop_m$edu_yrs == 22, na.rm=T)/nrow(dat_pop_m)*100
sum(dat_sam_m$edu_yrs == 22, na.rm=T)
sum(dat_sam_m$edu_yrs == 22, na.rm=T)/nrow(dat_sam_m)*100

#female

sum(dat_pop_f$edu_yrs == 7, na.rm=T)
sum(dat_pop_f$edu_yrs == 7, na.rm=T)/nrow(dat_pop_f)*100
sum(dat_sam_f$edu_yrs == 7, na.rm=T)
sum(dat_sam_f$edu_yrs == 7, na.rm=T)/nrow(dat_sam_f)*100

sum(dat_pop_f$edu_yrs == 10, na.rm=T)
sum(dat_pop_f$edu_yrs == 10, na.rm=T)/nrow(dat_pop_f)*100
sum(dat_sam_f$edu_yrs == 10, na.rm=T)
sum(dat_sam_f$edu_yrs == 10, na.rm=T)/nrow(dat_sam_f)*100

sum(dat_pop_f$edu_yrs == 13, na.rm=T)
sum(dat_pop_f$edu_yrs == 13, na.rm=T)/nrow(dat_pop_f)*100
sum(dat_sam_f$edu_yrs == 13, na.rm=T)
sum(dat_sam_f$edu_yrs == 13, na.rm=T)/nrow(dat_sam_f)*100

sum(dat_pop_f$edu_yrs == 15, na.rm=T)
sum(dat_pop_f$edu_yrs == 15, na.rm=T)/nrow(dat_pop_f)*100
sum(dat_sam_f$edu_yrs == 15, na.rm=T)
sum(dat_sam_f$edu_yrs == 15, na.rm=T)/nrow(dat_sam_f)*100

sum(dat_pop_f$edu_yrs == 19, na.rm=T)
sum(dat_pop_f$edu_yrs == 19, na.rm=T)/nrow(dat_pop_f)*100
sum(dat_sam_f$edu_yrs == 19, na.rm=T)
sum(dat_sam_f$edu_yrs == 19, na.rm=T)/nrow(dat_sam_f)*100

sum(dat_pop_f$edu_yrs == 20, na.rm=T)
sum(dat_pop_f$edu_yrs == 20, na.rm=T)/nrow(dat_pop_f)*100
sum(dat_sam_f$edu_yrs == 20, na.rm=T)
sum(dat_sam_f$edu_yrs == 20, na.rm=T)/nrow(dat_sam_f)*100

sum(dat_pop_f$edu_yrs == 22, na.rm=T)
sum(dat_pop_f$edu_yrs == 22, na.rm=T)/nrow(dat_pop_f)*100
sum(dat_sam_f$edu_yrs == 22, na.rm=T)
sum(dat_sam_f$edu_yrs == 22, na.rm=T)/nrow(dat_sam_f)*100



x = edu$ISCED11
edu$edu_yrs<-ifelse(is.na(x), NA,
                    ifelse(!is.na(x) & x == 0, 1, # combines ppl with edu up to age 2 & up to age 5
                           ifelse(!is.na(x) & x == 1, 7, # primary
                                  ifelse(!is.na(x) & x == 2, 10, # secondary lower
                                         ifelse(!is.na(x) & x == 3, 13,# secondary upper AND post secondary non tertiary shorter than 2 yrs
                                                ifelse(!is.na(x) & x == 5, 15,  # postsecondary vocational 0.5-1.5 or 2 years tertiary--not distinguished so dunno if isced 4 vs 5
                                                       ifelse(!is.na(x) & x == 6, 19, #undergrad
                                                              ifelse(!is.na(x) & x == 7, 20, #MSc/MA
                                                                     ifelse(!is.na(x) & x == 8, 22, x))))))))) #PhD

