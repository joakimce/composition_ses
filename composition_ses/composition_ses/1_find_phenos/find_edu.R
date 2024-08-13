
library(tidyverse)
library(data.table)

edu_in <- fread("/ess/p805/data/durable/data/registers/delivery_202310/csv/W19_0634_BU_UTD.csv") %>%
  select(-starts_with("igang")) %>% rename(lnr = w19_0634_lnr)

edu_long = edu_in %>% gather(key="year", value = "edu", -lnr) %>%
  mutate(year = as.numeric(sub("^BU_", "", year))) 

lnr_byr = fread("/ess/p805/data/durable/data/registers/original 2023 04/csv/W19_0634_FASTE_OPPLYSNINGER.csv", na.strings = "", stringsAsFactors = F) %>% 
  select(w19_0634_lnr,foedsels_aar_mnd) %>% rename(birth_year = foedsels_aar_mnd, lnr = w19_0634_lnr) %>%
  mutate(birth_year = as.numeric(substring(birth_year,1,4))) 

edu = left_join(edu_long, lnr_byr, by="lnr") %>% 
  filter(year-birth_year <=45 & year-birth_year >=35)

edu = edu %>%
  group_by(lnr) %>%
  filter(edu == max(edu)) %>%
  mutate(edu = as.numeric(str_sub(edu,1,1)))

x<-edu$edu
edu$ISCED11 = ifelse(is.na(x), NA,
                     ifelse(!is.na(x) & x == 0, 0, # combines ppl with edu up to age 2 & up to age 5
                            ifelse(!is.na(x) & x == 1, 1, # primary
                                   ifelse(!is.na(x) & x == 2, 2, # secondary lower
                                          ifelse(!is.na(x) & x == 4 | x == 3, 3, # secondary upper AND post secondary non tertiary shorter than 2 yrs
                                                 ifelse(!is.na(x) & x == 5, 5,  # postsecondary vocational 0.5-1.5 or 2 years tertiary--not distinguished so dunno if isced 4 vs 5
                                                        ifelse(!is.na(x) & x == 6, 6, #undergrad
                                                               ifelse(!is.na(x) & x == 7, 7, #MSc/MA
                                                                      ifelse(!is.na(x) & x == 8, 8, x))))))))) #PhD

x<-edu$ISCED11
edu$edu_years<-ifelse(is.na(x), NA,
                      ifelse(!is.na(x) & x == 0, 1, # combines ppl with edu up to age 2 & up to age 5
                             ifelse(!is.na(x) & x == 1, 7, # primary
                                    ifelse(!is.na(x) & x == 2, 10, # secondary lower
                                           ifelse(!is.na(x) & x == 3, 13,# secondary upper AND post secondary non tertiary shorter than 2 yrs
                                                  ifelse(!is.na(x) & x == 5, 14.5,  # postsecondary vocational 0.5-1.5 or 2 years tertiary--not distinguished so dunno if isced 4 vs 5
                                                         ifelse(!is.na(x) & x == 6, 16, #undergrad
                                                                ifelse(!is.na(x) & x == 7, 18, #MSc/MA
                                                                       ifelse(!is.na(x) & x == 8, 21, x))))))))) #PhD

edu = edu %>% select(lnr,edu_years) %>% distinct(.keep_all = TRUE)

write.csv(edu, "/ess/p805/data/durable/projects/joakim/SES_composition/data/phenos/edu.csv", row.names = F)


