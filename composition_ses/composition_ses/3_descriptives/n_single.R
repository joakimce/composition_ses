library(tidyverse)
library(data.table)
#/ess/p805/data
path = ("/ess/p805/data/durable/projects/joakim/SES_composition/data/")
dat_pop = fread(paste0(path,"phenos/population_phenos.csv")) %>% select(lnr)
dat_sam = fread(paste0(path,"phenos/sample_phenos.csv")) %>% select(lnr)

faste = fread("/ess/p805/data/durable/data/registers/original 2023 04/csv/W19_0634_FASTE_OPPLYSNINGER.csv", na.strings = "", stringsAsFactors = F)
byr = faste %>% select(w19_0634_lnr,foedsels_aar_mnd) %>% rename(birth_year = foedsels_aar_mnd, lnr = w19_0634_lnr) %>%
  mutate(birth_year = as.numeric(substring(birth_year,1,4)))
sex = faste %>% select(w19_0634_lnr, kjoenn) %>% rename(lnr = w19_0634_lnr, sex=kjoenn) %>%  mutate(sex=if_else(sex==2,1,0))

partner = fread("/ess/p805/data/durable/data/registers/original 2023 04/csv/W19_0634_EKTEF_SAMBO.csv") %>% rename(lnr=w19_0634_lnr)
partner_long = partner %>% gather(key="relationship", value = "partner", -lnr) %>%
  mutate(year = str_sub(relationship, -4)) %>%
  mutate(relationship = substr(relationship, 8, nchar(relationship) - 5))

dat_pop = dat_pop %>% left_join(byr, by="lnr") %>% left_join(sex, by="lnr") %>% left_join(partner_long, by="lnr")
dat_sam = dat_sam %>% left_join(byr, by="lnr") %>% left_join(sex, by="lnr") %>% left_join(partner_long, by="lnr")

dat_pop_40 = dat_pop %>% mutate(yr40=birth_year+40) %>% filter(year == yr40) 
dat_pop_d = dat_pop_40 %>% select(lnr, sex, relationship, partner) %>% filter(relationship=="samboer") %>% 
  group_by(sex) %>% mutate(has_partner = if_else((partner==""),0,1)) 
                                                                                  
print("pop")
res_pop=(cbind(rbind(sum(dat_pop_d$sex == 0 & dat_pop_d$has_partner == 0),
sum(dat_pop_d$sex == 0 & dat_pop_d$has_partner == 1)),
rbind(sum(dat_pop_d$sex == 1 & dat_pop_d$has_partner == 0),
sum(dat_pop_d$sex == 1 & dat_pop_d$has_partner == 1))))
colnames(res_pop) = c("no partner","has partner")
rownames(res_pop) = c("male","female")
(res_pop)
(sum(dat_pop_d$sex == 0))
(sum(dat_pop_d$sex == 1))


dat_sam_40 = dat_sam %>% mutate(yr40=birth_year+40) %>% filter(year == yr40) 
dat_sam_d = dat_sam_40 %>% select(lnr, sex, relationship, partner) %>% filter(relationship=="samboer") %>% 
  group_by(sex) %>% mutate(has_partner = if_else((partner==""),0,1)) 

print("sam")
res_sam=(cbind(rbind(sum(dat_sam_d$sex == 0 & dat_sam_d$has_partner == 0),
                     sum(dat_sam_d$sex == 0 & dat_sam_d$has_partner == 1)),
               rbind(sum(dat_sam_d$sex == 1 & dat_sam_d$has_partner == 0),
                     sum(dat_sam_d$sex == 1 & dat_sam_d$has_partner == 1))))
colnames(res_sam) = c("no partner","has partner")
rownames(res_sam) = c("male","female")
(res_sam)
(sum(dat_sam_d$sex == 0))
(sum(dat_sam_d$sex == 1))
