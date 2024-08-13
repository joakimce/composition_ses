# import phenotypes ----

library(data.table)
library(tidyverse)

path = ("N:/durable/projects/joakim/SES_composition/data/")
dat = fread(paste0(path,"phenos/sample_phenos.csv"))

# - log income and wealth ----

dat = dat %>%
  mutate_all(~ifelse(. <= 0, 1, .)) %>%
  mutate(wea_log = log(wea_avg)) %>%
  mutate(inc_log = log(inc_avg)) %>%
  mutate_all(~ifelse(. == -Inf, NA, .)) %>%
  mutate(wea_log = ifelse(wea_log<0,NA, wea_log))

dat = dat %>% rename(edu=edu_years,occ=occ_avg,inc=inc_log,wea=wea_log)


# -- link family (FID) and individual (IID) genetic IDs to phenotypes ----

moba_to_lnr_f = fread("N:/durable/data/moba/linkage/PDB2601_kobling_SSB_v12.csv") %>% rename ( c(role = rolle, lnr = w19_0634_lnr)) %>% filter(role=="SU2PT_FATHER")
moba_to_lnr_m = fread("N:/durable/data/moba/linkage/PDB2601_kobling_SSB_v12.csv") %>% rename ( c(role = rolle, lnr = w19_0634_lnr)) %>% filter(role=="SU2PT_MOTHER")
fam_gen = read.table("N:/durable/data/genetics/MoBaPsychGen_v1/MoBaPsychGen_v1-ec-eur-batch-basic-qc.fam", quote="\"", comment.char="") %>% select(V1,V2) %>% rename(FID=V1, SENTRIX_ID=V2)
preg_id_f = fread("N:/durable/data/moba/MoBaGenetics/key_CENTRIX_ID_Aug_22/PDB2601_SV_INFO_V12_20220829.csv") %>% select(PREG_ID_2601, F_ID_2601)
preg_id_m = fread("N:/durable/data/moba/MoBaGenetics/key_CENTRIX_ID_Aug_22/PDB2601_SV_INFO_V12_20220829.csv") %>% select(PREG_ID_2601, M_ID_2601)
c2m_f = fread("N:/durable/data/moba/MoBaGenetics/key_CENTRIX_ID_Aug_22/PDB2601_MoBaGeneticsTot_Father_20220829.csv") %>% select(SENTRIX_ID,F_ID_2601) 
c2m_m = fread("N:/durable/data/moba/MoBaGenetics/key_CENTRIX_ID_Aug_22/PDB2601_MoBaGeneticsTot_Mother_20220829.csv") %>% select(SENTRIX_ID,M_ID_2601) 

link1 = left_join(fam_gen, c2m_m, by="SENTRIX_ID") %>% left_join(c2m_f, by="SENTRIX_ID") 
link2 = left_join(link1, preg_id_f, by="F_ID_2601") %>% left_join(moba_to_lnr_f, by="PREG_ID_2601") 
link3 = left_join(link1, preg_id_m, by="M_ID_2601") %>% left_join(moba_to_lnr_m, by="PREG_ID_2601")
link4 = rbind(link2, link3)
link5 =  link4 %>% filter(!is.na(lnr))
link = link5 %>% select(FID, SENTRIX_ID, lnr) %>% rename(IID =SENTRIX_ID) %>% distinct(., .keep_all = T)

dat_gen = left_join(dat, link, by="lnr") %>% select(FID, IID, lnr, edu, occ, inc, wea) %>% distinct(IID, .keep_all = T) %>% filter(!is.na(IID))

nrow(dat_gen)-colSums(is.na(dat_gen))

dat_gcta = dat_gen %>% ungroup() %>% select(FID,IID,edu,occ,inc,wea)


# --- export files ----

write.csv(dat, file= paste0(path,"phenos/phenos_log.csv"), row.names = F, quote=F)

write.csv(dat_gen, file= paste0(path,"phenos/phenos_gen_lnr.csv"), row.names = F, quote=F)

write.table(dat_gcta, file= paste0(path,"gcta/phenos_gen.txt"), row.names = F, col.names = F, quote=F)






