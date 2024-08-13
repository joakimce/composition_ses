# import libraries and data ----

library(tidyverse)
library(OpenMx)
library(data.table)

path = ("N:/durable/projects/joakim/SES_composition/")
moba_rels = fread("N:/durable/users/nhe/moba_parents_rel9.csv") %>% dplyr::select(x, rel, i_lnr, j_lnr) %>% rename(cor=x)
p = fread(paste0(path,"data/phenos/phenos_log.csv"), header=T) #phenotypes

#- add sex variable from Moba-linkage----

sex = fread("N:/durable/data/moba/linkage/PDB2601_kobling_SSB_v12.csv") %>% filter(rolle != "SU2PT_CHILD") %>% 
  rename(lnr = w19_0634_lnr, sex = rolle) %>% select(lnr, sex) %>% mutate(sex = ifelse(sex=="SU2PT_MOTHER",1,0)) %>% filter(!is.na(sex))     
p = left_join(p, sex, by ="lnr")


#-- make i and j rel pairs for education, occupation, income, and wealth ----

i_dat = p %>% ungroup() %>% select(lnr,sex,edu,occ,inc,wea)%>%
  rename(i_lnr=lnr,i_sex=sex,i_edu=edu,i_occ=occ,i_inc=inc,i_wea=wea)
j_dat = p %>% ungroup() %>% select(lnr,sex,edu,occ,inc,wea)%>%
  rename(j_lnr=lnr,j_sex=sex,j_edu=edu,j_occ=occ,j_inc=inc,j_wea=wea)

rel_dat = left_join(moba_rels,i_dat,by="i_lnr", relationship = "many-to-many") %>% left_join(j_dat,by="j_lnr", relationship = "many-to-many") %>%
  filter(!is.na(cor)) %>% filter(!is.na(j_sex)) %>% filter(!is.na(i_sex)) 

#remove half-sibs
rel_dat %>% filter(rel != "half_sibs")

#--- variable names and covariances----

v_nms = c("edu","occ","inc","wea")
vc_vrs = cov(rel_dat[, c("i_edu","i_occ","i_inc","i_wea")], use = "pairwise")

#----------------------------ACE rsib = 1, rcous = est --------------------------------------

#Siblings in group g1 and first cousins in group g2

rel_dat$g1 = ifelse(rel_dat$cor > .125, 1, 0) 
rel_dat$g2 = ifelse(rel_dat$cor > .125, 0, 1)

modACE = mxModel(
  mxMatrix("Lo", 4, 4, T, t(chol(0.5 * vc_vrs)), name = "la"),
  mxMatrix("Lo", 4, 4, T, t(chol(0.1 * vc_vrs)), name = "lc"),
  mxMatrix("Lo", 4, 4, T, t(chol(0.4 * vc_vrs)), name = "le"),
  mxAlgebra(la%*%t(la),name="va"),
  mxAlgebra(lc%*%t(lc),name="vc"),
  mxAlgebra(le%*%t(le),name="ve"),
  
  mxMatrix("Fu", 1, 1, T, 0.1, "rg2", name = "rg2_mat"),
  mxAlgebra(data.g1 * 1 + data.g2 * rg2, name = "rc"),
  
  mxAlgebra(rbind(cbind(1,data.cor),
                  cbind(data.cor,1)), name="cora"),
  mxAlgebra(rbind(cbind(1,rc),
                  cbind(rc,1)), name="corc"),
  mxAlgebra(rbind(cbind(1,0),
                  cbind(0,1)), name="core"),
  mxAlgebra(cora%x%va + corc%x%vc + core%x%ve, name="V"),
  mxMatrix("Fu", 1, 4, T, 0, paste0("male_", v_nms), name = "Mmale"),
  mxMatrix("Fu", 1, 4, T, 0, paste0("intercept_", v_nms), name = "Mconst"),
  mxAlgebra(cbind(Mconst + Mmale * data.i_sex, Mconst + Mmale * data.j_sex), name = "means"),
  mxAlgebra(diag2vec(va / (va + vc + ve)), name="h2"),
  mxAlgebra(diag2vec(vc / (va + vc + ve)), name="c2"),
  mxAlgebra(diag2vec(ve / (va + vc + ve)), name="e2"),
  mxExpectationNormal("V", "means", dimnames = c("i_edu","i_inc","i_wea","i_occ","j_edu","j_inc","j_wea","j_occ")),
  mxFitFunctionML()
)

#----- Run model ----

modACE = mxModel(modACE, mxData(rel_dat, "raw"))
modACE = mxTryHard(modACE, intervals = T, extraTries = 30)


#----- save model ----

saveRDS(modACE, file = paste0(path,"data/fam_ped_modACE.rds"))


#----- export variance estimates in .csv file ----

modACE = readRDS("N:/durable/projects/joakim/SES_composition/data/fam_ped_modACE.rds")

va = mxEval(va, modACE)
dimnames(va) = list(v_nms, v_nms)
vc = mxEval(vc, modACE)
dimnames(vc) = list(v_nms, v_nms)
ve = mxEval(ve, modACE)
dimnames(ve) = list(v_nms, v_nms)
h2 = diag(va / (va + vc + ve))
c2 = diag(vc / (va + vc + ve))
e2 = diag(ve / (va + vc + ve))
fam = cbind(h2, c2, e2)
(fam)

write.csv(fam, file = paste0(path,"results/fam_ped_ACE_est.csv"))

#----- export variance estimate SE in .csv file ----

fam_se = cbind(mxSE(h2, modACE),mxSE(c2, modACE),mxSE(e2, modACE))
colnames(fam_se) = c("h2_se","c2_se","e2_se")
rownames(fam_se) = v_nms
(fam_se)

write.csv(fam_se, file = paste0(path,"results/fam_ped_ACE_est_SE.csv"))


