# import libraries and files----

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


#------ACE model rsib=1 , rcous = various ----

#Siblings in group g1 and first cousins in group g2
rel_dat$g1 = ifelse(rel_dat$cor > .13, 1, 0) 
rel_dat$g2 = ifelse(rel_dat$cor > .13, 0, 1)

#different r for first cousins
r = c(.7,.65,.60,.55,.50,.45,.40,.35,.30,.25,.20,.15,.10,.05,0)

for(i in 1:length(r)){
  xr = r[i]
  modACE = mxModel(
    mxMatrix("Lo", 4, 4, T, t(chol(0.5 * vc_vrs)), name = "la"),
    mxMatrix("Lo", 4, 4, T, t(chol(0.1 * vc_vrs)), name = "lc"),
    mxMatrix("Lo", 4, 4, T, t(chol(0.4 * vc_vrs)), name = "le"),
    mxAlgebra(la%*%t(la),name="va"),
    mxAlgebra(lc%*%t(lc),name="vc"),
    mxAlgebra(le%*%t(le),name="ve"),
    mxAlgebra(data.g1 * 1 + data.g2 * xr, name = "rc"),
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
  modACE = mxModel(modACE, mxData(rel_dat, "raw"))
  modACE = mxTryHard(modACE, intervals = T, extraTries = 10)
    
  saveRDS(modACE, file = paste0(path,"data/fam_ped_modACE_sib-r1_cous-r",r[i],".rds"))
}

