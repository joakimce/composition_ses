#  Import libraries and prepare data----

library(OpenMx)
library(data.table)
library(tidyverse)


path = ("N:/durable/projects/joakim/SES_composition/")
moba_rels = fread("N:/durable/users/nhe/moba_parents_rel9.csv") %>% dplyr::select(x, rel, i_lnr, j_lnr) %>% rename(cor=x) #relatives
p = fread(paste0(path,"data/phenos/phenos_gen_lnr.csv"), header=T) #phenotypes

#add sex
sex = fread("N:/durable/data/moba/linkage/PDB2601_kobling_SSB_v12.csv") %>% filter(rolle != "SU2PT_CHILD") %>% 
  rename(lnr = w19_0634_lnr, sex = rolle) %>% select(lnr, sex) %>% mutate(sex = ifelse(sex=="SU2PT_MOTHER",1,0)) %>% filter(!is.na(sex))     
p = left_join(p, sex, by ="lnr")

#- make i and j sibling pairs and add ibd data ----

i_dat = p %>% ungroup() %>% select(lnr,sex,edu,occ,inc,wea)%>%
  rename(i_lnr=lnr,i_sex=sex,i_edu=edu,i_occ=occ,i_inc=inc,i_wea=wea)
j_dat = p %>% ungroup() %>% select(lnr,sex,edu,occ,inc,wea)%>%
  rename(j_lnr=lnr,j_sex=sex,j_edu=edu,j_occ=occ,j_inc=inc,j_wea=wea)

#ibd data
fs_dat = read.table(paste0(path,"data/phenos_prep/fs_par_ibd.txt"), header = T) %>% select(PropIBD,lnr1,lnr2) %>% rename(i_lnr = lnr1, j_lnr = lnr2, cor = PropIBD)
fs_dat = fs_dat %>% left_join(i_dat, by="i_lnr", relationship = "many-to-many") %>% left_join(j_dat, by="j_lnr", relationship = "many-to-many") %>% 
  filter(!is.na(j_sex)) %>% filter(!is.na(i_sex)) %>% distinct(., .keep_all = T)

#-- variable names and covariances----

v_nms = c("edu","occ","inc","wea")
vc_vrs = cov(fs_dat[, c("i_edu","i_occ","i_inc","i_wea")], use = "pairwise")

#--- mx model ----

modAE = mxModel(
  mxMatrix("Lo", 4, 4, T, t(chol(0.5 * vc_vrs)), name = "la"),
  mxMatrix("Lo", 4, 4, F, 0, name = "lc"),
  mxMatrix("Lo", 4, 4, T, t(chol(0.4 * vc_vrs)), name = "le"),
  mxAlgebra(la%*%t(la),name="va"),
  mxAlgebra(lc%*%t(lc),name="vc"),
  mxAlgebra(le%*%t(le),name="ve"),
  mxAlgebra(rbind(cbind(1,data.cor),
                  cbind(data.cor,1)), name="cora"),
  mxAlgebra(rbind(cbind(1,1),
                  cbind(1,1)), name="corc"),
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
modAE = mxModel(modAE, mxData(fs_dat, "raw"))
modAE = mxTryHard(modAE, intervals = T, extraTries = 10)

#---- save model ----

saveRDS(mod, file = paste0(path,"data/ibd_mod_ad.rds"))

#----- export variance estimates in .csv file ----
va = mxEval(va, modAE)
dimnames(va) = list(v_nms, v_nms)
vc = mxEval(vc, modAE)
dimnames(vc) = list(v_nms, v_nms)
ve = mxEval(ve, modAE)
dimnames(ve) = list(v_nms, v_nms)
h2 = diag(va / (va + vc + ve))
c2 = diag(vc / (va + vc + ve))
e2 = diag(ve / (va + vc + ve))
ibd = cbind(h2, c2, e2)
(ibd)
write.csv(ibd, file = paste0(path,"results/ibdAE.csv"))


#----- export variance estimate SE in .csv file ----

ibd_se = cbind(mxSE(h2, modAE),mxSE(c2, modAE),mxSE(e2, modAE))
colnames(ibd_se) = c("h2_se","c2_se","e2_se")
rownames(ibd_se) = v_nms
(ibd_se)
write.csv(ibd_se, file = paste0(path,"results/ibdAE_SE.csv"))

