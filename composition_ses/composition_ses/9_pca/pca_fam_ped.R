library(OpenMx)
library(tidyverse)
library(psych)

path = ("N:/durable/projects/joakim/SES_composition/data/")
mod = readRDS(paste0(path,"fam_ped_mod.rds"))

#variable names
v_nms = c("edu","occ","inc","wea")
va = mxEval(va, mod)
dimnames(va) = list(v_nms, v_nms)
vc = mxEval(vc, mod)
dimnames(vc) = list(v_nms, v_nms)
ve = mxEval(ve, mod)
dimnames(ve) = list(v_nms, v_nms)

#correlation
cor_a = cov2cor(va)
cor_c = cov2cor(vc)
cor_e = cov2cor(ve)

#parallel analysis
fa.parallel(cor_a, n.obs=78742, fa ="pc")
fa.parallel(cor_c, n.obs=78742, fa ="pc")
fa.parallel(cor_e, n.obs=78742, fa ="pc")

#pca
eigen(cor_a)
pca_va = princomp(covmat = va, cor = T)
summary(pca_va)
pca_va$loadings
pca_va$loadings %*% diag(pca_va$sdev)

eigen(cor_c)
pca_vc = princomp(covmat = vc, cor = T)
summary(pca_vc)
pca_vc$loadings
pca_vc$loadings %*% diag(pca_vc$sdev)

eigen(cor_e)
pca_ve = princomp(covmat = ve, cor = T)
summary(pca_ve)
pca_ve$loadings
pca_ve$loadings %*% diag(pca_ve$sdev)

#variance explained
fam_va_expl_pc1 = pca_va$sdev[1]^2 / (pca_va$sdev[1]^2 + pca_va$sdev[2]^2 +pca_va$sdev[3]^2 +pca_va$sdev[4]^2 )
fam_vc_expl_pc1 = pca_vc$sdev[1]^2 / (pca_vc$sdev[1]^2 + pca_vc$sdev[2]^2 +pca_vc$sdev[3]^2 +pca_vc$sdev[4]^2 )
fam_ve_expl_pc1 = pca_ve$sdev[1]^2 / (pca_ve$sdev[1]^2 + pca_ve$sdev[2]^2 +pca_ve$sdev[3]^2 +pca_ve$sdev[4]^2 )
fam_ve_expl_pc2 = pca_ve$sdev[2]^2 / (pca_ve$sdev[1]^2 + pca_ve$sdev[2]^2 +pca_ve$sdev[3]^2 +pca_ve$sdev[4]^2 )

#standardized loadings
fam_va_loadings = pca_va$loadings %*% diag(pca_va$sdev) 
fam_vc_loadings = pca_vc$loadings %*% diag(pca_vc$sdev) 
fam_ve_loadings = pca_ve$loadings %*% diag(pca_ve$sdev) 
fam_va_loadings_pc1 = fam_va_loadings[,1]
fam_vc_loadings_pc1 = fam_vc_loadings[,1]
fam_ve_loadings_pc1 = fam_ve_loadings[,1]
fam_ve_loadings_pc2 = fam_ve_loadings[,2]

#export files
write.table(cor_a, "N:/durable/projects/joakim/SES_composition/results/correlations/cor_fam_a.txt")
write.table(cor_c, "N:/durable/projects/joakim/SES_composition/results/correlations/cor_fam_c.txt")
write.table(cor_e, "N:/durable/projects/joakim/SES_composition/results/correlations/cor_fam_e.txt")
write.table(fam_va_expl_pc1, file="N:/durable/projects/joakim/SES_composition/results/pca/fam_va_expl_pc1.txt", row.names = F, col.names = F)
write.table(fam_vc_expl_pc1, file="N:/durable/projects/joakim/SES_composition/results/pca/fam_vc_expl_pc1.txt", row.names = F, col.names = F)
write.table(fam_ve_expl_pc1, file="N:/durable/projects/joakim/SES_composition/results/pca/fam_ve_expl_pc1.txt", row.names = F, col.names = F)
write.table(fam_ve_expl_pc2, file="N:/durable/projects/joakim/SES_composition/results/pca/fam_ve_expl_pc2.txt", row.names = F, col.names = F)
write.table(fam_va_loadings_pc1, file="N:/durable/projects/joakim/SES_composition/results/pca/fam_va_loadings_pc1.txt", row.names = F, col.names = F)
write.table(fam_vc_loadings_pc1, file="N:/durable/projects/joakim/SES_composition/results/pca/fam_vc_loadings_pc1.txt", row.names = F, col.names = F)
write.table(fam_ve_loadings_pc1, file="N:/durable/projects/joakim/SES_composition/results/pca/fam_ve_loadings_pc1.txt", row.names = F, col.names = F)
write.table(fam_ve_loadings_pc2, file="N:/durable/projects/joakim/SES_composition/results/pca/fam_ve_loadings_pc2.txt", row.names = F, col.names = F)
