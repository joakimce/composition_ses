library(OpenMx)
library(tidyverse)
library(psych)
library(factoextra)

path = ("N:/durable/projects/joakim/SES_composition/data/")
mod = readRDS(paste0(path,"ibd_mod.rds"))

#variable names
v_nms = c("edu","occ","inc","wea")
va = mxEval(va, mod)
dimnames(va) = list(v_nms, v_nms)
ve = mxEval(ve, mod)
dimnames(ve) = list(v_nms, v_nms)

#correlation
cor_a = cov2cor(va)
cor_e = cov2cor(ve)

#parallel analysis
fa.parallel(cor_a, n.obs=22982, fa ="pc")
fa.parallel(cor_e, n.obs=22982, fa ="pc")

#pca
eigen(cor_a)
pca_va = princomp(covmat = mod$va$result, cor = T)
summary(pca_va)
pca_va$loadings
ibd_va_loadings = pca_va$loadings %*% diag(pca_va$sdev) 

eigen(cor_e)
pca_ve = princomp(covmat = mod$ve$result, cor = T)
summary(pca_ve)
pca_ve$loadings
pca_ve$loadings %*% diag(pca_ve$sdev)
pca_ve$loadings = pca_ve$loadings %*% diag(pca_ve$sdev)
ibd_ve_loadings = pca_ve$loadings %*% diag(pca_ve$sdev)

#loadings graph
rownames(pca_ve$loadings) = c("Education", "Occupation", "Income", "Wealth")
fviz_pca_var(pca_ve,
             col.var = "contrib", # Color by contributions to the PC
             #gradient.cols = c("#00AFBB", "#E7B800"),
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             title = "")

#explained variance
ibd_va_expl_pc1 = pca_va$sdev[1]^2 / (pca_va$sdev[1]^2 + pca_va$sdev[2]^2 +pca_va$sdev[3]^2 +pca_va$sdev[4]^2 )
ibd_ve_expl_pc1 = pca_ve$sdev[1]^2 / (pca_ve$sdev[1]^2 + pca_ve$sdev[2]^2 +pca_ve$sdev[3]^2 +pca_ve$sdev[4]^2 )
ibd_ve_expl_pc2 = pca_ve$sdev[2]^2 / (pca_ve$sdev[1]^2 + pca_ve$sdev[2]^2 +pca_ve$sdev[3]^2 +pca_ve$sdev[4]^2 )

#standardize loadings
ibd_va_loadings_pc1 = ibd_va_loadings[,1]
ibd_ve_loadings_pc1 = ibd_ve_loadings[,1]
ibd_ve_loadings_pc2 = ibd_ve_loadings[,2]

#export data
write.table(cor_a, "N:/durable/projects/joakim/SES_composition/results/correlations/cor_ibd_a.txt")
write.table(cor_e, "N:/durable/projects/joakim/SES_composition/results/correlations/cor_ibd_e.txt")
write.table(ibd_va_expl_pc1, file="N:/durable/projects/joakim/SES_composition/results/pca/ibd_va_expl_pc1.txt", row.names = F, col.names = F)
write.table(ibd_ve_expl_pc1, file="N:/durable/projects/joakim/SES_composition/results/pca/ibd_ve_expl_pc1.txt", row.names = F, col.names = F)
write.table(ibd_ve_expl_pc2, file="N:/durable/projects/joakim/SES_composition/results/pca/ibd_ve_expl_pc2.txt", row.names = F, col.names = F)
write.table(ibd_va_loadings_pc1, file="N:/durable/projects/joakim/SES_composition/results/pca/ibd_va_loadings_pc1.txt", row.names = F, col.names = F)
write.table(ibd_ve_loadings_pc1, file="N:/durable/projects/joakim/SES_composition/results/pca/ibd_ve_loadings_pc1.txt", row.names = F, col.names = F)
write.table(ibd_ve_loadings_pc2, file="N:/durable/projects/joakim/SES_composition/results/pca/ibd_ve_loadings_pc2.txt", row.names = F, col.names = F)
