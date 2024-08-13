library("devtools")
library(GenomicSEM)
library(psych)

load("N:/durable/projects/joakim/SES_composition/results/ses_ldsr_out.RData")

cor_ldsr = cov2cor(ses_ldsr_out$S)
v_nms = c("edu","occ","inc","wea")
dimnames(cor_ldsr) = list(v_nms, v_nms)

#parallel analysis
fa.parallel(cor_ldsr,n.obs = min(ses_ldsr_out$N), fa ="pc")

#pca
eigen(cor_ldsr)
pca_ldsr = princomp(covmat = cor_ldsr, cor = T)
summary(pca_ldsr)
pca_ldsr$loadings
pca_ldsr$loadings %*% diag(pca_ldsr$sdev)

#variance explained
ldsr_expl_pc1 = pca_ldsr$sdev[1]^2 / (pca_ldsr$sdev[1]^2 + pca_ldsr$sdev[2]^2 +pca_ldsr$sdev[3]^2 +pca_ldsr$sdev[4]^2 )
ldsr_loadings = pca_ldsr$loadings %*% diag(pca_ldsr$sdev) 
ldsr_loadings_pc1 = ldsr_loadings[,1]

#standardized loadings
write.table(cor_ldsr, "N:/durable/projects/joakim/SES_composition/results/correlations/cor_ldsr.txt")
write.table(ldsr_expl_pc1, file="N:/durable/projects/joakim/SES_composition/results/pca/ldsr_expl_pc1.txt", row.names = F, col.names = F)
write.table(ldsr_loadings_pc1, file="N:/durable/projects/joakim/SES_composition/results/pca/ldsr_loadings_pc1.txt", row.names = F, col.names = F)
