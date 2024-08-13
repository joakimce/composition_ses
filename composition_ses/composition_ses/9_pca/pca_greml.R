library(data.table)
library(psych)

path = ("N:/durable/projects/joakim/SES_composition/results/")
greml_cor = fread(paste0(path,"correlations/cor_greml.txt")) %>% select(-1)

#correlations
cor_greml = as.matrix(greml_cor)
greml_r_cov = fread(paste0(path,"greml_e_cov.txt")) %>% select(-1) 
cor_r_greml = cov2cor(as.matrix(greml_r_cov))
rownames(cor_r_greml) = c("edu","occ","inc","wea")

#parallel analysis
fa.parallel(cor_greml, n.obs=36277, fa ="pc") #additive genetics
fa.parallel(cor_r_greml,n.obs=36277, fa ="pc") #residual

#pca
pca_greml = princomp(covmat = cor_greml, cor = T)
summary(pca_greml)
pca_greml$loadings
pca_greml$loadings %*% diag(pca_greml$sdev)

eigen(cor_r_greml)
pca_greml_r = princomp(covmat = cor_r_greml, cor=T)
summary(pca_greml_r)
pca_greml_r$loadings
pca_greml_r$loadings %*% diag(pca_greml_r$sdev)

#variance explained
greml_expl_pc1 = pca_greml$sdev[1]^2 / (pca_greml$sdev[1]^2 + pca_greml$sdev[2]^2 +pca_greml$sdev[3]^2 +pca_greml$sdev[4]^2 )
greml_r_expl_pc1 = pca_greml_r$sdev[1]^2 / (pca_greml_r$sdev[1]^2 + pca_greml_r$sdev[2]^2 +pca_greml_r$sdev[3]^2 +pca_greml_r$sdev[4]^2 )

#standardized loadings
greml_loadings = pca_greml$loadings %*% diag(pca_greml$sdev) 
greml_loadings_pc1 = greml_loadings[,1]
greml_r_loadings = pca_greml_r$loadings %*% diag(pca_greml_r$sdev) 
greml_r_loadings_pc1 = greml_r_loadings[,1]

#export data
write.table(cor_r_greml, file="N:/durable/projects/joakim/SES_composition/results/correlations/cor_greml_r.txt")
write.table(greml_expl_pc1, file="N:/durable/projects/joakim/SES_composition/results/pca/greml_expl_pc1.txt", row.names = F, col.names = F)
write.table(greml_r_expl_pc1, file="N:/durable/projects/joakim/SES_composition/results/pca/greml_r_expl_pc1.txt", row.names = F, col.names = F)
write.table(greml_loadings_pc1, file="N:/durable/projects/joakim/SES_composition/results/pca/greml_loadings_pc1.txt", row.names = F, col.names = F)
write.table(greml_r_loadings_pc1, file="N:/durable/projects/joakim/SES_composition/results/pca/greml_r_loadings_pc1.txt", row.names = F, col.names = F)

