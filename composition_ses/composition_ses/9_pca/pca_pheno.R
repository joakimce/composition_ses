library(data.table)
library(tidyverse)
library(psych)

path = ("N:/durable/projects/joakim/SES_composition/data/")
dat = fread(paste0(path,"phenos/phenos_gen_lnr.csv")) %>% select(edu, occ, inc, wea)

cor_dat = cor(dat, use = "pairwise.complete.obs")
write.table(cor_dat, "N:/durable/projects/joakim/SES_composition/results/cor_phen.txt")

#parallel analysis
fa.parallel(cor_dat, n.obs=128310, fa ="pc")

#pca
eigen(cor_dat)
pca_pheno = princomp(covmat = cor_dat, cor = T)
summary(pca_pheno)
pca_pheno$loadings
pca_pheno$loadings %*% diag(pca_pheno$sdev)

#explained variance
pheno_expl_pc1 = pca_pheno$sdev[1]^2 / (pca_pheno$sdev[1]^2 + pca_pheno$sdev[2]^2 +pca_pheno$sdev[3]^2 +pca_pheno$sdev[4]^2 )
write.table(pheno_expl_pc1, file="N:/durable/projects/joakim/SES_composition/results/pca/pheno_expl_pc1.txt", row.names = F, col.names = F)

#standardized loadings
pheno_loadings = pca_pheno$loadings %*% diag(pca_pheno$sdev) 
pheno_loadings_pc1 = pheno_loadings[,1]
write.table(pheno_loadings_pc1, file="N:/durable/projects/joakim/SES_composition/results/pca/pheno_loadings_pc1.txt", row.names = F, col.names = F)



