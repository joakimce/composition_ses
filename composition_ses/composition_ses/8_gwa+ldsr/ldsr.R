library("devtools")
library(GenomicSEM)


setwd("N:/durable/projects/joakim/SES_composition/data/gwas/")

ses_files <- c("edu.fastGWA","occ.fastGWA","inc.fastGWA","wea.fastGWA")
ses_names <- c("edu","occ","inc","wea")
ses_N <- c(127490,121356,127008,120407)


munge(files=ses_files, hm3 = "N:/durable/data/genetics/ld/w_hm3.noMHC.snplist", trait.names=ses_names, N=ses_N)


ld <- "N:/durable/data/genetics/ld/eur_w_ld_chr/"
wld <- "N:/durable/data/genetics/ld/eur_w_ld_chr/"


ses_traits <- c("edu.sumstats.gz","occ.sumstats.gz","inc.sumstats.gz","wea.sumstats.gz")
ses_sample.prev <- c(NA,NA,NA,NA)
ses_population.prev <- c(NA,NA,NA,NA)


ses_ldsr_out <- ldsc(ses_traits, ses_sample.prev, ses_population.prev, ld, wld) 
save(ses_ldsr_out, file="N:/durable/projects/joakim/SES_composition/results/ses_ldsr_out.RData")
