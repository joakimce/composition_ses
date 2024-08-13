library(tidyverse)
library(data.table)

path = ("N:/durable/projects/joakim/SES_composition/data/gcta/")
phenos = fread(paste0(path,"phenos_gen.txt"))

edu_occ = phenos %>% select(1:4)
write.table(edu_occ, file= paste0(path,"edu_occ.txt"), row.names = F, col.names = F, quote=F)

edu_inc = phenos %>% select(1:3,5)
write.table(edu_inc, file= paste0(path,"edu_inc.txt"), row.names = F, col.names = F, quote=F)

edu_wea = phenos %>% select(1:3,6)
write.table(edu_wea, file= paste0(path,"edu_wea.txt"), row.names = F, col.names = F, quote=F)

occ_inc = phenos %>% select(1,2,4,5)
write.table(occ_inc, file= paste0(path,"occ_inc.txt"), row.names = F, col.names = F, quote=F)

occ_wea = phenos %>% select(1,2,4,6)
write.table(occ_wea, file= paste0(path,"occ_wea.txt"), row.names = F, col.names = F, quote=F)

inc_wea = phenos %>% select(1,2,5,6)
write.table(inc_wea, file= paste0(path,"inc_wea.txt"), row.names = F, col.names = F, quote=F)
