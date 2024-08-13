#make keep file


library(data.table)
library(tidyverse)

path = ("N:/durable/projects/joakim/SES_composition/data/")
dat_gcta = fread(paste0(path,"phenos/phenos_gen.txt"))

ses_keep = dat_gcta %>% select(FID,IID)

write.table(ses_keep, file = paste0(path, "gcta/ses.keep"),  row.names = F, col.names = F, quote=F)