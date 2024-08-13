library(tidyverse)
library(data.table)

measure_order = c( "Education", "Occupation", "Income", "Wealth")

path = ("N:/durable/projects/joakim/SES_composition/results/")

ldsr_res = fread(paste0(path,"ldsr_res.txt")) %>% mutate(measure = measure_order) %>% mutate(mod = "A")
greml_res = fread(paste0(path,"greml_res.txt")) %>% mutate(measure = measure_order) %>% mutate(mod = "A")

fam_ae_se = fread(paste0(path,"fam_ped_modAE_se.csv")) %>% mutate(measure = measure_order)
fam_ae_res = fread(paste0(path,"fam_ped_modAE_res.csv")) %>% left_join(fam_se, by = "V1") %>% select(h2, h2_se) %>% 
  mutate(measure = measure_order) %>% rename(estimate=h2, se=h2_se) %>% mutate(method = "FP") %>% mutate(mod = "AE")

fam_ae_se = fread(paste0(path,"fam_ped_ACE_r1_r0_se.csv")) %>% mutate(measure = measure_order)
fam_ae_res = fread(paste0(path,"fam_ped_ACE_r1_r0_res.csv")) %>% left_join(fam_se, by = "V1") %>% select(h2, h2_se) %>% 
  mutate(measure = measure_order) %>% rename(estimate=h2, se=h2_se) %>% mutate(method = "FP") %>% mutate(mod = "ACE1")

fam_se = fread(paste0(path,"fam_ped_se.csv")) %>% mutate(measure = measure_order)
fam_res = fread(paste0(path,"fam_ped_res.csv")) %>% left_join(fam_se, by = "V1") %>% select(h2, h2_se) %>% 
  mutate(measure = measure_order) %>% rename(estimate=h2, se=h2_se) %>% mutate(method = "FP") %>% mutate(mod = "ACE")

ibd_se = fread(paste0(path,"ibd_se.csv"))
ibd_res = fread(paste0(path,"ibd_res.csv")) %>% mutate(measure = measure_order) %>% select(V1,h2,measure) %>%
  left_join(ibd_se, by="V1") %>% rename(estimate=h2, se=h2_se) %>% mutate(method = "IBD") %>%
  select(measure,estimate,se,method) %>% mutate(mod = "AE")

dat = rbind(ibd_res,fam_res,greml_res,ldsr_res,fam_ae_res)

colorBlindPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

method_order = c("FP","IBD","GREML","LDSR")
shape_order = c("A","AE","ACE")

ggplot(dat, aes(x = factor(measure, levels = measure_order), 
                y = estimate, 
                color = factor(method, levels=method_order,),
                shape = factor(mod, levels=shape_order))) +
  geom_point(position = position_dodge(width = 0.5), size=3) +
  geom_errorbar(aes(ymin = estimate - 1.96*se, ymax = estimate + 1.96*se), 
                width = 0.0, position = position_dodge(width = 0.5), linewidth = .1) +
  theme_minimal() +
  scale_shape_manual(values = c("AE" = 17, "A"=16, "ACE" = 15))+
  scale_color_manual(values = c("#0072B2", "#56B4E9", "#D55E00", "#E69F00")) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(color = "Method", x = "Measures", y = "Heritability", shape = "Modelled variance components")

