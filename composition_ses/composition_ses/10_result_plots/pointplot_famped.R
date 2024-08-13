library(tidyverse)
library(data.table)


path = ("N:/durable/projects/joakim/SES_composition/results/")

measure_order = c( "Education", "Occupation", "Income", "Wealth")

fam_1_a = fread(paste0(path,"fam_ped_ACE_r1_r0_res.csv")) %>% select(V1, h2) %>% rename(estimate=h2) %>% mutate(measure = measure_order) %>% mutate(varcomp = "A") %>% mutate(model = "Assumed sibling rc = 1, Assumed cousin rc = 0")
fam_1_c = fread(paste0(path,"fam_ped_ACE_r1_r0_res.csv")) %>% select(V1, c2) %>% rename(estimate=c2) %>% mutate(measure = measure_order) %>% mutate(varcomp = "C") %>% mutate(model = "Assumed sibling rc = 1, Assumed cousin rc = 0")

fam_2_a = fread(paste0(path,"fam_ped_modAE_res.csv")) %>% select(V1, h2) %>% rename(estimate=h2) %>% mutate(measure = measure_order) %>% mutate(varcomp = "A") %>% mutate(model = "No C")
fam_2_c = fread(paste0(path,"fam_ped_modAE_res.csv")) %>% select(V1, c2) %>% rename(estimate=c2) %>% mutate(measure = measure_order) %>% mutate(varcomp = "C") %>% mutate(model = "No C")

fam_3_a = fread(paste0(path,"fam_ped_res.csv")) %>% select(V1, h2) %>% rename(estimate=h2) %>% mutate(measure = measure_order) %>% mutate(varcomp = "A") %>% mutate(model = "Assumed sibling rc = 1, Estimated cousin rc = .59 (.04)")
fam_3_c = fread(paste0(path,"fam_ped_res.csv")) %>% select(V1, c2) %>% rename(estimate=c2) %>% mutate(measure = measure_order) %>% mutate(varcomp = "C") %>% mutate(model = "Assumed sibling rc = 1, Estimated cousin rc = .59 (.04)")

fam_res = rbind(fam_1_c,fam_1_a,fam_2_c,fam_2_a,fam_3_c,fam_3_a)

fam_se_1_a = fread(paste0(path,"fam_ped_ACE_r1_r0_se.csv"))   %>% select(V1, h2_se) %>% rename(se=h2_se)
fam_se_2_a = fread(paste0(path,"fam_ped_modAE_se.csv"))  %>% select(V1, h2_se) %>% rename(se=h2_se)
fam_se_3_a = fread(paste0(path,"fam_ped_se.csv"))  %>% select(V1, h2_se) %>% rename(se=h2_se)
fam_se_1_c = fread(paste0(path,"fam_ped_ACE_r1_r0_se.csv"))  %>% select(V1, c2_se)  %>% rename(se=c2_se)
fam_se_2_c = fread(paste0(path,"fam_ped_modAE_se.csv"))  %>% select(V1, c2_se) %>% rename(se=c2_se)
fam_se_3_c = fread(paste0(path,"fam_ped_se.csv")) %>% select(V1, c2_se) %>% rename(se=c2_se)
fam_se = rbind(fam_se_1_c,fam_se_1_a,fam_se_2_c,fam_se_2_a,fam_se_3_c,fam_se_3_a)

dat = cbind(fam_res, fam_se) %>% select(-V1)

varcomp_order = c("A", "C")
shape_order = c("No C","Assumed sibling rc = 1, Assumed cousin rc = 0", "Assumed sibling rc = 1, Estimated cousin rc = .59 (.04)")


ggplot(dat, aes(x = factor(measure, levels = measure_order), y = estimate, shape = factor(model, levels = shape_order), color =factor(varcomp, levels = varcomp_order))) +
  geom_point(position = position_dodge(width = 0.3), size=3) +
  scale_shape_manual(values = c(18, 17, 15)) +
  geom_errorbar(aes(ymin = estimate - 1.96*se, ymax = estimate + 1.96*se), width = 0.0, position = position_dodge(width = .3), linewidth = 1) +
  theme_minimal() +
  scale_color_manual(values = c("#0072B2","#009E73")) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(color = "Variance component", x = "SES Indicators", y = "Estimate", shape = "Model")

dat_a = dat %>% filter(varcomp == "A")

ggplot(dat_a, aes(x = factor(measure, levels = measure_order), y = estimate, shape = factor(model, levels = shape_order), color =factor(varcomp, levels = varcomp_order))) +
  geom_point(position = position_dodge(width = 0.3), size=3) +
  scale_shape_manual(values = c(16, 17, 15)) +
  geom_errorbar(aes(ymin = estimate - 1.96*se, ymax = estimate + 1.96*se), width = 0.0, position = position_dodge(width = .3), linewidth = 1) +
  theme_minimal() +
  scale_color_manual(values = c("#0072B2","#009E73")) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(color = "Variance component", x = "SES indicators", y = "Estimate", shape = "Shared environment assumptions")

dat_c = dat %>% filter(varcomp == "C") 

ggplot(dat_c, aes(x = factor(measure, levels = measure_order), y = estimate, shape = factor(model, levels = shape_order), color = factor(varcomp, levels = varcomp_order))) +
  geom_point(position = position_dodge(width = 0.1), size=3,color = "#009E73") +
  #scale_shape_manual(values = c(15,19)) +
  scale_shape_manual(values = c(16, 17, 15)) +
  geom_errorbar(aes(ymin = estimate - 1.96*se, ymax = estimate + 1.96*se), width = 0.0, position = position_dodge(width = .1), linewidth = .75, color = "#009E73") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 1)) +
  labs(color = "Variance component", x = "SES Indicators", y = "Estimate", shape = "Shared environment assumptions")

