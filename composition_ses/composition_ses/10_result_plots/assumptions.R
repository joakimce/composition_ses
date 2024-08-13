
library(tidyverse)
library(OpenMx)
library(data.table)
library(gridExtra)

v_nms = c("edu","occ","inc","wea")
r = c(.7,.65,.60,.55,.50,.45,.40,.35,.30,.25,.20,.15,.10,.05,0)

xr=r[1]

mod_r1_r07 = readRDS("N:/durable/projects/joakim/SES_composition/data/fam_ped_modACE_sib-r1_cous-r0.7.rds")

summary(mod_r1_r07)

va = mxEval(va, mod_r1_r07)
dimnames(va) = list(v_nms, v_nms)
vc = mxEval(vc, mod_r1_r07)
dimnames(vc) = list(v_nms, v_nms)
ve = mxEval(ve, mod_r1_r07)
dimnames(ve) = list(v_nms, v_nms)
h2 = diag(va / (va + vc + ve))
c2 = diag(vc / (va + vc + ve))
e2 = diag(ve / (va + vc + ve))
fam = cbind(h2, c2, e2)

fam_se = cbind(mxSE(h2, mod_r1_r07),mxSE(c2, mod_r1_r07),mxSE(e2, mod_r1_r07))
colnames(fam_se) = c("h2_se","c2_se","e2_se")
rownames(fam_se) = v_nms
fam = cbind(fam, fam_se)
fam = data.frame(fam) %>% rownames_to_column(var = "indicator")
fam = fam %>% mutate(sib.cor = 1) %>% mutate(cousin.cor = r[1])

res = fam

for(i in 2:length(r)){
  xr = r[i]
  mod = readRDS(paste0("N:/durable/projects/joakim/SES_composition/data/fam_ped_modACE_sib-r1_cous-r",r[i],".rds"))
  va = mxEval(va, mod)
  dimnames(va) = list(v_nms, v_nms)
  vc = mxEval(vc, mod)
  dimnames(vc) = list(v_nms, v_nms)
  ve = mxEval(ve, mod)
  dimnames(ve) = list(v_nms, v_nms)
  h2 = diag(va / (va + vc + ve))
  c2 = diag(vc / (va + vc + ve))
  e2 = diag(ve / (va + vc + ve))
  fam = cbind(h2, c2, e2)
  
  fam_se = cbind(mxSE(h2, mod),mxSE(c2, mod),mxSE(e2, mod))
  colnames(fam_se) = c("h2_se","c2_se","e2_se")
  rownames(fam_se) = v_nms
  fam = cbind(fam, fam_se)
  fam = data.frame(fam) %>% rownames_to_column(var = "indicator")
  fam = fam %>% mutate(sib.cor = .7) %>% mutate(cousin.cor = r[i])
  
  res = rbind(res, fam)
}

edu = res %>% filter(indicator == "edu")
edu_long = edu %>%
  gather(key = "varcomp", value = "estimate", -c(indicator,cousin.cor, sib.cor,h2_se,c2_se,e2_se)) %>%
  gather(key = "varcomp_se", value = "se", -c(indicator,cousin.cor, sib.cor,varcomp,estimate))
edu_pl = ggplot(edu_long, aes(x = cousin.cor, y = estimate, fill=varcomp)) + 
  geom_line() +
  geom_ribbon(aes(ymin = estimate - 1.96*se, ymax = estimate + 1.96*se), alpha = 0.4) +
  scale_color_manual(values = c("#52a675","#ff7a59","#1982c4"))+
  scale_fill_manual(values = c("#52a675","#ff7a59","#1982c4"))+
  coord_cartesian(ylim = c(0, 0.75)) +
  labs(x = "Assumed cousin shared environment correlation", y = "Estimate", fill = "Variance component")
edu_pl

occ = res %>% filter(indicator == "occ")
occ_long = occ %>%
  gather(key = "varcomp", value = "estimate", -c(indicator,cousin.cor, sib.cor,h2_se,c2_se,e2_se)) %>%
  gather(key = "varcomp_se", value = "se", -c(indicator,cousin.cor, sib.cor,varcomp,estimate))
occ_pl = ggplot(occ_long, aes(x = cousin.cor, y = estimate, fill=varcomp)) + 
  geom_line() +
  geom_ribbon(aes(ymin = estimate - 1.96*se, ymax = estimate + 1.96*se), alpha = 0.4) +
  scale_color_manual(values = c("#52a675","#ff7a59","#1982c4"))+
  scale_fill_manual(values = c("#52a675","#ff7a59","#1982c4"))+
  coord_cartesian(ylim = c(0, 0.75)) +
  labs(x = "Assumed cousin shared environment correlation", y = "Estimate", fill = "Variance component")
occ_pl

inc = res %>% filter(indicator == "inc")
inc_long = inc %>%
  gather(key = "varcomp", value = "estimate", -c(indicator,cousin.cor, sib.cor,h2_se,c2_se,e2_se)) %>%
  gather(key = "varcomp_se", value = "se", -c(indicator,cousin.cor, sib.cor,varcomp,estimate))
inc_pl = ggplot(inc_long, aes(x = cousin.cor, y = estimate, fill=varcomp)) + 
  geom_line() +
  geom_ribbon(aes(ymin = estimate - 1.96*se, ymax = estimate + 1.96*se), alpha = 0.4) +
  scale_color_manual(values = c("#52a675","#ff7a59","#1982c4"))+
  scale_fill_manual(values = c("#52a675","#ff7a59","#1982c4"))+
  coord_cartesian(ylim = c(0, 0.75)) +
  labs(x = "Assumed cousin shared environment correlation", y = "Estimate", fill = "Variance component")
inc_pl

wea = res %>% filter(indicator == "wea")
wea_long = wea %>%
  gather(key = "varcomp", value = "estimate", -c(indicator,cousin.cor, sib.cor,h2_se,c2_se,e2_se)) %>%
  gather(key = "varcomp_se", value = "se", -c(indicator,cousin.cor, sib.cor,varcomp,estimate))
wea_pl = ggplot(wea_long, aes(x = cousin.cor, y = estimate, fill=varcomp)) + 
  geom_line() +
  geom_ribbon(aes(ymin = estimate - 1.96*se, ymax = estimate + 1.96*se), alpha = 0.4) +
  scale_color_manual(values = c("#52a675","#ff7a59","#1982c4"))+
  scale_fill_manual(values = c("#52a675","#ff7a59","#1982c4"))+
  coord_cartesian(ylim = c(0, 0.75)) +
  labs(x = "Assumed cousin shared environment correlation", y = "Estimate", fill = "Variance component")
wea_pl



plots = list(edu_pl, occ_pl, inc_pl, wea_pl)
grid.arrange(grobs = plots, nrow = 2, ncol = 2)

