
library(tidyverse)
library(data.table)

path = ("N:/durable/projects/joakim/SES_composition/data/")
dat_pop = fread(paste0(path,"phenos/population_phenos.csv"))
dat_sam = fread(paste0(path,"phenos/sample_phenos.csv"))

pop_par = fread(paste0(path,"phenos_prep/eligible_parents_n883147.csv"), header = T) %>% rename(lnr=w19_0634_lnr)
sam_par = fread("N:/durable/data/moba/linkage/PDB2601_kobling_SSB_v12.csv") %>% filter(rolle != "SU2PT_CHILD") %>% 
  rename(lnr = w19_0634_lnr, sex = rolle) %>% select(lnr, sex) %>% mutate(sex = ifelse(sex=="SU2PT_MOTHER",1,0))   


# ---- density plot occupation ----


occ_avg_dens_pop = dat_pop %>% select(occ_avg) %>%
  mutate(group = "Population")
occ_avg_dens_sample = dat_sam %>% select(occ_avg) %>%
  mutate(group = "Sample")
occ_avg_dens = rbind(occ_avg_dens_pop, occ_avg_dens_sample)
mean_occ_avg_pop = mean(dat_pop$occ_avg, na.rm=T)
mean_occ_avg_sample = mean(dat_sam$occ_avg, na.rm=T)

sd(dat_pop$occ_avg, na.rm=T)
sd(dat_sam$occ_avg, na.rm=T)

median(dat_pop$occ_avg, na.rm=T)
median(dat_sam$occ_avg, na.rm=T)

ggplot(occ_avg_dens, aes(x = occ_avg, fill = factor(group, level=c("Population","Sample")))) +
  #geom_density(alpha = 0.5) +
  geom_histogram(aes(y=..density..), alpha=0.5, 
                 position="identity")+
  geom_vline(xintercept = mean_occ_avg_pop, color = "salmon", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = mean_occ_avg_sample, color = "turquoise3", linetype = "dashed", linewidth = 1) +
  #annotate("text", size=4.5, x = mean_occ_avg_pop-3, y=.04, label = round(mean_occ_avg_pop,0), vjust = -1) +
  #annotate("text", size=4.5, x = mean_occ_avg_sample+6.5,y=.04,  label = round(mean_occ_avg_sample, 0), vjust = -1, ) +
  labs(
    title = "Average Occupational Status (age 35-45)",
    x = "SIOPS Occupational Status Score",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    plot.title = element_text(size = 16),
    legend.title = element_blank()
  )


# ---- density plot income ----

inc_avg_dens_pop = dat_pop %>% select(inc_avg) %>%
  mutate(group = "Population") %>% 
  mutate(inc_avg = ifelse(inc_avg > 1e6, 1e6, inc_avg)) %>%
  mutate(inc_avg = ifelse(inc_avg < 0, 0, inc_avg)) 
inc_avg_dens_sample = dat_sam %>% select(inc_avg) %>%
  mutate(group = "Sample") %>%
  mutate(inc_avg = ifelse(inc_avg > 1e6, 1e6, inc_avg)) %>%
  mutate(inc_avg = ifelse(inc_avg < 0, 0, inc_avg))
inc_avg_dens = rbind(inc_avg_dens_pop, inc_avg_dens_sample)

mean_inc_avg_pop = mean(dat_pop$inc_avg, na.rm=T)
mean_inc_avg_sample = mean(dat_sam$inc_avg, na.rm=T)

sd(dat_pop$inc_avg, na.rm=T)
sd(dat_sam$inc_avg, na.rm=T)

median(dat_pop$inc_avg, na.rm=T)
median(dat_sam$inc_avg, na.rm=T)

ggplot(inc_avg_dens, aes(x = inc_avg, fill =factor(group, level=c("Population","Sample")))) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = mean_inc_avg_pop, color = "salmon", linetype = "dashed", size = 1) +
  geom_vline(xintercept = mean_inc_avg_sample, color = "turquoise3", linetype = "dashed", size = 1) +
  #annotate("text", size=4.5, x = mean_inc_avg_pop-130000, y = 1.5e-6, label = round(mean_inc_avg_pop,0), vjust = -1) +
  #annotate("text", size=4.5, x = mean_inc_avg_sample+130000, y = 1.5e-6, label = round(mean_inc_avg_sample, 0), vjust = -1, ) +
  labs(
    title = "Average Total Income (age 35-45)",
    x = "Income NOK",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    plot.title = element_text(size = 14),
    legend.title = element_blank()
  )



# --- density plot wealth ----

wea_avg_dens_pop = dat_pop %>% select(wea_avg) %>%
  mutate(group = "Population") %>% 
  mutate(wea_avg = ifelse(wea_avg > 1e7, 1e7, wea_avg)) %>%
  mutate(wea_avg = ifelse(wea_avg < 0, 0, wea_avg)) 
wea_avg_dens_sample = dat_sam %>% select(wea_avg) %>%
  mutate(group = "Sample") %>% 
  mutate(wea_avg = ifelse(wea_avg > 1e7, 1e7, wea_avg)) %>%
  mutate(wea_avg = ifelse(wea_avg < 0, 0, wea_avg))
wea_avg_dens = rbind(wea_avg_dens_pop, wea_avg_dens_sample)
mean_wea_avg_pop = mean(dat_pop$wea_avg, na.rm=T)
mean_wea_avg_sample = mean(dat_sam$wea_avg, na.rm=T)

sd(dat_pop$wea_avg, na.rm=T)
sd(dat_sam$wea_avg, na.rm=T)

median(dat_pop$wea_avg, na.rm=T)
median(dat_sam$wea_avg, na.rm=T)

ggplot(wea_avg_dens, aes(x = wea_avg, fill =factor(group, level=c("Population","Sample")))) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = mean_wea_avg_pop, color = "salmon", linetype = "dashed", size = 1) +
  geom_vline(xintercept = mean_wea_avg_sample, color = "turquoise3", linetype = "dashed", size = 1) +
  #annotate("text", size=4.5, x = mean_wea_avg_pop-1e6, y=4e-7, label = round(mean_wea_avg_pop,0), vjust = -1) +
  #annotate("text", size=4.5, x = mean_wea_avg_sample+1e6, y=4e-7, label = round(mean_wea_avg_sample, 0), vjust = -1, ) +
  labs(
    title = "Average Net Wealth (age 35-45)",
    x = "Net Wealth NOK",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    plot.title = element_text(size = 16),
    legend.title = element_blank()
  ) 




library(gridExtra)
grid.arrange(occpl,incpl,weapl, ncol = 1)
