
library(ggplot2)
library(here)
library(dplyr)
library(reshape2)
library(readxl)
# 
# rm(list = ls())
# .rs.restartR()
# load(here('calib_params.RData'))


source(here('code/network_simulations_import.R'))
# new scenarios 10p
source(here('code/network_simulations_import_p10.R'))
# new scenarios x40
source(here('code/network_simulations_import_x40.R'))

source(here('code/manuscript_figures_funs.R'))
source(here('stat_network/init_SIS_model.R'))
########################################################################################################
# select 
calbs <- compile_run(pr_b, inc_b, d_b, s_b, calib_params$params.all[[1]],  id_b) 

nonzeros_id_p <- nonzeros_id_d <- nonzeros_id_s <- nonzeros_id_i <- numeric(nrow(calbs$inc.rate_m))

# select at parameter level
for (j in 1:nrow(calbs$inc.rate_m)){
  
  nonzeros_id_p[j] <- ifelse(calbs$prev_m[j,209]>=0.02 & calbs$prev_m[j,209]<=0.1 & calbs$prev_m[j,417]>=0.02, 1, 0)
  nonzeros_id_d[j] <- ifelse(calbs$diag.rate_m[j,4]>=0.01 & calbs$diag.rate_m[j,4]<=0.1, 1, 0)
  nonzeros_id_s[j] <- ifelse(calbs$prop.sympt_m[j,4]>=0.3 & calbs$prop.sympt_m[j,4]<=0.8, 1, 0)
  nonzeros_id_i[j] <- ifelse(calbs$inc.rate_m[j,4]>=0.02  & calbs$inc.rate_m[j,4]<=0.23, 1, 0)
  
}

param_id_logic <- nonzeros_id_p==1 & nonzeros_id_d==1 & nonzeros_id_s==1 & nonzeros_id_i==1 
param_id <- (1:length(param_id_logic))[param_id_logic]
param_id_s_logic <- id_b %in%  param_id
param_id_s <- id_b[param_id_s_logic]


##############################################################################
######       Figure 3

# individual-level infections

incid_yr4 <- yearfun_s(inc_b[param_id_s_logic,], "basecase")
# hist(incid_yr4[[1]] /2000, breaks=100)
# quantile(incid_yr4[[1]] /2000, probs=seq(0,1,by=0.1))
incid_cat <- incid_yr4[[1]] /2000 > 0.11

as.vector(incid_cat) -> test
## how many infections by partner number (does not count for there being more people)
binf_tab <- i10inf_tab <- i20inf_tab <- i40inf_tab <- pnum_tab <- matrix(NA, max(partn_num)+1, nrow(partn_num)+2)

# discrete
binf_tab[,1] <- i10inf_tab[,1] <- i20inf_tab[,1] <- i40inf_tab[,1] <- pnum_tab[,1] <- 0:max(partn_num)


# categorical
binf_tab[1,2] <- i10inf_tab[1,2] <- i20inf_tab[1,2] <- pnum_tab[1,2] <-  0
binf_tab[2:3,2] <- i10inf_tab[2:3,2] <- i20inf_tab[2:3,2] <- pnum_tab[2:3,2] <-  1
binf_tab[4:6,2] <- i10inf_tab[4:6,2] <- i20inf_tab[4:6,2] <- pnum_tab[4:6,2] <-  2
binf_tab[7:10,2] <- i10inf_tab[7:10,2] <- i20inf_tab[7:10,2] <- pnum_tab[7:10,2] <-  3
binf_tab[11:length(binf_tab[,1]),2] <- i10inf_tab[11:length(binf_tab[,1]),2] <- i20inf_tab[11:length(binf_tab[,1]),2] <- pnum_tab[11:length(binf_tab[,1]),2] <-  4

avinf_tab <- avdur_tab <- numeric(0)

for (i in 1:nrow(partn_num)){

  avinf <- avind_simul(dat=cbind(incind_b[i,], incind_i10[i,], incind_i20[i,], incind_i40[i,], partn_num[i,]), id0=id_b[i], baseinf0=incid_cat[i], degr0=prmsdat_s[i,"nw_degree"])
  avinf_tab <- rbind(avinf_tab, avinf)
}

avinf_tab$av10_rate <- avinf_tab$av10_sum/avinf_tab$pnum
avinf_tab$av20_rate <- avinf_tab$av20_sum/avinf_tab$pnum
avinf_tab$av40_rate <- avinf_tab$av40_sum/avinf_tab$pnum

avinf_tab$av10_prop <- avinf_tab$av10_sum/avinf_tab$avtot_10
avinf_tab$av20_prop <- avinf_tab$av20_sum/avinf_tab$avtot_20
avinf_tab$av40_prop <- avinf_tab$av40_sum/avinf_tab$avtot_40


##############################################################################
######## RATES 3A

avinf_tab %>%
  select(V5, id, binc, degree, av10_rate, av20_rate, av40_rate) %>%
  pivot_longer(cols = c("av10_rate", "av20_rate", "av40_rate"), names_to = "Scenario", values_to = "av_rate") %>%
  group_by(V5,  Scenario) %>%
  summarize( mean = mean(100*av_rate, na.rm=T),
             p50 = quantile(100*av_rate, probs=0.5, na.rm=T),
             p25 = quantile(100*av_rate, probs=0.25, na.rm=T),
             p75=  quantile(100*av_rate, probs=0.75, na.rm=T),
             n  = n()) %>%
  ungroup()  %>%
  mutate(Scenario = ifelse(Scenario=="av10_rate", "PN x10%", ifelse(Scenario=="av20_rate", "PN x20%", "PN x40%"))) %>%
  ggplot(aes(x=V5, y=-mean, color=Scenario)) +
  geom_point(size=1) + geom_line() +
  theme_classic() +
  xlab("Number of partners") + ylab("Averted infections per 100 persons") + theme_classic() +
  scale_color_manual(values= c("gray80", "gray50", "gray20")) +
  theme(legend.position = "bottom") + ggtitle("A) Rate of averted infections")  -> fig3A 


ggsave(filename = here("output/figure_3A.png"),
       plot = fig3A,
       device = png(),
       scale = 1, 
       width = 12,
       height = 10, 
       units = "cm",
       dpi = 300)

dev.off()


########## ABSOLUTE NUMBER 3B

avinf_tab %>%
  # mutate(part_cat = factor(part_cat, levels = c("0", "1-2", "3-5", "6+"))) %>%
  select(V5, id, binc, degree, av10_sum, av20_sum, av40_sum) %>%
  pivot_longer(cols = c("av10_sum", "av20_sum", "av40_sum"), names_to = "Scenario", values_to = "av_sum") %>%
  group_by(V5,  Scenario) %>%
  summarize( mean = mean(100*av_sum, na.rm=T),
             p50 = quantile(100*av_sum, probs=0.5, na.rm=T),
             p25 = quantile(100*av_sum, probs=0.25, na.rm=T),
             p75=  quantile(100*av_sum, probs=0.75, na.rm=T),
             n  = n()) %>%
  ungroup()  %>%
  mutate(Scenario = ifelse(Scenario=="av10_sum", "PN x10%", ifelse(Scenario=="av20_sum", "PN x20%", "PN x40%"))) %>%
  ggplot(aes(x=V5, y=-mean, color=Scenario)) +
  geom_point(size=1) + geom_line() +
  theme_classic() +
  scale_color_manual(values= c("gray80", "gray50", "gray20")) +
  xlab("Number of partners") + ylab("Averted infections") + theme_classic() +
  theme(legend.position = "bottom") + ggtitle("B) Absolute number of averted infections") -> fig3B

ggsave(filename = here("output/figure_3B.png"),
       plot = fig3B,
       device = png(),
       scale = 1, 
       width = 12,
       height = 10, 
       units = "cm",
       dpi = 300)

dev.off()

##############################################################################
######## Supplement figure S7
## Number of people
pnum_params <- numeric(0)

for (i in 1:length(calib_params$parts.all)){
  pnum_params <- rbind(pnum_params, rowSums(calib_params$parts.all[[i]]>0))
}

cbind(1:length(calib_params$parts.all),  prmsdat[, "nw_degree"], pnum_params) %>%
  as_tibble() %>%
  rename(degree_type = V2) %>%
  melt(id=c("V1", "degree_type")) %>%
  group_by(V1, variable, degree_type) %>% 
  count(value) %>%
  group_by(value, V1, degree_type) %>% # 
  summarise( n = sum(n)) %>%
  ggplot(aes(x=as.numeric(value), y=as.numeric(n), group=as.factor(V1),  color = degree_type)) + #
  # scale_y_log10()  +
   geom_line(linewidth=0.09) +  theme_bw() +
  xlab("Number of partners") + ylab("Number of individuals") -> figS7

ggsave(filename = here("output/figure_S7.png"),
       plot = figS7,
       device = png(),
       scale = 1, 
       width = 15,
       height = 10, 
       units = "cm",
       dpi = 300)

dev.off()
  
  
