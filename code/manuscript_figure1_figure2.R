
library(cowplot)
library(glmtoolbox)
library(ggplot2)
library(here)
library(dplyr)
library(reshape2)
library(readxl)
library(tidyr)

 
# rm(list = ls())
# .rs.restartR()

## loading the data takes time (not included in github due to size)
# order matters - id_b and non_zeros needed here
source(here('code/network_simulations_import.R'))

# new scenarios 10p
source(here('code/network_simulations_import_p10.R'))
# new scenarios x40
source(here('code/network_simulations_import_x40.R'))

# figure funs
source(here('code/manuscript_figures_funs.R'))
source(here('code/tornado_plot_funs.R'))
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

########################################################################################################

# parameter level
popinc_p <- cbind(yearfun_p(inc_b[param_id_s_logic,], "basecase", id_b[param_id_s_logic]), yearfun_p(inc_i10[param_id_s_logic,], "PN: 10%x", id_b[param_id_s_logic]), 
                  yearfun_p(inc_i10p[param_id_s_logic,], "PN: 10%+", id_b[param_id_s_logic]), yearfun_p(inc_i20[param_id_s_logic,], "PN: 20%x", id_b[param_id_s_logic]), 
                  yearfun_p(inc_i40[param_id_s_logic,], "PN: 40%x", id_b[param_id_s_logic]))


##############################################################
# all 4 scenarion combined
IRR_10  <- cbind( popinc_p[,6]/popinc_p[,1],  popinc_p[,7]/popinc_p[,2], popinc_p[,8]/popinc_p[,3], popinc_p[,9]/popinc_p[,4])
IRR_10p <- cbind( popinc_p[,11]/popinc_p[,1], popinc_p[,12]/popinc_p[,2], popinc_p[,13]/popinc_p[,3], popinc_p[,14]/popinc_p[,4])
IRR_20 <- cbind( popinc_p[,16]/popinc_p[,1],  popinc_p[,17]/popinc_p[,2], popinc_p[,18]/popinc_p[,3], popinc_p[,19]/popinc_p[,4])
IRR_40 <- cbind( popinc_p[,21]/popinc_p[,1],  popinc_p[,22]/popinc_p[,2], popinc_p[,23]/popinc_p[,3], popinc_p[,24]/popinc_p[,4])

cbind(rbind(IRR_10, IRR_10p, IRR_20, IRR_40), 
      rep(c("x10% vs base", "+10% vs base", "x20% vs base", "x40% vs base"), each=nrow(IRR_10)),
      rep(c("10% increase in PN", "Higher increase in PN"), each=2*nrow(IRR_10))) %>%
  as_tibble() %>%
  mutate(V5= factor(V5, levels=c("x10% vs base", "+10% vs base", "x20% vs base", "x40% vs base"))) %>%
  melt(id=c("V5", "V6")) %>%
  mutate(Year = factor(ifelse(variable=="V1", 1, ifelse(variable=="V2", 2, ifelse(variable=="V3", 3, 4)))))  %>%
  mutate(value = as.numeric(value)) %>%
  ggplot() +
  geom_hline(yintercept=1, color = "black") +
  geom_hline(yintercept=0.94, color = "gray40", linetype="dashed", size=0.6) +
  geom_hline(yintercept=0.97, color = "gray60", linetype="dotted", size=0.6) +
  geom_hline(yintercept=0.91, color = "gray60", linetype="dotted", size=0.6) +
  geom_boxplot(aes(x=V5, y=value, fill=Year), alpha=0.6) + 
  scale_fill_manual(values = c("lightblue", "skyblue3", "steelblue4", "navy")) +
  facet_wrap(~V6, scales = "free_x") +
  theme_classic() + ylab("IRR") + xlab("Scenario") + theme(legend.position = "bottom") -> fig1

ggsave(filename = here("output/figure_1-new.png"),
       plot = fig1,
       device = png(),
       scale = 1, 
       width = 15,
       height = 10, 
       units = "cm",
       dpi = 300)

dev.off()

#######
cbind(cbind(IRR_10, IRR_10p, IRR_20, IRR_40)) %>%
  as_tibble() %>%
  reframe(across(V1:V16, ~ quantile(.x, probs=c(0.5, 0.025, 0.975, 0.25, 0.75), na.rm = TRUE))) %>%
  t(.) -> IRR_distr

colnames(IRR_distr) <- c("p0.5", "p0.025", "p0.975", "p0.25", "p0.75")

IRR_distr %>%
  as_tibble() %>%
  mutate(Scenario = factor(rep(c("x10% vs base", "+10% vs base", "x20% vs base", "x40% vs base"), each=4),
                           levels = c("x10% vs base", "+10% vs base", "x20% vs base", "x40% vs base"))) %>%
  mutate(Year = rep(1:4, 4)) %>%
  mutate(Level=rep(c("10% increase in PN", "Higher increase in PN"), each=8)) -> IRR_distr2

IRR_distr2 %>%
    ggplot() +
    #geom_hline(yintercept=1, color = "black", size=0.4) +
    geom_hline(yintercept=0.94, color = "gray20", linetype="dashed", size=0.6) +
    geom_hline(yintercept=0.97, color = "gray20", linetype="dotted", size=0.6) +
    geom_hline(yintercept=0.91, color = "gray20", linetype="dotted", size=0.6) +
    geom_linerange(aes(x=interaction(Year, Scenario, lex.order = FALSE), ymin=p0.025, ymax=p0.975, color=Scenario), alpha=0.3, linewidth=0.9) +
    geom_linerange(aes(x=interaction(Year, Scenario, lex.order = FALSE), ymin=p0.25, ymax=p0.75, color=Scenario),  alpha=0.6, size=0.8, linewidth=2) +
    geom_point(aes(x=interaction(Year, Scenario, lex.order = FALSE), y=p0.5, color=Scenario),  size=2) +
    coord_cartesian(ylim = c(0.7, 1.2), xlim = c(0, 17), expand = FALSE, clip = "off") +
    annotate(geom = "text", x = seq_len(nrow(IRR_distr)), y = 0.2, label = IRR_distr2$Year , size = 4) +
    annotate(geom = "text", x = c(3, 10), y = -0.1, label = unique(IRR_distr2$Level), size = 6) +
    annotate(geom = "text", x = seq(1.02,16, by=4), y = 1.9, label = unique(IRR_distr2$Scenario), size = 5, hjust=0) +
    #scale_fill_manual(values = c("lightblue", "skyblue3", "steelblue4", "navy")) +
    #facet_wrap(~Level, scales = "free_x", ncol=4) +
    ylab("IRR") + xlab("Scenario") +
    theme_bw() +
    theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(color = "black", size = 5, angle = 0, hjust = 1, vjust = 0, face = "plain"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none")  -> fig1

ggsave(filename = here("output/figure_1.png"),
       plot = fig1,
       device = png(),
       scale = 1,
       width = 15,
       height = 10,
       units = "cm",
       dpi = 300)

dev.off()

########################################################################################################
####################        FIGURE 2

# Univariate analysis -parameter level

df_pall <- data.frame(# base_incidence = as.factor(ifelse( (popinc_p[,4]/2000)<0.15, "low", "high")),
            base_incidence = popinc_p[,1]/2000,
            incidence_rr  =  IRR_10[,1],
            network = as.factor(prmsdat[param_id_logic,"nw_degree"]),
            prop_symptomatic = as.numeric(prmsdat[param_id_logic,"sympt_pr"]), 
            weekly_sympt_test_pr = as.numeric(prmsdat[param_id_logic,"sympt_test_pr"]), 
            weekly_clear_pr = as.numeric(prmsdat[param_id_logic,"clear_pr"]), 
            weekly_screen_pr = as.numeric(prmsdat[param_id_logic,"screen_pr"]), 
            prop_w_instant_pship = as.numeric(prmsdat[param_id_logic,"pr_inst_type"]),
            prop_ps_main = as.numeric(prmsdat[param_id_logic,"pr_mainpartn"]),
            pr_of_inst_act = as.numeric(prmsdat[param_id_logic,"act_pr_inst"]), 
            num_inst_acts = as.numeric(prmsdat[param_id_logic,"inst_acts"]),
            trnsm_pr_inst = as.numeric(prmsdat[param_id_logic,"trnsm_pr_inst"]),
            trnsm_pr_casual = as.numeric(prmsdat[param_id_logic,"trnsm_pr_cas"]),
            trnsm_pr_main = as.numeric(prmsdat[param_id_logic,"trnsm_pr_main"]),
            pn_base_coverage =  as.numeric(prmsdat[param_id_logic,"pn_pr_main"])  )


all1 <- lm(incidence_rr ~. , data = df_pall)
all1_summary <-  summary(all1) 

###################################################
sampleruns <- 50000
lm_mean <- lm_max <- lm_min <- numeric(0)

for (n in 1:sampleruns){
  
  id_temp <- sample(1:nrow(df_pall), replace = T)
  df_temp <- df_pall[id_temp,]
  all_lm  <-  lm(incidence_rr ~. , data = df_temp)
  
  # Univariate analysis -simulation level
  df_temp %>%
    #  mutate(across(base_incidence:pn_base_coverage, ~ mean(., na.rm = TRUE))) %>% # 
    mutate(across(!network, ~ mean(., na.rm = TRUE))) %>% # 
    #  mutate(id = 42) %>% 
    slice_head(n=14) -> df_mean
  df_mean$network <- "higher degree"
  #  df_mean$num_inst_acts <- median(df_temp$num_inst_acts)
  
  # min and max 
  df_pall_min <- apply(df_temp, 2, min)
  df_pall_max <- apply(df_temp, 2, max)
  
  df_mean_min <-  df_mean_max <- df_mean 
  df_mean_min[2,3] <-"lower dregree" 
  
  df_mean_max[1,1] <-  as.numeric(df_pall_max[1]) # df_mean0_max[1,2] <-
  df_mean_min[1,1] <-  as.numeric(df_pall_min[1]) # df_mean0_min[1,2] <-
  
  r <- 3
  for (i in 4:(ncol(df_mean)) ){
    # df_mean0_min[r,i] <- df_mean[r,i] *0.8
    # df_mean0_max[r,i] <- df_mean[r,i] *1.2
    
    df_mean_min[r,i]   <- as.numeric(df_pall_min[i])
    df_mean_max[r,i]   <- as.numeric(df_pall_max[i])
    r = r+1
  }
  
  # average prediction
  lm_mean <- cbind(lm_mean, predict(all_lm, newdata = df_mean[1,], type="response"))
  
  # MIN / MAX parameter set
  lm_min <- cbind(lm_min, predict(all_lm, newdata = df_mean_min, type="response"))
  lm_max <- cbind(lm_max, predict(all_lm, newdata = df_mean_max, type="response"))
  
}

rownames(lm_min) <-  rownames(lm_max) <- colnames(df_pall)[c(1, 3:15)]

###################################################
# min and max of the 
df_pall_min <- apply(df_pall, 2, min)
df_pall_max <- apply(df_pall, 2, max)

###################################################

to_lab_name_p <- c( paste0("Baseline incidence ", "(", round(as.numeric(df_pall_min["base_incidence"]),3), "-", round(as.numeric(df_pall_max["base_incidence"]),3), ")" ), 
                    paste0("Network degree (higher or lower degree), ", "reference:higher"),
                    paste0("Probability infection is symptomatic ", "(", round(as.numeric(df_pall_min["prop_symptomatic"]),3), "-", round(as.numeric(df_pall_max["prop_symptomatic"]),3), ")" ),
                    paste0("Test probability (symptomatic) per week ", "(", round(as.numeric(df_pall_min["weekly_sympt_test_pr"]),3), "-", round(as.numeric(df_pall_max["weekly_sympt_test_pr"]),3), ")"),
                    paste0("Clearance probability per week ", "(", round(as.numeric(df_pall_min["weekly_clear_pr"]),3), "-", round(as.numeric(df_pall_max["weekly_clear_pr"]),3), ")" ),
                    paste0("Test probability (asymptomatic) per week ", "(", round(as.numeric(df_pall_min["weekly_screen_pr"]),3), "-", round(as.numeric(df_pall_max["weekly_screen_pr"]),3), ")" ), 
                    paste0("Proportion of people with one-off partnerhips ", "(", round(as.numeric(df_pall_min["prop_w_instant_pship"]),3), "-", round(as.numeric(df_pall_max["prop_w_instant_pship"]),3), ")" ),
                    paste0("Proportion of ties which are main partnership ", "(", round(as.numeric(df_pall_min["prop_ps_main"]),3), "-", round(as.numeric(df_pall_max["prop_ps_main"]),3), ")" ), 
                    paste0("Probability of one-off partnerhip per week ", "(", round(as.numeric(df_pall_min["pr_of_inst_act"]),3), "-", round(as.numeric(df_pall_max["pr_of_inst_act"]),3), ")" ), 
                    paste0("Number of acts per one-off partnerhip ", "(", round(as.numeric(df_pall_min["num_inst_acts"]),3), "-", round(as.numeric(df_pall_max["num_inst_acts"]),3), ")" ), 
                    paste0("Transmission probability per one-off act ", "(", round(as.numeric(df_pall_min["trnsm_pr_inst"]),3), "-", round(as.numeric(df_pall_max["trnsm_pr_inst"]),3), ")" ), 
                    paste0("Transmission probability in casual partnerhip per week ", "(", round(as.numeric(df_pall_min["trnsm_pr_casual"]),3), "-", round(as.numeric(df_pall_max["trnsm_pr_casual"]),3), ")" ), 
                    paste0("Transmission probability in main partnerhip per week ", "(", round(as.numeric(df_pall_min["trnsm_pr_main"]),3), "-", round(as.numeric(df_pall_max["trnsm_pr_main"]),3), ")" ), 
                    paste0("Coverage of partner notification at baseline ", "(", round(as.numeric(df_pall_min["pn_base_coverage"]),3), "-", round(as.numeric(df_pall_max["pn_base_coverage"]),3), ")" )
)

###################################################

df_range <- data.frame( varname = colnames(df_pall)[c(1, 3:15)],
                        var_long = to_lab_name_p,
                        min_mean = apply(lm_min, 1, mean), 
                        min_min = apply(lm_min, 1, min), 
                        min_max = apply(lm_min, 1, max), 
                        max_mean = apply(lm_max, 1, mean), 
                        max_min = apply(lm_max, 1, min), 
                        max_max = apply(lm_max, 1, max), 
                        mean_mean = apply(lm_mean, 1, mean), 
                        mean_min = apply(lm_mean, 1, min), 
                        mean_max = apply(lm_mean, 1, max))

df_range %>%
  mutate(diff = abs(max_min-min_max)) %>%
  ggplot(aes(y=reorder( var_long, diff))) + 
  geom_pointrange(aes(x=min_mean, xmin=min_min, xmax=min_max, color="min prediction"), alpha = 0.4)+
  geom_pointrange(aes(x=max_mean, xmin=max_min, xmax=max_max, color="max prediction"), alpha = 0.4) +
  geom_pointrange(aes(x=mean_mean, xmin=mean_min, xmax=mean_max, color="average prediction"), alpha = 0.3) +
  geom_vline(aes(xintercept = mean_mean), colour = "gray40", linewidth = 0.3) +
  xlab("IRR") + xlim(0.87, 1) + scale_color_manual(values = c("gray40", "navy", "darkorange")) +
  theme( 
    panel.background = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major.x = element_line(linewidth = 0.15, linetype = 'solid', colour = "gray80"),
    panel.grid.minor.x = element_line(linewidth = 0.15, linetype = 'solid', colour = "gray80"),
    panel.grid.major.y = element_blank(), # Remove horizontal grid lines
    panel.grid.minor.y = element_blank(),
    axis.text.y  = element_text( size = 10 ),
    axis.text.x  = element_text( size = 11 ),
    axis.title.x = element_text( size = 10 ),
    title        = element_text( size = 10),
    legend.position = "bottom",
    legend.justification = "left",
    legend.background = element_rect(fill = "white"),
    legend.key = element_rect(fill = "white", color = NA),
    legend.title = element_blank()) -> fig2

ggsave(filename = here("output/figure-2.png"),
       plot = fig2,
       device = png(),
       scale = 1, 
       width = 21,
       height = 10, 
       units = "cm",
       dpi = 300)

dev.off()
##############################################################

# Univariate analysis -simulation level
df_pall %>%
  #  mutate(across(base_incidence:pn_base_coverage, ~ mean(., na.rm = TRUE))) %>% # 
  mutate(across(!network, ~ mean(., na.rm = TRUE))) %>% # 
  #  mutate(id = 42) %>% 
  slice_head(n=1) -> df_mean_all
df_mean_all$network <- "higher degree"

full_lm  <-  lm(incidence_rr ~. , data = df_pall)

predict(full_lm, newdata = df_mean_all, type="response")

df_scr0_all <- df_scr0_sympt_all <- df_mean_all
df_scr0_all["weekly_screen_pr"] <- 0
df_scr0_sympt_all["weekly_screen_pr"] <- 0
df_scr0_sympt_all["weekly_sympt_test_pr"] <- 0.47

predict(full_lm, newdata = df_scr0_all, type="response")
predict(full_lm, newdata = df_scr0_sympt_all, type="response")


