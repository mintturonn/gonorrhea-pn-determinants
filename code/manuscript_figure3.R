
library(plotly)
library(ggplot2)
library(here)
library(dplyr)
library(reshape2)
library(tidyr)
library(stringr)

# rm(list = ls())
# .rs.restartR()

## loading the data takes time (not included in github due to size)
# order matters - id_b and non_zeros needed here
source(here('code/network_simulations_import.R'))
# new scenarios x40
source(here('code/network_simulations_import_x40.R'))
# 
source(here('stat_network/init_SIS_model.R'))
source(here('code/manuscript_figures_funs.R'))
#####################################################################################
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
popinc_p2 <- cbind(yearfun_p(inc_b[param_id_s_logic,], "basecase", id_b[param_id_s_logic]), 
                   yearfun_p(inc_i10[param_id_s_logic,], "PN: 10%x", id_b[param_id_s_logic]), 
                   yearfun_p(inc_i20[param_id_s_logic,], "PN: 20%x", id_b[param_id_s_logic]), 
                   yearfun_p(inc_i40[param_id_s_logic,], "PN: 40%x", id_b[param_id_s_logic]))

##############################################################
# 
IRR_10_2 <- cbind( popinc_p2[,6]/popinc_p2[,1],  popinc_p2[,7]/popinc_p2[,2], popinc_p2[,8]/popinc_p2[,3], popinc_p2[,9]/popinc_p2[,4])
IRR_20_2 <- cbind( popinc_p2[,11]/popinc_p2[,1],  popinc_p2[,12]/popinc_p2[,2], popinc_p2[,13]/popinc_p2[,3], popinc_p2[,14]/popinc_p2[,4])
IRR_40_2 <- cbind( popinc_p2[,16]/popinc_p2[,1],  popinc_p2[,17]/popinc_p2[,2], popinc_p2[,18]/popinc_p2[,3], popinc_p2[,19]/popinc_p2[,4])

##############################################################
pn <- paramavfun(pn_b[param_id_s_logic], pn_i10[param_id_s_logic], pn_i20[param_id_s_logic], pn_i40[param_id_s_logic], id_b[param_id_s_logic])
ind_sympt <- paramavfun(diag_sympt_num_b[param_id_s_logic], diag_sympt_num_i10[param_id_s_logic], diag_sympt_num_i20[param_id_s_logic], diag_sympt_num_i40[param_id_s_logic], id_b[param_id_s_logic])
ind_asympt <- paramavfun(diag_asympt_num_b[param_id_s_logic], diag_asympt_num_i10[param_id_s_logic], diag_asympt_num_i20[param_id_s_logic], diag_asympt_num_i40[param_id_s_logic], id_b[param_id_s_logic])

melt(pn[,2:4]) %>%
 rename(pn_scen = variable,
        pn_value = value) %>%
  mutate(pn_rate = pn_value/2000) -> pn_long

melt(ind_sympt[,2:4]) %>%
  rename(ind_sympt_scen = variable,
         ind_sympt_value = value) -> ind_sympt_long

melt(ind_asympt[,2:4]) %>%
  rename(ind_asympt_scen = variable,
         ind_asympt_value = value) -> ind_asympt_long
  
cbind(IRR_10_2[,4], IRR_20_2[,4], IRR_40_2[,4]) %>%
  as_tibble() %>%
  rename(`PN x10%` = V1,
         `PN x20%` = V2,
         `PN x40%`= V3) %>%
  melt() %>%
  rename(irr_scen = variable,
       irr_value = value) -> irr_long

cbind(irr_long, pn_long, ind_sympt_long, ind_asympt_long) -> fig_df


fig_df$index_test_rate <- (fig_df$ind_sympt_value+fig_df$ind_asympt_value)/2000

# Du et al. 
fig_df$p94 <- ifelse(fig_df$irr_value < 0.94, 1, 0)
fig_df$p90 <- ifelse(fig_df$irr_value < 0.90, 1, 0)

# dummy variable for testing rate
fig_df$test_percs <- ifelse(fig_df$index_test_rate <= 0.1, 0.1, 
                            ifelse(fig_df$index_test_rate > 0.1 & fig_df$index_test_rate <= 0.2, 0.2,
                                   ifelse(fig_df$index_test_rate > 0.2 & fig_df$index_test_rate <= 0.3, 0.3,
                                          ifelse(fig_df$index_test_rate > 0.3, 0.4, NA))))

# fig_df$irr_percs <- ifelse(fig_df$irr_value <= 0.1, 0.1, 
#                             ifelse(fig_df$index_test_rate > 0.1 & fig_df$index_test_rate <= 0.2, 0.2,
#                                    ifelse(fig_df$index_test_rate > 0.2 & fig_df$index_test_rate <= 0.3, 0.3,
#                                           ifelse(fig_df$index_test_rate > 0.3, 0.4, NA))))

# overall
fig_df %>% 
  group_by(irr_scen) %>%
  summarize(
    pr0.06 =  sum(p94)/n(),
    pr0.1  =  sum(p90)/n(),
    n = n()
  ) %>%
  ungroup() -> irr_overall

# cumulative probability of hitting the target
fig_df %>% 
  group_by(irr_scen, test_percs) %>%
  summarize(
    n0.06 =  sum(p94),
    n0.1  =  sum(p90)
  ) %>%  
  ungroup() %>%
  group_by(irr_scen) %>%
    reframe(
      test_percs = test_percs,
      n0.06 =  cumsum(n0.06),
      n0.1  =  cumsum(n0.1),
    ) %>%
  ungroup() %>%
  mutate(`IRR < 0.96` = n0.06/irr_overall$n[1],
         `IRR < 0.90` = n0.1/irr_overall$n[1]) -> irr_test

irr_test %>%
  pivot_longer(cols = c("IRR < 0.96", "IRR < 0.90"), names_to = "threshold", values_to = "cumpr") -> irr_test_long

##############################################################
# 3A

  ggplot(fig_df, aes(y=irr_value, x=100*index_test_rate, color=100*pn_rate)) +
    geom_point(size=3, alpha=0.5) + 
    scale_color_continuous(name= "Partners notified\nper 100 population", type = "viridis", direction=-1) +
    theme_minimal() + xlab("Cumulative diagnosis rate of index cases per 100 population") +
    ylab("Year  4 IRR")  + xlim(c(0,40)) + 
    facet_wrap(~irr_scen, ncol=3) +
    theme( 
           panel.background = element_rect(fill = "white",
                                          #colour = "white",
                                          size = 0.2, linetype = "solid"),
            axis.ticks.y = element_blank(),
            panel.grid.major.x = element_line(linewidth = 0.15, linetype = 'solid', colour = "gray80"),
            panel.grid.minor.x = element_line(linewidth = 0.15, linetype = 'solid', colour = "gray80"),
            panel.grid.major.y = element_line(linewidth = 0.15, linetype = 'solid', colour = "gray80"),
            panel.grid.minor.y = element_line(linewidth = 0.15, linetype = 'solid', colour = "gray80"),
            axis.text.y  = element_text( size = 12 ),
            axis.text.x  = element_text( size = 12 ),
            axis.title.x = element_text( size = 12 ),
            axis.title.y = element_text( size = 12 ),
            title        = element_text( size = 14),
            legend.position = "right",
            legend.justification = "center") -> fig3A
  
  # ggsave(filename = here("output/figure_3A.png"),
  #        plot = fig3A,
  #        device = png(),
  #        scale = 1,
  #        width = 25,
  #        height = 15,
  #        units = "cm",
  #        dpi = 300)
  # 
  # dev.off()

##############################################################
# 3B
irr_test %>%
    pivot_longer(cols = c("IRR < 0.96", "IRR < 0.90"), names_to = "threshold", values_to = "cumpr") %>%
    ggplot( aes(y=100*cumpr, x=100*test_percs, color=threshold, group=threshold)) +
    geom_line(linewidth=0.5, alpha=0.5) + 
    geom_point(alpha=0.5) + 
    scale_color_manual(name= "IRR threshold", values = c("gray30", "navy", "darkorange")) +
    theme_minimal() + xlab("Cumulative diagnosis rate of index cases per 100 population") +
    ylab("Reached the threshold (%)")  + xlim(c(0,40)) + 
    facet_wrap(~irr_scen, ncol=3) +
    theme( 
      panel.background = element_rect(fill = "white",
                                      #colour = "white",
                                      size = 0.2, linetype = "solid"),
      axis.ticks.y = element_blank(),
      panel.grid.major.x = element_line(linewidth = 0.15, linetype = 'solid', colour = "gray80"),
      panel.grid.minor.x = element_line(linewidth = 0.15, linetype = 'solid', colour = "gray80"),
      panel.grid.major.y = element_line(linewidth = 0.15, linetype = 'solid', colour = "gray80"),
      panel.grid.minor.y = element_line(linewidth = 0.15, linetype = 'solid', colour = "gray80"),
      axis.text.y  = element_text( size = 12 ),
      axis.text.x  = element_text( size = 12 ),
      axis.title.x = element_text( size = 12 ),
      axis.title.y = element_text( size = 12 ),
      title        = element_text( size = 14),
      legend.position = "right",
      legend.justification = "center") -> fig3B
  
  # ggsave(filename = here("output/figure_3B.png"),
  #        plot = fig3B,
  #        device = png(),
  #        scale = 1,
  #        width = 25,
  #        height = 10,
  #        units = "cm",
  #        dpi = 300)
  # 
  # dev.off()
  
##############################################################
  
  fig3_both <- plot_grid(fig3A, fig3B, rel_heights = c(1.5, 1), ncol=1, labels=c('A)', 'B)'))
  
  ggsave(filename = here("output/figure_3both.png"),
         plot = fig3_both,
         device = png(),
         scale = 1,
         width = 26,
         height = 27,
         units = "cm",
         dpi = 300)
  
  dev.off()
  