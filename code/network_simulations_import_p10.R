####### Load data from multiple files
# if you run network simulations import first you will have the 
# this section is separate because the p10 is saved as inter10, similar to original x10
# order is different for p10 and x40
rm(inter10)
# 1st batch
cnames_int10p_1 <- list.files("~/gc_pn_impact/analysis_data_1st_set", "interp10",  full.names=TRUE)
cruns_int10p_1 <- length(cnames_int10p_1)
cnames_int10p_1_2 <- cnames_int10p_1[c(1,2,8,3,4,5,6,7)]

# 2n batch
cnames_int10p_2 <- list.files("~/gc_pn_impact/analysis_data_2nd_set", "interp10",  full.names=TRUE)
cruns_int10p_2 <- length(cnames_int10p_2)
cnames_int10p_2_2 <- cnames_int10p_2[c(1,2,8,3,4,5,6,7)] # 

# 3rd batch
cnames_int10p_3 <- list.files("~/gc_pn_impact/analysis_data_3rd_set", "interp10",  full.names=TRUE)
cruns_int10p_3 <- length(cnames_int10p_3)
cnames_int10p_3_2 <- cnames_int10p_3[c(1,2,8,3,4,5,6,7)] # 

# 4th batch
cnames_int10p_4 <- list.files("~/gc_pn_impact/analysis_data_4th_set", "interp10",  full.names=TRUE)
cruns_int10p_4 <- length(cnames_int10p_4)
# manually check to make sure the order is the same
cnames_int10p_4_2 <- cnames_int10p_4[c(1,2,8,3,4,5,6,7)] # 

# 5th batch
cnames_int10p_5 <- list.files("~/gc_pn_impact/analysis_data_5th_set", "interp10-",  full.names=TRUE)
cruns_int10p_5 <- length(cnames_int10p_5)
# manually check to make sure the order is the same
cnames_int10p_5_2 <- cnames_int10p_5[c(3,1,8,2,4,5,6,7)] # 

cnames_int10p_all <- c(cnames_int10p_1_2, cnames_int10p_2_2, cnames_int10p_3_2, cnames_int10p_4_2, cnames_int10p_5_2)

inc_i10p  <- numeric(0)
incind_i10p  <-  numeric(0)
#ind_dur_i10p  <- numeric(0)
pr_i10p  <- numeric(0)
pr2_i10p  <- numeric(0)
d_i10p <- numeric(0)
#
for (i in 1:length(cnames_int10p_all)) {

  load(cnames_int10p_all[i])

  inc_i10p <-  rbind(inc_i10p, inter10$scr_inc[nonzeros_id,])
  pr_i10p <-  rbind(pr_i10p, inter10$scr_pr[nonzeros_id,]/2000)
  pr2_i10p <-  rbind(pr2_i10p, inter10$scr_pr[nonzeros_id,])
  d_i10p <-  rbind(d_i10p, inter10$scr_diag[nonzeros_id,])
  incind_i10p <- rbind(incind_i10p, t(inter10$ind_inc2[,nonzeros_id])) # individual level outputs
 # ind_dur_i10p <- rbind(ind_dur_i10p, t(inter10$ind_dur2[,non_zeros_list[[i]]]))
}




