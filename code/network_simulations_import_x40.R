# order is different for p10 and x40
####### Load data from multiple files
# the loaded variables are called inter10 in these runs too

loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

# if you run network simulations import first you will have the 
# this section is separate because the x40 is saved as inter10
# x40
## INTER40
# 1st batch
cnames_int40_1 <- list.files("~/gc_pn_impact/analysis_data_1st_set", "inter40",  full.names=TRUE)
cruns_int40_1 <- length(cnames_int40_1)
cnames_int40_1_2 <- cnames_int40_1[c(3,1,8,2,4,5,6,7)]

# 2n batch
cnames_int40_2 <- list.files("~/gc_pn_impact/analysis_data_2nd_set", "inter40",  full.names=TRUE)
cruns_int40_2 <- length(cnames_int40_2)
cnames_int40_2_2 <- cnames_int40_2[c(3,1,8,2,4,5,6,7)] # 

# 3rd batch
cnames_int40_3 <- list.files("~/gc_pn_impact/analysis_data_3rd_set", "inter40",  full.names=TRUE)
cruns_int40_3 <- length(cnames_int40_3)
cnames_int40_3_2 <- cnames_int40_3[c(3,1,8,2,4,5,6,7)]

# 4th batch
cnames_int40_4 <- list.files("~/gc_pn_impact/analysis_data_4th_set", "inter40",  full.names=TRUE)
cruns_int40_4 <- length(cnames_int40_4)
# manually check to make sure the order is the same
cnames_int40_4_2 <- cnames_int40_4[c(3,1,8,2,4,5,6,7)]

# 5th batch
cnames_int40_5 <- list.files("~/gc_pn_impact/analysis_data_5th_set", "inter40-",  full.names=TRUE)
cruns_int40_5 <- length(cnames_int40_5)
# manually check to make sure the order is the same
cnames_int40_5_2 <- cnames_int40_5[c(3,1,8,2,4,5,6,7)] 

cnames_int40_all <- c(cnames_int40_1_2, cnames_int40_2_2, cnames_int40_3_2, cnames_int40_4_2, cnames_int40_5_2)

inc_i40  <- numeric(0)
incind_i40 <- numeric(0)
# ind_dur_i40   <- numeric(0)
pr_i40 <- numeric(0)
pr2_i40  <- numeric(0)
d_i40 <- numeric(0)

for (i in 1:length(cnames_int40_all)) {

  # some cluster runs had been stored as inter10 instead of inter40!
  inter40 <- loadRData(cnames_int40_all[i])

  inc_i40 <-  rbind(inc_i40, inter40$scr_inc[nonzeros_id,])
  pr_i40 <-  rbind(pr_i40, inter40$scr_pr[nonzeros_id,]/2000)
  pr2_i40 <-  rbind(pr2_i40, inter40$scr_pr[nonzeros_id,])
  d_i40 <-  rbind(d_i40, inter40$scr_diag[nonzeros_id,])
  incind_i40 <- rbind(incind_i40, t(inter40$ind_inc2[,nonzeros_id]))
 # ind_dur_i40 <- rbind(ind_dur_i40, t(inter40$ind_dur2[,non_zeros_list[[i]]]))
 
}


