
####### Load data from multiple files

## BASE
# 1st batch
cnames_base1 <- list.files("~/gc_pn_impact/analysis_data_1st_set", "base_",  full.names=TRUE)
cruns_base1 <- length(cnames_base1)
cnames_base1_2 <- cnames_base1[c(3,1,8,2,4,5,6,7)]

# 2n batch
cnames_base2 <- list.files("~/gc_pn_impact/analysis_data_2nd_set", "base_",  full.names=TRUE)
cruns_base2 <- length(cnames_base2)
cnames_base2_2 <- cnames_base2[c(3,1,8,2,4,5,6,7)] # 

# 3rd batch
cnames_base3 <- list.files("~/gc_pn_impact/analysis_data_3rd_set", "base_",  full.names=TRUE)
cruns_base3 <- length(cnames_base3)
cnames_base3_2 <- cnames_base3[c(3,1,8,2,4,5,6,7)]

# 4th batch
cnames_base4 <- list.files("~/gc_pn_impact/analysis_data_4th_set", "base_",  full.names=TRUE)
cruns_base4 <- length(cnames_base4)
# manually check to make sure the order is the same
cnames_base4_2 <- cnames_base4[c(3,1,8,2,4,5,6,7)]

# 5th batch
cnames_base5 <- list.files("~/gc_pn_impact/analysis_data_5th_set", "base_",  full.names=TRUE)
cruns_base5 <- length(cnames_base5)
# manually check to make sure the order is the same
cnames_base5_2 <- cnames_base5[c(3,1,8,2,4,5,6,7)]

cnames_base_all <- c(cnames_base1_2, cnames_base2_2, cnames_base3_2, cnames_base4_2, cnames_base5_2)

## INTER10
# 1st batch
cnames_int10_1 <- list.files("~/gc_pn_impact/analysis_data_1st_set", "inter10",  full.names=TRUE)
cruns_int10_1 <- length(cnames_int10_1)
cnames_int10_1_2 <- cnames_int10_1[c(3,1,8,2,4,5,6,7)]

# 2n batch
cnames_int10_2 <- list.files("~/gc_pn_impact/analysis_data_2nd_set", "inter10",  full.names=TRUE)
cruns_int10_2 <- length(cnames_int10_2)
cnames_int10_2_2 <- cnames_int10_2[c(3, 1, 8, 2, 4, 5, 6, 7)] # 

# 3rd batch
cnames_int10_3 <- list.files("~/gc_pn_impact/analysis_data_3rd_set", "inter10",  full.names=TRUE)
cruns_int10_3 <- length(cnames_int10_3)
cnames_int10_3_2 <- cnames_int10_3[c(3,1,8,2,4,5,6,7)]

# 4th batch
cnames_int10_4 <- list.files("~/gc_pn_impact/analysis_data_4th_set", "inter10",  full.names=TRUE)
cruns_int10_4 <- length(cnames_int10_4)
# manually check to make sure the order is the same
cnames_int10_4_2 <- cnames_int10_4[c(3,1,8,2,4,5,6,7)]

# 5th batch
cnames_int10_5 <- list.files("~/gc_pn_impact/analysis_data_5th_set", "inter10_",  full.names=TRUE)
cruns_int10_5 <- length(cnames_int10_5)
# manually check to make sure the order is the same
cnames_int10_5_2 <- cnames_int10_5[c(3,1,8,2,4,5,6,7)]

cnames_int10_all <- c(cnames_int10_1_2, cnames_int10_2_2, cnames_int10_3_2, cnames_int10_4_2, cnames_int10_5_2)



## INTER20
# 1st batch
cnames_int20_1 <- list.files("~/gc_pn_impact/analysis_data_1st_set", "inter20",  full.names=TRUE)
cruns_int20_1 <- length(cnames_int20_1)
cnames_int20_1_2 <- cnames_int20_1[c(3,1,8,2,4,5,6,7)]

# 2n batch
cnames_int20_2 <- list.files("~/gc_pn_impact/analysis_data_2nd_set", "inter20",  full.names=TRUE)
cruns_int20_2 <- length(cnames_int20_2)
cnames_int20_2_2 <- cnames_int20_2[c(3, 1, 8, 2, 4, 5, 6, 7)] # 

# 3rd batch
cnames_int20_3 <- list.files("~/gc_pn_impact/analysis_data_3rd_set", "inter20",  full.names=TRUE)
cruns_int20_3 <- length(cnames_int20_3)
cnames_int20_3_2 <- cnames_int20_3[c(3,1,8,2,4,5,6,7)]

# 4th batch
cnames_int20_4 <- list.files("~/gc_pn_impact/analysis_data_4th_set", "inter20",  full.names=TRUE)
cruns_int20_4 <- length(cnames_int20_4)
# manually check to make sure the order is the same
cnames_int20_4_2 <- cnames_int20_4[c(3,1,8,2,4,5,6,7)]

# 5th batch
cnames_int20_5 <- list.files("~/gc_pn_impact/analysis_data_5th_set", "inter20_",  full.names=TRUE)
cruns_int20_5 <- length(cnames_int20_5)
# manually check to make sure the order is the same
cnames_int20_5_2 <- cnames_int20_5[c(3,1,8,2,4,5,6,7)]

cnames_int20_all <- c(cnames_int20_1_2, cnames_int20_2_2, cnames_int20_3_2, cnames_int20_4_2, cnames_int20_5_2)

params_num <- 5 * 83
##################################

# 
inc_b <-  inc_i20 <- inc_i10 <- numeric(0)
incind_b <- incind_i20  <- incind_i10  <- prt_num <- numeric(0)
ind_dur_b <-    ind_dur_i20  <-  ind_dur_i10  <- numeric(0)
pr_b <- pr_i20 <-  pr_i10   <- numeric(0)
pr2_b <- pr2_i20 <- pr2_i10  <- numeric(0)
d_b <- d_i20 <- d_i10 <- numeric(0)
id_b <- numeric(0) 
s_b <- numeric(0) 

partn_num <- incind_b <- incind_i20 <- incind_i10 <- numeric(0)
degree_type <- basepr <- baseinc <- numeric(0)
non_zeros_list <-  vector("list", length(cnames_base_all))
cas_num <- main_num <- inst_types <- numeric(0)

id_b <- c( rep(rep(1:83,         each=10),8), #inner rep is for individually saved run, outer rep is number of runs per parameter set
           rep(rep(84:(83+83),   each=10),8),
           rep(rep(167:(166+83), each=10),8),
           rep(rep(250:(249+83), each=10),8),
           rep(rep(333:(332+83), each=10),8))
# 
for (i in 1:length(cnames_base_all)) {
  
  load(cnames_base_all[i])
  load(cnames_int10_all[i])
  load(cnames_int20_all[i])

  nonzeros_id <- 1:nrow(base$scr_pr) # selection is no done at this level anymore, select all simulations
 
##############################   

  # this is at parameter level (83, 2000 )
  inst_types <- rbind(inst_types, base$inst.type.all)
  
  # this is at parameter level (83, 2000 )
  for (j in 1:length(base$parts.all)){
    
    main_num <- rbind(main_num, rowSums(base$parts.all[[j]]==2))
    cas_num <-  rbind(cas_num, rowSums(base$parts.all[[j]]==1))
    # test_num <- rbind(test_num, rowSums(base$parts.all[[j]]>0))
  }
  
  #id_b <- id_b0[nonzeros_id]

  inc_b <- rbind(inc_b, base$scr_inc[nonzeros_id,])
  inc_i20 <-  rbind(inc_i20, inter20$scr_inc[nonzeros_id,])
  inc_i10 <-  rbind(inc_i10, inter10$scr_inc[nonzeros_id,])

  s_b <- rbind(s_b, base$scr_s[nonzeros_id,])
  
  pr_b <- rbind(pr_b, base$scr_pr[nonzeros_id,]/2000) # per person
  pr_i20 <-  rbind(pr_i20, inter20$scr_pr[nonzeros_id,]/2000)
  pr_i10 <-  rbind(pr_i10, inter10$scr_pr[nonzeros_id,]/2000)
  
  pr2_b <- rbind(pr2_b, base$scr_pr[nonzeros_id,]) # total number
  pr2_i20 <-  rbind(pr2_i20, inter20$scr_pr[nonzeros_id,])
  pr2_i10 <-  rbind(pr2_i10, inter10$scr_pr[nonzeros_id,])
  
  d_b <- rbind(d_b, base$scr_diag[nonzeros_id,]) # 
  d_i20 <-  rbind(d_i20, inter20$scr_diag[nonzeros_id,])
  d_i10 <-  rbind(d_i10, inter10$scr_diag[nonzeros_id,])
  
  basepr <- c(basepr, base$scr_pr[nonzeros_id,417]/2000 > 0.03)
  baseinc <- c(baseinc, (base$scr_inc[nonzeros_id,417]-base$scr_inc[nonzeros_id,365])/2000 > 0.15)
  
  # individiual level estimates
  # prt_num <- rbind(prt_num, apply(base$ind_ties, 1, function(x) ifelse(x>3, 1, 0) ))
  
  partn_num <- rbind(partn_num, t(base$ind_ties[,nonzeros_id]))
  
  incind_b <- rbind(incind_b, t(base$ind_inc2[,nonzeros_id])) # individual level outputs
  incind_i20 <- rbind(incind_i20, t(inter20$ind_inc2[,nonzeros_id]))
  incind_i10 <- rbind(incind_i10, t(inter10$ind_inc2[,nonzeros_id]))
  # 
  # ind_dur_b <- rbind(ind_dur_b, t(base$ind_dur2[,nonzeros_id]))
  # ind_dur_i20 <- rbind(ind_dur_i20, t(inter20$ind_dur2[,nonzeros_id]))
  # ind_dur_i10 <- rbind(ind_dur_i10, t(inter10$ind_dur2[,nonzeros_id]))
  
}

#############################
# all params in order:

load(here('calib_params_1st_set.RData'))
calib_params_all <- calib_params$params.all
#dgr_sall <- rep(calib_params$degr_type, each=length(calib_params$crn_sympt_pr[,1])/83 )
dgr_pall <- calib_params$degr_type

load(here('calib_params_2nd_set.RData'))
calib_params_all <- c(calib_params_all, calib_params$params.all)
# dgr_sall <- c(dgr_sall, rep(calib_params$degr_type, each=length(calib_params$crn_sympt_pr[,1])/83 ))
dgr_pall <- c(dgr_pall ,calib_params$degr_type)

load(here('calib_params_3rd_set.RData'))
calib_params_all <- c(calib_params_all, calib_params$params.all)
#dgr_sall <- c(dgr_sall, rep(calib_params$degr_type, each=length(calib_params$crn_sympt_pr[,1])/83 ))
dgr_pall <- c(dgr_pall ,calib_params$degr_type)

load(here('calib_params_4th_set.RData'))
calib_params_all <- c(calib_params_all, calib_params$params.all)
#dgr_sall <- c(dgr_sall, rep(calib_params$degr_type, each=length(calib_params$crn_sympt_pr[,1])/83 ))
dgr_pall <- c(dgr_pall ,calib_params$degr_type)


load(here('calib_params_5th_set.RData'))
calib_params_all <- c(calib_params_all, calib_params$params.all)
#dgr_sall <- c(dgr_sall, rep(calib_params$degr_type, each=length(calib_params$crn_sympt_pr[,1])/83 ))
dgr_pall <- c(dgr_pall ,calib_params$degr_type)

#################################################################

dat  <- read_excel(here::here('data', 'params.xlsx'), 
                   col_types = c("guess", "guess", "guess", "guess", "numeric", "numeric", "numeric", "guess", "guess", "guess"))

# seq(1, length(base$parts.all)*8*5, by=664) : (seq(1, length(base$parts.all)*8*5, by=664)+82)
## to select the parameter-level indexes from the inst type and cas/main outputs
part_id <- c(1:83, 665:747, 1329:1411, 1993:2075, 2657:2739)

prmsdat <- matrix(NA,  params_num, length(dat$params)+2)
colnames(prmsdat) <- c(dat$params, "pr_mainpartn", "nw_degree")
# 
  for (i in 1:params_num) {
    
    prmsdat[i, "sympt_pr"]      <-  calib_params_all[[i]]$sympt_pr
    prmsdat[i, "sympt_test_pr"] <-  calib_params_all[[i]]$sympt_test_pr
    prmsdat[i, "trnsm_pr_cas"]  <-  calib_params_all[[i]]$trnsm_pr_cas
    prmsdat[i, "trnsm_pr_main"] <-  calib_params_all[[i]]$trnsm_pr_rr*calib_params_all[[i]]$trnsm_pr_cas
    prmsdat[i, "clear_pr"]      <-  calib_params_all[[i]]$clear_pr
    prmsdat[i, "screen_pr"]     <-  calib_params_all[[i]]$screen_pr
    prmsdat[i, "index_interview"]<- calib_params_all[[i]]$pn_pr_interv
    prmsdat[i, "partner_reached"]<- calib_params_all[[i]]$pn_pr_reach
    prmsdat[i,"act_pr_inst"]     <- calib_params_all[[i]]$act_pr_inst
    prmsdat[i,"inst_acts"]       <- calib_params_all[[i]]$inst_acts
    prmsdat[i,"trnsm_pr_inst"]  <- calib_params_all[[i]]$ trnsm_pr_inst
    prmsdat[i, "pn_pr_main"]    <-  calib_params_all[[i]]$pn_pr_interv * calib_params_all[[i]]$pn_pr_reach
    
    prmsdat[i, "nw_degree"] <- dgr_pall[i]
  }  
### Here adding to params data the proportion of people having instantenous partnerships
inst_type_temp <- rowSums(inst_types)/2000
main_part_temp <- (rowSums(main_num) / (rowSums(main_num)+rowSums(cas_num)))

  prmsdat[,"pr_inst_type"] <- inst_type_temp[part_id]
  prmsdat[,"pr_mainpartn"] <- main_part_temp[part_id]  
  
# 
prmsdat_s <- matrix(NA, length(id_b), length(dat$params)+3)
colnames(prmsdat_s) <- c(dat$params, "pr_mainpartn", "nw_degree", "id")
#
  for (i in 1:params_num) {

    id_temp <- which(id_b==i)

    prmsdat_s[id_temp, "sympt_pr"]       <-  prmsdat[i, "sympt_pr"]
    prmsdat_s[id_temp, "sympt_test_pr"]  <-  prmsdat[i, "sympt_test_pr"]
    prmsdat_s[id_temp, "trnsm_pr_cas"]   <-  prmsdat[i, "trnsm_pr_cas"]
    prmsdat_s[id_temp, "trnsm_pr_main"]  <-  prmsdat[i, "trnsm_pr_main"]
    prmsdat_s[id_temp, "clear_pr"]       <-  prmsdat[i, "clear_pr"]
    prmsdat_s[id_temp, "screen_pr"]      <-  prmsdat[i, "screen_pr"]
    prmsdat_s[id_temp, "index_interview"]<- prmsdat[i, "index_interview"]
    prmsdat_s[id_temp, "partner_reached"]<- prmsdat[i, "partner_reached"]
    prmsdat_s[id_temp,"act_pr_inst"]     <- prmsdat[i,"act_pr_inst"]
    prmsdat_s[id_temp,"pr_inst_type"]    <- prmsdat[i,"pr_inst_type"]
    prmsdat_s[id_temp,"inst_acts"]       <- prmsdat[i,"inst_acts"]
    prmsdat_s[id_temp,"trnsm_pr_inst"]   <- prmsdat[i,"trnsm_pr_inst"]
    prmsdat_s[id_temp, "pn_pr_main"]     <-  prmsdat[i, "pn_pr_main"]

    prmsdat_s[id_temp, "pr_inst_type"]   <- prmsdat[i,"pr_inst_type"]
    prmsdat_s[id_temp, "pr_mainpartn"]   <- prmsdat[i,"pr_mainpartn"]

    prmsdat_s[id_temp, "nw_degree"]      <- dgr_pall[i]
    prmsdat_s[id_temp, "id"] <- i

  }



