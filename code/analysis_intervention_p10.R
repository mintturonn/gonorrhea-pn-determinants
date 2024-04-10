# 

library(here)
library(dplyr)
library(reshape2)
library(readxl)

dat  <- read_excel(here::here('data', 'params.xlsx'), 
                   col_types = c("guess", "guess", "guess", "guess", "numeric", "numeric", "numeric", "guess", "guess", "guess"))

# rm(list = ls())
# .rs.restartR()
###################################
# network funs, need the parttype
source(here('stat_network/network_funs.R'))
# transmission
source(here('stat_network/gc_transm.R'))
# parameters
source(here('stat_network/init_SIS_model.R'))

#################

# load the master CRN
load(here('data/crn_main.RData'))

# parameter sets
load(here('calib_params.RData'))

runs1 <- length(calib_params$params.all)

pn_index1 <- pn_index2 <-  scr_pr <- scr_inc <- scr_diag <- scr_s <- scr_dur_per_inf <- scr_reinf <- inst_type.all <- numeric(0)
ind_inc1 <- ind_inc2 <- ind_ties <-   ind_dur1 <- ind_dur2 <- run_id <- numeric(0)

scr_screen <- ind_all_pos1yr <- ind_all_pos2yr<- ind_all_pos3yr <- ind_all_pos4yr <- ind_all_pos <- ind_screen <- ind_pn_pos <- ind_pn_all <- numeric(0)
  
# print("start")

for (r in 1:runs1 ){ #
  
  #
  init_inf <- calib_params$init_inf.all[[r]]
  parts    <-  calib_params$parts.all[[r]]
  params <- calib_params$params.all[[r]]
  inst_type <- calib_params$inst.type.all[r,]
  
  # 8 years in total
  params$simlength <- 416
  params_pn <- params[["pn_pr_main"]] + 0.1
  #
  for (j in 1:10 ){ #
  
    crn_sympt_pr    <-  calib_params$crn_sympt_pr[((r-1)*10+j),]
    crn_clear_pr    <-  calib_params$crn_clear_pr[((r-1)*10+j),] 
    crn_screen_pr   <-  calib_params$crn_screen_pr[((r-1)*10+j),] 
    crn_sympt_test  <-  calib_params$crn_sympt_test[((r-1)*10+j),] 
    crn_pn_partn_main <-  calib_params$crn_pn_partn_main[((r-1)*10+j),] 
    crn_inst_pr       <-  calib_params$crn_inst_pr[((r-1)*10+j),]  
    crn_instinf_pr    <-  calib_params$crn_instinf_pr[((r-1)*10+j),]
    crn_inf_cas       <-  calib_params$crn_inf_cas[[((r-1)*10+j)]]
    crn_inf_main      <-  calib_params$crn_inf_main[[((r-1)*10+j)]]
    
    mod <- sims_pn_crn(init_inf, parts, inst_type, params, params_pn, crn_main, crn_sympt_pr, crn_clear_pr, crn_screen_pr, crn_sympt_test, crn_pn_partn_main, crn_inst_pr, crn_instinf_pr, crn_inf_cas, crn_inf_main) 
      
    # # 
    run_id <- rbind(run_id, paste(r))   
    # # Individual
    ind_inc1 <-   cbind(ind_inc1, rowSums(mod$inc[,1:209]>0)) 
    ind_inc2 <-   cbind(ind_inc2, rowSums(mod$inc[,210:417]>0)) 
    ind_ties <-   cbind(ind_ties, rowSums(parts>0))
    ind_dur1 <-   cbind( ind_dur1, rowSums(mod$inf[,1:209]>0)) 
    ind_dur2 <-   cbind( ind_dur2, rowSums(mod$inf[,210:417]>0)) 
    # population level  
    scr_pr   <-  rbind(scr_pr, colSums(mod$inf>0))
    scr_inc  <-  rbind(scr_inc, cumsum(colSums(mod$inc>0)))
    scr_diag <-  rbind(scr_diag, cumsum(colSums(mod$diagn>0)))
    scr_s    <-  rbind(scr_s, cumsum(colSums(mod$diagn==2)))
    inst_type.all <- rbind(inst_type.all, inst_type)
    pn_index1 <-  rbind(pn_index1, cumsum(colSums(mod$pn_index[,1:209])))
    pn_index2 <-  rbind(pn_index2, cumsum(colSums(mod$pn_index[,210:417])))
    
    # new outputs (23-11-10)
    # population level: number of screens
    scr_screen <- rbind(scr_screen, cumsum(colSums(mod$diagn==1)))
    # individual level 
    ind_all_pos1yr <- cbind(ind_all_pos1yr,rowSums(mod$inf_partners[,210:261]))
    ind_all_pos2yr <- cbind(ind_all_pos2yr,rowSums(mod$inf_partners[,262:313]))
    ind_all_pos3yr <- cbind(ind_all_pos3yr,rowSums(mod$inf_partners[,314:365]))
    ind_all_pos4yr <- cbind(ind_all_pos4yr,rowSums(mod$inf_partners[,366:417]))
    ind_all_pos <- cbind(ind_all_pos,rowSums(mod$inf_partners[,210:417]))
    ind_screen  <- cbind(ind_screen, rowSums(mod$diagn[,210:417]==1))
    ind_pn_pos  <- cbind(ind_pn_pos, rowSums(mod$infstat_partners[,210:417]==3))
    ind_pn_all  <- cbind(ind_pn_all, rowSums(mod$infstat_partners[,210:417]==3)+rowSums(mod$infstat_partners[,210:417]==-3))
    
    
    # print(r)
  }
}

inter10 <- list(params.all = calib_params$params.all,
             init_inf.all = calib_params$init_inf.all,
             parts.all = calib_params$parts.all,
             inst.type.all = calib_params$inst.type.all,
             ind_inc1 = ind_inc1,
             ind_inc2 = ind_inc2,
             ind_ties = ind_ties,
             ind_dur1 = ind_dur1,
             ind_dur2 = ind_dur2,
             scr_pr = scr_pr, 
             scr_inc = scr_inc,
             scr_diag = scr_diag,
             scr_s = scr_s,
             pn_index1= pn_index1,
             pn_index2 = pn_index2,
             scr_screen=scr_screen,
             ind_all_pos=ind_all_pos,
             ind_screen=ind_screen,
             ind_pn_pos=ind_pn_pos,
             ind_pn_all=ind_pn_all,
             ind_all_pos1yr=ind_all_pos1yr,
             ind_all_pos2yr=ind_all_pos2yr,
             ind_all_pos3yr=ind_all_pos3yr,
             ind_all_pos4yr=ind_all_pos4yr)

save(inter10, file = paste0("interp10", "_", Sys.time(), ".RData"))

quit(save = "no", status = 0)




