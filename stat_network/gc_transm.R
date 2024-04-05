

sims_pn_gencrn <- function(init_inf, types, inst_type, params, params_pn, crn_main) {
  
  inf_state <- incidence <- clearance <- diagn <- pn_index <- which_partners <- main_partn <- matrix(0, dim(init_inf)[1], params[["simlength"]] +1)
  inf_state[,1] <- init_inf[,1]
  pn_coverage <-  vector()
  
  # list index cases' partner types
    main_partn <- apply(types, 1, function (x) which(x==2))
    cas_partn  <- apply(types, 1, function (x) which(x==1))

  # 
  # individual-level events
  crn_sympt_pr   <- sample(1:nrow(crn_main), 2000, replace = TRUE) 
  crn_clear_pr   <- sample(1:nrow(crn_main), 2000, replace = TRUE) 
  crn_screen_pr  <- sample(1:nrow(crn_main), 2000, replace = TRUE) 
  crn_sympt_test <- sample(1:nrow(crn_main), 2000, replace = TRUE) 
  crn_pn_partn_main   <- sample(1:nrow(crn_main), 2000, replace = TRUE)  
  crn_inst_pr <-  sample(1:nrow(crn_main), 2000, replace = TRUE)  
  crn_instinf_pr <-  sample(1:nrow(crn_main), 2000, replace = TRUE)  
  # tie-level events (assumes max 100 of certain type of partner!)
  crn_inf_cas  <- matrix(sample(1:nrow(crn_main), 130*2000, replace = TRUE), nrow=2000, byrow = TRUE)
  crn_inf_main <- matrix(sample(1:nrow(crn_main), 130*2000, replace = TRUE), nrow=2000, byrow = TRUE)

  
  for (i in 2:(params[["simlength"]] +1)){

    for (m in 1:params[["popsize"]]){
      
      # infection acquisition
      if ( inf_state[m,i-1]==0 ) {
    
        
        # tie level CRN used /// 
        # for each person, take the number of ties needed (number of partners had)
        # this changes the m x p matrix of row numbers to p matrix with CRN representing the partners of m
        # if no partners, the crn_main[1,] is selected, but there will not be any infected partners so there will not be infection
       cas_crn_id <- crn_inf_cas[m, 1:length(cas_partn[[m]])]
       main_crn_id <- crn_inf_main[m, 1:length(main_partn[[m]])]
        
        # partners are infected and transmission occurs
        cas_inf  <- (inf_state[cas_partn[[m]],i-1]>0)==TRUE & (crn_main[cas_crn_id,i] < params[["trnsm_pr_cas"]])==TRUE
        main_inf <- (inf_state[main_partn[[m]],i-1]>0)==TRUE & (crn_main[main_crn_id,i] < params[["trnsm_pr_main"]])==TRUE

        # here prevalence among those with instantaneous partners
        ifelse(inst_type[m] == 1, 
               inst_inf_pr <-  1-((1-sum(inf_state[inst_type==1,i-1]>0)/sum(inst_type==1))+sum(inf_state[inst_type==1,i-1]>0)/sum(inst_type==1)*(1-params[["trnsm_pr_inst"]]))^params[["inst_acts"]], 
               inst_inf_pr <- 0)
        
        inst_inf <- ifelse(crn_main[crn_inst_pr[m],i]< params[["act_pr_inst"]] & crn_main[crn_instinf_pr[m],i]< inst_inf_pr, 1, 0)
        
        trnsm_event <- ifelse( any(cas_inf) | any(main_inf) | inst_inf>0, 1, 0)
      
        new_inf <- ifelse(trnsm_event==1, ifelse(crn_main[crn_sympt_pr[m],i]< params[["sympt_pr"]], 2, 1), 0)
        
        
        # if (is.na(new_inf)){
        #   break
        # }
        
        inf_state[m,i] <- new_inf
        incidence[m,i] <- new_inf
        
        # infection clearance  
      }else{
        # symptomatic
        if ( inf_state[m,i-1]==2 ) {
          
          clearance[m,i] <-  ifelse(crn_main[crn_clear_pr[m],i]< params[["clear_pr"]], 1, 0)
          diagn[m,i]     <-  ifelse(crn_main[crn_sympt_pr[m],i]< params[["sympt_test_pr"]], 2, 0)
          
          inf_state[m,i] <- ifelse(clearance[m,i]>0 | diagn[m,i]>0 , 0, inf_state[m,i-1])
        }
        
        # asymptomatic
        if ( inf_state[m,i-1]==1 ) {
          
          clearance[m,i] <- ifelse(crn_main[crn_clear_pr[m],i] < params[["clear_pr"]], 1, 0)
          diagn[m,i]     <- ifelse(crn_main[crn_screen_pr[m],i] < params[["screen_pr"]], 1, 0)
          
          inf_state[m,i] <- ifelse(clearance[m,i]>0 | diagn[m,i]>0 , 0, inf_state[m,i-1])
        }
        
        # partner notification
        if (diagn[m,i] >0 &  params[["pn_pr_main"]]>0 & sum(which(types[m,]==2))>0 ) {
          
          # intervention
          ifelse(i>209, params[["pn_pr_main"]] <- params_pn, params[["pn_pr_main"]])
          
          pn_index[m,i] <- ifelse( sum(pn_index)/(sum(diagn==1)+sum(diagn==2)+0.0000001) < params[["pn_pr_main"]], 1, 0)
         
          if (  pn_index[m,i] > 0) {

            # which partner is selected, defined based on number of main partners and CRN probability 
             partn_select_pr <- rep(crn_main[crn_pn_partn_main[m],i], length(main_partn[[m]])) # length of intervals
             partn_lgth_pr   <- seq(0, 1, by =  1/length(main_partn[[m]])) # this is +1 longer than number of partners
             partn_select    <- between(partn_select_pr, partn_lgth_pr[-length(partn_lgth_pr)], partn_lgth_pr[-1])
             which_partners[m,i] <-  main_partn[[m]][partn_select]
            # 
            # if the notified partner is infected, they are treated
            pn_succ <- ifelse(inf_state[which_partners[m,i],i-1]>0, 3, -3)
            # update infection state of the notified partner
            inf_state[which_partners[m,i],i] <- ifelse(pn_succ == 3, 0, inf_state[which_partners[m,i],i-1])
            # record the notified partner was cured via PN
            diagn[which_partners[m,i],i] <- ifelse(pn_succ == 3 & diagn[which_partners[m,i],i]==0, 3, diagn[which_partners[m,i],i-1])
            
          }
        }
      }
    }
    pn_coverage[i] <- sum( sum(pn_index)/(sum(diagn==1)+sum(diagn==2)), na.rm=T)
  }
 
  return(list(inf = inf_state, inc = incidence, clear = clearance, diagn = diagn,  pn_partners = which_partners, pn_index=pn_index,  pn_coverage= pn_coverage,
              crn_sympt_pr= crn_sympt_pr, crn_clear_pr=crn_clear_pr, crn_screen_pr=crn_screen_pr, crn_sympt_test= crn_sympt_test,
              crn_pn_partn_main=crn_pn_partn_main, crn_inst_pr=crn_inst_pr, crn_instinf_pr=crn_instinf_pr,
              crn_inf_cas=crn_inf_cas, crn_inf_main=crn_inf_main )) 
}

########################################################################################################################
########################################################################################################################


sims_pn_crn <- function(init_inf, types, inst_type, params, params_pn, crn_main, crn_sympt_pr, crn_clear_pr, crn_screen_pr, crn_sympt_test, crn_pn_partn_main, crn_inst_pr, crn_instinf_pr, crn_inf_cas, crn_inf_main) {
  
  inf_state <- incidence <- clearance <- diagn <- pn_index <- which_partners <- inf_partners <- infstat_partners <- main_partn <- matrix(0, dim(init_inf)[1], params[["simlength"]] +1)
  inf_state[,1] <- init_inf[,1]
  pn_coverage <-  vector()
  
  # list index cases' partner types
  main_partn <- apply(types, 1, function (x) which(x==2))
  cas_partn  <- apply(types, 1, function (x) which(x==1))

  for (i in 2:(params[["simlength"]] +1)){
    for (m in 1:params[["popsize"]]){
      
      inf_partners[m,i] <- sum(inf_state[types[m,]>0,i-1])
      
      # infection acquisition
      if ( inf_state[m,i-1]==0 ) {
        
        # tie level CRN used /// 
        # for each person, take the number of ties needed (number of partners had)
        # this changes the m x p matrix of row numbers to p matrix with CRN representing the partners of m
        # if no partners, the crn_main[1,] is selected, but there will not be any infected partners so there will not be infection
        cas_crn_id <- crn_inf_cas[m, 1:length(cas_partn[[m]])]
        main_crn_id <- crn_inf_main[m, 1:length(main_partn[[m]])]
        
        # partners are infected and transmission occurs
        cas_inf  <- (inf_state[cas_partn[[m]],i-1]>0)==TRUE & (crn_main[cas_crn_id,i] < params[["trnsm_pr_cas"]])==TRUE
        main_inf <- (inf_state[main_partn[[m]],i-1]>0)==TRUE & (crn_main[main_crn_id,i] < params[["trnsm_pr_main"]])==TRUE
        
        # here prevalence among those with instantaneous partners
        ifelse(inst_type[m] == 1, 
               inst_inf_pr <- 1-((1-sum(inf_state[inst_type==1,i-1]>0)/sum(inst_type==1))+sum(inf_state[inst_type==1,i-1]>0)/sum(inst_type==1)*(1-params[["trnsm_pr_inst"]]))^params[["inst_acts"]], 
               inst_inf_pr <- 0)
        
        inst_inf <- ifelse(crn_main[crn_inst_pr[m],i]< params[["act_pr_inst"]] & crn_main[crn_instinf_pr[m],i]< inst_inf_pr, 1, 0)
        
        trnsm_event <- ifelse( any(cas_inf) | any(main_inf) | inst_inf>0, 1, 0)
        
        new_inf <- ifelse(trnsm_event==1, ifelse(crn_main[crn_sympt_pr[m],i]< params[["sympt_pr"]], 2, 1), 0)
        inf_state[m,i] <- new_inf
        incidence[m,i] <- new_inf
        
        # infection clearance  
      }else{
        # symptomatic
        if ( inf_state[m,i-1]==2 ) {
          
          clearance[m,i] <-  ifelse(crn_main[crn_clear_pr[m],i]< params[["clear_pr"]], 1, 0)
          diagn[m,i]     <-  ifelse(crn_main[crn_sympt_pr[m],i]< params[["sympt_test_pr"]], 2, 0)
          
          inf_state[m,i] <- ifelse(clearance[m,i]>0 | diagn[m,i]>0 , 0, inf_state[m,i-1])
        }
        
        # asymptomatic
        if ( inf_state[m,i-1]==1 ) {
          
          clearance[m,i] <- ifelse(crn_main[crn_clear_pr[m],i] < params[["clear_pr"]], 1, 0)
          diagn[m,i]     <- ifelse(crn_main[crn_screen_pr[m],i] < params[["screen_pr"]], 1, 0)
          
          inf_state[m,i] <- ifelse(clearance[m,i]>0 | diagn[m,i]>0 , 0, inf_state[m,i-1])
        }
        
        # partner notification -- happens on the same timestep as diagnosis
        if (diagn[m,i] >0 &  params[["pn_pr_main"]]>0 & sum(which(types[m,]==2))>0 ) {
          
          # intervention
          ifelse(i>209, params[["pn_pr_main"]] <- params_pn, params[["pn_pr_main"]])
          
          pn_index[m,i] <- ifelse( sum(pn_index)/(sum(diagn==1)+sum(diagn==2)+0.0000001) < params[["pn_pr_main"]], 1, 0)
         
          if (  pn_index[m,i] > 0) {
            
            # which partner is selected, defined based on number of main partners and CRN probability 
            partn_select_pr <- rep(crn_main[crn_pn_partn_main[m],i], length(main_partn[[m]])) # length of intervals
            partn_lgth_pr   <- seq(0, 1, by =  1/length(main_partn[[m]])) # this is +1 longer than number of partners
            partn_select    <- between(partn_select_pr, partn_lgth_pr[-length(partn_lgth_pr)], partn_lgth_pr[-1])
            which_partners[m,i] <-  main_partn[[m]][partn_select]
            # 
            # if the notified partner is infected, they are treated
            pn_succ <- ifelse(inf_state[which_partners[m,i],i-1]>0, 3, -3)
            infstat_partners[m,i] <-  pn_succ # PN infection status by index case
            # update infection state of the notified partner
            inf_state[which_partners[m,i],i] <- ifelse(pn_succ == 3, 0, inf_state[which_partners[m,i],i-1])
            # record the notified partner was cured via PN
            diagn[which_partners[m,i],i] <- ifelse(pn_succ == 3 & diagn[which_partners[m,i],i]==0, 3, diagn[which_partners[m,i],i-1])
            
          }
        }
      }
    }
   pn_coverage[i] <- sum( sum(pn_index)/(sum(diagn==1)+sum(diagn==2)), na.rm=T)
  }
  
  return(list(inf = inf_state, inc = incidence, clear = clearance, diagn = diagn,  
              pn_partners = which_partners, pn_index=pn_index,  pn_coverage= pn_coverage, infstat_partners=infstat_partners, inf_partners=inf_partners,
              crn_sympt_pr= crn_sympt_pr, crn_clear_pr=crn_clear_pr, crn_screen_pr=crn_screen_pr, crn_sympt_test= crn_sympt_test,
              crn_pn_partn_main=crn_pn_partn_main, crn_inst_pr=crn_inst_pr, crn_instinf_pr=crn_instinf_pr,
              crn_inf_cas=crn_inf_cas, crn_inf_main=crn_inf_main )) 
}


########################################################################################################################




sims_pn_instalt_crn <- function(init_inf, types, inst_type, params, params_pn, crn_main, crn_sympt_pr, crn_clear_pr, crn_screen_pr, crn_sympt_test, crn_pn_partn_main, crn_inst_pr, crn_instinf_pr, crn_inf_cas, crn_inf_main) {
  
  inf_state <- incidence <- clearance <- diagn <- pn_index <- which_partners <- main_partn <- matrix(0, dim(init_inf)[1], params[["simlength"]] +1)
  inf_state[,1] <- init_inf[,1]
  pn_coverage <-  vector()
  
  # list index cases' partner types
  main_partn <- apply(types, 1, function (x) which(x==2))
  cas_partn  <- apply(types, 1, function (x) which(x==1))
  
  
  for (i in 2:(params[["simlength"]] +1)){
    for (m in 1:params[["popsize"]]){
      
      # infection acquisition
      if ( inf_state[m,i-1]==0 ) {
        
        # tie level CRN used /// 
        # for each person, take the number of ties needed (number of partners had)
        # this changes the m x p matrix of row numbers to p matrix with CRN representing the partners of m
        # if no partners, the crn_main[1,] is selected, but there will not be any infected partners so there will not be infection
        cas_crn_id <- crn_inf_cas[m, 1:length(cas_partn[[m]])]
        main_crn_id <- crn_inf_main[m, 1:length(main_partn[[m]])]
        
        # partners are infected and transmission occurs
        cas_inf  <- (inf_state[cas_partn[[m]],i-1]>0)==TRUE & (crn_main[cas_crn_id,i] < params[["trnsm_pr_cas"]])==TRUE
        main_inf <- (inf_state[main_partn[[m]],i-1]>0)==TRUE & (crn_main[main_crn_id,i] < params[["trnsm_pr_main"]])==TRUE
        
        # here prevalence among those with instantaneous partners
        ifelse(inst_type[m] == 1, 
               inst_inf_pr <- ifelse(sample(inf_state[inst_type==1,i-1], 1)>0, 1, 0),
               inst_inf_pr <- 0)
        
        inst_inf <- ifelse(crn_main[crn_inst_pr[m],i]< params[["act_pr_inst"]] & inst_inf_pr==1 & crn_main[crn_instinf_pr[m],i]< 1-(1-params[["trnsm_pr_inst"]])^params[["inst_acts"]], 1, 0)
        
        trnsm_event <- ifelse( any(cas_inf) | any(main_inf) | inst_inf>0, 1, 0)
        
        new_inf <- ifelse(trnsm_event==1, ifelse(crn_main[crn_sympt_pr[m],i]< params[["sympt_pr"]], 2, 1), 0)
        inf_state[m,i] <- new_inf
        incidence[m,i] <- new_inf
        
        # infection clearance  
      }else{
        # symptomatic
        if ( inf_state[m,i-1]==2 ) {
          
          clearance[m,i] <-  ifelse(crn_main[crn_clear_pr[m],i]< params[["clear_pr"]], 1, 0)
          diagn[m,i]     <-  ifelse(crn_main[crn_sympt_pr[m],i]< params[["sympt_test_pr"]], 2, 0)
          
          inf_state[m,i] <- ifelse(clearance[m,i]>0 | diagn[m,i]>0 , 0, inf_state[m,i-1])
        }
        
        # asymptomatic
        if ( inf_state[m,i-1]==1 ) {
          
          clearance[m,i] <- ifelse(crn_main[crn_clear_pr[m],i] < params[["clear_pr"]], 1, 0)
          diagn[m,i]     <- ifelse(crn_main[crn_screen_pr[m],i] < params[["screen_pr"]], 1, 0)
          
          inf_state[m,i] <- ifelse(clearance[m,i]>0 | diagn[m,i]>0 , 0, inf_state[m,i-1])
        }
        
        # partner notification
        if (diagn[m,i] >0 &  params[["pn_pr_main"]]>0 & sum(which(types[m,]==2))>0 ) {
          
          # intervention
          ifelse(i>209, params[["pn_pr_main"]] <- params_pn, params[["pn_pr_main"]])
          
          pn_index[m,i] <- ifelse( sum(pn_index)/(sum(diagn==1)+sum(diagn==2)+0.0000001) < params[["pn_pr_main"]], 1, 0)
          
          if (  pn_index[m,i] > 0) {

            # which partner is selected, defined based on number of main partners and CRN probability 
            partn_select_pr <- rep(crn_main[crn_pn_partn_main[m],i], length(main_partn[[m]])) # length of intervals
            partn_lgth_pr   <- seq(0, 1, by =  1/length(main_partn[[m]])) # this is +1 longer than number of partners
            partn_select    <- between(partn_select_pr, partn_lgth_pr[-length(partn_lgth_pr)], partn_lgth_pr[-1])
            which_partners[m,i] <-  main_partn[[m]][partn_select]
            # 
            # if the notified partner is infected, they are treated
            pn_succ <- ifelse(inf_state[which_partners[m,i],i-1]>0, 3, -3)
            # update infection state of the notified partner
            inf_state[which_partners[m,i],i] <- ifelse(pn_succ == 3, 0, inf_state[which_partners[m,i],i-1])
            # record the notified partner was cured via PN
            diagn[which_partners[m,i],i] <- ifelse(pn_succ == 3 & diagn[which_partners[m,i],i]==0, 3, diagn[which_partners[m,i],i-1])
            
          }
        }
      }
    }
    pn_coverage[i] <- sum( sum(pn_index)/(sum(diagn==1)+sum(diagn==2)), na.rm=T)
  }
  
  return(list(inf = inf_state, inc = incidence, clear = clearance, diagn = diagn,  pn_partners = which_partners, pn_index=pn_index, pn_coverage,
              crn_sympt_pr= crn_sympt_pr, crn_clear_pr=crn_clear_pr, crn_screen_pr=crn_screen_pr, crn_sympt_test= crn_sympt_test,
              crn_pn_partn_main=crn_pn_partn_main, crn_inst_pr=crn_inst_pr, crn_instinf_pr=crn_instinf_pr,
              crn_inf_cas=crn_inf_cas, crn_inf_main=crn_inf_main )) 
}


########################################################################################################################


