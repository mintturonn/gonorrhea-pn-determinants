

sims_pn_nocrn <- function(init_inf, types, inst_type, params, params_pn) {
  
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
        
        # partners are infected and transmission occurs
        cas_inf  <- (inf_state[cas_partn[[m]],i-1]>0)==TRUE &  (rbinom(length(inf_state[cas_partn[[m]],i-1]) ,1, params[["trnsm_pr_cas"]]))==TRUE
        main_inf <- (inf_state[main_partn[[m]],i-1]>0)==TRUE & (rbinom(length(inf_state[main_partn[[m]],i-1]) ,1, params[["trnsm_pr_main"]]))==TRUE 

        # here prevalence among those with instantaneous partners
        ifelse(inst_type[m] == 1, 
               inst_inf_pr <-  1-((1-sum(inf_state[inst_type==1,i-1]>0)/sum(inst_type==1))+sum(inf_state[inst_type==1,i-1]>0)/sum(inst_type==1)*(1-params[["trnsm_pr_inst"]]))^params[["inst_acts"]],  
               inst_inf_pr <- 0)
        
        
        inst_inf <- ifelse(rbinom(1,1, params[["act_pr_inst"]])==1 & rbinom(1,1, inst_inf_pr), 1, 0)
        
        trnsm_event <- ifelse( any(cas_inf) | any(main_inf) | inst_inf>0, 1, 0)
            
        new_inf <- ifelse(trnsm_event==1, ifelse(rbinom(1,1, params[["sympt_pr"]])==1, 2, 1), 0)
        inf_state[m,i] <- new_inf
        incidence[m,i] <- new_inf
        
        # infection clearance  
      }else{
        # symptomatic
        if ( inf_state[m,i-1]==2 ) {
          
          clearance[m,i] <-  rbinom(1,1, params[["clear_pr"]])
          diagn[m,i]     <-  ifelse(rbinom(1,1, params[["sympt_test_pr"]])==1, 2, 0) 
          
          inf_state[m,i] <- ifelse(clearance[m,i]>0 | diagn[m,i]>0 , 0, inf_state[m,i-1])
        }
        
        # asymptomatic
        if ( inf_state[m,i-1]==1 ) {
          
          clearance[m,i] <- rbinom(1,1, params[["clear_pr"]])
          diagn[m,i]     <- ifelse(rbinom(1,1, params[["screen_pr"]])==1, 1, 0)  
          
          inf_state[m,i] <- ifelse(clearance[m,i]>0 | diagn[m,i]>0 , 0, inf_state[m,i-1])
        }
        
        # partner notification
        if (diagn[m,i] >0 &  params[["pn_pr_main"]]>0 & sum(which(types[m,]==2))>0 ) {
          
          # intervention
          ifelse(i>209, params[["pn_pr_main"]] <- params_pn, params[["pn_pr_main"]])
          
          pn_index[m,i] <- ifelse( sum(pn_index)/(sum(diagn==1)+sum(diagn==2)+0.0000001) < params[["pn_pr_main"]], 1, 0)
          
          if (  pn_index[m,i] > 0) {
            
            # which partner is selected
            partv <- which(types[m,]==2)
            # how many partners notified (n=1
            which_partners[m,i] <- sample(partv, 1, replace = FALSE)
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
 
  return(list(inf = inf_state, inc = incidence, clear = clearance, diagn = diagn,  pn_partners = which_partners, pn_index=pn_index, pn_coverage=pn_coverage))
}



