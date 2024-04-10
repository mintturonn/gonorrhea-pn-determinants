

box1 <- function(dat){
  
  rbind(dat) %>%
    as_tibble() %>%
    melt(id="V4") %>%
    mutate(scenario = ifelse(variable=="V1", "PN 20%", ifelse(variable=="V2", "PN 40%", "PN 60%"))) %>%
    mutate(change = 100*as.numeric(value)) %>%
    ggplot(aes(x=scenario, y=change, fill=scenario)) +
    geom_hline(yintercept=0, color = "black") + # ylim(c(-70, 70)) +
    geom_boxplot() +
    theme_bw() + ylab("Relative change (%)") + xlab("Baseline prevalence") 
  
}

############################################################################
#  Individual-level data

avind_simul <- function(dat, id0, baseinf0, degr0){
  
dat %>%
  as_tibble() %>% 
  mutate(av10 = V2-V1,
         av20 = V3-V1,
         av40 = V4-V1) %>%  
  mutate(part_cat = ifelse(V5==0, "0", 
                           ifelse(V5>0 & V5<3, "1-2", 
                                  ifelse(V5>2 & V5<6, "3-5", 
                                         ifelse(V5>5, "6+", NA))))) %>%
  group_by(V5) %>%
  summarize(
   base_mean = mean(V1), 
    i10_mean = mean(V2), 
    i20_mean = mean(V3), 
    i40_mean = mean(V4), 
    base_sum = sum(V1), 
     i10_sum = sum(V2), 
     i20_sum = sum(V3), 
     i40_sum = sum(V4), 
    av10_sum = sum(av10, na.rm=T),
    av20_sum = sum(av20, na.rm=T),
    av40_sum = sum(av40, na.rm=T),
    pnum = n() ) %>%
  ungroup() %>%
  mutate(
         avtot_10 =  sum(av10_sum),
         avtot_20 =  sum(av20_sum),
         avtot_40 =  sum(av40_sum)) %>%
  mutate(id = id0,
         binc = baseinf0,
         degree = degr0,
         base_rate = base_sum/pnum, 
         i10_rate  = i10_sum/pnum, 
         i20_rate  = i20_sum/pnum, 
         i40_rate  = i40_sum/pnum)  -> avinf
  
  return(avinf)
  
}

#############################################################################

yearfun_pRR <- function (dat, scen, run_id){
  
  dat %>%
    as_tibble() %>%
    mutate(run_id = run_id)  %>%
    group_by(run_id) %>%
    summarize(yr5 = mean(RR_1), 
              yr6 = mean(RR_2),
              yr7 = mean(RR_3),
              yr8 = mean(RR_4)) %>%
    ungroup() %>%
    mutate(scenario = scen) %>%
    select(-run_id)-> dat0
  
  return(dat0)
}

yearfun_p <- function (dat, scen, run_id){
  
  dat %>%
    as_tibble() %>%
    mutate(run_id = run_id)  %>%
    group_by(run_id) %>%
    summarize(yr5 = mean(V261-V209), 
              yr6 = mean(V313-V261),
              yr7 = mean(V365-V313),
              yr8 = mean(V417-V365)) %>%
    ungroup() %>%
    mutate(scenario = scen)  %>%
    select(-run_id)-> dat0
  
  return(dat0)
}


yearfun_s <- function (dat, scen){
  
  dat %>%
    as_tibble() %>% 
    rowwise() %>%
    summarize(yr5 = mean(V261-V209), 
              yr6 = mean(V313-V261),
              yr7 = mean(V365-V313),
              yr8 = mean(V417-V365)) %>%
    mutate(scenario = scen) %>%
    select(yr5, yr6, yr7, yr8, scenario) -> dat0
  
  return(dat0)
}

yearfun_sp <- function (dat, scen, run_id){ # average for every simulation and then for every parameter set
  
  # average prevalence over 26 weeks
  dat %>%
    as_tibble() %>%
    mutate(run_id = run_id)  %>%
    rowwise() %>%
    mutate(   yr5 = mean(V261-V210), 
              yr6 = mean(V313-V262),
              yr7 = mean(V365-V314),
              yr8 = mean(V417-V366)) %>%
    group_by(run_id) %>%
    summarize(YR5 = mean(yr5),
              YR6 = mean(yr6),
              YR7 = mean(yr7),
              YR8 = mean(yr8)) %>%
    ungroup() %>%
    mutate(scenario = scen) %>%
    select(-run_id) -> dat0
  
  return(dat0)
  
}

###############################################################

yearfun2 <- function (dat, scen, run_id){
  
  dat %>%
    as_tibble() %>%
    mutate(run_id = run_id)  %>%
    group_by(run_id) %>%
    summarize(yr4 = mean(mean(V209)),
              yr5 = mean(mean(V261)), 
              yr6 = mean(mean(V313)),
              yr7 = mean(mean(V365)),
              yr8 = mean(mean(V417))) %>%
    ungroup() %>%
    mutate(scenario = scen) %>%
    select(-run_id) -> dat0
  
  return(dat0)
  
}

yearfun2_sp <- function (dat, scen, run_id){ # average for every simulation and then for every parameter set
  
  # average prevalence over 26 weeks
  dat %>%
    as_tibble() %>%
    mutate(run_id = run_id)  %>%
    rowwise() %>%
    mutate(   yr5 = mean(c_across(V236:V261)), 
              yr6 = mean(c_across(V288:V313)), 
              yr7 = mean(c_across(V340:V365)),
              yr8 = mean(c_across(V392:V417))) %>%
    group_by(run_id) %>%
    summarize(YR5 = mean(yr5),
              YR6 = mean(yr6),
              YR7 = mean(yr7),
              YR8 = mean(yr8)) %>%
    ungroup() %>%
    mutate(scenario = scen) %>%
    select(-run_id) -> dat0
  
  return(dat0)
  
}

yearfun2_s <- function (dat, scen, run_id){ # average for every simulation
  
  dat %>%
    as_tibble() %>%
    rowwise() %>%
    mutate(   yr5 = mean(c_across(V236:V261)), 
              yr6 = mean(c_across(V288:V313)), 
              yr7 = mean(c_across(V340:V365)),
              yr8 = mean(c_across(V392:V417))) %>%
    mutate(scenario = scen) %>%
    select(yr5, yr6, yr7, yr8, scenario) -> dat0
  
  return(dat0)
  
}

yearfun3 <- function(dat, run_id){
  
  dat %>%
    as_tibble() %>%
    mutate(run_id = run_id)  %>%
    group_by(run_id) %>%
    summarize(yr8 = mean(V207)/4) %>%
    ungroup() %>%
    # mutate(scenario = scen) %>%
    select(-run_id) -> dat0
  
  return(dat0)
  
}

##############################################################
indfun <- function (dat, scen, run_id, parts){
  
  inf <-  matrix(NA, nrow(parts), 2)
  pop <-  matrix(NA, nrow(parts), 2)
  
  for (i in 1:nrow(parts)){
    inf[i,1] <- sum(dat[i, parts[i,]==1])
    inf[i,2] <- sum(dat[i, parts[i,]==0])
    
    pop[i,1] <- sum(parts[i,]==1)
    pop[i,2] <- sum(parts[i,]==0)
    
  }
  
  inf %>%
    as_tibble() %>%
    mutate(run_id = run_id)  %>%
    group_by(run_id) %>%
    summarize(high_mean = mean(V1),
              low_mean = mean(V2)) %>%
    ungroup() %>%
    mutate(scenario = scen) %>%
    select(-run_id) -> dat0
  
  pop %>%
    as_tibble() %>%
    mutate(run_id = run_id)  %>%
    group_by(run_id) %>%
    summarize(high_mean = mean(V1),
              low_mean = mean(V2)) %>%
    ungroup() %>%
    mutate(scenario = scen) %>%
    select(-run_id) -> dat1
  
  return(list(inf=dat0, pop=dat1))
  
}

# 
# yearfun2 <- function (dat, scen, run_id){
#   
#   dat %>%
#     as_tibble() %>%
#     mutate(run_id = run_id)  %>%
#     group_by(run_id) %>%
#     summarize(yr8 = mean(mean(V417))) %>%
#     ungroup() %>%
#     mutate(scenario = scen) %>%
#     select(-run_id) -> dat0
#   
#   return(dat0)
#   
# }

# popprev2 <- cbind(yearfun2(base$scr_pr, "basecase",1), yearfun2(inter60$scr_pr, "PN:60%",2))
# 
# popprev_crn <- mean((inter60_crn$scr_pr[,417]-base_crn$scr_pr[,417])/base_crn$scr_pr[,417], na.rm=T)
# 
# prinstalt_crn <- rbind(inter60_crn_2$scr_pr[,417], inter60_instalt_crn$scr_pr[,417])
# 
# 
# (popprev2[,3]-popprev2[,1])/popprev2[,1]
# 
# cumincid60 <- (inter60_crn$scr_inc[,417]-base_crn$scr_inc[,417])/base_crn$scr_inc[,417]
# cumincid300 <- (interX3_crn$scr_inc[,417]-base_crn$scr_inc[,417])/base_crn$scr_inc[,417]
# 
# pn_succ <- (inter60_crn$pn_index2[,207]-base_crn$pn_index2[,207])/base_crn$pn_index2[,207]
# 
# 
# act_inf_red <- rbind( cbind( (act_inf[,4]-act_inf[,1])/act_inf[,1] , (act_inf[,7]-act_inf[,1])/act_inf[,1], (act_inf[,10]-act_inf[,1])/act_inf[,1], rep(">4 partners", 15) ),
#                       cbind( (act_inf[,5]-act_inf[,2])/act_inf[,2] , (act_inf[,8]-act_inf[,2])/act_inf[,2], (act_inf[,11]-act_inf[,2])/act_inf[,2], rep("1-3 partners", 15) ))
# 
# prev_red_8yr <- cbind( (popprev2[,11]-popprev2[,5])/popprev2[,5], (popprev2[,17]-popprev2[,5])/popprev2[,5], (popprev2[,23]-popprev2[,5])/popprev2[,5], rep("4yrs", 15) )
# prev_red_6yr <- cbind( (popprev2[,9]-popprev2[,3])/popprev2[,3], (popprev2[,15]-popprev2[,3])/popprev2[,3], (popprev2[,21]-popprev2[,3])/popprev2[,3], rep("2yrs", 15) )
# 
# inc_red_8yr <- cbind( (popcuminc[,3]-popcuminc[,1])/popcuminc[,1], (popcuminc[,5]-popcuminc[,1])/popcuminc[,1], (popcuminc[,7]-popcuminc[,1])/popcuminc[,1])
# 
# 
# rbind(prev_red_6yr, prev_red_8yr) %>%
#   as_tibble() %>%
#   melt(id="V4") %>%
#   mutate(scenario = ifelse(variable=="V1", "PN 20%", ifelse(variable=="V2", "PN 40%", "PN 60%"))) %>%
#   mutate(time_horizon = V4) %>%
#   mutate(change = 100*as.numeric(value)) %>%
#   ggplot() +
#   geom_hline(yintercept=0, color = "black") +
#   geom_boxplot(aes(x=scenario, y=change, fill=time_horizon)) + theme_bw() + ylab("Relative change (%)") + xlab("PN increase") 
