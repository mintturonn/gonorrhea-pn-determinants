
gen.params <- function(dat){
###################################
# initialize the model

params <- list(
  timehorizon = dat$fixed[dat$params=="years"],  # years
  timestep = dat$fixed[dat$params=="timestep"],   # weeks 
  popsize = dat$fixed[dat$params=="popsize"],
  gc_prev = dat$fixed[dat$params=="gc_prev_init"],
  sympt_pr = rbeta(1, dat$shape1[dat$params=="sympt_pr"], dat$shape2[dat$params=="sympt_pr"]),
  sympt_test_pr = rbeta(1, dat$shape1[dat$params=="sympt_test_pr"], dat$shape2[dat$params=="sympt_test_pr"]),
  trnsm_pr_cas = rbeta(1, dat$shape1[dat$params=="trnsm_pr_cas"], dat$shape2[dat$params=="trnsm_pr_cas"]),
  trnsm_pr_inst = rbeta(1, dat$shape1[dat$params=="trnsm_pr_inst"], dat$shape2[dat$params=="trnsm_pr_inst"]),
  trnsm_pr_rr = 1+rgamma(1, dat$shape1[dat$params=="trnsm_pr_main"], dat$shape2[dat$params=="trnsm_pr_main"]),
  clear_pr = rbeta(1, dat$shape1[dat$params=="clear_pr"], dat$shape2[dat$params=="clear_pr"]),
  screen_pr = rbeta(1, dat$shape1[dat$params=="screen_pr"], dat$shape2[dat$params=="screen_pr"]),
  pn_pr_interv = rbeta(1, dat$shape1[dat$params=="index_interview"], dat$shape2[dat$params=="index_interview"]),
  pn_pr_reach = rbeta(1, dat$shape1[dat$params=="partner_reached"], dat$shape2[dat$params=="partner_reached"]),
  act_pr_inst = rbeta(1, dat$shape1[dat$params=="act_pr_inst"], dat$shape2[dat$params=="act_pr_inst"]),
  inst_acts = 1+rpois(1, dat$shape2[dat$params=="inst_acts"])
  
)

inst_type <- rbinom(params[["popsize"]], 1, rbeta(1, dat$shape1[dat$params=="pr_inst_type"], dat$shape2[dat$params=="pr_inst_type"]))

params$simlength <-  params$timehorizon*params$timestep
params$trnsm_pr_main <- params$trnsm_pr_rr * params$trnsm_pr_cas
params$pn_pr_main <- params$pn_pr_interv * params$pn_pr_reach 

init_inf <- matrix(0, params$popsize, params$simlength+1)

rand_id1 <- sample(1:params[["popsize"]], params[["popsize"]]*params[["gc_prev"]]*(1-params[["sympt_pr"]]), replace = FALSE)
rand_id2 <- sample(1:params[["popsize"]], params[["popsize"]]*params[["gc_prev"]]*params[["sympt_pr"]], replace = FALSE)

init_inf[rand_id1,1] <- 1
init_inf[rand_id2,1] <- 2

id <- paste("M", 1:params$popsize, sep="")
rownames(init_inf) <- id
colnames(init_inf) <- paste("w", 0:params$simlength, sep ="")

 return(list(params = params, init_inf=init_inf, inst_type=inst_type))
}


eval.fit <- function(scr_pr, scr_inc, scr_diag, scr_s, params){
  
 params0 <- params
  
 t <-  seq(1, length(scr_pr[1,]), by = (params0$timestep-1))
 tend <- length(t)
 
 prev <-  rowMeans(scr_pr[,(t[tend-1]+1):t[tend]])/params0$popsize
 inc.rate <-  (scr_inc[,t[tend]]-scr_inc[,t[tend-1]])/params0$popsize
 diag.rate <-  (scr_diag[,t[tend]]-scr_diag[,t[tend-1]])/params0$popsize
 prop.sympt <- (scr_s[,t[tend]]-scr_s[,t[tend-1]])/(scr_diag[,t[tend]]-scr_diag[,t[tend-1]]+0.01)

 pr <-  mean(prev, na.rm=T) > 0.02 & mean(prev, na.rm=T) < 0.1
 ic <-  mean(inc.rate, na.rm=T) > 0.01 & mean(inc.rate, na.rm=T) < 0.23
 di <- mean(diag.rate, na.rm=T) > 0.01 & mean(diag.rate, na.rm=T) < 0.10
 sy <- mean( prop.sympt, na.rm=T) > 0.3 & mean(prop.sympt, na.rm=T) < 0.8

 totals <- tryCatch( {  pr == T &  ic == T & di == T & sy == T },warning= function(w) { totals <-  FALSE }, error=function(e) { totals <-  FALSE }) 

 if (totals==T) {res = TRUE} else {res = FALSE}  
 
  return(list(res=res, p=pr, i=ic, d=di, s=sy, 
              prev=mean(prev), 
              inc.rate=mean(inc.rate), 
              diag.rate=mean(diag.rate), 
              prop.sympt = mean(prop.sympt)) )
}


compile_run <- function(scr_pr0, scr_inc0, scr_diag0, scr_s0, params0, id){ #pn_index, pn_partner,  
 
  t <-  seq(1, length(scr_pr0[1,]), by = params0$timestep)
  tend <- length(t)
  prev <-  scr_pr0
  totlgt <- length(prev[,1])
  inc.rate <- diag.rate <- prop.sympt <- matrix(NA, nrow(prev), length(t)-1)

  # simulation level yearly results
  for (j in 1: totlgt){
      for (i in 1:(tend-1)){
      inc.rate[j,i] <-  (scr_inc0[j,t[i+1]]-scr_inc0[j,t[i]])/params0$popsize
      diag.rate[j,i] <-  (scr_diag0[j,t[i+1]]-scr_diag0[j,t[i]])/params0$popsize
      prop.sympt[j,i] <- (scr_s0[j,t[i+1]]-scr_s0[j,t[i]])/(scr_diag0[j,t[i+1]]-scr_diag0[j,t[i]])
      }
  }
  
  cbind(prev, id) %>%
    as_tibble() %>%
    group_by(id ) %>%
    summarise(across(everything(), ~ mean(., na.rm = TRUE))) %>%
    ungroup() %>%
    select(-id) -> prev_m
  
  cbind(inc.rate, id) %>%
    as_tibble() %>%
    group_by(id ) %>%
    summarise(across(everything(), ~ mean(., na.rm = TRUE))) %>%
    ungroup() %>%
    select(-id) -> inc.rate_m
  
  cbind(diag.rate, id) %>%
    as_tibble() %>%
    group_by(id ) %>%
    summarise(across(everything(), ~ mean(., na.rm = TRUE))) %>%
    ungroup() %>%
    select(-id) -> diag.rate_m
  
  cbind(prop.sympt, id) %>%
    as_tibble() %>%
    group_by(id ) %>%
    summarise(across(everything(), ~ mean(., na.rm = TRUE))) %>%
    ungroup() %>%
    select(-id) -> prop.sympt_m
  
  return(list(prev        = prev, 
              inc.rate    = inc.rate, 
              diag.rate   = diag.rate,
              prop.sympt  = prop.sympt,
              prev_m      = prev_m, 
              inc.rate_m  = inc.rate_m, 
              diag.rate_m = diag.rate_m,
              prop.sympt_m= prop.sympt_m))  
  
}

gen.crn <- function(crn_main){
  
  crn_sympt_pr <- crn_clear_pr <- crn_screen_pr <- crn_sympt_test <-  crn_pn_partn_main <-  crn_inst_pr <- crn_instinf_pr <- numeric(0)
  crn_inf_cas <- crn_inf_main <-  vector("list", 70)
  
for (i in 1:(30*83)){  
  # individual-level events
  crn_sympt_pr   <- rbind(crn_sympt_pr, sample(1:nrow(crn_main), 2000, replace = TRUE))
  crn_clear_pr   <- rbind(crn_clear_pr, sample(1:nrow(crn_main), 2000, replace = TRUE)) 
  crn_screen_pr  <- rbind(crn_screen_pr, sample(1:nrow(crn_main), 2000, replace = TRUE)) 
  crn_sympt_test <- rbind(crn_sympt_test, sample(1:nrow(crn_main), 2000, replace = TRUE)) 
  crn_pn_partn_main   <- rbind(crn_pn_partn_main, sample(1:nrow(crn_main), 2000, replace = TRUE))  
  crn_inst_pr <-  rbind(crn_inst_pr, sample(1:nrow(crn_main), 2000, replace = TRUE))  
  crn_instinf_pr <-  rbind(crn_instinf_pr, sample(1:nrow(crn_main), 2000, replace = TRUE))  
  # tie-level events (assumes max 130 of certain type of partner!)
  crn_inf_cas[[i]]  <- matrix(sample(1:nrow(crn_main), 130*2000, replace = TRUE), nrow=2000, byrow = TRUE)
  crn_inf_main[[i]] <- matrix(sample(1:nrow(crn_main), 130*2000, replace = TRUE), nrow=2000, byrow = TRUE)
  
}
  
  return(list(crn_sympt_pr=crn_sympt_pr,
              crn_clear_pr=crn_clear_pr,
              crn_screen_pr=crn_screen_pr,
              crn_sympt_test= crn_sympt_test,
              crn_pn_partn_main=crn_pn_partn_main,
              crn_inst_pr=crn_inst_pr,
              crn_instinf_pr=crn_instinf_pr,
              crn_inf_cas=crn_inf_cas,
              crn_inf_main=crn_inf_main ))
  
}

