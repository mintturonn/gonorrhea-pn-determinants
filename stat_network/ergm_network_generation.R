
library(here)
library(statnet)

# helper function
nw_description <- function(parts_temp){
  hasmain <- mainpartner <- caspartner <- partnquants <-  numeric(0)
  
  for(i in 1:10){
    mainpartner <- rbind(mainpartner, sum(rowSums(parts_temp[[i]]==2)) )
    caspartner <- rbind(caspartner, sum(rowSums(parts_temp[[i]]==1)) )
    hasmain <- rbind(hasmain, mean(ifelse(rowSums(parts_temp[[i]]==2)>0, 1, 0)) )
    partnquants <- rbind(partnquants,  quantile(rowSums(parts_temp[[i]]>0), probs=seq(0,1, by=0.1) ))
  }
  
  pships <- mainpartner/(mainpartner+caspartner) 
  
  rbind( c(mean(hasmain), min(hasmain), max(hasmain)),  c(mean(pships), min(pships), max(pships))) %>%
    as_tibble() %>%
    mutate(type = c("has main", "prop of partnerships")) %>%
    ggplot() +
    geom_bar(aes(x=type, y=V1), stat="identity", fill="skyblue", alpha=0.7) +
    geom_errorbar(aes(x= type, ymin=V2, ymax=V3), width=0.2, colour="navy", alpha=0.9, size=1.3) +
    #geom_hline(yintercept=0.65, color = "gray40", linetype="dashed") +
    ylim(c(0,1)) + theme_bw() + ylab("") + xlab("") -> p0
  
  partnquants %>%
    as_tibble() %>%
    mutate(id = 1:nrow(.)) %>%
    melt(id="id") %>%
    ggplot(aes(x=variable, y = value, group = id)) +
    geom_line(size=0.1, color="blue") + 
    theme_bw() + ylab("partner number (log scale)") + xlab("quantile") +
    scale_y_continuous(trans='log2', breaks = c(1, 5, 10,  50,  100)) ->p1
  
  p2 <- gplot(parts_temp[[5]]) 
  
  return(list(quants = partnquants, p0=p0, p1=p1, p2=p2))
  
}    

############################################################################################################################
############################################################################################################################
############################################################################################################################

tr.pop.size <- 2000 * 0.1
hr.pop.size <- 2000 * 0.2
mr.pop.size <- 2000 * 0.2
lr.pop.size <- 2000 * 0.5

risk <- c(rep(0, round(tr.pop.size)), rep(1, round(hr.pop.size)),rep(2, round(mr.pop.size)), rep(3, round(lr.pop.size)))

# data frame
df_pop <- data_frame( risk = risk)
## this is shuffle the risk groups more evenly (given the way PN is implemented)
df_sufflepop <- df_pop[sample(nrow(df_pop)),]

nw <- network_initialize(2000)

# all infor is at nodes, and setting the network based on egocentric data
nw <- network::set.vertex.attribute(nw, "risk", df_sufflepop$risk)

############################################################################################################################
############################################################################################################################
############################################################################################################################


# ergm model -- low degree
# risk categories are not used
regnw <- ergm(nw ~ edges, target.stats = c(450))
casnw <- ergm(nw ~ edges, target.stats = c(740))

# simulated networks from ergm model
rx <- simulate(regnw, nsim = 10)
cx <- simulate(casnw, nsim = 10)

parts_temp <- vector("list", 10)

for (i in 1:10){
  
  contacts.rx <- network::as.matrix.network(rx[[i]], unit="edges")
  
  # ID for regular partners is 2
  contacts.rx[contacts.rx==1] <- 2
  contacts.cx <- network::as.matrix.network(cx[[i]], unit="edges")
  
  parts_temp[[i]] <- contacts.rx + contacts.cx
  parts_temp[[i]][parts_temp[[i]]==3] <- 1
  # sum(parts_temp==3)
}


# gplot(parts_temp[[1]])

ld_res <- nw_description(parts_temp)

parts_low <- parts_temp

filename <- paste0("data/parts_low", "_", Sys.Date(), ".RData")

save(parts_low, file = here(filename))

############################################################################################################################
############################################################################################################################
############################################################################################################################

# ergm model high degree
regnw <- ergm(nw ~ edges+ nodefactor("risk"), target.stats = c(2490,	697,	588,	1078))  #control = control.ergm(SAN.maxit = 10, SAN.nsteps.times =16 ) )
casnw <- ergm(nw ~ edges+ nodefactor("risk"), target.stats = c(3320,	1627,	588,	660	))

# simulated network from ergm model
rx <- simulate(regnw, nsim = 10)
cx <- simulate(casnw, nsim = 10)

# summary(rx)

parts_temp <- vector("list", 10)

for (i in 1:10){
  
    contacts.rx <- network::as.matrix.network(rx[[i]], unit="edges")
    
    # ID for regular partners is 2
    contacts.rx[contacts.rx==1] <- 2
    contacts.cx <- network::as.matrix.network(cx[[i]], unit="edges")
    
    parts_temp[[i]] <- contacts.rx + contacts.cx
    parts_temp[[i]][parts_temp[[i]]==3] <- 1
    # sum(parts_temp==3)
}


# gplot(parts_temp[[1]])

hd_res <- nw_description(parts_temp)

parts_high <- parts_temp

filename <- paste0("data/parts_high", "_", Sys.Date(), ".RData")

save(parts_high, file = here(filename))


############################################################################################################################
############################################################################################################################
############################################################################################################################



