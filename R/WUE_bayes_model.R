# script for running the WUE and d13 models selected (orignially in RWI_models.Rmd)

# saveRDS(full.iso, 'data/full_WUE_dataset_v2.rds')
# saveRDS(train.iso, 'data/train_WUE_dataset_v2.rds')
# saveRDS(test.iso, 'data/test_WUE_dataset_v2.rds')

full.iso %>% group_by( site, ageclass, ID) %>% summarise(n = n(), WUE.mean = mean(iWUE, na.rm=TRUE))
full.iso %>% group_by(ageclass) %>% summarise(n = n(), WUE.mean = mean(iWUE, na.rm=TRUE))
train.iso %>% group_by(age.code) %>% summarise(n = n(), WUE.mean = mean(iWUE, na.rm=TRUE))


iWUE_MAP_tmax_dbh_re <- "model{

# for each the overall population include re for sites:

# Likelihood
for(i in 1:n){
# process model for iWUE:
iWUE[i]   ~ dnorm(iWUEfunc[i], inv.var) # where Yi is already log transformed

# function g()
iWUEfunc[i] <- beta1[struct.cohort[i]] + beta2[struct.cohort[i]]*MAP_scaled[i] + beta3[struct.cohort[i]]*TMAX_scaled[i] + beta4[struct.cohort[i]]*DBH_scaled[i]+beta5[site[i]]

}


# Assume normal priors for betas, but generate a beta + alpha for each ageclass
for(s in 1:length(SF)){
beta1[s] ~ dnorm(mu_beta1, inv_beta1)
beta2[s] ~ dnorm(mu_beta2, inv_beta2)
beta3[s] ~ dnorm(mu_beta3, inv_beta3)
beta4[s] ~ dnorm(mu_beta4, inv_beta4)

}

for(ns in 1:4){
beta5[ns] ~ dnorm(mu_beta5, inv_beta5)
}


# use normal hyperpriors for each hyperparamters 

mu_beta1 ~ dnorm(0, 0.1)
mu_beta2 ~ dnorm(0, 0.1)
mu_beta3 ~ dnorm(0, 0.1)
mu_beta4 ~ dnorm(0, 0.1)
mu_beta5 ~ dnorm(0, 0.1)


inv_beta1   ~ dgamma(0.01, 0.01)
sigma_beta1 <- 1/sqrt(inv_beta1)

inv_beta2   ~ dgamma(0.01, 0.01)
sigma_beta2 <- 1/sqrt(inv_beta2)

inv_beta3   ~ dgamma(0.01, 0.01)
sigma_beta3 <- 1/sqrt(inv_beta3)

inv_beta4   ~ dgamma(0.01, 0.01)
sigma_beta4 <- 1/sqrt(inv_beta4)

inv_beta5   ~ dgamma(0.01, 0.01)
sigma_beta5 <- 1/sqrt(inv_beta5)

# Non-informative Prior for the inverse population variances

inv.var   ~ dgamma(0.001, 0.001)
sigma     <- 1/sqrt(inv.var)


# # Predict test data
for(i in 1:np){
 # process model for iWUE:
 iWUE.p[i]   ~ dnorm(iWUEfunc.p[i], inv.var) # where Yi is already log transformed
 
 # function g()
 iWUEfunc.p[i] <- beta1[struct.cohort.p[i]] + beta2[struct.cohort.p[i]]*MAP_scaled.p[i]+ beta3[struct.cohort.p[i]]*TMAX_scaled.p[i]+ beta4[struct.cohort.p[i]]*DBH_scaled.p[i]+beta5[site.p[i]]

}


# # Probe values
# for(i in 1:nprobe){
# # process model for iWUE:
# iWUE.probe[i]   ~ dnorm(iWUEfunc.probe[i], inv.var) # where Yi is already log transformed
# 
# # function g()
# iWUEfunc.probe[i] <- beta1[struct.cohort.probe[i]] + beta2[struct.cohort.probe[i]]*MAP_scaled.probe[i]+ beta3[struct.cohort.probe[i]]*TMAX_scaled.probe[i]+ beta4[struct.cohort.probe[i]]*DBH_scaled.probe[i]

#}

}"


# create probe to generate posterior predictions:
MAP.probe <- seq(range(full.iso$MAP.scaled)[1], range(full.iso$MAP.scaled)[2], by = 1)
T.probe <- seq(range(full.iso$T.scaled)[1], range(full.iso$T.scaled)[2], by = 1)
DBH.probe <- seq(range(full.iso$DBH.scaled)[1], range(full.iso$T.scaled)[2], by = 1)

iWUEprobe <- expand.grid(MAP.probe = MAP.probe, T.probe = T.probe, DBH.probe = DBH.probe, age.probe = 1:2)

if(!"age.code" %in% colnames(full.iso)){
  train.iso$age.code <- ifelse(train.iso$ageclass %in% "Modern", 1, 
                               ifelse(train.iso$ageclass %in% "Past",2, NA))
  test.iso$age.code <- ifelse(test.iso$ageclass %in% "Modern", 1,  
                              ifelse(test.iso$ageclass %in% "Past",2, NA))
  full.iso$age.code <- ifelse(full.iso$ageclass %in% "Modern", 1, 
                              ifelse(full.iso$ageclass %in% "Past",2, NA))
}


site.cd.df <- data.frame(site.code = 1:4, 
                         site = c("BON", "GLA", "GLL2", "MOU"))

if(!"site.code" %in% colnames(full.iso)){
  train.iso <- merge(train.iso, site.cd.df, by = "site")
  test.iso <- merge(test.iso, site.cd.df, by = "site")
  full.iso <- merge(full.iso, site.cd.df, by = "site")
  }



iWUE_map_tmax <- jags.model(textConnection(iWUE_MAP_tmax_dbh_re), 
                            data = list(iWUE = train.iso$iWUE, n=length(train.iso$iWUE), 
                                        struct.cohort = as.numeric(train.iso$age.code), 
                                        SF = unique(train.iso$age.code),
                                        MAP_scaled = train.iso$MAP.scaled, TMAX_scaled = train.iso$T.scaled, 
                                        DBH_scaled = train.iso$DBH.scaled, site = train.iso$site.code,
                                        np = length( full.iso$site), 
                                        struct.cohort.p = as.numeric( full.iso$age.code), 
                                        MAP_scaled.p =  full.iso$MAP.scaled, TMAX_scaled.p =  full.iso$T.scaled, 
                                        DBH_scaled.p =  full.iso$DBH.scaled, site.p = full.iso$site.code), n.chains = 3, n.adapt = 100) 
                                        #nprobe = length(iWUEprobe$MAP.probe),
                                        #MAP_scaled.probe = iWUEprobe$MAP.probe,
                                        #TMAX_scaled.probe = iWUEprobe$T.probe, 
                                        #DBH_scaled.probe = iWUEprobe$DBH.probe,
                                        #struct.cohort.probe = iWUEprobe$age.probe), n.chains = 3, n.adapt = 100)

update(iWUE_map_tmax, 1000); # Burnin for 1000 samples to start, then go higher later


iWUE_map_tmax.re <- coda.samples(iWUE_map_tmax, 
                                 variable.names=c("beta1", "beta2","beta3","beta4","mu_beta1","mu_beta2","mu_beta3","mu_beta4", "iWUE.p"), 
                                 n.chains = 3, n.iter = 20000, thin = 15)



samps       <- iWUE_map_tmax.re [[1]]
saveRDS(samps, "outputs/growth_model/iWUE_MAP_TMAX_dbh_cohort/samps.iWUE_v2.rds")
#Yp.samps <- samps[,1:660] # one alpha for each of 16 sites
#iWUEpred <- samps
alpha.samps  <- samps[,1:2]
beta.samps  <- samps[,3:4]
beta2.samps  <- samps[,5:6]
beta3.samps  <- samps[,7:8]
params <- samps[,1:8]
saveRDS(params, "outputs/growth_model/iWUE_MAP_TMAX_dbh_cohort/params.iWUE_ageclass_v2.rds")
iWUE.samps  <- samps[,9:(8+length(full.iso$iWUE))]


hdi( (( alpha.samps[,1]-alpha.samps[,2] )/alpha.samps[,2])*100)
alpha.diff <- ((alpha.samps[,1]-alpha.samps[,2] )/alpha.samps[,2])*100

wue.pct.diff <- data.frame(
  pct.change = mean(alpha.diff),
  Ci.low = quantile(alpha.diff, 0.025), 
  Ci.high = quantile(alpha.diff, 0.975), 
  median = median(alpha.diff), 
  hdi.low = hdi(alpha.diff)[1], 
  hdi.high = hdi(alpha.diff)[2])


Yp.samps <- data.frame(iWUE.samps) 
Yp.m <- melt(Yp.samps)
Yp.summary <- Yp.m %>% group_by(variable) %>% dplyr::summarise(Predicted = mean(value),
                                                               ci.hi = quantile(value,0.975),
                                                               ci.lo = quantile(value,0.025))
Yp.summary$Observed <- full.iso$iWUE
Yp.summary$ageclass <- full.iso$ageclass


pred.obs <- summary(lm(colMeans(Yp.samps)~ full.iso$iWUE))

# this does a poor job representing iWUE values by itself, but explains som of the variation
p.o.plot <- ggplot(Yp.summary, aes(Observed, Predicted))+geom_point(color = "black", size = 0.5)+geom_errorbar(data = Yp.summary,aes(ymin=ci.lo, ymax=ci.hi), color = "grey", alpha = 0.5)+geom_point(data = Yp.summary, aes(Observed, Predicted), color = "black", size = 0.5)+geom_abline(aes(slope = 1, intercept = 0), color = "red", linetype = "dashed")+geom_text(data=data.frame(pred.obs$r.squared), aes( label = paste("R^2: ", pred.obs$r.squared, sep="")),parse=T,x=120, y=175)

# note fairly poor model fit
png(width = 6, height = 5, units = "in", res = 300, "outputs/growth_model/iWUE_MAP_TMAX_dbh/pred_vs_obs_v2.png")
p.o.plot
dev.off()

Predicted.growth <- Yp.summary %>% group_by(ageclass) %>% summarise(mean = mean(Predicted, na.rm = TRUE), 
                                                          Ci.low = quantile(Predicted,0.025, na.rm = TRUE),
                                                          Ci.high = quantile(Predicted,0.975, na.rm = TRUE), 
                                                          hdi.low = hdi(Predicted, na.rm = TRUE)[1],
                                                          hdi.high = hdi(Predicted,na.rm = TRUE)[2])


ggplot(Predicted.growth , aes(ageclass, y = mean))+geom_bar(stat = "identity")+geom_errorbar()
# ----------------- Plot average predicted difference in iWUE between ageclasses--------------------
Yp.samps <- data.frame(iWUE.samps) 
Yp.m <- melt(Yp.samps)

obs.wue <- full.iso
obs.wue$num <- 1:length(Yp.samps)
obs.wue$variable <- paste0("iWUE.p.", obs.wue$num, ".")

obs.preds <- left_join(obs.wue, Yp.m, by = "variable")
WUE.age <- obs.preds %>% group_by(ageclass) %>% summarise(mean = mean(value, na.rm = TRUE), 
                                                          Ci.low = quantile(value,0.025, na.rm = TRUE),
                                                          Ci.high = quantile(value,0.975, na.rm = TRUE), 
                                                          hdi.low = hdi(value, na.rm = TRUE)[1],
                                                          hdi.high = hdi(value,na.rm = TRUE)[2])


# save obs.wue for use later:

#saveRDS(obs.wue, "outputs/growth_model/iWUE_MAP_TMAX_dbh/WUE_preds.rds")

#--------------------fit the model for d13C--is basically the same model as for iWUE, but without site intercept---------------------
d13_MAP_tmax_dbh_re <- "model{

# for each the overall population include re for sites:

# Likelihood
for(i in 1:n){
# process model for iWUE:
iWUE[i]   ~ dnorm(iWUEfunc[i], inv.var) # where Yi is already log transformed

# function g()
iWUEfunc[i] <- beta1[struct.cohort[i]] + beta2[struct.cohort[i]]*MAP_scaled[i] + beta3[struct.cohort[i]]*TMAX_scaled[i] + beta4[struct.cohort[i]]*DBH_scaled[i]+beta5[site[i]]

}


# Assume normal priors for betas, but generate a beta + alpha for each ageclass
for(s in 1:length(SF)){
beta1[s] ~ dnorm(mu_beta1, inv_beta1)
beta2[s] ~ dnorm(mu_beta2, inv_beta2)
beta3[s] ~ dnorm(mu_beta3, inv_beta3)
beta4[s] ~ dnorm(mu_beta4, inv_beta4)

}

for(ns in 1:4){
 beta5[ns] ~ dnorm(mu_beta5, inv_beta5)
}


# use normal hyperpriors for each hyperparamters 

mu_beta1 ~ dnorm(0, 0.1)
mu_beta2 ~ dnorm(0, 0.1)
mu_beta3 ~ dnorm(0, 0.1)
mu_beta4 ~ dnorm(0, 0.1)
mu_beta5 ~ dnorm(0, 0.1)


inv_beta1   ~ dgamma(0.01, 0.01)
sigma_beta1 <- 1/sqrt(inv_beta1)

inv_beta2   ~ dgamma(0.01, 0.01)
sigma_beta2 <- 1/sqrt(inv_beta2)

inv_beta3   ~ dgamma(0.01, 0.01)
sigma_beta3 <- 1/sqrt(inv_beta3)

inv_beta4   ~ dgamma(0.01, 0.01)
sigma_beta4 <- 1/sqrt(inv_beta4)

 inv_beta5   ~ dgamma(0.01, 0.01)
 sigma_beta5 <- 1/sqrt(inv_beta5)

# Non-informative Prior for the inverse population variances

inv.var   ~ dgamma(0.001, 0.001)
sigma     <- 1/sqrt(inv.var)


# # Predict test data
for(i in 1:np){
 # process model for iWUE:
 iWUE.p[i]   ~ dnorm(iWUEfunc.p[i], inv.var) # where Yi is already log transformed
 
 # function g()
 iWUEfunc.p[i] <- beta1[struct.cohort.p[i]] + beta2[struct.cohort.p[i]]*MAP_scaled.p[i]+ beta3[struct.cohort.p[i]]*TMAX_scaled.p[i]+ beta4[struct.cohort.p[i]]*DBH_scaled.p[i]+beta5[site.p[i]]

}


# # Probe values
# for(i in 1:nprobe){
# # process model for iWUE:
# iWUE.probe[i]   ~ dnorm(iWUEfunc.probe[i], inv.var) # where Yi is already log transformed
# 
# # function g()
# iWUEfunc.probe[i] <- beta1[struct.cohort.probe[i]] + beta2[struct.cohort.probe[i]]*MAP_scaled.probe[i]+ beta3[struct.cohort.probe[i]]*TMAX_scaled.probe[i]+ beta4[struct.cohort.probe[i]]*DBH_scaled.probe[i]

#}

}"



d13_map_tmax <- jags.model(textConnection(d13_MAP_tmax_dbh_re), 
                            data = list(iWUE =train.iso$Cor.d13C.suess, n=length(train.iso$Cor.d13C.suess), 
                                        struct.cohort = as.numeric(train.iso$age.code), 
                                        SF = unique(train.iso$age.code),
                                        MAP_scaled = train.iso$MAP.scaled, TMAX_scaled = train.iso$T.scaled, 
                                        DBH_scaled = train.iso$DBH.scaled, site = train.iso$site.code,
                                        np = length(test.iso$site), 
                                        struct.cohort.p = as.numeric( test.iso$age.code), 
                                        MAP_scaled.p =  test.iso$MAP.scaled, TMAX_scaled.p =  test.iso$T.scaled, 
                                        DBH_scaled.p =  test.iso$DBH.scaled, site.p = test.iso$site.code), n.chains = 3, n.adapt = 100) 
#nprobe = length(iWUEprobe$MAP.probe),
#MAP_scaled.probe = iWUEprobe$MAP.probe,
#TMAX_scaled.probe = iWUEprobe$T.probe, 
#DBH_scaled.probe = iWUEprobe$DBH.probe,
#struct.cohort.probe = iWUEprobe$age.probe), n.chains = 3, n.adapt = 100)

update(d13_map_tmax, 1000); # Burnin for 1000 samples to start, then go higher later


d13C_map_tmax.re <- coda.samples(d13_map_tmax, 
                                 variable.names=c("beta1", "beta2","beta3","beta4","mu_beta1","mu_beta2","mu_beta3","mu_beta4", "iWUE.p"), 
                                 n.chains = 3, n.iter = 20000, thin = 15)


d13samps       <- d13C_map_tmax.re [[1]]
saveRDS(d13samps, "outputs/growth_model/iWUE_MAP_TMAX_dbh_cohort/samps.d13_v2.rds")
#Yp.samps <- samps[,1:660] # one alpha for each of 16 sites
#iWUEpred <- samps
alpha.samps  <- d13samps[,1:2]
beta.samps  <- d13samps[,3:4]
beta2.samps  <- d13samps[,5:6]
beta3.samps  <- d13samps[,7:8]
params <- d13samps[,1:8]
saveRDS(params, "outputs/growth_model/iWUE_MAP_TMAX_dbh_cohort/params.d13_ageclass_v2.rds")
d13.samps  <- d13samps[,9:(8+length(test.iso$Cor.d13C.suess))]


hdi( (( alpha.samps[,1]-alpha.samps[,2] )/alpha.samps[,2])*100)
alpha.diff <- ((alpha.samps[,1]-alpha.samps[,2] )/alpha.samps[,2])*100

d13.pct.diff <- data.frame(
  pct.change = mean(alpha.diff),
  Ci.low = quantile(alpha.diff, 0.025), 
  Ci.high = quantile(alpha.diff, 0.975), 
  median = median(alpha.diff), 
  hdi.low = hdi(alpha.diff)[1], 
  hdi.high = hdi(alpha.diff)[2])


Yp.samps <- data.frame(d13.samps) 
Yp.m <- melt(Yp.samps)
Yp.summary <- Yp.m %>% group_by(variable) %>% dplyr::summarise(Predicted = mean(value),
                                                               ci.hi = quantile(value,0.975),
                                                               ci.lo = quantile(value,0.025))
Yp.summary$Observed <- test.iso$Cor.d13C.suess
Yp.summary$ageclass <- test.iso$ageclass


pred.obs <- summary(lm(colMeans(Yp.samps)~ test.iso$Cor.d13C.suess))

# this does a poor job representing iWUE values by itself, but explains som of the variation
p.o.plot <- ggplot(Yp.summary, aes(Observed, Predicted))+geom_point(color = "black", size = 0.5)+geom_errorbar(data = Yp.summary,aes(ymin=ci.lo, ymax=ci.hi), color = "grey", alpha = 0.5)+geom_point(data = Yp.summary, aes(Observed, Predicted), color = "black", size = 0.5)+geom_abline(aes(slope = 1, intercept = 0), color = "red", linetype = "dashed")+geom_text(data=data.frame(pred.obs$r.squared), aes( label = paste("R^2: ", pred.obs$r.squared, sep="")),parse=T,x=120, y=175)

# note fairly poor model fit
png(width = 6, height = 5, units = "in", res = 300, "outputs/growth_model/iWUE_MAP_TMAX_dbh/pred_vs_obs_d13C_v2.png")
p.o.plot
dev.off()

