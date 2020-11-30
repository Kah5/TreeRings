# script for running the WUE and d13 models selected (orignially in RWI_models.Rmd)
# Author: Kelly A. Heilman
# Last Updated: March 25, 2020
# note: Need to run clean_separate_data.R before running this script, this loads required packages and required datasets

full.iso <-readRDS( 'data/full_WUE_struct_cohort_scaled_dataset_v4.rds')
train.iso <- readRDS('data/train_WUE_struct_cohort_scaled_dataset_v4.rds')
test.iso <- readRDS('data/test_WUE_struct_cohort_scaled_dataset_v4.rds')

full.iso[full.iso$Cor.d13C.suess <= -21 & full.iso$Cor.d13C.suess >= -27.5, ] %>% group_by( site, ageclass) %>% dplyr::summarise(n = n(), WUE.mean = mean(iWUE, na.rm=TRUE), WUE.lo = quantile(iWUE, 0.025, na.rm=TRUE),WUE.hi = quantile(iWUE, 0.975, na.rm = TRUE))

iso.summary <- full.iso %>% group_by( site, ageclass) %>% dplyr::summarise(n = n(), WUE.mean = mean(iWUE, na.rm=TRUE), WUE.lo = quantile(iWUE, 0.025, na.rm=TRUE),WUE.hi = quantile(iWUE, 0.975, na.rm = TRUE))

ggplot(iso.summary, aes(site, WUE.mean ,color = ageclass))+geom_point() + geom_errorbar(aes(ymin = WUE.lo, ymax = WUE.hi))
ggplot(full.iso, aes(site, iWUE ,color = ageclass))+geom_boxplot()

full.iso%>% group_by(ageclass, structure) %>% dplyr::summarise(n = n(), WUE.mean = mean(iWUE, na.rm=TRUE), d13 = mean(Cor.d13C.suess))

#full.iso <- full.iso[full.iso$Cor.d13C.suess < -21.9, ]
#test.iso <- test.iso[test.iso$Cor.d13C.suess < -21.9, ]
#train.iso <- train.iso[train.iso$Cor.d13C.suess < -21.9 , ]


unique(full.iso$site)

iWUE_MAP_tmax_dbh_re <- "model{

# for each the overall population include re for sites:

# Likelihood
for(i in 1:n){
# process model for iWUE:
iWUE[i]   ~ dnorm(iWUEfunc[i], inv.var) # where Yi is already log transformed

# function g()
iWUEfunc[i] <- beta1[struct.cohort[i]] + beta2[struct.cohort[i]]*MAP_scaled[i] + beta3[struct.cohort[i]]*TMAX_scaled[i] + beta4[struct.cohort[i]]*DBH_scaled[i]

}


# Assume normal priors for betas, but generate a beta + alpha for each ageclass
for(s in 1:length(SF)){
beta1[s] ~ dnorm(mu_beta1, inv_beta1)
beta2[s] ~ dnorm(mu_beta2, inv_beta2)
beta3[s] ~ dnorm(mu_beta3, inv_beta3)
beta4[s] ~ dnorm(mu_beta4, inv_beta4)

}

#for(ns in 1:5){
#beta5[ns] ~ dnorm(mu_beta5, inv_beta5)
#}


# use normal hyperpriors for each hyperparamters 

mu_beta1 ~ dnorm(0, 0.1)
mu_beta2 ~ dnorm(0, 0.1)
mu_beta3 ~ dnorm(0, 0.1)
mu_beta4 ~ dnorm(0, 0.1)
#mu_beta5 ~ dnorm(0, 0.1)


inv_beta1   ~ dgamma(0.01, 0.01)
sigma_beta1 <- 1/sqrt(inv_beta1)

inv_beta2   ~ dgamma(0.01, 0.01)
sigma_beta2 <- 1/sqrt(inv_beta2)

inv_beta3   ~ dgamma(0.01, 0.01)
sigma_beta3 <- 1/sqrt(inv_beta3)

inv_beta4   ~ dgamma(0.01, 0.01)
sigma_beta4 <- 1/sqrt(inv_beta4)

# inv_beta5   ~ dgamma(0.01, 0.01)
# sigma_beta5 <- 1/sqrt(inv_beta5)

# Non-informative Prior for the inverse population variances

inv.var   ~ dgamma(0.001, 0.001)
sigma     <- 1/sqrt(inv.var)


# # Predict test data
for(i in 1:np){
 # process model for iWUE:
 iWUE.p[i]   ~ dnorm(iWUEfunc.p[i], inv.var) # where Yi is already log transformed
 
 # function g()
 iWUEfunc.p[i] <- beta1[struct.cohort.p[i]] + beta2[struct.cohort.p[i]]*MAP_scaled.p[i]+ beta3[struct.cohort.p[i]]*TMAX_scaled.p[i]+ beta4[struct.cohort.p[i]]*DBH_scaled.p[i]

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

if(!"age.code" %in% colnames(full.iso)){
  train.iso$age.code <- ifelse(train.iso$ageclass %in% "Modern", 1, 
                               ifelse(train.iso$ageclass %in% "Past",2, NA))
  test.iso$age.code <- ifelse(test.iso$ageclass %in% "Modern", 1,  
                              ifelse(test.iso$ageclass %in% "Past",2, NA))
  full.iso$age.code <- ifelse(full.iso$ageclass %in% "Modern", 1, 
                              ifelse(full.iso$ageclass %in% "Past",2, NA))
}

site.cd.df <- data.frame(site.code = 1:5, 
                         site = c( "GLA", "GLL2", "MOU", "UNC", "BON"))

if(!"site.code" %in% colnames(full.iso)){
  train.iso <- merge(train.iso, site.cd.df, by = "site")
  test.iso <- merge(test.iso, site.cd.df, by = "site")
  full.iso <- merge(full.iso, site.cd.df, by = "site")
  }


saveRDS(full.iso, "outputs/growth_model/iWUE_MAP_TMAX_dbh_cohort/full.iso_struct_cohort_scaled_dataset_v4.rds")
saveRDS(test.iso, "outputs/growth_model/iWUE_MAP_TMAX_dbh_cohort/test.iso_struct_cohort_scaled_dataset_v4.rds")
saveRDS(train.iso, "outputs/growth_model/iWUE_MAP_TMAX_dbh_cohort/train.iso_struct_cohort_scaled_dataset_v4.rds")

# first lets run the WUE model where we have random effects for structure-cohort class:
# note that this model does well explaining the data, and acco

iWUE_map_tmax <- jags.model(textConnection(iWUE_MAP_tmax_dbh_re), 
                            data = list(iWUE = train.iso$iWUE, n=length(train.iso$iWUE), 
                                        struct.cohort = as.numeric(train.iso$struct.cohort.code), 
                                        SF = unique(train.iso$struct.cohort.code),
                                        MAP_scaled = train.iso$MAP.scaled, TMAX_scaled = train.iso$T.scaled, 
                                        DBH_scaled = train.iso$DBH.scaled, site = train.iso$site.code,
                                        np = length( full.iso$site), 
                                        struct.cohort.p = as.numeric( full.iso$struct.cohort.code), 
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

iWUE_map_tmax.re <- coda.samples(iWUE_map_tmax, 
                                 variable.names=c("beta1", "beta2","beta3","beta4","mu_beta1","mu_beta2","mu_beta3","mu_beta4", "iWUE.p"), 
                                 n.chains = 3, n.iter = 180000, thin = 15)
iWUE_map_tmax.re <- coda.samples(iWUE_map_tmax, 
                                 variable.names=c("beta1", "beta2","beta3","beta4","mu_beta1","mu_beta2","mu_beta3","mu_beta4", "iWUE.p"), 
                                 n.chains = 3, n.iter = 180000, thin = 15)

# kept updating to get to 770995 iterations
par(mfrow = c(3,4))
traceplot(iWUE_map_tmax.re[, c("beta1[1]","beta1[2]", "beta1[3]","beta1[4]","beta2[1]", "beta2[2]", "beta3[1]", "beta3[2]", "beta4[1]", "beta4[2]")])

# get gelmen-rubin stats
gelman.diag(iWUE_map_tmax.re[, c("beta1[1]","beta1[2]", "beta1[3]","beta1[4]","beta2[1]", "beta2[2]", "beta3[1]", "beta3[2]", "beta4[1]", "beta4[2]")])
# gelman.diag values all look good <= 1.02


# need to fix this to sample
samps       <- iWUE_map_tmax.re [[3]]
saveRDS(iWUE_map_tmax.re, "outputs/growth_model/iWUE_MAP_TMAX_dbh_cohort/samps.iWUE_struct.cohort.re_struct_cohort_scaled_dataset_v4.rds")

#Yp.samps <- samps[,1:660] # one alpha for each of 16 sites
#iWUEpred <- samps
alpha.samps  <- samps[,1:4]
beta.samps  <- samps[,5:8]
beta2.samps  <- samps[,9:12]
beta3.samps  <- samps[,13:16]
params <- samps[,1:16]
saveRDS(params, "outputs/growth_model/iWUE_MAP_TMAX_dbh_cohort/params.iWUE_struct.cohort_struct_cohort_scaled_dataset_v4.rds")
iWUE.samps  <- samps[,17:(16+length(full.iso$iWUE))]

# for refenence:
# unique(full.iso[, c("struct.cohort", "struct.cohort.code")])
# struct.cohort struct.cohort.code
#   Modern-Savanna               4
#   Past-Savanna                 3
#   Past-Forest                  1
#   Modern-Forest                2

library(HDInterval)
hdi( (( alpha.samps[,1]-alpha.samps[,2] )/alpha.samps[,2])*100)
alpha.diff.forest <- ((alpha.samps[,2]-alpha.samps[,1] )/alpha.samps[,1])*100 # (modern - past)/past * 100 = pct increase

wue.pct.diff.forest <- data.frame(
  structure = "forest",
  pct.change = mean(alpha.diff.forest),
  Ci.low = quantile(alpha.diff.forest, 0.025), 
  Ci.high = quantile(alpha.diff.forest, 0.975), 
  median = median(alpha.diff.forest), 
  hdi.low = hdi(alpha.diff.forest)[1], 
  hdi.high = hdi(alpha.diff.forest)[2])

alpha.diff.savanna <- ((alpha.samps[,4]-alpha.samps[,3] )/alpha.samps[,3])*100

wue.pct.diff.savanna <- data.frame(
  structure = "savanna",
  pct.change = mean(alpha.diff.savanna),
  Ci.low = quantile(alpha.diff.savanna, 0.025), 
  Ci.high = quantile(alpha.diff.savanna, 0.975), 
  median = median(alpha.diff.savanna), 
  hdi.low = hdi(alpha.diff.savanna)[1], 
  hdi.high = hdi(alpha.diff.savanna)[2])

# save the alpha differences:
alpha.pct.diff <- rbind(wue.pct.diff.savanna, wue.pct.diff.forest)
saveRDS(alpha.pct.diff, "outputs/growth_model/iWUE_MAP_TMAX_dbh/WUE_baseline_struct_cohort_differences_struct_cohort_scaled_dataset_v4.rds")

Yp.samps <- data.frame(iWUE.samps) 
Yp.m <- melt(Yp.samps)
Yp.summary <- Yp.m %>% group_by(variable) %>% dplyr::summarise(Predicted = mean(value),
                                                               ci.hi = quantile(value,0.975),
                                                               ci.lo = quantile(value,0.025))
Yp.summary$Observed <- full.iso$iWUE
Yp.summary$ageclass <- full.iso$ageclass
Yp.summary$struct.cohort <- full.iso$struct.cohort
Yp.summary$ind <- full.iso$ID
Yp.summary$site <- full.iso$site

pred.obs <- summary(lm(colMeans(Yp.samps)~ full.iso$iWUE))

# this does a poor job representing iWUE values by itself, but explains som of the variation
p.o.plot <- ggplot(Yp.summary, aes(Observed, Predicted))+geom_point( size = 0.5)+geom_errorbar(data = Yp.summary,aes(ymin=ci.lo, ymax=ci.hi), color = "grey", alpha = 0.5)+geom_point(data = Yp.summary, aes(Observed, Predicted), color = "black", size = 0.5)+geom_abline(aes(slope = 1, intercept = 0), color = "red", linetype = "dashed")+geom_text(data=data.frame(pred.obs$r.squared), aes( label = paste("R^2: ", pred.obs$r.squared, sep="")),parse=T,x=120, y=175)

# note fairly poor model fit
png(width = 6, height = 5, units = "in", res = 300, "outputs/growth_model/iWUE_MAP_TMAX_dbh/pred_vs_obs_struct_cohort_WUE_struct_cohort_scaled_dataset_v4.png")
p.o.plot
dev.off()

Predicted.growth <- Yp.summary %>% group_by(struct.cohort) %>% dplyr::summarise(mean = mean(Predicted, na.rm = TRUE), 
                                                          Ci.low = quantile(Predicted,0.025, na.rm = TRUE),
                                                          Ci.high = quantile(Predicted,0.975, na.rm = TRUE), 
                                                          hdi.low = hdi(Predicted, na.rm = TRUE)[1],
                                                          hdi.high = hdi(Predicted,na.rm = TRUE)[2])

saveRDS(Predicted.growth, "outputs/growth_model/iWUE_MAP_TMAX_dbh/WUE_predicted_struct_cohort_struct_cohort_scaled_dataset_v4.rds")

Predicted.growth[Predicted.growth$struct.cohort %in% "Modern-Savanna",c("mean", "Ci.low", "Ci.high")]-Predicted.growth[Predicted.growth$struct.cohort %in% "Past-Savanna",c("mean", "Ci.low", "Ci.high")]
Predicted.growth[Predicted.growth$struct.cohort %in% "Modern-Forest",c("mean", "Ci.low", "Ci.high")]-Predicted.growth[Predicted.growth$struct.cohort %in% "Past-Forest",c("mean", "Ci.low", "Ci.high")]

#predicted growth for our sites shows that strongest increas in predicted WUE for forests, bute savannas generally have a higher WUE in the past
ggplot(Predicted.growth , aes(struct.cohort, y = mean))+geom_bar(stat = "identity")+geom_errorbar(aes(ymin = Ci.low, ymax = Ci.high,width = 0.1))
# ----------------- Plot average predicted difference in iWUE between ageclasses--------------------
Yp.samps <- data.frame(iWUE.samps) 
Yp.m <- melt(Yp.samps)

obs.wue <- full.iso
obs.wue$num <- 1:length(Yp.samps)
obs.wue$variable <- paste0("iWUE.p.", obs.wue$num, ".")

obs.preds <- left_join(obs.wue, Yp.m, by = "variable")
WUE.age <- obs.preds %>% group_by(ageclass) %>% dplyr::summarise(mean = mean(value, na.rm = TRUE), 
                                                          Ci.low = quantile(value,0.025, na.rm = TRUE),
                                                          Ci.high = quantile(value,0.975, na.rm = TRUE), 
                                                          hdi.low = hdi(value, na.rm = TRUE)[1],
                                                          hdi.high = hdi(value,na.rm = TRUE)[2])


#saveRDS(WUE.age, "outputs/growth_model/iWUE_MAP_TMAX_dbh/WUE_predicted_diffs_struct_cohort_struct_cohort_scaled_dataset_v4.rds")

#------------------------- ageclass only WUE model run ---------------------------------------
# Next lets run the WUE model where we have random effects for just age class:
# note that this model does well explaining the data, and acco

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

iWUE_map_tmax.re <- coda.samples(iWUE_map_tmax, 
                                 variable.names=c("beta1", "beta2","beta3","beta4","mu_beta1","mu_beta2","mu_beta3","mu_beta4", "iWUE.p"), 
                                 n.chains = 3, n.iter = 20000, thin = 15)

iWUE_map_tmax.re <- coda.samples(iWUE_map_tmax, 
                                 variable.names=c("beta1", "beta2","beta3","beta4","mu_beta1","mu_beta2","mu_beta3","mu_beta4", "iWUE.p"), 
                                 n.chains = 3, n.iter = 20000, thin = 15)
iWUE_map_tmax.re <- coda.samples(iWUE_map_tmax, 
                                 variable.names=c("beta1", "beta2","beta3","beta4","mu_beta1","mu_beta2","mu_beta3","mu_beta4", "iWUE.p"), 
                                 n.chains = 3, n.iter = 20000, thin = 15)

#iWUE_map_tmax.re <- coda.samples(iWUE_map_tmax, 
 #                                variable.names=c("beta1", "beta2","beta3","beta4","mu_beta1","mu_beta2","mu_beta3","mu_beta4", "iWUE.p"), 
  #                               n.chains = 3, n.iter = 10000, thin = 15)

# kept updating to get to 770995 iterations
par(mfrow = c(3,4))
traceplot(iWUE_map_tmax.re[, c("beta1[1]","beta1[2]")])
traceplot(iWUE_map_tmax.re[, c("beta1[1]","beta1[2]","beta2[1]", "beta2[2]", "beta3[1]", "beta3[2]", "beta4[1]", "beta4[2]")])
gelman.diag(iWUE_map_tmax.re[, c("beta1[1]","beta1[2]","beta2[1]", "beta2[2]", "beta3[1]", "beta3[2]", "beta4[1]", "beta4[2]")])
# gelman.diag values all look good <= 1.01


# need to fix this to sample
samps       <- iWUE_map_tmax.re [[1]]
saveRDS(iWUE_map_tmax.re, "outputs/growth_model/iWUE_MAP_TMAX_dbh_cohort/samps.iWUE_age.cohort.re_struct_cohort_scaled_dataset_v4.rds")

#Yp.samps <- samps[,1:660] # one alpha for each of 16 sites
#iWUEpred <- samps
alpha.samps  <- samps[,1:2]
beta.samps  <- samps[,3:4]
beta2.samps  <- samps[,5:6]
beta3.samps  <- samps[,7:8]
params <- samps[,1:8]
saveRDS(params, "outputs/growth_model/iWUE_MAP_TMAX_dbh_cohort/params.iWUE_ageclass_struct_cohort_scaled_dataset_v4.rds")
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

saveRDS(wue.pct.diff, "outputs/growth_model/iWUE_MAP_TMAX_dbh/WUE_baseline_cohort_differences_struct_cohort_scaled_dataset_v4.rds")


Yp.samps <- data.frame(iWUE.samps) 
Yp.m <- melt(Yp.samps)
Yp.summary <- Yp.m %>% group_by(variable) %>% dplyr::summarise(Predicted = mean(value),
                                                               ci.hi = quantile(value,0.975),
                                                               ci.lo = quantile(value,0.025))
Yp.summary$Observed <- full.iso$iWUE
Yp.summary$ageclass <- full.iso$ageclass
Yp.summary$struct.cohort <- full.iso$struct.cohort
Yp.summary$site <- full.iso$site
Yp.summary$ID <- full.iso$ID
pred.obs <- summary(lm(colMeans(Yp.samps)~ full.iso$iWUE))

# this does a poor job representing iWUE values by itself, but explains som of the variation
p.o.plot <- ggplot(Yp.summary, aes(Observed, Predicted))+geom_point(color = "black", size = 0.5)+geom_errorbar(data = Yp.summary,aes(ymin=ci.lo, ymax=ci.hi), color = "grey", alpha = 0.5)+geom_point(data = Yp.summary, aes(Observed, Predicted), color = "black", size = 0.5)+geom_abline(aes(slope = 1, intercept = 0), color = "red", linetype = "dashed")+geom_text(data=data.frame(pred.obs$r.squared), aes( label = paste("R^2: ", pred.obs$r.squared, sep="")),parse=T,x=120, y=175)

# note fairly poor model fit
png(width = 6, height = 5, units = "in", res = 300, "outputs/growth_model/iWUE_MAP_TMAX_dbh/pred_vs_obs_ageclass_WUE_struct_cohort_scaled_dataset_v4.png")
p.o.plot
dev.off()

Predicted.growth <- Yp.summary %>% group_by(ageclass) %>% dplyr::summarise(mean = mean(Predicted, na.rm = TRUE), 
                                                                           Ci.low = quantile(Predicted,0.025, na.rm = TRUE),
                                                                           Ci.high = quantile(Predicted,0.975, na.rm = TRUE), 
                                                                           hdi.low = hdi(Predicted, na.rm = TRUE)[1],
                                                                           hdi.high = hdi(Predicted,na.rm = TRUE)[2])


Predicted.growth[Predicted.growth$ageclass %in% "Modern",c("mean", "Ci.low", "Ci.high")]-Predicted.growth[Predicted.growth$ageclass %in% "Past",c("mean", "Ci.low", "Ci.high")]
saveRDS(Predicted.growth, "outputs/growth_model/iWUE_MAP_TMAX_dbh/WUE_predicted_cohort_struct_cohort_scaled_dataset_v4.rds")

ggplot(Predicted.growth , aes(ageclass, y = mean))+geom_bar(stat = "identity")+geom_errorbar(aes(ymin = Ci.low, ymax = Ci.high, width = 0.1))
# ----------------- Plot average predicted difference in iWUE between ageclasses--------------------
Yp.samps <- data.frame(iWUE.samps) 
Yp.m <- melt(Yp.samps)

obs.wue <- full.iso
obs.wue$num <- 1:length(Yp.samps)
obs.wue$variable <- paste0("iWUE.p.", obs.wue$num, ".")

obs.preds <- left_join(obs.wue, Yp.m, by = "variable")
WUE.age <- obs.preds %>% group_by(ageclass) %>% dplyr::summarise(mean = mean(value, na.rm = TRUE), 
                                                          Ci.low = quantile(value,0.025, na.rm = TRUE),
                                                          Ci.high = quantile(value,0.975, na.rm = TRUE), 
                                                          hdi.low = hdi(value, na.rm = TRUE)[1],
                                                          hdi.high = hdi(value,na.rm = TRUE)[2])


# save obs.wue for use later:

#saveRDS(obs.wue, "outputs/growth_model/iWUE_MAP_TMAX_dbh/WUE_preds.rds")


#--------------------fit the model for d13C--is basically the same model as WUE---------------------
d13_MAP_tmax_dbh_re <- "model{

# for each the overall population include re for sites:

# Likelihood
for(i in 1:n){
# process model for iWUE:
iWUE[i]   ~ dnorm(iWUEfunc[i], inv.var) # where Yi is already log transformed

# function g()
iWUEfunc[i] <- beta1[struct.cohort[i]] + beta2[struct.cohort[i]]*MAP_scaled[i] + beta3[struct.cohort[i]]*TMAX_scaled[i] + beta4[struct.cohort[i]]*DBH_scaled[i]#+beta5[s[i]]

}


# Assume normal priors for betas, but generate a beta + alpha for each ageclass
for(s in 1:length(SF)){
beta1[s] ~ dnorm(mu_beta1, inv_beta1)
beta2[s] ~ dnorm(mu_beta2, inv_beta2)
beta3[s] ~ dnorm(mu_beta3, inv_beta3)
beta4[s] ~ dnorm(mu_beta4, inv_beta4)

}

 #  for(ns in 1:5){
 #   beta5[ns] ~ dnorm(mu_beta5, inv_beta5)
 # }


# use normal hyperpriors for each hyperparamters 

mu_beta1 ~ dnorm(0, 0.1)
mu_beta2 ~ dnorm(0, 0.1)
mu_beta3 ~ dnorm(0, 0.1)
mu_beta4 ~ dnorm(0, 0.1)
#mu_beta5 ~ dnorm(0, 0.1)


inv_beta1   ~ dgamma(0.01, 0.01)
sigma_beta1 <- 1/sqrt(inv_beta1)

inv_beta2   ~ dgamma(0.01, 0.01)
sigma_beta2 <- 1/sqrt(inv_beta2)

inv_beta3   ~ dgamma(0.01, 0.01)
sigma_beta3 <- 1/sqrt(inv_beta3)

inv_beta4   ~ dgamma(0.01, 0.01)
sigma_beta4 <- 1/sqrt(inv_beta4)

# inv_beta5   ~ dgamma(0.01, 0.01)
# sigma_beta5 <- 1/sqrt(inv_beta5)

# Non-informative Prior for the inverse population variances

inv.var   ~ dgamma(0.001, 0.001)
sigma     <- 1/sqrt(inv.var)


# # Predict test data
for(i in 1:np){
 # process model for iWUE:
 iWUE.p[i]   ~ dnorm(iWUEfunc.p[i], inv.var) # where Yi is already log transformed
 
 # function g()
 iWUEfunc.p[i] <- beta1[struct.cohort.p[i]] + beta2[struct.cohort.p[i]]*MAP_scaled.p[i]+ beta3[struct.cohort.p[i]]*TMAX_scaled.p[i]+ beta4[struct.cohort.p[i]]*DBH_scaled.p[i]#+beta5[structure.p[i]]

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
                           # textConnection(iWUE_MAP_tmax_dbh_re),
                            data = list(iWUE =train.iso$Cor.d13C.suess, n=length(train.iso$Cor.d13C.suess), 
                                        struct.cohort = as.numeric(train.iso$struct.cohort.code), 
                                        SF = unique(train.iso$struct.cohort.code),
                                        MAP_scaled = train.iso$MAP.scaled, TMAX_scaled = train.iso$T.scaled, 
                                        DBH_scaled = train.iso$DBH.scaled, site = train.iso$site.code,
                                        structure = as.numeric(train.iso$structure),
                                        np = length(full.iso$site), 
                                        struct.cohort.p = as.numeric( full.iso$struct.cohort.code), 
                                        structure.p = as.numeric(full.iso$structure),
                                        MAP_scaled.p =  full.iso$MAP.scaled, TMAX_scaled.p =  full.iso$T.scaled, 
                                        DBH_scaled.p =  full.iso$DBH.scaled, site.p = full.iso$site.code), n.chains = 3, n.adapt = 100) 
#nprobe = length(iWUEprobe$MAP.probe),
#MAP_scaled.probe = iWUEprobe$MAP.probe,
#TMAX_scaled.probe = iWUEprobe$T.probe, 
#DBH_scaled.probe = iWUEprobe$DBH.probe,
#struct.cohort.probe = iWUEprobe$age.probe), n.chains = 3, n.adapt = 100)

update(d13_map_tmax, 1000); # Burnin for 1000 samples to start, then go higher later


d13C_map_tmax.re <- coda.samples(d13_map_tmax, 
                                 variable.names=c("beta1", "beta2","beta3","beta4","beta5","mu_beta1","mu_beta2","mu_beta3","mu_beta4", "iWUE.p"), 
                                 n.chains = 3, n.iter = 20000, thin = 15)
d13C_map_tmax.re <- coda.samples(d13_map_tmax, 
                                 variable.names=c("beta1", "beta2","beta3","beta4","beta5","mu_beta1","mu_beta2","mu_beta3","mu_beta4", "iWUE.p"), 
                                 n.chains = 3, n.iter = 20000, thin = 15)

d13C_map_tmax.re <- coda.samples(d13_map_tmax, 
                                 variable.names=c("beta1", "beta2","beta3","beta4","beta5","mu_beta1","mu_beta2","mu_beta3","mu_beta4", "iWUE.p"), 
                                 n.chains = 3, n.iter = 20000, thin = 15)
d13C_map_tmax.re <- coda.samples(d13_map_tmax, 
                                 variable.names=c("beta1", "beta2","beta3","beta4","beta5","mu_beta1","mu_beta2","mu_beta3","mu_beta4", "iWUE.p"), 
                                 n.chains = 3, n.iter = 20000, thin = 15)
d13C_map_tmax.re <- coda.samples(d13_map_tmax, 
                                 variable.names=c("beta1", "beta2","beta3","beta4","beta5","mu_beta1","mu_beta2","mu_beta3","mu_beta4", "iWUE.p"), 
                                 n.chains = 3, n.iter = 20000, thin = 15)

par(mfrow = c(3,4))
traceplot(d13C_map_tmax.re[, c("beta1[1]","beta1[2]","beta1[3]","beta1[4]","beta2[1]", "beta2[2]", "beta3[1]", "beta3[2]", "beta4[1]", "beta4[2]")])
gelman.diag(d13C_map_tmax.re[, c("beta1[1]","beta1[2]","beta1[3]","beta1[4]","beta2[1]", "beta2[2]", "beta3[1]", "beta3[2]", "beta4[1]", "beta4[2]")])
# gelman.diag values all look good <= 1.01


d13samps       <- d13C_map_tmax.re [[1]]
saveRDS(d13C_map_tmax.re, "outputs/growth_model/iWUE_MAP_TMAX_dbh_cohort/samps.d13_struct.cohort_struct_cohort_scaled_dataset_v4.rds")
d13C_map_tmax.re<- readRDS( "outputs/growth_model/iWUE_MAP_TMAX_dbh_cohort/samps.d13_struct.cohort_struct_cohort_scaled_dataset_v4.rds")
d13samps       <-  d13C_map_tmax.re[[1]]
#Yp.samps <- samps[,1:660] # one alpha for each of 16 sites
#iWUEpred <- samps
alpha.samps  <- d13samps[,1:4]

beta.samps  <- d13samps[,5:8]
beta2.samps  <- d13samps[,9:12]
beta3.samps  <- d13samps[,13:16]
#beta4.samps  <- d13samps[,9:13]
params <- d13samps[,1:16]
saveRDS(params, "outputs/growth_model/iWUE_MAP_TMAX_dbh_cohort/params.d13_struct.cohort_struct_cohort_scaled_dataset_v4.rds")
d13.samps  <- d13samps[,17:(16+length(full.iso$Cor.d13C.suess))]


hdi( (( alpha.samps[,1]-alpha.samps[,2] )/alpha.samps[,2])*100)

# unique(full.iso[, c("struct.cohort", "struct.cohort.code")])
# struct.cohort struct.cohort.code
# 1   Modern-Savanna                  4
# 19    Past-Savanna                  3
# 331    Past-Forest                  1
# 354  Modern-Forest                  2


alpha.diff.forest <- ((alpha.samps[,2]-alpha.samps[,1] )/alpha.samps[,1])*100

d13.pct.diff.forest <- data.frame(
  pct.change = mean(alpha.diff.forest),
  Ci.low = quantile(alpha.diff.forest, 0.025), 
  Ci.high = quantile(alpha.diff.forest, 0.975), 
  median = median(alpha.diff.forest), 
  hdi.low = hdi(alpha.diff.forest)[1], 
  hdi.high = hdi(alpha.diff.forest)[2])


alpha.diff.savanna <- ((alpha.samps[,4]-alpha.samps[,3] )/alpha.samps[,3])*100
mean(alpha.samps[,4]-alpha.samps[,3])
d13.pct.diff.savanna <- data.frame(
  pct.change = mean(alpha.diff.savanna),
  Ci.low = quantile(alpha.diff.savanna, 0.025), 
  Ci.high = quantile(alpha.diff.savanna, 0.975), 
  median = median(alpha.diff.savanna), 
  hdi.low = hdi(alpha.diff.savanna)[1], 
  hdi.high = hdi(alpha.diff.savanna)[2])


Yp.samps <- data.frame(d13.samps) 
Yp.m <- melt(Yp.samps)
Yp.summary <- Yp.m %>% group_by(variable) %>% dplyr::summarise(Predicted = mean(value),
                                                               ci.hi = quantile(value,0.975),
                                                               ci.lo = quantile(value,0.025))
Yp.summary$Observed <- full.iso$Cor.d13C.suess
Yp.summary$ageclass <- full.iso$ageclass
Yp.summary$struct.cohort.code <- full.iso$struct.cohort.code
Yp.summary$struct.cohort <- full.iso$struct.cohort
Yp.summary$site <- full.iso$site

pred.obs <- summary(lm(colMeans(Yp.samps)~ full.iso$Cor.d13C.suess))

# this does a poor job representing iWUE values by itself, but explains som of the variation
p.o.plot <- ggplot(data = Yp.summary, aes(x = Observed, y= Predicted))+geom_point( size = 0.5)+geom_errorbar(data = Yp.summary,aes(ymin=ci.lo, ymax=ci.hi), color = "grey", alpha = 0.5)+geom_point(data = Yp.summary, aes(Observed, Predicted), color = "black", size = 0.5)+geom_abline(aes(slope = 1, intercept = 0), color = "red", linetype = "dashed")+
  geom_text(data=data.frame(pred.obs$r.squared), aes( label = paste("R^2: ", pred.obs$r.squared, sep="")),parse=T,x=-26, y=-22)

# note fairly poor model fit
png(width = 6, height = 5, units = "in", res = 300, "outputs/growth_model/iWUE_MAP_TMAX_dbh/pred_vs_obs_d13C_struct_cohort_scaled_dataset_v4.png")
p.o.plot
dev.off()


#---------Run d13 model for just ageclasses:
d13_map_tmax <- jags.model(textConnection(d13_MAP_tmax_dbh_re), 
                           data = list(iWUE =train.iso$Cor.d13C.suess, n=length(train.iso$Cor.d13C.suess), 
                                       struct.cohort = as.numeric(train.iso$age.code), 
                                       SF = unique(train.iso$age.code),
                                       MAP_scaled = train.iso$MAP.scaled, TMAX_scaled = train.iso$T.scaled, 
                                       DBH_scaled = train.iso$DBH.scaled, site = train.iso$site.code,
                                       structure = as.numeric(train.iso$structure),
                                       np = length(full.iso$site), 
                                       struct.cohort.p = as.numeric( full.iso$age.code), 
                                       structure.p = as.numeric(full.iso$structure),
                                       MAP_scaled.p =  full.iso$MAP.scaled, TMAX_scaled.p =  full.iso$T.scaled, 
                                       DBH_scaled.p =  full.iso$DBH.scaled, site.p = full.iso$site.code), n.chains = 3, n.adapt = 100) 
#nprobe = length(iWUEprobe$MAP.probe),
#MAP_scaled.probe = iWUEprobe$MAP.probe,
#TMAX_scaled.probe = iWUEprobe$T.probe, 
#DBH_scaled.probe = iWUEprobe$DBH.probe,
#struct.cohort.probe = iWUEprobe$age.probe), n.chains = 3, n.adapt = 100)

update(d13_map_tmax, 1000); # Burnin for 1000 samples to start, then go higher later


d13C_map_tmax.re <- coda.samples(d13_map_tmax, 
                                 variable.names=c("beta1", "beta2","beta3","beta4","beta5","mu_beta1","mu_beta2","mu_beta3","mu_beta4", "iWUE.p"), 
                                 n.chains = 3, n.iter = 20000, thin = 15)


par(mfrow = c(3,4))
traceplot(d13C_map_tmax.re[, c("beta1[1]","beta1[2]","beta2[1]", "beta2[2]", "beta3[1]", "beta3[2]", "beta4[1]", "beta4[2]")])
gelman.diag(d13C_map_tmax.re[, c("beta1[1]","beta1[2]","beta2[1]", "beta2[2]", "beta3[1]", "beta3[2]", "beta4[1]", "beta4[2]")])
# gelman.diag values all look good <= 1.01



d13samps       <- d13C_map_tmax.re [[1]]
saveRDS(d13samps, "outputs/growth_model/iWUE_MAP_TMAX_dbh_cohort/samps.d13_ageclass_struct_cohort_scaled_dataset_v4.rds")
#Yp.samps <- samps[,1:660] # one alpha for each of 16 sites
#iWUEpred <- samps
alpha.samps  <- d13samps[,1:2]

beta.samps  <- d13samps[,3:4]
beta2.samps  <- d13samps[,5:6]
beta3.samps  <- d13samps[,7:8]
#beta4.samps  <- d13samps[,9:13]
params <- d13samps[,1:8]
saveRDS(params, "outputs/growth_model/iWUE_MAP_TMAX_dbh_cohort/params.d13_ageclass_struct_cohort_scaled_dataset_v4.rds")
d13.samps  <- d13samps[,9:(8+length(full.iso$Cor.d13C.suess))]


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
Yp.summary$Observed <- full.iso$Cor.d13C.suess
Yp.summary$ageclass <- full.iso$ageclass
Yp.summary$struct.cohort.code <- full.iso$struct.cohort.code
Yp.summary$struct.cohort <- full.iso$struct.cohort
Yp.summary$site <- full.iso$site

pred.obs <- summary(lm(colMeans(Yp.samps)~ full.iso$Cor.d13C.suess))

# this does a poor job representing iWUE values by itself
p.o.plot <- ggplot(data = Yp.summary, aes(x = Observed, y= Predicted))+geom_point( size = 0.5)+geom_errorbar(data = Yp.summary,aes(ymin=ci.lo, ymax=ci.hi), color = "grey", alpha = 0.5)+geom_point(data = Yp.summary, aes(Observed, Predicted), color = "black", size = 0.5)+geom_abline(aes(slope = 1, intercept = 0), color = "red", linetype = "dashed")+
  geom_text(data=data.frame(pred.obs$r.squared), aes( label = paste("R^2: ", pred.obs$r.squared, sep="")),parse=T,x=-26, y=-22)

# note fairly poor model fit
png(width = 6, height = 5, units = "in", res = 300, "outputs/growth_model/iWUE_MAP_TMAX_dbh/pred_vs_obs_d13C_ageclass_struct_cohort_scaled_dataset_v4.png")
p.o.plot
dev.off()

