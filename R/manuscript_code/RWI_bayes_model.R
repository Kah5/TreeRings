# script for the final RWI models for the paper:

# read in the data:




# this model is specified in basically the same as the site level model, but we replace site with "cohort"

population_model_site_int_structure_x_cohort_re_lag2_temp_MAP_interaction.ageclass <- "model{

# for each the overall population include re for sites:

# Likelihood
for(i in 1:n){
# process model
Y[i]   ~ dnorm(gfunc[i], inv.var) # where Yi is already log transformed

# function g()
gfunc[i] <- beta1[site[i]] + beta2[age[i]]*DI.scaled[i] +   beta3[age[i]]*DBH.scaled[i]+beta4[age[i]]*log_RWI_1[i]+beta5[age[i]]*log_RWI_2[i]  + beta6[age[i]]*Temp.scaled[i] + beta7[age[i]]*Temp.scaled[i]*DI.scaled[i] # inclued beta 7 as interaction term 
   

}



# Prediction from known data
  for(i in 1:np){
    Yp[i]  ~ dnorm(mup[i],inv.var)
    mup[i] <- beta1[site.p[i]] + beta2[age.p[i]]*DI.scaled.p[i] + beta3[age.p[i]]*DBH.scaled.p[i] +beta4[age.p[i]]*log_RWI_1.p[i] +beta5[age.p[i]]*log_RWI_2.p[i] + beta6[age.p[i]]*Temp.scaled.p[i]+ beta7[age.p[i]]*Temp.scaled.p[i]*DI.scaled.p[i]
  }

# probe for prediction plots:
  for(i in 1:nprobe){
    Yprobe[i]  ~ dnorm(muprobe[i],inv.var)
    muprobe[i] <- beta1[site.probe[i]] + beta2[age.probe[i]]*DI.scaled.probe[i] + beta3[age.probe[i]]*DBH.scaled.probe[i] +beta4[age.probe[i]]*log_RWI_1.probe[i] + beta5[age.probe[i]]*log_RWI_2.probe[i] + beta6[age.probe[i]]*Temp.scaled.probe[i] + beta7[age.probe[i]]*Temp.scaled.probe[i]*DI.scaled.probe[i]
  }

# project into the future (assuming no further change in drought senseiivty):
  for(i in 1:nfut){
   Yfut[i]  ~ dnorm(mufut[i],inv.var)
    mufut[i] <- beta1[site.fut[i]] + beta2[age.fut[i]]*DI.scaled.fut[i] + beta3[age.fut[i]]*DBH.scaled.fut[i] +beta4[age.fut[i]]*log_RWI_1.fut[i] +beta5[age.fut[i]]*log_RWI_2.fut[i] + beta6[age.fut[i]]*Temp.scaled.fut[i] + (beta7[age.fut[i]])*Temp.scaled.fut[i]*DI.scaled.fut[i]
  }

# project into the future (assuming 35% further change in drought senseiivty):
  for(i in 1:nfut){
   Yfut.35[i]  ~ dnorm(mufut2[i],inv.var)
    mufut2[i] <- beta1[site.fut[i]] + (beta2[age.fut[i]]*0.92)*DI.scaled.fut[i] + beta3[age.fut[i]]*DBH.scaled.fut[i] +beta4[age.fut[i]]*log_RWI_1.fut[i] +beta5[age.fut[i]]*log_RWI_2.fut[i] + beta6[age.fut[i]]*Temp.scaled.fut[i]+ (beta7[age.fut[i]])*Temp.scaled.fut[i]*DI.scaled.fut[i]
  }



# project into the future (assuming 50% further change in drought senseiivty):
  for(i in 1:nfut){
   Yfut.50[i]  ~ dnorm(mufut3[i],inv.var)
    mufut3[i] <- beta1[site.fut[i]] + (beta2[age.fut[i]]*0.85)*DI.scaled.fut[i] + beta3[age.fut[i]]*DBH.scaled.fut[i] +beta4[age.fut[i]]*log_RWI_1.fut[i] +beta5[age.fut[i]]*log_RWI_2.fut[i] + beta6[age.fut[i]]*Temp.scaled.fut[i]+ (beta7[age.fut[i]])*Temp.scaled.fut[i]*DI.scaled.fut[i]
  }


# project into the future (assuming 8% further change in drought senseiivty & temperature):
  for(i in 1:nfut){
   Yfut.35.35[i]  ~ dnorm(mufut35[i],inv.var)
    mufut35[i] <- beta1[site.fut[i]] + (beta2[age.fut[i]]*0.92)*DI.scaled.fut[i] + beta3[age.fut[i]]*DBH.scaled.fut[i] +beta4[age.fut[i]]*log_RWI_1.fut[i] +beta5[age.fut[i]]*log_RWI_2.fut[i] + (beta6[age.fut[i]]*1.08)*Temp.scaled.fut[i]+ (beta7[age.fut[i]]*0.92)*Temp.scaled.fut[i]*DI.scaled.fut[i]
  }



# project into the future (assuming 7% further change in drought senseiivty):
  for(i in 1:nfut){
   Yfut.50.50[i]  ~ dnorm(mufut50[i],inv.var)
    mufut50[i] <- beta1[site.fut[i]] + (beta2[age.fut[i]]*0.85)*DI.scaled.fut[i] + beta3[age.fut[i]]*DBH.scaled.fut[i] +beta4[age.fut[i]]*log_RWI_1.fut[i] +beta5[age.fut[i]]*log_RWI_2.fut[i] + (beta6[age.fut[i]]*1.15)*Temp.scaled.fut[i]+(beta7[age.fut[i]]*0.85)*Temp.scaled.fut[i]*DI.scaled.fut[i]
  }

# Assume normal priors for betas, but generate a beta + alpha for each ageclass

for(s in 1:2){

beta2[s] ~ dnorm(mu_beta2, inv_beta2)
beta3[s] ~ dnorm(mu_beta3, inv_beta3)
beta4[s] ~ dnorm(mu_beta4, inv_beta4)
beta5[s] ~ dnorm(mu_beta5, inv_beta5)
beta6[s] ~ dnorm(mu_beta6, inv_beta6)
beta7[s] ~ dnorm(mu_beta7, inv_beta7)
}


for(k in 1:Nsites){
beta1[k] ~ dnorm(mu_beta1, inv_beta1)
}


# use uniform priors for each  Mubeta paramters 
mu_beta1 ~ dunif(-1, 1)
mu_beta2 ~ dunif(-1, 1)
mu_beta3 ~ dunif(-1, 1)
mu_beta4 ~ dunif(-1, 1)
mu_beta5 ~ dunif(-1, 1)
mu_beta6 ~ dunif(-1, 1)
mu_beta7 ~ dunif(-1, 1)

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
inv_beta6   ~ dgamma(0.01, 0.01)
sigma_beta6 <- 1/sqrt(inv_beta6)
inv_beta7   ~ dgamma(0.01, 0.01)
sigma_beta7 <- 1/sqrt(inv_beta7)


# Non-informative Prior for the inverse population variances

#alpha_ref ~ dnorm(0,0.1)
inv.var   ~ dgamma(0.001, 0.001)
sigma     <- 1/sqrt(inv.var)



# calculate differences between parameters:
# one.two <- beta2[1] - beta2[2]
# two.three<- beta2[2] - beta2[3]
# three.four<- beta2[3] - beta2[4]
# two.four <- beta2[2] - beta2[4]


}"

# save these for use later:
#readRDS(train.dry.pair, "outputs/growth_model/lag2_reg_cohort_only_re_t_pr_dry_yrs_site_rs_inter/train.rds")
#readRDS(test.dry.pair, "outputs/growth_model/lag2_reg_cohort_only_re_t_pr_dry_yrs_site_rs_inter/test.rds")
train.dry.pair <- readRDS("data/full_dry_paired_dataset.rds")
test.dry.pair <- readRDS("data/train_dry_paired_dataset.rds")
full.dry.pair <- readRDS("data/test_dry_paired_dataset.rds")

# assign site numbers to each site:

site.num.df <- data.frame(site = as.character(unique(train.dry.pair$site)), 
                          site.num = 1:length(as.character(unique(train.dry.pair$site))))

# if site num is not in the df, add it
if(! "site.num" %in% colnames(train.dry.pair)){
  train.dry.pair <- merge(train.dry.pair, site.num.df, by = "site" )
}
if(! "site.num" %in% colnames(test.dry.pair)){
  test.dry.pair <- merge(test.dry.pair, site.num.df, by = "site" )
}
if(! "site.num" %in% colnames(dyr.yrs.paired)){
  full.dry.pair <- merge(dry.yrs.paired, site.num.df, by = "site" )
}

# generate probe dataset:
DIprobe <- round(seq(range(train.dry.pair$MAP.scaled)[1], range(train.dry.pair$MAP.scaled)[2], by = 0.75), 3)
DBHprobe <- round(seq(range(train.dry.pair$DBH.scaled)[1], range(train.dry.pair$DBH.scaled)[2], by = 0.75), 3)
Tempprobe <- round(seq(range(train.dry.pair$T.scaled)[1], range(train.dry.pair$T.scaled)[2], by = 0.75), 3)
RWI1probe <- round(seq(range(log(train.dry.pair$RWI_1))[1], range(log(train.dry.pair$RWI_1))[2], by = 0.5), 3)
RWI2probe <- round(seq(range(log(train.dry.pair$RWI_2))[1], range(log(train.dry.pair$RWI_2))[2], by = 0.5), 3)

# expand into full probe
probe <- expand.grid(DI.scaled = DIprobe, DBH.scaled = DBHprobe, T.scaled = Tempprobe,
                     RWI_1 = RWI1probe, RWI_2= RWI2probe, struct.cohort.code= 1:4, age.code = 1:2,site.num = unique(train.dry.pair$site.num))

sit.cohorts <- unique(train.dry.pair[,c("struct.cohort.code", "site.num")])
probe <- merge(probe, sit.cohorts, by = c("struct.cohort.code", "site.num")) # make sure that each site only has savanna or forest in the probe

# make probe smaller to help with saving:
short.probe <- probe[probe$RWI_1 %in% c("-1.609", "0.391", "1.891") & probe$RWI_2 %in% c("-1.196", "0.304", "1.804") & probe$DBH.scaled %in% c("-1.163", "0.337", "1.837"),]

#short.probe <- probe

# make a probe for future climate projections at our sites!
rcp85 <- rcp85[!rcp85$site %in% c("COR", "GLL4", "HIC", "PLE", "PVC", "STC", "TOW"),]
future.probe_70 <- data.frame(MAP.scaled = rcp85$PR_85_70.scaled, DBH.scaled= -0.413, 
                              T.scaled = rcp85$Tx_70.scaled,RWI_1 = 0.103, 
                              RWI_2 = 0.103,  struct.cohort.code = rcp85$struct.cohort.code,age.code = 1, site = rcp85$site, model = rcp85$model)
future.probe_70$year <- 2070
future.probe_50 <- data.frame(MAP.scaled= rcp85$Pr_85_50.scaled, DBH.scaled = -0.413, 
                              T.scaled  = rcp85$Tx_50.scaled,RWI_1 = 0.103, 
                              RWI_2 = 0.103, struct.cohort.code = rcp85$struct.cohort.code, age.code = 1, site = rcp85$site,  model = rcp85$model)
future.probe_50$year <- 2050


# get climate averages from 1950 - present at each site:
train.dry.pair$struct.cohort.code2 <- as.numeric(train.dry.pair$struct.cohort.code)
train.clim.summary <-
  train.dry.pair[train.dry.pair$ageclass %in% "Modern", ] %>% group_by(site) %>% dplyr::summarise(
    MAP.scaled = mean(MAP.scaled),
    DBH.scaled = -0.413,
    T.scaled = mean(T.scaled),
    RWI_1 = 0.103,
    RWI_2 = 0.103,
    struct.cohort.code = mean(struct.cohort.code2),
    age.code = 1,
    model = "none",
    year = 2000
  )



Average.prob <- data.frame(MAP.scaled= rcp85$Pr_85_50.scaled, DBH.scaled = -0.413, 
                           T.scaled  = rcp85$Tx_50.scaled,RWI_1 = 0.103, 
                           RWI_2 = 0.103, struct.cohort.code = rcp85$struct.cohort.code, age.code = 1,site = rcp85$site, model = rcp85$model)
future.probe_50$year <- 2050


fut <- rbind(future.probe_50, future.probe_70, train.clim.summary[,c("MAP.scaled", "DBH.scaled",  "T.scaled", "RWI_1", "RWI_2", "struct.cohort.code", "age.code","site","year","model")])


fut <- merge(fut, site.num.df, by = "site")

# look at the effects under two different DBH values
short.fut <- fut
fut.small <- fut
fut.large <- fut
fut.small$DBH.scaled <- -0.413
fut.large$DBH.scaled <- 1.900
long.fut <- rbind(fut.small, fut.large)


# look at the effects under average, below average, and above average different prev year growth values
lfut.low <- long.fut
lfut.avg <- long.fut
lfut.high <- long.fut

lfut.low$RWI_1 <- -1.609
lfut.low$RWI_2 <- -1.47

lfut.high$RWI_1 <- 1.891
lfut.high$RWI_2 <- 2.03

long.fut <- rbind(lfut.high, lfut.avg, lfut.low)


train.dry.pair$ageclass_code <- ifelse(train.dry.pair$ageclass %in% "Modern", 1, 2)
test.dry.pair$ageclass_code <- ifelse(test.dry.pair$ageclass %in% "Modern", 1, 2)


# create a single dataframe with all the values we care about
colsforYpred <- c("RWI", "MAP.scaled", "DBH.scaled", "RWI_1", "RWI_2", "T.scaled", "ageclass_code", "site.num", "site")

full.dry.pair <- rbind(train.dry.pair[,colsforYpred], test.dry.pair[,colsforYpred])


# run the model but predict all the values for the full dataset
lag2.model.by_structure_x_cohort_t_pr_int <- jags.model(textConnection(population_model_site_int_structure_x_cohort_re_lag2_temp_MAP_interaction.ageclass), 
                                                        data = list(Y=log(train.dry.pair$RWI), 
                                                                    n=length(train.dry.pair$RWI), 
                                                                    DI.scaled = train.dry.pair$MAP.scaled, 
                                                                    DBH.scaled = train.dry.pair$DBH.scaled,
                                                                    log_RWI_1 = log(train.dry.pair$RWI_1),
                                                                    log_RWI_2 = log(train.dry.pair$RWI_2), 
                                                                    Temp.scaled = train.dry.pair$T.scaled, 
                                                                    age = as.numeric(train.dry.pair$ageclass_code), 
                                                                    #SF = unique(train.dry.pair$ageclass_code), 
                                                                    site = train.dry.pair$site.num, 
                                                                    Nsites = length(unique(train.dry.pair$site)),
                                                                    
                                                                    
                                                                    age.p = as.numeric(full.dry.pair$ageclass_code), 
                                                                    DBH.scaled.p = full.dry.pair$DBH.scaled, 
                                                                    DI.scaled.p = full.dry.pair$MAP.scaled, 
                                                                    log_RWI_1.p = log(full.dry.pair$RWI_1), 
                                                                    log_RWI_2.p = log(full.dry.pair$RWI_2), 
                                                                    Temp.scaled.p = full.dry.pair$T.scaled, 
                                                                    site.p = full.dry.pair$site.num, 
                                                                    np = length(full.dry.pair$ageclass_code),
                                                                    
                                                                    age.probe = as.numeric(short.probe$age.code), 
                                                                    DBH.scaled.probe = short.probe$DBH.scaled, 
                                                                    DI.scaled.probe = short.probe$DI.scaled, 
                                                                    log_RWI_1.probe = short.probe$RWI_1, 
                                                                    log_RWI_2.probe = short.probe$RWI_2, 
                                                                    Temp.scaled.probe = short.probe$T.scaled, 
                                                                    site.probe = short.probe$site.num, 
                                                                    nprobe = length(as.numeric(short.probe$age.code)), 
                                                                    # 
                                                                    age.fut = as.numeric(long.fut$age.code), 
                                                                    DBH.scaled.fut = long.fut$DBH.scaled, 
                                                                    DI.scaled.fut = long.fut$MAP.scaled, 
                                                                    log_RWI_1.fut = long.fut$RWI_1, 
                                                                    log_RWI_2.fut =long.fut$RWI_2, 
                                                                    Temp.scaled.fut = long.fut$T.scaled, 
                                                                    site.fut = long.fut$site.num, nfut = length(as.numeric(long.fut$age.code))),
                                                        #, 
                                                        n.chains = 3, n.adapt = 100)




update(lag2.model.by_structure_x_cohort_t_pr_int, 10000); # Burnin for 1000 samples to start, then go higher later


samp.structure.cohort.re <- coda.samples(lag2.model.by_structure_x_cohort_t_pr_int, 
                                         variable.names=c("beta1", "beta2","beta3","beta4","beta5","beta6","beta7","sigma", "mu_beta1", "mu_beta2","mu_beta3", "mu_beta4","mu_beta5", "mu_beta6", "mu_beta7"), 
                                         n.chains = 3, n.iter=10000, thin = 15)

# jointly track all variables, including Y
# samp.structure.cohort.re <- coda.samples(lag2.model.by_structure_x_cohort_t_pr_int, 
#                      variable.names=c("beta1", "beta2","beta3","beta4","beta5","beta6","beta7","sigma", "mu_beta1", "mu_beta2","mu_beta3", "mu_beta4","mu_beta5", "mu_beta6", "mu_beta7", "Y"), 
#                     n.chains = 3, n.iter=10000, thin = 15)


#beta2.diffs <- coda.samples(lag2.model.by_structure_x_cohort_t_pr_int, 
#                    variable.names=c("one.two", "two.three", "three.four", "two.four"), 
#                  n.chains = 3, n.iter=10000, thin = 15)

# note that this is predicting over the full dataset because I used the full dataset in the predicted data
Yp <- coda.samples(lag2.model.by_structure_x_cohort_t_pr_int, 
                   variable.names=c("Yp"), 
                   n.chains = 3, n.iter=10000, thin = 15)

saveRDS(Yp, "outputs/growth_model/lag2_reg_cohort_only_re_t_pr_dry_yrs_site_rs_inter/Ypred_full_dataset.rds")


# Y.probe <- coda.samples(lag2.model.by_structure_x_cohort_t_pr_int, 
#                         variable.names=c("Yprobe"), 
#                         n.chains = 3, n.iter=5000, thin = 15)
# saveRDS(Y.probe, "outputs/growth_model/lag2_reg_cohort_only_re_t_pr_dry_yrs_site_rs_inter/Y.probe.rds")
# 
# Y.fut <- coda.samples(lag2.model.by_structure_x_cohort_t_pr_int, 
#                       variable.names=c("Yfut"), 
#                       n.chains = 3, n.iter=10000, thin = 15)
# saveRDS(Y.fut, "outputs//growth_model/lag2_reg_cohort_only_re_t_pr_dry_yrs_site_rs_inter/Y.fut.rds")
# 
# Y.fut.35 <- coda.samples(lag2.model.by_structure_x_cohort_t_pr_int, 
#                          variable.names=c("Yfut.35"), 
#                          n.chains = 3, n.iter=10000, thin = 15)
# saveRDS(Y.fut.35, "outputs//growth_model/lag2_reg_cohort_only_re_t_pr_dry_yrs_site_rs_inter/Y.fut.35.rds")
# 
# Y.fut.50 <- coda.samples(lag2.model.by_structure_x_cohort_t_pr_int, 
#                          variable.names=c("Yfut.50"), 
#                          n.chains = 3, n.iter=10000, thin = 15)
# saveRDS(Y.fut.50, "outputs//growth_model/lag2_reg_cohort_only_re_t_pr_dry_yrs_site_rs_inter/Y.fut.50.rds")
# 
# Y.fut.35.35 <- coda.samples(lag2.model.by_structure_x_cohort_t_pr_int, 
#                             variable.names=c("Yfut.35.35"), 
#                             n.chains = 3, n.iter=10000, thin = 15)
# saveRDS(Y.fut.35.35, "outputs//growth_model/lag2_reg_cohort_only_re_t_pr_dry_yrs_site_rs_inter/Y.fut.35.35.rds")
# 
# Y.fut.50.50 <- coda.samples(lag2.model.by_structure_x_cohort_t_pr_int, 
#                             variable.names=c("Yfut.50.50"), 
#                             n.chains = 3, n.iter=10000,thin = 15)
# saveRDS(Y.fut.50.50, "outputs//growth_model/lag2_reg_cohort_only_re_t_pr_dry_yrs_site_rs_inter/Y.fuut.50.50.rds")
# 

dic.full <- dic.samples(lag2.model.by_structure_x_cohort_t_pr_int, n.chains = 3, n.iter=10000,thin = 15)
saveRDS(dic.full, "outputs/growth_model/model_summary/lag2_reg_cohort_only_re_t_pr_dry_yrs_site_rs_inter_DIC.rds")

summary(samp.structure.cohort.re)

traceplot(samp.structure.cohort.re[,"beta2[2]"])

# most just around 1
gelman.diag(samp.structure.cohort.re, multivariate = FALSE)
acfplot(samp.structure.cohort.re)


autocorr.plot(samp.structure.cohort.re [[1]][ , 'beta2[2]'], auto.layout = FALSE, lwd = 4, col = "red", lag.max = 100)

#Extract the samples for each parameter for a basic exploration of effects

samps   <- samp.structure.cohort.re[[1]]
saveRDS(samps, "outputs//growth_model/lag2_reg_cohort_only_re_t_pr_dry_yrs_site_rs_inter/samps.rds")
samps <- readRDS("outputs//growth_model/lag2_reg_cohort_only_re_t_pr_dry_yrs_site_rs_inter/samps.rds")
#test.dry <- readRDS("outputs//growth_model/lag2_reg_struct_x_cohort_re_t_pr_dry_yrs_site_rs_inter/test.rds")
#train.dry <- readRDS("outputs//growth_model/lag2_reg_struct_x_cohort_re_t_pr_dry_yrs_site_rs_inter/train.rds")
#Yfut.samps <- samps[, 1:length(fut$RWI_1)]
#samps <- samps[,(length(fut$RWI_1)+1):length(colnames(samps))]
Yp.samps <- Yp
#Yprobe.samps <- Y.probe
alpha.samps  <- samps[,1:9]# one alpha for each of 4 cohort-strcuture groups
beta2.samps <- samps[,10:11]
beta3.samps <- samps[,12:13]
beta4.samps <- samps[,14:15]
beta5.samps <- samps[,16:17]
beta6.samps <- samps[,18:19]
beta7.samps <- samps[,20:21]
mu_beta.samps <- samps[,22:27]
sigma.samps <- samps[,29]
#sigma_betas <- samps[,28:34]




# plot predicted vs observed and assess model fit:
Yp.samps <- data.frame(Yp.samps[[1]]) 
Yp.m <- melt(Yp.samps)
Yp.summary <- Yp.m %>% group_by(variable) %>% dplyr::summarise(Predicted = mean(exp(value)),
                                                               ci.hi = quantile(exp(value),0.975),
                                                               ci.lo = quantile(exp(value),0.025))

full.dry.pair2 <- full.dry.pair
full.dry.pair2$variable <- unique(Yp.m$variable)

Yp.summary$Observed <- full.dry.pair$RWI



pred.obs <- summary(lm(colMeans(exp(Yp.samps)) ~ full.dry.pair$RWI))

p.o.plot <- ggplot(Yp.summary, aes(Observed, Predicted))+geom_point(color = "black", size = 0.5)+geom_errorbar(data = Yp.summary,aes(ymin=ci.lo, ymax=ci.hi), color = "grey", alpha = 0.5)+geom_point(data = Yp.summary, aes(Observed, Predicted), color = "black", size = 0.5)+geom_abline(aes(slope = 1, intercept = 0), color = "red", linetype = "dashed")+ylim(0, 8)+xlim(0,8)+geom_text(data=data.frame(pred.obs$r.squared), aes( label = paste("R^2: ", pred.obs$r.squared, sep="")),parse=T,x=1, y=7)+theme_bw(base_size = 15)

# note best model fit so far
png(width = 6, height = 5, units = "in", res = 300, "outputs/growth_model/lag2_reg_cohort_only_re_t_pr_dry_yrs_site_rs_inter/pred_vs_obs_full_dataset.png")
p.o.plot
dev.off()

# plot average growth boxplots for modern & past:

Yp.full <- left_join(Yp.m, full.dry.pair2, by = "variable")


# get estimates for predicted growth:
Yp.full$ageclass <- ifelse(Yp.full$ageclass_code == 1, "Modern", "Past")
Yp.summary.cohort  <- Yp.full %>% group_by(ageclass) %>% dplyr::summarise(Predicted = mean(exp(value)),
                                                                          ci.hi = quantile(exp(value),0.975),
                                                                          ci.lo = quantile(exp(value),0.025), 
                                                                          Observed = mean(RWI))


Yp.summary.cohort$ageclass <- factor(Yp.summary.cohort$ageclass, levels = c("Past", "Modern"))
mean.pred.growth <- ggplot(Yp.summary.cohort, aes(ageclass,Predicted, fill = ageclass))+geom_bar(stat = "identity")+geom_errorbar(data = Yp.summary.cohort,aes(ymin=ci.lo, ymax=ci.hi), color = "grey", alpha = 0.8, size = 0.5, width = 0.2)+ylim(0, 6) + ylab("Mean predicted tree growth (mm)")+scale_fill_manual(values = c("Past"='#2166ac', 'Modern' = "#b2182b"))+theme_bw(base_size = 16)+theme(panel.grid = element_blank(), axis.title.x = element_blank(), legend.position = "none")

png(height = 4, width = 4.5, units = "in", res = 300, "outputs/growth_model/lag2_reg_cohort_only_re_t_pr_dry_yrs_site_rs_inter/predicted_growth_barplot.png")
mean.pred.growth
dev.off()


saveRDS(Yp.full, "outputs/growth_model/lag2_reg_cohort_only_re_t_pr_dry_yrs_site_rs_inter/predicted_growth_YP.rds")
saveRDS(Yp.summary.cohort, "outputs/growth_model/lag2_reg_cohort_only_re_t_pr_dry_yrs_site_rs_inter/predicted_growth_YP_summary.rds")


# -------make overall summary of the differences in parameters between groups:

beta_diffs <- data.frame(diff_beta2 = (beta2.samps[,1]-beta2.samps[,2]), 
                         diff_beta3 = (beta3.samps[,1]-beta3.samps[,2]) , 
                         diff_beta4 = (beta4.samps[,1]-beta4.samps[,2]), 
                         diff_beta5 = (beta5.samps[,1]-beta5.samps[,2]), 
                         diff_beta6 = (beta6.samps[,1]-beta6.samps[,2]), 
                         diff_beta7 = (beta7.samps[,1]-beta7.samps[,2]))

beta_diffs.pct <- data.frame(diff_beta2 = (beta2.samps[,1]-beta2.samps[,2]), 
                             diff_beta3 = (beta3.samps[,1]-beta3.samps[,2]) , 
                             diff_beta4 = (beta4.samps[,1]-beta4.samps[,2]), 
                             diff_beta5 = (beta5.samps[,1]-beta5.samps[,2]), 
                             diff_beta6 = (beta6.samps[,1]-beta6.samps[,2]), 
                             diff_beta7 = (beta7.samps[,1]-beta7.samps[,2]))


colnames(beta_diffs) <- c("MAP", "DBH", "lag_1", "lag_2", "Tmax","MAPxTmax")

beta_diffsm <- beta_diffs %>% gather(beta, difference) %>% group_by(beta) %>% 
  summarise(mean = mean(difference), 
            hdi.low = hdi(difference)[1],
            hdi.high = hdi(difference)[2]) 


beta.diffs.plot.mod.past <- ggplot(beta_diffsm, aes(beta, mean))+geom_bar(stat = "identity")+geom_errorbar(data = beta_diffsm,aes(ymin=hdi.low, ymax=hdi.high), color = "grey", alpha = 0.8, size = 0.5, width = 0.2)+ylab("Difference in beta parameter (Modern - Past)")+xlab("beta parameters")+theme_bw(base_size = 16)+theme(panel.grid = element_blank())

png(height = 4, width = 4.5, units = "in", res = 300, "outputs/growth_model/lag2_reg_cohort_only_re_t_pr_dry_yrs_site_rs_inter/beta_differences_hdi.png")
beta.diffs.plot.mod.past
dev.off()

# save for later plot:
saveRDS(beta_diffsm, "outputs/growth_model/lag2_reg_cohort_only_re_t_pr_dry_yrs_site_rs_inter/beta_differences_cohorts.rds")

saveRDS(beta_diffs, "outputs/growth_model/lag2_reg_cohort_only_re_t_pr_dry_yrs_site_rs_inter/beta_diffs_cohorts.rds")

# calculate MSE & BIAS:

MSE1   <- mean((colMeans(exp(Yp.samps))-test.dry.pair$RWI)^2)
BIAS1  <- mean(colMeans(exp(Yp.samps))-test.dry.pair$RWI)

# write model summary output to a file!

model.summary <- data.frame(model = "lag2_reg_cohort_only_re_t_pr_dry_yrs_site_rs_inter", 
                            model.summary = "beta1[site[i]] + beta2[struct.cohort[i]]*DI.scaled[i] +   beta3[struct.cohort[i]]*DBH.scaled[i]+beta4[struct.cohort[i]]*log_RWI_1[i]+beta5[struct.cohort[i]]*log_RWI_2[i]  + beta6[struct.cohort[i]]*Temp.scaled[i] + beta7[struct.cohort[i]]*Temp.scaled[i]*DI.scaled[i] ",
                            MSE = MSE1, 
                            BIAS = BIAS1, 
                            Rsq = pred.obs$r.squared)

write.csv(model.summary, paste0("outputs/growth_model/model_summary/", model.summary$model, "_summary.csv"), row.names = FALSE)


# summarise the datasource to make model comparisons for best fit models
data.summary <- data.frame(model = "lag2_reg_cohort_only_re_t_pr_dry_yrs_site_rs_inter", 
                           train = "cohort.omit.dry.years", 
                           DI.scaled = "MAP")

write.csv(data.summary, paste0("outputs/growth_model/data_summary/", data.summary$model, "_data.csv"), row.names = FALSE)


