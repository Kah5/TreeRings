# script for the final RWI models for the paper:
# Author: Kelly A. Heilman
# Last Updated: March 25, 2020
# note: Need to run clean_separate_data.R before running this script, this loads required packages and required datasets

#----------------------------------------------------------------------------------------------
#                         Tree Ring Growth Model with Cohort-Only Random Slopes
#----------------------------------------------------------------------------------------------



# JAGS mmodel selected with cohort-only-level randdom effects: 

population_model_site_int_structure_x_cohort_re_lag2_temp_MAP_interaction.ageclass <- "model{


# Likelihood

for(i in 1:n){ # n is the number of individual tree-years

# ## ------------Process Model---------------------------
Y[i]   ~ dnorm(gfunc[i], inv.var) # where Yi is already log transformed

# function g() 
# site level random intercept (beta1)
# ageclass-structure random slopes for climate (DI.scaled, Temp.scaled + interaction), tree size (DBH.scaled), and log transformed previous years of growth. 

gfunc[i] <- beta1[site[i]] + beta2[age[i]]*DI.scaled[i] +   beta3[age[i]]*DBH.scaled[i]+beta4[age[i]]*log_RWI_1[i]+beta5[age[i]]*log_RWI_2[i]  + beta6[age[i]]*Temp.scaled[i] + beta7[age[i]]*Temp.scaled[i]*DI.scaled[i] # inclued beta 7 as interaction term 
   

}

## -------------Posterior predictions---------------------------

# Prediction from known data (for testing predictions with held out training data)
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


## -------------Priors--------------------
# Assume normal, uninformative priors for betas, but generate a beta + alpha for each ageclass

# cohort random slopes
for(s in 1:2){ 

beta2[s] ~ dnorm(mu_beta2, inv_beta2)
beta3[s] ~ dnorm(mu_beta3, inv_beta3)
beta4[s] ~ dnorm(mu_beta4, inv_beta4)
beta5[s] ~ dnorm(mu_beta5, inv_beta5)
beta6[s] ~ dnorm(mu_beta6, inv_beta6)
beta7[s] ~ dnorm(mu_beta7, inv_beta7)
}


# site level random intercept
for(k in 1:Nsites){ 
beta1[k] ~ dnorm(mu_beta1, inv_beta1)
}


# use uniform priors for each Mu_beta hyperparamter
mu_beta1 ~ dunif(-1, 1)
mu_beta2 ~ dunif(-1, 1)
mu_beta3 ~ dunif(-1, 1)
mu_beta4 ~ dunif(-1, 1)
mu_beta5 ~ dunif(-1, 1)
mu_beta6 ~ dunif(-1, 1)
mu_beta7 ~ dunif(-1, 1)

# priors for error terms
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


# Non-informative Prior for the inverse additive population variances

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
train.dry.pair <- readRDS("data/full_dataset_v4.rds")
test.dry.pair <- readRDS("data/train_dataset_v4.rds")
full.dry.pair <- readRDS("data/test_dataset_v4.rds")

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
if(! "site.num" %in% colnames(full.dry.pair)){
  full.dry.pair <- merge(full.dry.pair, site.num.df, by = "site" )
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

saveRDS(Yp, "outputs/growth_model/lag2_reg_cohort_only_re_t_pr_dry_yrs_site_rs_inter/Ypred_full_dataset_v4_allyears.rds")




dic.full <- dic.samples(lag2.model.by_structure_x_cohort_t_pr_int, n.chains = 3, n.iter=10000,thin = 15)
saveRDS(dic.full, "outputs/growth_model/model_summary/lag2_reg_cohort_only_re_t_pr_dry_yrs_site_rs_inter_DIC_v4_allyears.rds")

summary(samp.structure.cohort.re)

traceplot(samp.structure.cohort.re[,"beta2[2]"])
traceplot(samp.structure.cohort.re[,"beta2[1]"])

# most just around 1
gelman.diag(samp.structure.cohort.re, multivariate = FALSE)
acfplot(samp.structure.cohort.re)


autocorr.plot(samp.structure.cohort.re [[1]][ , 'beta2[2]'], auto.layout = FALSE, lwd = 4, col = "red", lag.max = 100)

#Extract the samples for each parameter for a basic exploration of effects

samps   <- samp.structure.cohort.re[[1]]
saveRDS(samps, "outputs//growth_model/lag2_reg_cohort_only_re_t_pr_dry_yrs_site_rs_inter/samps_v4_all_years.rds")
samps <- readRDS("outputs//growth_model/lag2_reg_cohort_only_re_t_pr_dry_yrs_site_rs_inter/samps_v4_all_years.rds")

Yp.samps <- Yp
#Yprobe.samps <- Y.probe
alpha.samps  <- samps[,1:9]# 
beta2.samps <- samps[,12:13]
beta3.samps <- samps[,14:15]
beta4.samps <- samps[,16:17]
beta5.samps <- samps[,18:19]
beta6.samps <- samps[,20:21]
beta7.samps <- samps[,22:23]
mu_beta.samps <- samps[,24:30]
sigma.samps <- samps[,31]
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
png(width = 6, height = 5, units = "in", res = 300, "outputs/growth_model/lag2_reg_cohort_only_re_t_pr_dry_yrs_site_rs_inter/pred_vs_obs_full_dataset_v2_all_years.png")
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

png(height = 4, width = 4.5, units = "in", res = 300, "outputs/growth_model/lag2_reg_cohort_only_re_t_pr_dry_yrs_site_rs_inter/predicted_growth_barplot_v4_all_years.png")
mean.pred.growth
dev.off()


saveRDS(Yp.full, "outputs/growth_model/lag2_reg_cohort_only_re_t_pr_dry_yrs_site_rs_inter/predicted_growth_YP_v4_all_years.rds")
saveRDS(Yp.summary.cohort, "outputs/growth_model/lag2_reg_cohort_only_re_t_pr_dry_yrs_site_rs_inter/predicted_growth_YP_summary_v4_all_years.rds")


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
library(HDInterval)
beta_diffsm <- beta_diffs %>% gather(beta, difference) %>% group_by(beta) %>% 
  summarise(mean = mean(difference), 
            hdi.low = hdi(difference)[1],
            hdi.high = hdi(difference)[2]) 


beta.diffs.plot.mod.past <- ggplot(beta_diffsm, aes(beta, mean))+geom_bar(stat = "identity")+geom_errorbar(data = beta_diffsm,aes(ymin=hdi.low, ymax=hdi.high), color = "grey", alpha = 0.8, size = 0.5, width = 0.2)+ylab("Difference in beta parameter (Modern - Past)")+xlab("beta parameters")+theme_bw(base_size = 16)+theme(panel.grid = element_blank())

png(height = 4, width = 4.5, units = "in", res = 300, "outputs/growth_model/lag2_reg_cohort_only_re_t_pr_dry_yrs_site_rs_inter/beta_differences_hdi_all_years.png")
beta.diffs.plot.mod.past
dev.off()

# save for later plot:
saveRDS(beta_diffsm, "outputs/growth_model/lag2_reg_cohort_only_re_t_pr_dry_yrs_site_rs_inter/beta_differences_cohorts_v4_all_years.rds")

saveRDS(beta_diffs, "outputs/growth_model/lag2_reg_cohort_only_re_t_pr_dry_yrs_site_rs_inter/beta_diffs_cohorts_v4_all_years.rds")

# calculate MSE & BIAS:

MSE1   <- mean((colMeans(exp(Yp.samps))-test.dry.pair$RWI)^2)
BIAS1  <- mean(colMeans(exp(Yp.samps))-test.dry.pair$RWI)

# write model summary output to a file!

model.summary <- data.frame(model = "lag2_reg_cohort_only_re_t_pr_dry_yrs_site_rs_inter", 
                            model.summary = "beta1[site[i]] + beta2[struct.cohort[i]]*DI.scaled[i] +   beta3[struct.cohort[i]]*DBH.scaled[i]+beta4[struct.cohort[i]]*log_RWI_1[i]+beta5[struct.cohort[i]]*log_RWI_2[i]  + beta6[struct.cohort[i]]*Temp.scaled[i] + beta7[struct.cohort[i]]*Temp.scaled[i]*DI.scaled[i] ",
                            MSE = MSE1, 
                            BIAS = BIAS1, 
                            Rsq = pred.obs$r.squared)

write.csv(model.summary, paste0("outputs/growth_model/model_summary/", model.summary$model, "_summary_v4_all_years.csv"), row.names = FALSE)


# summarise the datasource to make model comparisons for best fit models
data.summary <- data.frame(model = "lag2_reg_cohort_only_re_t_pr_dry_yrs_site_rs_inter", 
                           train = "cohort.omit.dry.years", 
                           DI.scaled = "MAP")

write.csv(data.summary, paste0("outputs/growth_model/model_summary/", data.summary$model, "_data_v4_all_years.csv"), row.names = FALSE)





#----------------------------------------------------------------------------------------------
#----------------------Tree Ring Growth Model with Cohort-Structure Random Slopes---------------------------
#----------------------------------------------------------------------------------------------


population_model_site_int_structure_x_cohort_re_lag2_temp_MAP_interaction.f <- "model{

# for each the overall population include re for sites:

# Likelihood
for(i in 1:n){
# process model
Y[i]   ~ dnorm(gfunc[i], inv.var) # where Yi is already log transformed

# function g()
gfunc[i] <- beta1[site[i]] + beta2[struct.cohort[i]]*DI.scaled[i] +   beta3[struct.cohort[i]]*DBH.scaled[i]+beta4[struct.cohort[i]]*log_RWI_1[i]+beta5[struct.cohort[i]]*log_RWI_2[i]  + beta6[struct.cohort[i]]*Temp.scaled[i] + beta7[struct.cohort[i]]*Temp.scaled[i]*DI.scaled[i] # inclued beta 7 as interaction term 
   

}



# Prediction from known data
  for(i in 1:np){
    Yp[i]  ~ dnorm(mup[i],inv.var)
    mup[i] <- beta1[site.p[i]] + beta2[struct.cohort.p[i]]*DI.scaled.p[i] + beta3[struct.cohort.p[i]]*DBH.scaled.p[i] +beta4[struct.cohort.p[i]]*log_RWI_1.p[i] +beta5[struct.cohort.p[i]]*log_RWI_2.p[i] + beta6[struct.cohort.p[i]]*Temp.scaled.p[i]+ beta7[struct.cohort.p[i]]*Temp.scaled.p[i]*DI.scaled.p[i]
  }

# # probe for prediction plots:
   for(i in 1:nprobe){
     Yprobe[i]  ~ dnorm(muprobe[i],inv.var)
     muprobe[i] <- beta1[site.probe[i]] + beta2[struct.cohort.probe[i]]*DI.scaled.probe[i] + beta3[struct.cohort.probe[i]]*DBH.scaled.probe[i] +beta4[struct.cohort.probe[i]]*log_RWI_1.probe[i] + beta5[struct.cohort.probe[i]]*log_RWI_2.probe[i] + beta6[struct.cohort.probe[i]]*Temp.scaled.probe[i] + beta7[struct.cohort.probe[i]]*Temp.scaled.probe[i]*DI.scaled.probe[i]
   }
# 
# # project into the future (assuming no further change in drought senseiivty):
#   for(i in 1:nfut){
#    Yfut[i]  ~ dnorm(mufut[i],inv.var)
#     mufut[i] <- beta1[site.fut[i]] + beta2[struct.cohort.fut[i]]*DI.scaled.fut[i] + beta3[struct.cohort.fut[i]]*DBH.scaled.fut[i] +beta4[struct.cohort.fut[i]]*log_RWI_1.fut[i] +beta5[struct.cohort.fut[i]]*log_RWI_2.fut[i] + beta6[struct.cohort.fut[i]]*Temp.scaled.fut[i] + (beta7[struct.cohort.fut[i]])*Temp.scaled.fut[i]*DI.scaled.fut[i]
#   }
# 
# # project into the future (assuming 35% further change in drought senseiivty):
#   for(i in 1:nfut){
#    Yfut.35[i]  ~ dnorm(mufut2[i],inv.var)
#     mufut2[i] <- beta1[site.fut[i]] + (beta2[struct.cohort.fut[i]]*0.92)*DI.scaled.fut[i] + beta3[struct.cohort.fut[i]]*DBH.scaled.fut[i] +beta4[struct.cohort.fut[i]]*log_RWI_1.fut[i] +beta5[struct.cohort.fut[i]]*log_RWI_2.fut[i] + beta6[struct.cohort.fut[i]]*Temp.scaled.fut[i]+ (beta7[struct.cohort.fut[i]])*Temp.scaled.fut[i]*DI.scaled.fut[i]
#   }
# 
# 
# 
# # project into the future (assuming 50% further change in drought senseiivty):
#   for(i in 1:nfut){
#    Yfut.50[i]  ~ dnorm(mufut3[i],inv.var)
#     mufut3[i] <- beta1[site.fut[i]] + (beta2[struct.cohort.fut[i]]*0.85)*DI.scaled.fut[i] + beta3[struct.cohort.fut[i]]*DBH.scaled.fut[i] +beta4[struct.cohort.fut[i]]*log_RWI_1.fut[i] +beta5[struct.cohort.fut[i]]*log_RWI_2.fut[i] + beta6[struct.cohort.fut[i]]*Temp.scaled.fut[i]+ (beta7[struct.cohort.fut[i]])*Temp.scaled.fut[i]*DI.scaled.fut[i]
#   }
# 
# 
# # project into the future (assuming 8% further change in drought senseiivty & temperature):
#   for(i in 1:nfut){
#    Yfut.35.35[i]  ~ dnorm(mufut35[i],inv.var)
#     mufut35[i] <- beta1[site.fut[i]] + (beta2[struct.cohort.fut[i]]*0.92)*DI.scaled.fut[i] + beta3[struct.cohort.fut[i]]*DBH.scaled.fut[i] +beta4[struct.cohort.fut[i]]*log_RWI_1.fut[i] +beta5[struct.cohort.fut[i]]*log_RWI_2.fut[i] + (beta6[struct.cohort.fut[i]]*1.08)*Temp.scaled.fut[i]+ (beta7[struct.cohort.fut[i]]*0.92)*Temp.scaled.fut[i]*DI.scaled.fut[i]
#   }
# 


# # project into the future (assuming 7% further change in drought senseiivty):
#   for(i in 1:nfut){
#    Yfut.50.50[i]  ~ dnorm(mufut50[i],inv.var)
#     mufut50[i] <- beta1[site.fut[i]] + (beta2[struct.cohort.fut[i]]*0.85)*DI.scaled.fut[i] + beta3[struct.cohort.fut[i]]*DBH.scaled.fut[i] +beta4[struct.cohort.fut[i]]*log_RWI_1.fut[i] +beta5[struct.cohort.fut[i]]*log_RWI_2.fut[i] + (beta6[struct.cohort.fut[i]]*1.15)*Temp.scaled.fut[i]+(beta7[struct.cohort.fut[i]]*0.85)*Temp.scaled.fut[i]*DI.scaled.fut[i]
#   }

# Assume normal priors for betas, but generate a beta + alpha for each ageclass
for(s in 1:length(SF)){

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
one.two <- beta2[1] - beta2[2]
two.three<- beta2[2] - beta2[3]
three.four<- beta2[3] - beta2[4]
two.four <- beta2[2] - beta2[4]


}"



full.dry.pair <- dry.yrs.paired  

# assign site numbers to each site:

site.num.df <- data.frame(site = as.character(unique(train.dry.pair$site)), 
                          site.num = 1:length(as.character(unique(train.dry.pair$site))))

#if site num is not in the df, add it
if(! "site.num" %in% colnames(full.dry.pair)){
  #train.dry.pair <- merge(train.dry.pair, site.num.df, by = "site" )
  #test.dry.pair <- merge(test.dry.pair, site.num.df, by = "site" )
  full.dry.pair <- merge(full.dry.pair, site.num.df, by = "site")
}

# generate probe dataset:
DIprobe <- round(seq(range(train.dry.pair$MAP.scaled)[1], range(train.dry.pair$MAP.scaled)[2], by = 0.75), 3)
DBHprobe <- round(seq(range(train.dry.pair$DBH.scaled)[1], range(train.dry.pair$DBH.scaled)[2], by = 0.75), 3)
Tempprobe <- round(seq(range(train.dry.pair$T.scaled)[1], range(train.dry.pair$T.scaled)[2], by = 0.75), 3)
RWI1probe <- round(seq(range(log(train.dry.pair$RWI_1))[1], range(log(train.dry.pair$RWI_1))[2], by = 0.5), 3)
RWI2probe <- round(seq(range(log(train.dry.pair$RWI_2))[1], range(log(train.dry.pair$RWI_2))[2], by = 0.5), 3)

# expand into full probe
probe <- expand.grid(DI.scaled = DIprobe, DBH.scaled = DBHprobe, T.scaled = Tempprobe,
                     RWI_1 = RWI1probe, RWI_2= RWI2probe, struct.cohort.code= 1:4, site.num = unique(train.dry.pair$site.num))

sit.cohorts <- unique(train.dry.pair[,c("struct.cohort.code", "site.num")])
probe <- merge(probe, sit.cohorts, by = c("struct.cohort.code", "site.num")) # make sure that each site only has savanna or forest in the probe

# make probe smaller to help with saving:
short.probe <- probe[probe$RWI_1 %in% c("-1.609", "0.391", "1.891") & probe$RWI_2 %in% c("-1.484", "0.016", "1.516") & probe$DBH.scaled %in% c("-0.413", "0.337", "1.837"),]

#short.probe <- probe

# make a probe for future climate projections at our sites!
rcp85 <- rcp85[!rcp85$site %in% c("COR", "GLL4", "HIC", "PLE", "PVC", "STC", "TOW"),]
future.probe_70 <- data.frame(MAP.scaled = rcp85$PR_85_70.scaled, DBH.scaled= -0.413, 
                              T.scaled = rcp85$Tx_70.scaled,RWI_1 = 0.103, 
                              RWI_2 = 0.103,  struct.cohort.code = rcp85$struct.cohort.code, site = rcp85$site, model = rcp85$model)
future.probe_70$year <- 2070
future.probe_50 <- data.frame(MAP.scaled= rcp85$Pr_85_50.scaled, DBH.scaled = -0.413, 
                              T.scaled  = rcp85$Tx_50.scaled,RWI_1 = 0.103, 
                              RWI_2 = 0.103, struct.cohort.code = rcp85$struct.cohort.code, site = rcp85$site,  model = rcp85$model)
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
    model = "none",
    year = 2000
  )



Average.prob <- data.frame(MAP.scaled= rcp85$Pr_85_50.scaled, DBH.scaled = -0.413, 
                           T.scaled  = rcp85$Tx_50.scaled,RWI_1 = 0.103, 
                           RWI_2 = 0.103, struct.cohort.code = rcp85$struct.cohort.code, site = rcp85$site, model = rcp85$model)
future.probe_50$year <- 2050


fut <- rbind(future.probe_50, future.probe_70, train.clim.summary[,c("MAP.scaled", "DBH.scaled",  "T.scaled", "RWI_1", "RWI_2", "struct.cohort.code", "site","year","model")])


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

lag2.model.by_structure_x_cohort_t_pr_int <- jags.model(textConnection(population_model_site_int_structure_x_cohort_re_lag2_temp_MAP_interaction.f), 
                                                        data = list(Y=log(train.dry.pair$RWI), n=length(train.dry.pair$RWI), DI.scaled = train.dry.pair$MAP.scaled, DBH.scaled = train.dry.pair$DBH.scaled,log_RWI_1 = log(train.dry.pair$RWI_1),log_RWI_2 = log(train.dry.pair$RWI_2), Temp.scaled = train.dry.pair$T.scaled, struct.cohort = as.numeric(train.dry.pair$struct.cohort.code), SF = as.numeric(unique(train.dry.pair$struct.cohort.code)), site = train.dry.pair$site.num, Nsites = length(unique(train.dry.pair$site)),
                                                                    
                                                                    struct.cohort.p = as.numeric(full.dry.pair$struct.cohort.code), DBH.scaled.p = full.dry.pair$DBH.scaled, DI.scaled.p = full.dry.pair$MAP.scaled, log_RWI_1.p = log(full.dry.pair$RWI_1), log_RWI_2.p = log(full.dry.pair$RWI_2), Temp.scaled.p = full.dry.pair$T.scaled, site.p = full.dry.pair$site.num, np = length(as.numeric(full.dry.pair$struct.cohort.code)),
                                                                    
                                                                     struct.cohort.probe = as.numeric(short.probe$struct.cohort.code), DBH.scaled.probe = short.probe$DBH.scaled, DI.scaled.probe = short.probe$DI.scaled, log_RWI_1.probe = short.probe$RWI_1, log_RWI_2.probe =short.probe$RWI_2, Temp.scaled.probe = short.probe$T.scaled, site.probe = short.probe$site.num, nprobe = length(as.numeric(short.probe$struct.cohort.code)), 
                                                                     
                                                                     struct.cohort.fut = as.numeric(long.fut$struct.cohort.code), DBH.scaled.fut = long.fut$DBH.scaled, DI.scaled.fut = long.fut$MAP.scaled, log_RWI_1.fut = long.fut$RWI_1, log_RWI_2.fut =long.fut$RWI_2, Temp.scaled.fut = long.fut$T.scaled, site.fut = long.fut$site.num, nfut = length(as.numeric(long.fut$struct.cohort.code))
                                                                    
                                                                    
                                                        ),
                                                        n.chains = 3, n.adapt = 100)

update(lag2.model.by_structure_x_cohort_t_pr_int, 10000); # Burnin for 1000 samples to start, then go higher later


samp.structure.cohort.re <- coda.samples(lag2.model.by_structure_x_cohort_t_pr_int, 
                                         variable.names=c("beta1", "beta2","beta3","beta4","beta5","beta6","beta7","sigma", "mu_beta1", "mu_beta2","mu_beta3", "mu_beta4","mu_beta5", "mu_beta6", "mu_beta7"), 
                                         n.chains = 3, n.iter=10000, thin = 15)

samp.structure.cohort.re <- coda.samples(lag2.model.by_structure_x_cohort_t_pr_int, 
                                         variable.names=c("beta1", "beta2","beta3","beta4","beta5","beta6","beta7","sigma", "mu_beta1", "mu_beta2","mu_beta3", "mu_beta4","mu_beta5", "mu_beta6", "mu_beta7", "Yprobe"), 
                                         n.chains = 3, n.iter=10000, thin = 15)


# beta2.diffs <- coda.samples(lag2.model.by_structure_x_cohort_t_pr_int, 
#                      variable.names=c("one.two", "two.three", "three.four", "two.four"), 
#                     n.chains = 3, n.iter=10000, thin = 15)

Yp <- coda.samples(lag2.model.by_structure_x_cohort_t_pr_int, 
                   variable.names=c("Yp"), 
                   n.chains = 3, n.iter=10000, thin = 15)


Y.probe <- coda.samples(lag2.model.by_structure_x_cohort_t_pr_int, 
                        variable.names=c("Yprobe"), 
                        n.chains = 3, n.iter=5000, thin = 15)

Y.fut <- coda.samples(lag2.model.by_structure_x_cohort_t_pr_int, 
                      variable.names=c("Yfut"), 
                      n.chains = 3, n.iter=10000, thin = 15)

Y.fut.35 <- coda.samples(lag2.model.by_structure_x_cohort_t_pr_int, 
                         variable.names=c("Yfut.35"), 
                         n.chains = 3, n.iter=10000, thin = 15)

Y.fut.50 <- coda.samples(lag2.model.by_structure_x_cohort_t_pr_int, 
                         variable.names=c("Yfut.50"), 
                         n.chains = 3, n.iter=10000, thin = 15)

Y.fut.35.35 <- coda.samples(lag2.model.by_structure_x_cohort_t_pr_int, 
                            variable.names=c("Yfut.35.35"), 
                            n.chains = 3, n.iter=10000, thin = 15)

Y.fut.50.50 <- coda.samples(lag2.model.by_structure_x_cohort_t_pr_int, 
                            variable.names=c("Yfut.50.50"), 
                            n.chains = 3, n.iter=10000,thin = 15)


dic.full <- dic.samples(lag2.model.by_structure_x_cohort_t_pr_int, n.chains = 3, n.iter=10000,thin = 15)
saveRDS(dic.full, "outputs/growth_model/model_summary/lag2_reg_struct_x_cohort_re_t_pr_dry_yrs_site_rs_inter_DIC_v4_all_years.rds")

summary(samp.structure.cohort.re)

traceplot(samp.structure.cohort.re[,"beta2[4]"])

# most just around 1
gelman.diag(samp.structure.cohort.re, multivariate = FALSE)
acfplot(samp.structure.cohort.re)


autocorr.plot(samp.structure.cohort.re [[1]][ , 'beta2[2]'], auto.layout = FALSE, lwd = 4, col = "red", lag.max = 100)

#Extract the samples for each parameter for a basic exploration of effects

samps   <- samp.structure.cohort.re[[1]]
saveRDS(samps, "outputs//growth_model/lag2_reg_struct_x_cohort_re_t_pr_dry_yrs_site_rs_inter/samps_v4_all_years.rds")
#samps <- readRDS("outputs//growth_model/lag2_reg_struct_x_cohort_re_t_pr_dry_yrs_site_rs_inter/samps.rds")
#test.dry <- readRDS("outputs//growth_model/lag2_reg_struct_x_cohort_re_t_pr_dry_yrs_site_rs_inter/test.rds")
#train.dry <- readRDS("outputs//growth_model/lag2_reg_struct_x_cohort_re_t_pr_dry_yrs_site_rs_inter/train.rds")
saveRDS(Yp,"outputs/growth_model/lag2_reg_struct_x_cohort_re_t_pr_dry_yrs_site_rs_inter/YP.samps_v4_all_years.rds")
# Y.probe <- readRDS("outputs/growth_model/lag2_reg_struct_x_cohort_re_t_pr_dry_yrs_site_rs_inter/full_yprobe_preds.rds")
#Yfut.samps <- samps[, 1:length(fut$RWI_1)]
#samps <- samps[,(length(fut$RWI_1)+1):length(colnames(samps))]
Yp.samps <- Yp
Yprobe.samps <- Y.probe
alpha.samps  <- samps[,1:11]# one alpha for each of 4 cohort-strcuture groups
beta2.samps <- samps[,12:15]
beta3.samps <- samps[,16:19]
beta4.samps <- samps[,20:23]
beta5.samps <- samps[,24:27]
beta6.samps <- samps[,28:31]
beta7.samps <- samps[,32:35]
mu_beta.samps <- samps[,37:42]
sigma.samps <- samps[,36]
sigma_betas <- samps[,30:35]



# plot predicted vs observed and assess model fit:
Yp.samps <- data.frame(Yp.samps[[1]]) 
Yp.m <- melt(Yp.samps)
Yp.summary <- Yp.m %>% group_by(variable) %>% dplyr::summarise(Predicted = mean(exp(value)),
                                                               ci.hi = quantile(exp(value),0.975),
                                                               ci.lo = quantile(exp(value),0.025))

test.dry.pair2 <- full.dry.pair
test.dry.pair2$variable <- unique(Yp.m$variable)

Yp.summary$Observed <- test.dry.pair2$RWI



pred.obs <- summary(lm(colMeans(exp(Yp.samps)) ~ test.dry.pair2$RWI))

p.o.plot <- ggplot(Yp.summary, aes(Observed, Predicted))+geom_point(color = "black", size = 0.5)+geom_errorbar(data = Yp.summary,aes(ymin=ci.lo, ymax=ci.hi), color = "grey", alpha = 0.5)+geom_point(data = Yp.summary, aes(Observed, Predicted), color = "black", size = 0.5)+geom_abline(aes(slope = 1, intercept = 0), color = "red", linetype = "dashed")+ylim(0, 8)+xlim(0,8)+geom_text(data=data.frame(pred.obs$r.squared), aes( label = paste("R^2: ", pred.obs$r.squared, sep="")),parse=T,x=1, y=7)+theme_bw(base_size = 15)

# note best model fit so far
png(width = 6, height = 5, units = "in", res = 300, "outputs/growth_model/lag2_reg_struct_x_cohort_re_t_pr_dry_yrs_site_rs_inter/pred_vs_obs_v4_all_years.png")
p.o.plot
dev.off()

# plot average growth boxplots for modern & past:

Yp.full <- left_join(Yp.m, test.dry.pair2, by = "variable")


# get estimates for predicted growth:
Yp.summary.cohort  <- Yp.full %>% group_by(struct.cohort) %>% dplyr::summarise(Predicted = mean(exp(value)),
                                                                               ci.hi = quantile(exp(value),0.975),
                                                                               ci.lo = quantile(exp(value),0.025), 
                                                                               Observed = mean(RWI))



ggplot(Yp.summary.cohort, aes(struct.cohort,Predicted, fill = struct.cohort))+geom_bar(stat = "identity")+geom_errorbar(data = Yp.summary.cohort,aes(ymin=ci.lo, ymax=ci.hi), color = "grey", alpha = 0.8, size = 0.5, width = 0.2)+ylim(0, 6)+theme_bw()

#ggplot(Yp.summary.cohort, aes(class, Predicted, fill= class))+geom_boxplot()+facet_wrap(~DBHclass)
#ggplot(Yp.summary.cohourt, aes(class, Observed, fill= class))+geom_boxplot()+facet_wrap(~site)

# calculate MSE & BIAS:

MSE1   <- mean((colMeans(exp(Yp.samps))-test.dry.pair$RWI)^2)
BIAS1  <- mean(colMeans(exp(Yp.samps))-test.dry.pair$RWI)

# write model summary output to a file!

model.summary <- data.frame(model = "lag2_reg_struct_x_cohort_re_t_pr_dry_yrs_site_rs_inter", 
                            model.summary = "beta1[site[i]] + beta2[struct.cohort[i]]*DI.scaled[i] +   beta3[struct.cohort[i]]*DBH.scaled[i]+beta4[struct.cohort[i]]*log_RWI_1[i]+beta5[struct.cohort[i]]*log_RWI_2[i]  + beta6[struct.cohort[i]]*Temp.scaled[i] + beta7[struct.cohort[i]]*Temp.scaled[i]*DI.scaled[i] ",
                            MSE = MSE1, 
                            BIAS = BIAS1, 
                            Rsq = pred.obs$r.squared)

write.csv(model.summary, paste0("outputs/growth_model/model_summary/", model.summary$model, "_summary_all_years.csv"), row.names = FALSE)


# summarise the datasource to make model comparisons for best fit models
data.summary <- data.frame(model = "lag2_reg_struct_x_cohort_re_t_pr_dry_yrs_site_rs_inter", 
                           train = "cohort.omit.dry.years", 
                           DI.scaled = "MAP")

write.csv(data.summary, paste0("outputs/growth_model/model_summary/", data.summary$model, "_data_v4_all_years.csv"), row.names = FALSE)



# get difference between beta2.samps: (i.e are they different?)
oneminustwo <- beta2.samps[,1] - beta2.samps[,2]
oneminusthree <- beta2.samps[,1] - beta2.samps[,3]
oneminusfour <- beta2.samps[,1] - beta2.samps[,4]
twominusthree <- beta2.samps[,2] - beta2.samps[,3]
twominusfour <- beta2.samps[,2] - beta2.samps[,4]
threeminusfour <- beta2.samps[,3] - beta2.samps[,4]




# plot marginal distributions of cohort + structure specific parameters:
a <- data.frame(alpha.samps)
colnames(a) <- unique(train.dry.pair$site)#[order(unique(train.dry.pair[,c("site", "site.num")])[,2])])
a$num <- rownames(a)
a.m <- melt(a, id.vars=c("num"))
alpha.mplots <- ggplot(a.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Random intercepts")


b2 <- data.frame(beta2.samps)
colnames(b2) <-c(unique(train.dry.pair$struct.cohort)[order(unique(train.dry.pair[,c("struct.cohort", "struct.cohort.code")])[,2])])
#colnames(b2) <- c(paste0(c(unique(train.dry.pair$struct.cohort))))
b2$num <- rownames(b2)
b2.m <- melt(b2, id.vars=c("num"))
b2.mplots <- ggplot(b2.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Precipitation Index slope")

b3 <- data.frame(beta3.samps)
colnames(b3) <- c(unique(train.dry.pair$struct.cohort)[order(unique(train.dry.pair[,c("struct.cohort", "struct.cohort.code")])[,2])])
b3$num <- rownames(b3)
b3.m <- melt(b3, id.vars=c("num"))
b3.mplots <- ggplot(b3.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("DBH Index slope")


b4 <- data.frame(beta4.samps)
colnames(b4) <- c(unique(train.dry.pair$struct.cohort)[order(unique(train.dry.pair[,c("struct.cohort", "struct.cohort.code")])[,2])])
b4$num <- rownames(b4)
b4.m <- melt(b4, id.vars=c("num"))
b4.mplots <- ggplot(b4.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Log_RWI-1 Index slope")

b5 <- data.frame(beta5.samps)
colnames(b5) <- c(unique(train.dry.pair$struct.cohort)[order(unique(train.dry.pair[,c("struct.cohort", "struct.cohort.code")])[,2])])
b5$num <- rownames(b5)
b5.m <- melt(b5, id.vars=c("num"))
b5.mplots <- ggplot(b5.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Log_RWI-2 Index slope")

b6 <- data.frame(beta6.samps)
colnames(b6) <- c(unique(train.dry.pair$struct.cohort)[order(unique(train.dry.pair[,c("struct.cohort", "struct.cohort.code")])[,2])])
b6$num <- rownames(b6)
b6.m <- melt(b6, id.vars=c("num"))
b6.mplots <- ggplot(b6.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Summer Temperature slope")

b7 <- data.frame(beta7.samps)
colnames(b7) <- c(unique(train.dry.pair$struct.cohort)[order(unique(train.dry.pair[,c("struct.cohort", "struct.cohort.code")])[,2])])
b7$num <- rownames(b7)
b7.m <- melt(b7, id.vars=c("num"))
b7.mplots <- ggplot(b7.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Precip*Temp slope")



png(height = 10, width = 12, units = "in", res = 300,"outputs/growth_model/lag2_reg_struct_x_cohort_re_t_pr_dry_yrs_site_rs_inter/drougt_beta_marginal_distn_bycohort_v4_all_years.png")
cowplot::plot_grid(alpha.mplots, b2.mplots, b3.mplots, b4.mplots, b5.mplots, b6.mplots, b7.mplots)
dev.off()



# ------------------------- DOTPLOT MODEL SUMMARIES -----------------------
# make dotplots for all the factors in the model--
# get summaries by struct-cohort class from the melted samples:

a1.sum <- a.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                          Ci.low = quantile(value, 0.025), 
                                                          Ci.high = quantile(value, 0.975))
a1.sum$variable <- unique(train.dry.pair$site)

df.site.struct <- unique(train.dry.pair[,c("site", "structure")])
a1.sum <- merge(a1.sum, df.site.struct, by.x = "variable", by.y = "site")
a1.sum$structure <- factor(a1.sum$structure, levels = c( "Forest", "Savanna"))

b2.sum <- b2.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))
b2.sum$variable <- factor(b2.sum$variable, levels = c( "Modern-Forest", "Modern-Savanna", "Past-Forest", "Past-Savanna"))


b3.sum <- b3.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))
b3.sum$variable <- factor(b3.sum$variable, levels = c( "Modern-Forest", "Modern-Savanna", "Past-Forest", "Past-Savanna"))


b4.sum <- b4.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))
b4.sum$variable <- factor(b4.sum$variable, levels = c( "Modern-Forest", "Modern-Savanna", "Past-Forest", "Past-Savanna"))

b5.sum <- b5.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))
b5.sum$variable <- factor(b5.sum$variable, levels = c( "Modern-Forest", "Modern-Savanna", "Past-Forest", "Past-Savanna"))

b6.sum <- b6.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))
b6.sum$variable <- factor(b6.sum$variable, levels = c( "Modern-Forest", "Modern-Savanna", "Past-Forest", "Past-Savanna"))


b7.sum <- b7.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))
b7.sum$variable <- factor(b7.sum$variable, levels = c(  "Modern-Forest", "Modern-Savanna", "Past-Forest", "Past-Savanna"))


# write out all the dotplots
int.dot <- ggplot(data.frame(a1.sum), aes(x = mean.val, y = variable, color = structure, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+geom_point()+xlim(-0.1, 0.25)+scale_color_manual(values = c("Savanna"='sienna4',
                                                                                                                                                                                                                                         "Forest"='forestgreen'))+theme_bw()+theme(legend.position = "none", axis.title.y = element_blank(), panel.grid = element_blank())+xlab("Estimated Intercept (alpha)")+xlim(-0.19, 0.71)+ geom_vline(xintercept = 0, linetype = "dashed")

b2.dot <- ggplot(data.frame(b2.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+geom_point()+xlim(-0.15, 0.8)+scale_color_manual(values = c("Past-Savanna"='#a6611a',
                                                                                                                                                                                                                                       "Modern-Savanna"='#dfc27d',
                                                                                                                                                                                                                                       "Modern-Forest"='#c7eae5',
                                                                                                                                                                                                                                       "Past-Forest"='#018571'))+theme_bw()+theme(legend.position = "none", axis.title.y = element_blank(), panel.grid = element_blank())+xlab("Estimated Drought Sensitivity (Beta2)")+xlim(-0.19, 0.71)+ geom_vline(xintercept = 0, linetype = "dashed")

b3.dot <- ggplot(data.frame(b3.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+geom_point()+xlim(-0.19, 0.71)+scale_color_manual(values = c("Past-Savanna"='#a6611a',
                                                                                                                                                                                                                                        "Modern-Savanna"='#dfc27d',
                                                                                                                                                                                                                                        "Modern-Forest"='#c7eae5',
                                                                                                                                                                                                                                        "Past-Forest"='#018571'))+theme_bw()+theme(legend.position = "none", axis.title.y = element_blank(), panel.grid = element_blank())+xlab("Estimated DBH Sensitivity (Beta3)")+xlim(-0.19, 0.71)+ geom_vline(xintercept = 0, linetype = "dashed")

b4.dot <- ggplot(data.frame(b4.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+geom_point()+scale_color_manual(values = c("Past-Savanna"='#a6611a',
                                                                                                                                                                                                                      "Modern-Savanna"='#dfc27d',
                                                                                                                                                                                                                      "Modern-Forest"='#c7eae5',
                                                                                                                                                                                                                      "Past-Forest"='#018571'))+theme_bw()+theme(legend.position = "none", axis.title.y = element_blank(), panel.grid = element_blank())+xlab("Estimated lag -1 growth coef. (Beta4)")+xlim(-0.19, 0.71)+ geom_vline(xintercept = 0, linetype = "dashed")

b5.dot <- ggplot(data.frame(b5.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+geom_point()+scale_color_manual(values = c("Past-Savanna"='#a6611a',
                                                                                                                                                                                                                      "Modern-Savanna"='#dfc27d',
                                                                                                                                                                                                                      "Modern-Forest"='#c7eae5',
                                                                                                                                                                                                                      "Past-Forest"='#018571'))+theme_bw()+theme(legend.position = "none", axis.title.y= element_blank(), panel.grid = element_blank())+xlab("Estimated lag -2 growth coef. (Beta5)")+xlim(-0.19, 0.71)+ geom_vline(xintercept = 0, linetype = "dashed")


b6.dot <- ggplot(data.frame(b6.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+geom_point()+scale_color_manual(values = c("Past-Savanna"='#a6611a',
                                                                                                                                                                                                                      "Modern-Savanna"='#dfc27d',
                                                                                                                                                                                                                      "Modern-Forest"='#c7eae5',
                                                                                                                                                                                                                      "Past-Forest"='#018571'))+theme_bw()+theme(legend.position = "none", axis.title.y = element_blank(), panel.grid = element_blank())+xlab("Estimated Max. Temp. sensitivity (Beta6)")+xlim(-0.19, 0.71)+ geom_vline(xintercept = 0, linetype = "dashed")

b7.dot <- ggplot(data.frame(b7.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+geom_point()+scale_color_manual(values = c("Past-Savanna"='#a6611a',
                                                                                                                                                                                                                      "Modern-Savanna"='#dfc27d',
                                                                                                                                                                                                                      "Modern-Forest"='#c7eae5',
                                                                                                                                                                                                                      "Past-Forest"='#018571'))+theme_bw()+theme(legend.position = "none", axis.title.y = element_blank(), panel.grid = element_blank())+xlab("Estimated Temp*Precip sensitivity (Beta7)")+xlim(-0.19, 0.71)+ geom_vline(xintercept = 0, linetype = "dashed")

png(height = 12, width = 5, units = "in", res = 300, "outputs/growth_model/lag2_reg_struct_x_cohort_re_t_pr_dry_yrs_site_rs_inter/full_dot_plot_cohort_struct_v4_all_years.png")
plot_grid(int.dot , b2.dot, b6.dot, b7.dot , b3.dot, b4.dot, b5.dot, ncol = 1, align = "v")
dev.off()


# -------------------- DOTPLOTS by modern vs. past only ---------------------
# make dotplots for all the factors in the model--
# get summaries by struct-cohort class from the melted samples:
# a does not vary by cohort 
a1.sum.age <- a1.sum

b2.m$cohort <- ifelse(b2.m$variable %in% c("Past-Forest", "Past-Savanna"), "Past", "Modern")
b2.sum.age <- b2.m %>% group_by(cohort) %>% dplyr::summarise(mean.val = mean(value),
                                                             Ci.low = quantile(value, 0.025), 
                                                             Ci.high = quantile(value, 0.975))
#b2.sum$variable <- factor(b2.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))

b3.m$cohort <- ifelse(b3.m$variable %in% c("Past-Forest", "Past-Savanna"), "Past", "Modern")

b3.sum.age <- b3.m %>% group_by(cohort) %>% dplyr::summarise(mean.val = mean(value),
                                                             Ci.low = quantile(value, 0.025), 
                                                             Ci.high = quantile(value, 0.975))
#b3.sum$variable <- factor(b3.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))

b4.m$cohort <- ifelse(b4.m$variable %in% c("Past-Forest", "Past-Savanna"), "Past", "Modern")

b4.sum.age <- b4.m %>% group_by(cohort) %>% dplyr::summarise(mean.val = mean(value),
                                                             Ci.low = quantile(value, 0.025), 
                                                             Ci.high = quantile(value, 0.975))
#b4.sum$variable <- factor(b4.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))

b5.m$cohort <- ifelse(b5.m$variable %in% c("Past-Forest", "Past-Savanna"), "Past", "Modern")

b5.sum.age <- b5.m %>% group_by(cohort) %>% dplyr::summarise(mean.val = mean(value),
                                                             Ci.low = quantile(value, 0.025), 
                                                             Ci.high = quantile(value, 0.975))
#b5.sum$variable <- factor(b5.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))
b6.m$cohort <- ifelse(b6.m$variable %in% c("Past-Forest", "Past-Savanna"), "Past", "Modern")

b6.sum.age <- b6.m %>% group_by(cohort) %>% dplyr::summarise(mean.val = mean(value),
                                                             Ci.low = quantile(value, 0.025), 
                                                             Ci.high = quantile(value, 0.975))
#b6.sum$variable <- factor(b6.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))

# write out all the dotplots
int.dot.age <- ggplot(data.frame(a1.sum.age), aes(x = mean.val, y = variable, color = structure, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+geom_point()+scale_color_manual(values = c("Savanna"='sienna4', 'Forest' = "forestgreen"))+theme_bw()+theme(panel.grid = element_blank(),legend.position = "none", axis.title.y = element_blank())+xlab("Estimated Intercept (alpha)")+xlim(-0.19, 0.71) + geom_vline(xintercept = 0, linetype = "dashed")

b2.dot.age <- ggplot(data.frame(b2.sum.age), aes(x = mean.val, y = cohort, color = cohort, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+geom_point()+xlim(-0.1, 0.25)+scale_color_manual(values = c("Past"='#2166ac', 'Modern' = "#b2182b"))+theme_bw()+theme(panel.grid = element_blank(),legend.position = "none", axis.title.y = element_blank())+xlab("Estimated Drought Sensitivity (Beta2)")+xlim(-0.19, 0.71)+ geom_vline(xintercept = 0, linetype = "dashed")

b3.dot.age <- ggplot(data.frame(b3.sum.age), aes(x = mean.val, y = cohort, color = cohort, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+geom_point()+xlim(-0.1, 0.25)+scale_color_manual(values = c("Past"='#2166ac', 'Modern' = "#b2182b"))+theme_bw()+theme(panel.grid = element_blank(),legend.position = "none", axis.title.y = element_blank())+xlab("Estimated DBH Sensitivity (Beta3)")+xlim(-0.19, 0.71)+ geom_vline(xintercept = 0, linetype = "dashed")

b4.dot.age <- ggplot(data.frame(b4.sum.age), aes(x = mean.val, y = cohort, color = cohort, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+geom_point()+scale_color_manual(values = c("Past"='#2166ac', 'Modern' = "#b2182b"))+theme_bw()+theme(panel.grid = element_blank(),legend.position = "none", axis.title.y = element_blank())+xlab("Estimated lag -1 growth coef. (Beta4)")+xlim(-0.3, 0.8)+ geom_vline(xintercept = 0, linetype = "dashed")

b5.dot.age <- ggplot(data.frame(b5.sum.age), aes(x = mean.val, y = cohort, color = cohort, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+geom_point()+scale_color_manual(values = c("Past"='#2166ac', 'Modern' = "#b2182b"))+theme_bw()+theme(panel.grid = element_blank(),legend.position = "none", axis.title.y= element_blank())+xlab("Estimated lag -1 growth coef. (Beta5)")+xlim(-0.19, 0.71)+ geom_vline(xintercept = 0, linetype = "dashed")


b6.dot.age <- ggplot(data.frame(b6.sum.age), aes(x = mean.val, y = cohort, color = cohort, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+geom_point()+theme_bw()+scale_color_manual(values = c("Past"='#2166ac', 'Modern' = "#b2182b"))+theme(panel.grid = element_blank(), legend.position = "none", axis.title.y = element_blank())+xlab("Estimated Max. Temp. sensitivity (Beta6)")+xlim(-0.19, 0.71)+ geom_vline(xintercept = 0, linetype = "dashed")

b7.dot.age <- ggplot(data.frame(b6.sum.age), aes(x = mean.val, y = cohort, color = cohort, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1, height = 0))+geom_point()+theme_bw()+scale_color_manual(values = c("Past"='#2166ac', 'Modern' = "#b2182b"))+theme(panel.grid = element_blank(), legend.position = "none", axis.title.y = element_blank())+xlab("Estimated Temp*Precip sensitivity (Beta7)")+xlim(-0.19, 0.71)+ geom_vline(xintercept = 0, linetype = "dashed")

png(height = 12, width = 5, units = "in", res = 300, "outputs/growth_model/lag2_reg_struct_x_cohort_re_t_pr_dry_yrs_site_rs_inter/full_dot_plot_cohort_only_v4_all_years.png")
plot_grid(int.dot.age, b2.dot.age, b6.dot.age,b7.dot.age, b3.dot.age, b4.dot.age, b5.dot.age, ncol = 1, align = "v")
dev.off()



png(height = 12, width = 8.5, units = "in", res = 300, "outputs/growth_model/lag2_reg_struct_x_cohort_re_t_pr_dry_yrs_site_rs_inter/full_dot_plot_cohort_and_cohortXstruct_v4_all_years.png")
plot_grid(int.dot.age+xlim(-0.18, 0.74),int.dot+xlim(-0.18, 0.74),
          b2.dot.age+xlim(-0.18, 0.74) , b2.dot+xlim(-0.18, 0.74), 
          b6.dot.age+xlim(-0.18, 0.74), b6.dot+xlim(-0.18, 0.74),
          b7.dot.age+xlim(-0.18, 0.74), b7.dot+xlim(-0.18, 0.74),
          b3.dot.age+xlim(-0.18, 0.74), b3.dot+xlim(-0.18, 0.74),
          b4.dot.age+xlim(-0.18, 0.74),  b4.dot+xlim(-0.18, 0.74),
          b5.dot.age+xlim(-0.18, 0.74), b5.dot+xlim(-0.18, 0.74), ncol = 2, align = "hv", labels = c("a","h",
                                                                                                     "b","i",
                                                                                                     "c","j",
                                                                                                     "d","k",
                                                                                                     "e","l",
                                                                                                     "f","m",
                                                                                                     "g", "n"), label_x = 0.28, label_y=0.97)
dev.off()



# ------------------ Do the site level intercepts correlate with site quality -----------
# read in information about site quality:
locs <- read.csv("/Users/kah/Documents/TreeRings/outputs/priority_sites_locs_with_soil_clim.csv")
locs <- data.frame(locs)
locs$code <- as.character(locs$code)
locs[9:12,]$code <- c( "GLL1", "GLL2", "GLL3", "GLL4")
sites <- c("COR", "HIC", "STC", "GLA", "TOW", "ENG", "UNC", "BON", "MOU", "GLL4", "GLL3", "GLL2", "GLL1", "PVC", "AVO", "PLE", "UNI")

# merge with a1.sum.age
site.summary <- merge(a1.sum.age, locs, by.x = "variable", by.y = "code")

# plot mean + 95% confidence interval vs. MAP
alpha.pr30yr <-ggplot(site.summary, aes(x = pr30yr, y = mean.val, color = structure, size = 2))+geom_errorbar( aes(ymin = Ci.low, ymax = Ci.high, size = 1,width = 0))+geom_point()+scale_color_manual(values = c("Savanna"='sienna4', 'Forest' = "forestgreen"))+theme(legend.position = "none")+xlab("Estimated Intercept (alpha)")+ylab("Total Precip")
# get an idea if there might be relationship
summary(lm(mean.val ~ pr30yr,data = site.summary))

# plot mean + 95% confidence interval vs. Tmean
alpha.tm30yr <-ggplot(site.summary, aes(x = tm30yr, y = mean.val, color = structure, size = 2))+geom_errorbar( aes(ymin = Ci.low, ymax = Ci.high, size = 1,width = 0))+geom_point()+scale_color_manual(values = c("Savanna"='sienna4', 'Forest' = "forestgreen"))+theme(legend.position = "none")+xlab("Estimated Intercept (alpha)")+ylab("Mean Temp")
# get an idea if there might be relationship
summary(lm(mean.val ~ tm30yr,data = site.summary))

# plot mean + 95% confidence interval vs. sand %
alpha.sand <-ggplot(site.summary, aes(x = sand, y = mean.val, color = structure, size = 2))+geom_errorbar( aes(ymin = Ci.low, ymax = Ci.high, size = 1,width = 0))+geom_point()+scale_color_manual(values = c("Savanna"='sienna4', 'Forest' = "forestgreen"))+theme(legend.position = "none")+xlab("Estimated Intercept (alpha)")+ylab("sand")#+stat_smooth(method = "lm")
# get an idea if there might be relationship
summary(lm(mean.val ~ sand,data = site.summary))

# plot mean + 95% confidence interval vs. ksat
alpha.ksat <-ggplot(site.summary, aes(x = ksat, y = mean.val, color = structure, size = 2))+geom_errorbar( aes(ymin = Ci.low, ymax = Ci.high, size = 1,width = 0))+geom_point()+scale_color_manual(values = c("Savanna"='sienna4', 'Forest' = "forestgreen"))+theme(legend.position = "none")+xlab("Estimated Intercept (alpha)")+ylab("ksat")
# get an idea if there might be relationship
summary(lm(mean.val ~ ksat,data = site.summary))

# plot mean + 95% confidence interval vs. awc

alpha.awc <- ggplot(site.summary, aes(x = awc, y = mean.val, color = structure, size = 2))+geom_errorbar( aes(ymin = Ci.low, ymax = Ci.high, size = 1,width = 0))+geom_point()+scale_color_manual(values = c("Savanna"='sienna4', 'Forest' = "forestgreen"))+theme(legend.position = "none")+xlab("Estimated Intercept (alpha)")+ylab("AWC")
# get an idea if there might be relationship
summary(lm(mean.val ~ awc,data = site.summary))

# plot mean + 95% confidence interval vs. n. species 

alpha.nspecies <- ggplot(site.summary, aes(x = nspecies, y = mean.val, color = structure, size = 2))+geom_errorbar( aes(ymin = Ci.low, ymax = Ci.high, size = 1,width = 0))+geom_point()+scale_color_manual(values = c("Savanna"='sienna4', 'Forest' = "forestgreen"))+theme(legend.position = "none")+xlab("Estimated Intercept (alpha)")+ylab("nspecies")#+stat_smooth(data = site.summary, aes(x = nspecies, y = mean.val), method = "lm")

# there seems to be no relationship between alpha and site characteristics
# but plot the relationships out here:
png(height = 10, width = 4, units = "in",res = 200, "outputs/growth_model/lag2_reg_struct_x_cohort_re_t_pr_dry_yrs_site_rs_inter/alpha_site_ints_vs_site_quality_all_years.png")
plot_grid(alpha.pr30yr, alpha.tm30yr, alpha.awc, alpha.sand, alpha.nspecies, ncol = 1, align = "v")
dev.off()

#---------------------- More beta summary plots -----------------------------

# plot slightly differently:
plot.dat.b2$forestclass <- ifelse(plot.dat.b2$class %in% c("Past-Forest", "Modern-Forest"), "Forest", "Savanna")

plot.dat.b2$ageclass <- ifelse(plot.dat.b2$class %in% c("Past-Forest", "Past-Savanna"), "Past", "Modern")
plot.dat.b2$ageclass <- factor(plot.dat.b2$ageclass, levels= c("Past", "Modern"))

# b2.dots.2 <- ggplot(plot.dat.b2, aes(x = b2.mean, y = ageclass, color = ageclass, size = 2))+geom_errorbarh( xmin = b2.lower, xmax = b2.upper, size = 2,height = 0)+geom_point()+xlim(-0.1, 0.25)+theme_bw(base_size = 18)+facet_wrap(~forestclass)+scale_color_manual(values = c("Past"='blue',
# "Modern"='red'))+coord_flip()+theme(legend.position = "none", axis.title.x = element_blank())+xlab("Estimated Drought Sensitivity")
# 
# png(width = 5, height = 4, units = "in", res = 300, "outputs/growth_model/lag2_reg_struct_x_cohort_re_t_pr_dry_yrs_site_rs_inter_site_rs/param_marginal_distn_bycohort_struct_rb.png")
# b2.dots.2
# dev.off()

b3 <- data.frame(beta3.samps)
colnames(b3) <- c(unique(train.dry.pair$struct.cohort)[order(unique(train.dry.pair[,c("struct.cohort", "struct.cohort.code")])[,2])])
b3$num <- rownames(b3)
b3.m <- melt(b3, id.vars=c("num"))
b3.mplots <- ggplot(b3.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("DBH slope")

b4 <- data.frame(beta4.samps)
colnames(b4) <- c(unique(train.dry.pair$struct.cohort)[order(unique(train.dry.pair[,c("struct.cohort", "struct.cohort.code")])[,2])])
b4$num <- rownames(b4)
b4.m <- melt(b4, id.vars=c("num"))
b4.mplots <- ggplot(b4.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("lag - 1 effect")

b5 <- data.frame(beta5.samps)
colnames(b5) <- c(unique(train.dry.pair$struct.cohort)[order(unique(train.dry.pair[,c("struct.cohort", "struct.cohort.code")])[,2])])
b5$num <- rownames(b5)
b5.m <- melt(b5, id.vars=c("num"))
b5.mplots <- ggplot(b5.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("lag - 1 effect")

library(cowplot)
png(width = 5, height = 10, units = "in", res = 300, "outputs/growth_model/lag2_reg_struct_x_cohort_re_t_pr_dry_yrs_site_rs_inter/param_marginal_distn_bycohort_struct_1_all_years.png")
plot_grid(alpha.mplots, b2.mplots, b3.mplots,b4.mplots, b5.mplots,ncol = 1)
dev.off()


# plot effect of temp conditional on MAP (interaction):
## Simulate the range of the moderating variable

T.sim <- seq(min(train.dry.pair$T.scaled), max(train.dry.pair$T.scaled), by = 0.1)

## Calculate conditional effect of X1 across the range of X2
#int.mcmc <- as.mcmc(samp.structure.cohort.re[[1]])
int.mcmc <- as.mcmc(samps)
int.mcmc.mat <- as.matrix(int.mcmc)
int.mcmc.dat <- data.frame(int.mcmc.mat)

int.1 <- matrix(rep(NA, nrow(int.mcmc.dat)*length(T.sim)), nrow = nrow(int.mcmc.dat))
int.2 <- matrix(rep(NA, nrow(int.mcmc.dat)*length(T.sim)), nrow = nrow(int.mcmc.dat))
int.3 <- matrix(rep(NA, nrow(int.mcmc.dat)*length(T.sim)), nrow = nrow(int.mcmc.dat))
int.4 <- matrix(rep(NA, nrow(int.mcmc.dat)*length(T.sim)), nrow = nrow(int.mcmc.dat))

for(i in 1:length(T.sim)){
  int.1[, i] <- int.mcmc.dat$beta2.1. + int.mcmc.dat$beta7.1. * T.sim[i]
  int.2[, i] <- int.mcmc.dat$beta2.2. + int.mcmc.dat$beta7.2. * T.sim[i]
  int.3[, i] <- int.mcmc.dat$beta2.3. + int.mcmc.dat$beta7.3. * T.sim[i]
  int.4[, i] <- int.mcmc.dat$beta2.4. + int.mcmc.dat$beta7.4. * T.sim[i]
}

## Note: the variance now comes from the posterior, not the vcov matrix

bayes.c.eff.mean1 <- apply(int.1, 2, mean)
bayes.c.eff.lower1 <- apply(int.1, 2, function(x) quantile(x, probs = c(0.025)))
bayes.c.eff.upper1 <- apply(int.1, 2, function(x) quantile(x, probs = c(0.975)))

bayes.c.eff.mean2 <- apply(int.2, 2, mean)
bayes.c.eff.lower2 <- apply(int.2, 2, function(x) quantile(x, probs = c(0.025)))
bayes.c.eff.upper2 <- apply(int.2, 2, function(x) quantile(x, probs = c(0.975)))


bayes.c.eff.mean3 <- apply(int.3, 2, mean)
bayes.c.eff.lower3 <- apply(int.3, 2, function(x) quantile(x, probs = c(0.025)))
bayes.c.eff.upper3 <- apply(int.3, 2, function(x) quantile(x, probs = c(0.975)))

bayes.c.eff.mean4 <- apply(int.4, 2, mean)
bayes.c.eff.lower4 <- apply(int.4, 2, function(x) quantile(x, probs = c(0.025)))
bayes.c.eff.upper4 <- apply(int.4, 2, function(x) quantile(x, probs = c(0.975)))

# summarise all the data -> this is ugly but it does the job
library(DMwR)

plot.dat1 <- data.frame(T.sim, Tmax = as.numeric(unscale(vals = T.sim, norm.data = T.scaled)), mean = bayes.c.eff.mean1, Ci.low = bayes.c.eff.lower1, Ci.high = bayes.c.eff.upper1, struct.cohort = 1)

plot.dat2 <- data.frame(T.sim,Tmax = as.numeric(unscale(vals = T.sim, norm.data = T.scaled)), mean = bayes.c.eff.mean2, Ci.low = bayes.c.eff.lower2, Ci.high = bayes.c.eff.upper2,struct.cohort = 2)
plot.dat3<- data.frame(T.sim, Tmax = as.numeric(unscale(vals = T.sim, norm.data = T.scaled)),mean = bayes.c.eff.mean3, Ci.low = bayes.c.eff.lower3, Ci.high = bayes.c.eff.upper3, struct.cohort=3)
plot.dat4 <- data.frame(T.sim, Tmax = as.numeric(unscale(vals = T.sim, norm.data = T.scaled)),mean = bayes.c.eff.mean4, Ci.low = bayes.c.eff.lower4, Ci.high = bayes.c.eff.upper4, struct.cohort = 4) 

plot.dat <- rbind(plot.dat1, plot.dat2, plot.dat3, plot.dat4)
plot.dat$struct.cohort <- as.factor(plot.dat$struct.cohort)

## Foundation for the plot & line for the posterior mean of the Bayesian conditional effect
p <- ggplot(plot.dat, aes(x = Tmax, y = mean, color = struct.cohort)) + geom_line( alpha = 0.8, size = 0.5)+ geom_ribbon(aes(ymin = Ci.low, ymax = Ci.high, fill = struct.cohort),  alpha = 0.2)

## Lines for the lower and upper bound of the Bayesian conditional effect
p <- p + geom_line(aes(x = Tmax, y = Ci.low, color = struct.cohort),  alpha = 0.8, size = 0.5) + geom_line(aes(x = Tmax, y = Ci.high, color = struct.cohort),  alpha = 0.8, size = 0.5)

p+ facet_wrap(~struct.cohort)

# 
# plot the conditional effect of Temperature across MAP:
MAP.sim <- seq(min(train.dry.pair$MAP.scaled), max(train.dry.pair$MAP.scaled), by = 0.1)

## Calculate conditional effect of X1 across the range of X2
#int.mcmc <- as.mcmc(samp.structure.cohort.re[[1]])

int.mcmc.mat <- as.matrix(int.mcmc)
int.mcmc.dat <- data.frame(int.mcmc.mat)

int.1 <- matrix(rep(NA, nrow(int.mcmc.dat)*length(MAP.sim)), nrow = nrow(int.mcmc.dat))
int.2 <- matrix(rep(NA, nrow(int.mcmc.dat)*length(MAP.sim)), nrow = nrow(int.mcmc.dat))
int.3 <- matrix(rep(NA, nrow(int.mcmc.dat)*length(MAP.sim)), nrow = nrow(int.mcmc.dat))
int.4 <- matrix(rep(NA, nrow(int.mcmc.dat)*length(MAP.sim)), nrow = nrow(int.mcmc.dat))

# simulate the effect of beta 1 conditional on Tmax
for(i in 1:length(MAP.sim)){
  int.1[, i] <- int.mcmc.dat$beta6.1. + int.mcmc.dat$beta7.1. * MAP.sim[i]
  int.2[, i] <- int.mcmc.dat$beta6.2. + int.mcmc.dat$beta7.2. * MAP.sim[i]
  int.3[, i] <- int.mcmc.dat$beta6.3. + int.mcmc.dat$beta7.3. * MAP.sim[i]
  int.4[, i] <- int.mcmc.dat$beta6.4. + int.mcmc.dat$beta7.4. * MAP.sim[i]
  
}

## Note: the variance now comes from the posterior, not the vcov matrix

bayes.c.eff.mean1 <- apply(int.1, 2, mean)
bayes.c.eff.lower1 <- apply(int.1, 2, function(x) quantile(x, probs = c(0.025)))
bayes.c.eff.upper1 <- apply(int.1, 2, function(x) quantile(x, probs = c(0.975)))

bayes.c.eff.mean2 <- apply(int.2, 2, mean)
bayes.c.eff.lower2 <- apply(int.2, 2, function(x) quantile(x, probs = c(0.025)))
bayes.c.eff.upper2 <- apply(int.2, 2, function(x) quantile(x, probs = c(0.975)))

bayes.c.eff.mean3 <- apply(int.3, 2, mean)
bayes.c.eff.lower3 <- apply(int.3, 2, function(x) quantile(x, probs = c(0.025)))
bayes.c.eff.upper3 <- apply(int.3, 2, function(x) quantile(x, probs = c(0.975)))

bayes.c.eff.mean4 <- apply(int.4, 2, mean)
bayes.c.eff.lower4 <- apply(int.4, 2, function(x) quantile(x, probs = c(0.025)))
bayes.c.eff.upper4 <- apply(int.4, 2, function(x) quantile(x, probs = c(0.975)))


# summarise all the data -> this is ugly but it does the job
library(DMwR)

plot.dat1 <- data.frame(MAP.sim,MAP = as.numeric(unscale(vals = MAP.sim, norm.data = MAP.scaled)) , mean = bayes.c.eff.mean1, Ci.low = bayes.c.eff.lower1, Ci.high = bayes.c.eff.upper1, cohort = "Past-Forest", structure = "Forest")

plot.dat2 <- data.frame(MAP.sim, MAP = as.numeric(unscale(vals = MAP.sim, norm.data = MAP.scaled)), mean = bayes.c.eff.mean2, Ci.low = bayes.c.eff.lower2, Ci.high = bayes.c.eff.upper2, cohort = "Modern-Forest", structure = "Forest")

plot.dat3 <- data.frame(MAP.sim, MAP = as.numeric(unscale(vals = MAP.sim, norm.data = MAP.scaled)) , mean = bayes.c.eff.mean3, Ci.low = bayes.c.eff.lower3, Ci.high = bayes.c.eff.upper3, cohort = "Past-Savanna", structure = "Savanna")

plot.dat4 <- data.frame(MAP.sim, MAP = as.numeric(unscale(vals = MAP.sim, norm.data = MAP.scaled)), mean = bayes.c.eff.mean4, Ci.low = bayes.c.eff.lower4, Ci.high = bayes.c.eff.upper4, cohort = "Modern-Savanna", structure = "Savanna")


plot.dat.MAP <- rbind(plot.dat1, plot.dat2, plot.dat3, plot.dat4)
plot.dat.MAP$cohort <- as.factor(plot.dat.MAP$cohort)

## Foundation for the plot & line for the posterior mean of the Bayesian conditional effect
MAP.conditional <- ggplot(plot.dat.MAP, aes(x = MAP, y = mean, color = cohort)) + geom_line( alpha = 0.8, size = 0.5)+ geom_ribbon(aes(ymin = Ci.low, ymax = Ci.high, fill = cohort),  alpha = 0.2, linetype = "blank") + ylab("Condtional effect of Tmax")+xlab("Precipitation (mm)")+scale_color_manual(values = c("Past-Savanna"='#a6611a',
                                                                                                                                                                                                                                                                                                                     "Modern-Savanna"='#dfc27d',
                                                                                                                                                                                                                                                                                                                     "Modern-Forest"='#c7eae5',
                                                                                                                                                                                                                                                                                                                     "Past-Forest"='#018571'))+scale_fill_manual(values = c("Past-Savanna"='#a6611a',
                                                                                                                                                                                                                                                                                                                                                                            "Modern-Savanna"='#dfc27d',
                                                                                                                                                                                                                                                                                                                                                                            "Modern-Forest"='#c7eae5',
                                                                                                                                                                                                                                                                                                                                                                            "Past-Forest"='#018571'))+theme_bw(base_size = 18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+facet_wrap(~structure, nrow = 2)

MAP.conditional

png(height = 4, width = 6,  units = "in", res = 300,  "outputs/growth_model/lag2_reg_cohort_only_re_t_pr_dry_yrs_site_rs_inter/interaction_plot_Tmax_cond_on_MAP_v4_all_years.png")
MAP.conditional
dev.off()



# --------------------make plots of probed predicted growth responses:
# plot probed values of tree growth predictions:
Yprobe <- data.frame(Yprobe.samps[[1]])
colnames(Yprobe) <- 1:length(Yprobe)
probe.m <- melt(Yprobe)
probe.m$RWI <- exp(probe.m$value) 
colnames(probe.m) <- c("num", "Ypred", "RWI")

probe <- short.probe
probe$num <- 1:length(probe[,1])
colnames(probe) <- c("struct.cohort","site","Drought", "DBH","Tmax", "RWI_1","RWI_2", "num")

# summarize by structure + cohort class only:
# summarize by structure + cohort class only:
probe$num <- as.factor(as.character(probe$num))
full.p <- probe

probtest.dry.pair <- dplyr::inner_join(probe.m, full.p, by=c("num"))

prob <- probtest.dry.pair
# save here:
saveRDS(prob, "outputs/growth_model/lag2_reg_struct_x_cohort_re_t_pr_dry_yrs_site_rs_inter/full_yprobe_preds_all_years.rds")
prob <- readRDS("outputs/growth_model/lag2_reg_struct_x_cohort_re_t_pr_dry_yrs_site_rs_inter/full_yprobe_preds_all_years.rds")




# convert prob$Drought back to MAP:
library(DMwR)
DIprobe <- round(seq(range(train$MAP.scaled)[1], range(train$MAP.scaled)[2], by = 0.75), 3)

prob$DBH <- as.numeric(round(unscale(vals = prob$DBH, norm.data = DBH.scaled))) 
prob$MAP <- as.numeric(round(unscale(vals = prob$Drought, norm.data = MAP.scaled)))
prob$Tmax <- as.numeric(round(unscale(vals = prob$Tmax, norm.data = T.scaled))) 


prob_10_60cm_avg <- prob %>% dplyr::filter(DBH == 27 | DBH == 39 | DBH == 63 ,  RWI_1 == 0.391, RWI_2 == 0.016 ) 

struc.conversion <- data.frame(struct.cohort = 1:4, cohort = c(paste0(c(unique(train$struct.cohort)))))
struc.conversion$ageclass <- ifelse(struc.conversion$cohort %in% c("Past-Forest", "Past-Savanna"), "Past", "Modern")
prob_10_60cm_avg <- merge(prob_10_60cm_avg,struc.conversion, by  = "struct.cohort")


preds_ci_10 <- prob_10_60cm_avg %>% group_by(DBH, Drought, MAP,Tmax, struct.cohort, ageclass) %>% dplyr::summarise(meanY = mean(RWI),
                                                                                                                   Ci.low = quantile(RWI, 0.05), Ci.high = quantile(RWI, 0.95))

preds_ci_10_age <- prob_10_60cm_avg %>% group_by(DBH, Drought, MAP,Tmax, ageclass) %>% dplyr::summarise(meanY = mean(RWI),
                                                                                                        Ci.low = quantile(RWI, 0.05), Ci.high = quantile(RWI, 0.95))

# same thing but with LOWER than average tree growth the years before:

prob_10_60cm_low <- prob %>% dplyr::filter(DBH == 27 | DBH == 39 | DBH == 63 ,  RWI_1 == -1.609, RWI_2 == -1.484 ) 


prob_10_60cm_low <- merge(prob_10_60cm_low, struc.conversion, by  = "struct.cohort")


preds_ci_10_low <- prob_10_60cm_low %>% group_by(DBH, Drought,MAP, Tmax, struct.cohort, ageclass) %>% dplyr::summarise(meanY = mean(RWI),
                                                                                                                       Ci.low = quantile(RWI, 0.05), Ci.high = quantile(RWI, 0.95))


preds_ci_10_low_age <- prob_10_60cm_low %>% group_by(DBH, Drought, MAP,Tmax, ageclass) %>% dplyr::summarise(meanY = mean(RWI),
                                                                                                            Ci.low = quantile(RWI, 0.05), Ci.high = quantile(RWI, 0.95))

# same thing but with HIGHER than average tree growth the years before:

prob_10_60cm_high <- prob %>% dplyr::filter(DBH == 27 | DBH == 39 | DBH == 63 , RWI_1 == 1.891, RWI_2 == 1.516  ) 


prob_10_60cm_high <- merge(prob_10_60cm_high, struc.conversion, by  = "struct.cohort")


preds_ci_10_high <- prob_10_60cm_high %>% group_by(DBH, Drought,MAP, Tmax, struct.cohort, ageclass) %>% dplyr::summarise(meanY = mean(RWI),
                                                                                                                         Ci.low = quantile(RWI, 0.05), Ci.high = quantile(RWI, 0.95))

preds_ci_10_high_age <- prob_10_60cm_high %>% group_by(DBH, Drought, MAP,Tmax, ageclass) %>% dplyr::summarise(meanY = mean(RWI),
                                                                                                              Ci.low = quantile(RWI, 0.05), Ci.high = quantile(RWI, 0.95))

#----------- plot drought response by max temperature and Dimaeter + cohort-struct class -----------

Drought.resp.DBH.temp.avgpyr <- ggplot(preds_ci_10, aes(MAP, meanY, color = ageclass))+geom_line()+geom_ribbon(aes(ymin = Ci.low, ymax = Ci.high, fill = ageclass), alpha = 0.25, linetype = "dashed", colour = NA)+scale_color_manual(values = c("Past-Savanna"='#a6611a',"Modern-Savanna"='#dfc27d',"Modern-Forest"='#c7eae5',"Past-Forest"='#018571'))+facet_grid(vars(DBH), vars(Tmax),labeller=label_both)+ylab("Tree growth (mm)")+xlab("Total Precipitation (mm)")+theme_bw()



Drought.resp.DBH.temp.lowpyr <- ggplot(preds_ci_10_low, aes(MAP, meanY, color = ageclass))+geom_line()+geom_ribbon(aes(ymin = Ci.low, ymax = Ci.high, fill = ageclass), alpha = 0.25, linetype = "dashed", colour = NA)+scale_color_manual(values = c("Past-Savanna"='#a6611a',"Modern-Savanna"='#dfc27d',"Modern-Forest"='#c7eae5',"Past-Forest"='#018571'))+facet_grid(vars(DBH), vars(Tmax),labeller=label_both)+ylab("Tree growth (mm)")+xlab("Total Precipitation (mm)")+theme_bw()


Drought.resp.DBH.temp.highpyr <- ggplot(preds_ci_10_high, aes(MAP, meanY, color = ageclass))+geom_line()+geom_ribbon(aes(ymin = Ci.low, ymax = Ci.high, fill = ageclass), alpha = 0.25, linetype = "dashed", colour = NA)+scale_color_manual(values = c("Past-Savanna"='#a6611a',"Modern-Savanna"='#dfc27d',"Modern-Forest"='#c7eae5',"Past-Forest"='#018571'))+facet_grid(vars(DBH), vars(Tmax),labeller=label_both)+ylab("Tree growth (mm)")+xlab("Total Precipitation (mm)")+theme_bw()


# output the above plots to PNG:
png(height = 10, width = 8, units = "in", res = 300, "outputs/growth_model/lag2_reg_struct_x_cohort_re_t_pr_dry_yrs_site_rs_inter/MAP_response_low_prevyr_high_temp_low_high_DBH_all_years.png")
Drought.resp.DBH.temp.lowpyr 
dev.off()

png(height = 10, width = 8, units = "in", res = 300, "outputs/growth_model/lag2_reg_struct_x_cohort_re_t_pr_dry_yrs_site_rs_inter/MAP_response_avg_prevyr_high_temp_low_high_DBH_all_years.png")
Drought.resp.DBH.temp.avgpyr 
dev.off()

png(height = 10, width = 8, units = "in", res = 300, "outputs/growth_model/lag2_reg_struct_x_cohort_re_t_pr_dry_yrs_site_rs_inter/MAP_response_high_prevyr_high_temp_low_high_DBH_all_years.png")
Drought.resp.DBH.temp.highpyr 
dev.off()


#----------- plot drought response by max temperature and Dimaeter + AGECLASS -----------

Drought.resp.DBH.temp.avgpyr.age <- ggplot(preds_ci_10_age, aes(MAP, meanY, color = ageclass))+geom_line()+geom_ribbon(aes(ymin = Ci.low, ymax = Ci.high, fill = ageclass), alpha = 0.25, colour = NA)+scale_color_manual(values = c("Past"='blue',"Modern"='red'))+facet_grid(vars(DBH), vars(Tmax),labeller=label_both)+ylab("Predicted Tree growth (mm)")+xlab("Total Precipitation (mm)")+theme_bw()




Drought.resp.DBH.temp.lowpyr.age <- ggplot(preds_ci_10_low_age, aes(MAP, meanY, color = ageclass))+geom_line()+geom_ribbon(data = preds_ci_10_low_age,aes(ymin = Ci.low, ymax = Ci.high, fill = ageclass), alpha = 0.25, colour = NA)+scale_color_manual(values = c("Past"='blue',"Modern"='red'))+facet_grid(vars(DBH), vars(Tmax),labeller=label_both)+ylab("Predicted Tree growth (mm)")+xlab("Total Precipitation (mm)")+theme_bw()


Drought.resp.DBH.temp.highpyr.age <- ggplot(preds_ci_10_high_age, aes(MAP, meanY, color = ageclass))+geom_line()+geom_ribbon(aes(ymin = Ci.low, ymax = Ci.high, fill = ageclass), alpha = 0.25, colour = NA)+scale_color_manual(values = c("Past"='blue',"Modern"='red'))+facet_grid(vars(DBH), vars(Tmax),labeller=label_both)+ylab("Predicted Tree growth (mm)")+xlab("Total Precipitation (mm)")+theme_bw()


# output the above plots to PNG:
png(height = 6, width = 10, units = "in", res = 300, "outputs/growth_model/lag2_reg_struct_x_cohort_re_t_pr_dry_yrs_site_rs_inter/MAP_response_low_prevyr_high_temp_low_high_DBH_ageclass_all_years.png")
Drought.resp.DBH.temp.lowpyr.age 
dev.off()

png(height = 6, width = 10, units = "in", res = 300, "outputs/growth_model/lag2_reg_struct_x_cohort_re_t_pr_dry_yrs_site_rs_inter/MAP_response_avg_prevyr_high_temp_low_high_DBH_ageclass_all_years.png")
Drought.resp.DBH.temp.avgpyr.age+theme_bw(base_size = 14)
dev.off()

png(height = 6, width = 10, units = "in", res = 300, "outputs/growth_model/lag2_reg_struct_x_cohort_re_t_pr_dry_yrs_site_rs_inter/MAP_response_high_prevyr_high_temp_low_high_DBH_ageclass_all_years.png")
Drought.resp.DBH.temp.highpyr.age 
dev.off()





#--------------- plot marginal effects of climate and previous years growth ----------------

struc.conversion <- data.frame(struct.cohort = 1:4, cohort = c(paste0(c(unique(train$struct.cohort)))))
struc.conversion$ageclass <- ifelse(struc.conversion$cohort %in% c("Past-Forest", "Past-Savanna"), "Past", "Modern")

prob <- left_join(prob, struc.conversion, by  = "struct.cohort")

# Marginal MAP effect:
preds_ci_mod_past_MAP <- prob %>% group_by( MAP, ageclass) %>% summarise(meanY = mean(RWI),                                                                           Ci.low = quantile(RWI, 0.05), Ci.high = quantile(RWI, 0.95))

Y_MAP_marg <- ggplot(preds_ci_mod_past_MAP, aes(MAP,meanY, color = ageclass)) + geom_line()+#geom_smooth(data = preds_ci_mod_past_MAP, aes(x = MAP, y = meanY, se = FALSE)) + 
  geom_ribbon(data = preds_ci_mod_past_MAP,aes(x = MAP, ymin = Ci.low, ymax = Ci.high, fill = ageclass), alpha = 0.25, colour = NA)+ scale_color_manual (values = c("Modern" = "red", "Past" = "blue")) + ylab("Tree growth (mm)")+xlab("Total Annual Precipitation (mm)")+ylim(0, 10)+theme_bw()


# marginal TMAX effect
preds_ci_mod_past_Tmax <- prob %>% group_by( Tmax,ageclass) %>% summarise(meanY = mean(RWI),
                                                                          Ci.low = quantile(RWI, 0.05), Ci.high = quantile(RWI, 0.95))

Y_Tmax_marg <- ggplot(preds_ci_mod_past_Tmax, aes(Tmax, meanY, color = ageclass))+geom_line()+geom_ribbon(data = preds_ci_mod_past_Tmax, aes(x = Tmax, ymin = Ci.low, ymax = Ci.high, fill = ageclass), alpha = 0.25, colour = NA)+scale_color_manual (values = c("Modern" = "red", "Past" = "blue")) + ylab("Tree growth (mm)")+xlab("Max. Temp. (DegF)")+ylim(0,10)+theme_bw()

# marginal DBH effect
preds_ci_mod_past_DBH <- prob %>% group_by( DBH,ageclass) %>% summarise(meanY = mean(RWI),
                                                                        Ci.low = quantile(RWI, 0.05), Ci.high = quantile(RWI, 0.95))

Y_DBH_marg <- ggplot(preds_ci_mod_past_DBH, aes(DBH, meanY, color = ageclass))+geom_line()+geom_ribbon(data = preds_ci_mod_past_DBH, aes(x = DBH, ymin = Ci.low, ymax = Ci.high, fill = ageclass), alpha = 0.25, colour = NA)+scale_color_manual (values = c("Modern" = "red", "Past" = "blue")) + ylab("Tree growth (mm)") + xlab("Tree DBH (cm)")+ylim(0,10)+theme_bw()

# marginal lag-1 effect
preds_ci_mod_past_RWI1 <- prob %>% group_by( RWI_1,ageclass) %>% summarise(meanY = mean(RWI),
                                                                           Ci.low = quantile(RWI, 0.05), Ci.high = quantile(RWI, 0.95))

Y_RWI_1_marg <- ggplot(preds_ci_mod_past_RWI1, aes(RWI_1, meanY, color = ageclass))+geom_line()+geom_ribbon(data = preds_ci_mod_past_RWI1, aes(x = RWI_1, ymin = Ci.low, ymax = Ci.high, fill = ageclass), alpha = 0.25, colour = NA)+scale_color_manual (values = c("Modern" = "red", "Past" = "blue")) + ylab("Tree growth (mm)")+xlab("log(Previous (-1) Year's growth)")+ylim(0, 10)+theme_bw()

# marginal lag-2 effect
preds_ci_mod_past_RWI2 <- prob %>% group_by( RWI_2,ageclass) %>% summarise(meanY = mean(RWI),
                                                                           Ci.low = quantile(RWI, 0.05), Ci.high = quantile(RWI, 0.95))

Y_RWI_2_marg <- ggplot(preds_ci_mod_past_RWI2, aes(RWI_2,meanY, color = ageclass))+geom_line()+geom_ribbon(data = preds_ci_mod_past_RWI2, aes(x = RWI_2, ymin = Ci.low, ymax = Ci.high, fill = ageclass), alpha = 0.25, colour = NA)+scale_color_manual (values = c("Modern" = "red", "Past" = "blue")) + ylab("Tree growth (mm)")+xlab("log(Previous (-2) Year's growth)")+ylim(0, 10)+theme_bw()


legend <- get_legend(Y_MAP_marg)

png(height = 10, width = 4, units = "in", res = 300, "outputs/growth_model/lag2_reg_struct_x_cohort_re_t_pr_dry_yrs_site_rs_inter/marginal_post_predictive_effects_ageclass_all_years.png")
plts <- plot_grid(Y_MAP_marg+theme(legend.position = "none"), Y_Tmax_marg+theme(legend.position = "none"), Y_DBH_marg+theme(legend.position = "none"), Y_RWI_1_marg+theme(legend.position = "none"), Y_RWI_2_marg+theme(legend.position = "none"), align = "v", ncol = 1)
plot_grid(plts, legend, ncol = 2, rel_widths = c(1, 0.35))
dev.off()

# --------------plot marginal effects for each stand structure and cohort class:
# plot marginal effects of climate and previous years growth 
# Marginal MAP effect:
preds_ci_mod_past_MAP <- prob %>% group_by( MAP, cohort) %>% summarise(meanY = mean(RWI),
                                                                       Ci.low = quantile(RWI, 0.05), Ci.high = quantile(RWI, 0.95))

Y_MAP_marg_c <- ggplot(preds_ci_mod_past_MAP, aes(MAP,meanY, color = cohort))+geom_line()+geom_ribbon(data = preds_ci_mod_past_MAP, aes(x = MAP, ymin = Ci.low, ymax = Ci.high, fill = cohort), alpha = 0.25, colour = NA)+scale_color_manual(values = c("Past-Savanna"='#a6611a',
                                                                                                                                                                                                                                                         "Modern-Savanna"='#dfc27d',
                                                                                                                                                                                                                                                         "Modern-Forest"='#c7eae5',
                                                                                                                                                                                                                                                         "Past-Forest"='#018571')) +scale_fill_manual(values = c("Past-Savanna"='#a6611a',
                                                                                                                                                                                                                                                                                                                 "Modern-Savanna"='#dfc27d',
                                                                                                                                                                                                                                                                                                                 "Modern-Forest"='#c7eae5',
                                                                                                                                                                                                                                                                                                                 "Past-Forest"='#018571'))+ ylab("Tree growth")+ylim(0, 10)+xlab("Total Precipitation")


# marginal TMAX effect
preds_ci_mod_past_Tmax <- prob %>% group_by( Tmax, cohort) %>% summarise(meanY = mean(RWI),
                                                                         Ci.low = quantile(RWI, 0.05), Ci.high = quantile(RWI, 0.95))

Y_Tmax_marg_c <- ggplot(preds_ci_mod_past_Tmax, aes(Tmax,meanY, color = cohort))+geom_line()+geom_ribbon(data = preds_ci_mod_past_Tmax, aes(x = Tmax, ymin = Ci.low, ymax = Ci.high, fill = cohort), alpha = 0.25, colour = NA)+scale_color_manual(values = c("Past-Savanna"='#a6611a',
                                                                                                                                                                                                                                                              "Modern-Savanna"='#dfc27d',
                                                                                                                                                                                                                                                              "Modern-Forest"='#c7eae5',
                                                                                                                                                                                                                                                              "Past-Forest"='#018571')) +scale_fill_manual(values = c("Past-Savanna"='#a6611a',
                                                                                                                                                                                                                                                                                                                      "Modern-Savanna"='#dfc27d',
                                                                                                                                                                                                                                                                                                                      "Modern-Forest"='#c7eae5',
                                                                                                                                                                                                                                                                                                                      "Past-Forest"='#018571'))+ ylab("Tree growth")+ylim(0, 10)+xlab("Max. Temperature (DegF)")

# marginal DBH effect
preds_ci_mod_past_DBH <- prob %>% group_by( DBH, cohort) %>% summarise(meanY = mean(RWI),
                                                                       Ci.low = quantile(RWI, 0.05), Ci.high = quantile(RWI, 0.95))

Y_DBH_marg_c <- ggplot(preds_ci_mod_past_DBH, aes(DBH, meanY, color = cohort))+geom_line()+geom_ribbon(data = preds_ci_mod_past_DBH, aes(x = DBH, ymin = Ci.low, ymax = Ci.high, fill = cohort), alpha = 0.25, colour = NA)+scale_color_manual(values = c("Past-Savanna"='#a6611a',
                                                                                                                                                                                                                                                          "Modern-Savanna"='#dfc27d',
                                                                                                                                                                                                                                                          "Modern-Forest"='#c7eae5',
                                                                                                                                                                                                                                                          "Past-Forest"='#018571')) +scale_fill_manual(values = c("Past-Savanna"='#a6611a',
                                                                                                                                                                                                                                                                                                                  "Modern-Savanna"='#dfc27d',
                                                                                                                                                                                                                                                                                                                  "Modern-Forest"='#c7eae5',
                                                                                                                                                                                                                                                                                                                  "Past-Forest"='#018571'))+ ylab("Tree growth")+ylim(0, 10)+xlab("Tree DBH (cm)")

# marginal lag-1 effect
preds_ci_mod_past_RWI1 <- prob %>% group_by( RWI_1, cohort) %>% summarise(meanY = mean(RWI),
                                                                          Ci.low = quantile(RWI, 0.05), Ci.high = quantile(RWI, 0.95))

Y_RWI_1_marg_c <- ggplot(preds_ci_mod_past_RWI1, aes(RWI_1, meanY, color = cohort))+geom_line()+geom_ribbon(data = preds_ci_mod_past_RWI1, aes(x = RWI_1, ymin = Ci.low, ymax = Ci.high, fill = cohort), alpha = 0.25, colour = NA)+scale_color_manual(values = c("Past-Savanna"='#a6611a',
                                                                                                                                                                                                                                                                  "Modern-Savanna"='#dfc27d',
                                                                                                                                                                                                                                                                  "Modern-Forest"='#c7eae5',
                                                                                                                                                                                                                                                                  "Past-Forest"='#018571')) +scale_fill_manual(values = c("Past-Savanna"='#a6611a',
                                                                                                                                                                                                                                                                                                                          "Modern-Savanna"='#dfc27d',
                                                                                                                                                                                                                                                                                                                          "Modern-Forest"='#c7eae5',
                                                                                                                                                                                                                                                                                                                          "Past-Forest"='#018571'))+ ylab("Tree growth")+ylim(0, 10)+xlab("log(Prev (-1) year's growth)")

# marginal lag-2 effect
preds_ci_mod_past_RWI2 <- prob %>% group_by( RWI_2, cohort) %>% summarise(meanY = mean(RWI),
                                                                          Ci.low = quantile(RWI, 0.05), Ci.high = quantile(RWI, 0.95))

Y_RWI_2_marg_c <- ggplot(preds_ci_mod_past_RWI2, aes(RWI_2, meanY, color = cohort))+geom_line()+geom_ribbon(data = preds_ci_mod_past_RWI2, aes(x = RWI_2, ymin = Ci.low, ymax = Ci.high, fill = cohort), alpha = 0.25, colour = NA)+scale_color_manual(values = c("Past-Savanna"='#a6611a',
                                                                                                                                                                                                                                                                  "Modern-Savanna"='#dfc27d',
                                                                                                                                                                                                                                                                  "Modern-Forest"='#c7eae5',
                                                                                                                                                                                                                                                                  "Past-Forest"='#018571')) +scale_fill_manual(values = c("Past-Savanna"='#a6611a',
                                                                                                                                                                                                                                                                                                                          "Modern-Savanna"='#dfc27d',
                                                                                                                                                                                                                                                                                                                          "Modern-Forest"='#c7eae5',
                                                                                                                                                                                                                                                                                                                          "Past-Forest"='#018571'))+ ylab("Tree growth")+ylim(0, 10)+xlab("log(Prev (-2) year's growth)")

png(height = 10, width = 5, units = "in", res = 300, "outputs/growth_model/lag2_reg_struct_x_cohort_re_t_pr_dry_yrs_site_rs_inter/marginal_post_predictive_effects_ageclassXStruct_all_years.png")
legend <- get_legend(Y_MAP_marg_c)
plts <- plot_grid(Y_MAP_marg_c+theme(legend.position = "none"), Y_Tmax_marg_c+theme(legend.position = "none"), Y_DBH_marg_c+theme(legend.position = "none"), Y_RWI_1_marg_c+theme(legend.position = "none"), Y_RWI_2_marg_c+theme(legend.position = "none"), align = "v", ncol = 1)
plot_grid(plts, legend, ncol = 2, rel_widths = c(1, 0.45))
dev.off()


# ------------- plot the marginal effects of each model parameter by site: ---------------

preds_ci_mod_past_MAP_site <- prob %>% group_by( MAP, ageclass, site) %>% summarise(meanY = mean(RWI),                                                                           Ci.low = quantile(RWI, 0.05), Ci.high = quantile(RWI, 0.95))

Y_MAP_marg_site <- ggplot(preds_ci_mod_past_MAP_site, aes(MAP,meanY, color = ageclass)) + geom_line()+#geom_smooth(data = preds_ci_mod_past_MAP, aes(x = MAP, y = meanY, se = FALSE)) + 
  geom_ribbon(data = preds_ci_mod_past_MAP_site,aes(x = MAP, ymin = Ci.low, ymax = Ci.high, fill = ageclass), alpha = 0.25, colour = NA)+ scale_color_manual (values = c("Modern" = "red", "Past" = "blue")) + ylab("Tree growth (mm)")+xlab("Total Annual Precipitation (mm)")+ylim(0, 10)+theme_bw()+facet_wrap(~site)

# marginal TMAX effect
preds_ci_mod_past_Tmax_site <- prob %>% group_by( Tmax,ageclass, site) %>% summarise(meanY = mean(RWI),
                                                                                     Ci.low = quantile(RWI, 0.05), Ci.high = quantile(RWI, 0.95))

Y_Tmax_marg_site <- ggplot(preds_ci_mod_past_Tmax_site, aes(Tmax, meanY, color = ageclass))+geom_line()+geom_ribbon(data = preds_ci_mod_past_Tmax_site, aes(x = Tmax, ymin = Ci.low, ymax = Ci.high, fill = ageclass), alpha = 0.25, colour = NA)+scale_color_manual (values = c("Modern" = "red", "Past" = "blue")) + ylab("Tree growth (mm)")+xlab("Max. Temp. (DegF)")+ylim(0,10)+theme_bw()+facet_wrap(~site)

# marginal DBH effect
preds_ci_mod_past_DBH_site <- prob %>% group_by( DBH,ageclass, site) %>% summarise(meanY = mean(RWI),
                                                                                   Ci.low = quantile(RWI, 0.05), Ci.high = quantile(RWI, 0.95))

Y_DBH_marg_site <- ggplot(preds_ci_mod_past_DBH_site, aes(DBH, meanY, color = ageclass))+geom_line()+geom_ribbon(data = preds_ci_mod_past_DBH_site, aes(x = DBH, ymin = Ci.low, ymax = Ci.high, fill = ageclass), alpha = 0.25, colour = NA)+scale_color_manual (values = c("Modern" = "red", "Past" = "blue")) + ylab("Tree growth (mm)") + xlab("Tree DBH (cm)")+ylim(0,10)+theme_bw()+facet_wrap(~site)

# marginal lag-1 effect
preds_ci_mod_past_RWI1_site <- prob %>% group_by( RWI_1,ageclass, site) %>% summarise(meanY = mean(RWI),
                                                                                      Ci.low = quantile(RWI, 0.05), Ci.high = quantile(RWI, 0.95))

Y_RWI_1_marg_site <- ggplot(preds_ci_mod_past_RWI1_site, aes(RWI_1, meanY, color = ageclass))+geom_line()+geom_ribbon(data = preds_ci_mod_past_RWI1_site, aes(x = RWI_1, ymin = Ci.low, ymax = Ci.high, fill = ageclass), alpha = 0.25, colour = NA)+scale_color_manual (values = c("Modern" = "red", "Past" = "blue")) + ylab("Tree growth (mm)")+xlab("log(Previous (-1) Year's growth)")+ylim(0, 10)+theme_bw()+facet_wrap(~site)

# marginal lag-2 effect
preds_ci_mod_past_RWI2_site <- prob %>% group_by( RWI_2, ageclass, site) %>% summarise(meanY = mean(RWI),
                                                                                       Ci.low = quantile(RWI, 0.05), Ci.high = quantile(RWI, 0.95))

Y_RWI_2_marg_site <- ggplot(preds_ci_mod_past_RWI2_site, aes(RWI_2,meanY, color = ageclass))+geom_line()+geom_ribbon(data = preds_ci_mod_past_RWI2_site, aes(x = RWI_2, ymin = Ci.low, ymax = Ci.high, fill = ageclass), alpha = 0.25, colour = NA)+scale_color_manual (values = c("Modern" = "red", "Past" = "blue")) + ylab("Tree growth (mm)")+xlab("log(Previous (-2) Year's growth)")+ylim(0, 10)+theme_bw()+facet_wrap(~site)


legend <- get_legend(Y_MAP_marg_site)

png(height = 25, width = 6, units = "in", res = 300, "outputs/growth_model/lag2_reg_struct_x_cohort_re_t_pr_dry_yrs_site_rs_inter/marginal_post_predictive_effects_ageclass_by_site_all_years.png")
plts <- plot_grid(Y_MAP_marg_site+theme(legend.position = "none"), Y_Tmax_marg_site+theme(legend.position = "none"), Y_DBH_marg_site+theme(legend.position = "none"), Y_RWI_1_marg_site+theme(legend.position = "none"), Y_RWI_2_marg_site+theme(legend.position = "none"), align = "v", ncol = 1)
plot_grid(plts, legend, ncol = 2, rel_widths = c(1, 0.35))
dev.off()


# below is some extra code



# ----------------------- Plot future predictions ------------------------
fut <- long.fut
# scenario # 1: no change from modern
Yfut.samps <- data.frame(Y.fut[[1]])

colnames(Yfut.samps) <- paste0(fut$site, "_",fut$year, "_", fut$DBH.scaled, "_", fut$RWI_1, "_", fut$RWI_2, "_", fut$model)
Yfut.samps$num <- 1:length(Yfut.samps[,2])
fut.m <- melt(Yfut.samps)
fut.m$RWI <- exp(fut.m$value)
colnames(fut.m) <- c("site_year", "Ypred", "RWI")

fut$site_year <- paste0(fut$site, "_",fut$year, "_", fut$DBH.scaled, "_", fut$RWI_1, "_", fut$RWI_2, "_", fut$model)
fut$scenario <- "constant"
colnames(fut) <- c("site","MAP", "DBH","Tmax", "RWI_1","RWI_2","struct.cohort","model",  "year", "site.num","site_year", "scenario")

# summarize by structure + cohort class only:

fut.proj <- dplyr::inner_join(fut.m, fut, by=c("site_year"))


# scenario # 2: 35% decrease in drought sensitivie from modern
Yfut.35.samps <- data.frame(Y.fut.35[[1]])

colnames(Yfut.35.samps) <-  paste0(fut$site, "_",fut$year, "_", fut$DBH.scaled, "_", fut$RWI_1, "_", fut$RWI_2, "_", fut$model)
Yfut.35.samps$num <- 1:length(Yfut.35.samps[,2])
fut.35.m <- melt(Yfut.35.samps)
fut.35.m $RWI <- exp(fut.35.m $value)
colnames(fut.35.m ) <- c("site_year", "Ypred", "RWI")

fut$site_year <-  paste0(fut$site, "_",fut$year, "_", fut$DBH.scaled, "_", fut$RWI_1, "_", fut$RWI_2, "_", fut$model)
fut$scenario <- "decrease_35"
colnames(fut) <- c("site","MAP", "DBH","Tmax", "RWI_1","RWI_2","struct.cohort", "model", "year", "site.num","site_year", "scenario")

fut.35.proj <- dplyr::inner_join(fut.35.m, fut, by=c("site_year"))


# scenario # 3: 50% decrease in drought sensitivie from modern
Yfut.50.samps <- data.frame(Y.fut.50[[1]])

colnames(Yfut.50.samps) <- paste0(fut$site, "_",fut$year, "_", fut$DBH.scaled, "_", fut$RWI_1, "_", fut$RWI_2, "_", fut$model)
Yfut.50.samps$num <- 1:length(Yfut.50.samps[,2])
fut.50.m <- melt(Yfut.50.samps)
fut.50.m $RWI <- exp(fut.50.m $value)
colnames(fut.50.m ) <- c("site_year", "Ypred", "RWI")

fut$site_year <-  paste0(fut$site, "_",fut$year, "_", fut$DBH.scaled, "_", fut$RWI_1, "_", fut$RWI_2, "_", fut$model)
fut$scenario <- "decrease_50"
colnames(fut) <- c("site","MAP", "DBH","Tmax", "RWI_1","RWI_2","struct.cohort", "model", "year", "site.num","site_year", "scenario")

fut.50.proj <- dplyr::inner_join(fut.50.m, fut, by=c("site_year"))

# scenario # 4: 50% decrease in drought sensitivy from modern & 50% increase in T sensitivity
Yfut.35.35.samps <- data.frame(Y.fut.35.35[[1]])

colnames(Yfut.35.35.samps) <-  paste0(fut$site, "_",fut$year, "_", fut$DBH.scaled, "_", fut$RWI_1, "_", fut$RWI_2, "_", fut$model)
Yfut.35.35.samps$num <- 1:length(Yfut.35.35.samps[,2])
fut.35.35.m <- melt(Yfut.35.35.samps)
fut.35.35.m $RWI <- exp(fut.35.35.m $value)
colnames(fut.35.35.m ) <- c("site_year", "Ypred", "RWI")

fut$site_year <- paste0(fut$site, "_",fut$year, "_", fut$DBH.scaled, "_", fut$RWI_1, "_", fut$RWI_2, "_", fut$model)
fut$scenario <- "decrease_35.35"
colnames(fut) <- c("site","MAP", "DBH","Tmax", "RWI_1","RWI_2","struct.cohort", "model", "year", "site.num","site_year", "scenario")

fut.35.35.proj <- dplyr::inner_join(fut.35.35.m, fut, by=c("site_year"))



# scenario # 5: 50% decrease in drought sensitivy from modern & 50% increase in T sensitivity
Yfut.50.50.samps <- data.frame(Y.fut.50.50[[1]])

colnames(Yfut.50.50.samps) <-  paste0(fut$site, "_",fut$year, "_", fut$DBH.scaled, "_", fut$RWI_1, "_", fut$RWI_2, "_", fut$model)
Yfut.50.50.samps$num <- 1:length(Yfut.50.50.samps[,2])
fut.50.50.m <- melt(Yfut.50.50.samps)
fut.50.50.m $RWI <- exp(fut.50.50.m $value)
colnames(fut.50.50.m ) <- c("site_year", "Ypred", "RWI")

fut$site_year <-  paste0(fut$site, "_",fut$year, "_", fut$DBH.scaled, "_", fut$RWI_1, "_", fut$RWI_2, "_", fut$model)
fut$scenario <- "decrease_50.50"
colnames(fut) <- c("site","MAP", "DBH","Tmax", "RWI_1","RWI_2","struct.cohort", "model", "year", "site.num","site_year", "scenario")

fut.50.50.proj <- dplyr::inner_join(fut.50.50.m, fut, by=c("site_year"))


# now join all together :
all.proj <- rbind(fut.proj, fut.35.proj[!fut.35.proj$year == 2000,], fut.50.proj[!fut.50.proj$year == 2000,], fut.35.35.proj[!fut.35.35.proj$year == 2000,], fut.50.50.proj[!fut.50.50.proj$year == 2000,])



# ------------ For 26 cm tree and average prev years growth ----------------------
small.proj <- all.proj[all.proj$DBH == -0.413 & all.proj$RWI_1 == 0.103 & all.proj$RWI_2 == 0.103,]
small.proj$year <- ifelse(small.proj$year == 2050, "2050-2069",
                          ifelse(small.proj$year == 2070, "2070-2099", "2000"))
scenario_names <- c(
  'constant' = "sensitivity constant",
  'decrease_35' = "drought (taper 8%)",
  'decrease_35.35' = "drought & temp. (taper 8%)",
  'decrease_50.50' = "drought & temp. (no taper 15%)",
  'decrease_50' = "drought (no taper 15%)"
)



# make one big plot for average previous years growth 
by.scenario <- ggplot(small.proj[!small.proj$year == 2000, ], aes(year, RWI, fill = as.character(year)))+geom_boxplot(position = "dodge2")+geom_hline(yintercept = mean(small.proj[small.proj$year ==2000,]$RWI, na.rm=TRUE), color = "black", linetype = "dashed")+facet_grid(~ model + scenario)+ylab("Predicted tree growth (mm)")+xlab("Time period")+scale_fill_manual(values = c("blue", "red"))+theme(legend.title = element_blank())


png(height = 4, width = 12, units = "in", res = 300, "outputs/growth_model/lag2_reg_struct_x_cohort_re_t_pr_dry_yrs_site_rs_inter/fut_growth_by_scenarios_26cm_avg_growth.png")
by.scenario+theme(legend.position = "none")
dev.off()


by.period <- ggplot(small.proj[!small.proj$year == 2000 , ], aes(year, RWI, fill =factor( scenario, labels = c("constant", "taper P 35%", "taper P & T 35%", "no taper P 50%", "no taper P & T 50%"))))+geom_boxplot(position = "dodge2")+geom_hline(yintercept = mean(small.proj[small.proj$year ==2000 & small.proj$scenario %in% "constant",]$RWI, na.rm=TRUE), color = "black", linetype = "dashed")+ylab("Predicted tree growth (mm)")+xlab("Time period")+scale_fill_manual(values = c('#fee090','#91bfdb','#fc8d59','#4575b4','#d73027'
))+theme(legend.title = element_blank())

png(height = 5, width = 6, units = "in", res = 300, "outputs/growth_model/lag2_reg_struct_x_cohort_re_t_pr_dry_yrs_site_rs_inter/fut_growth_by_time_scenarios_26cm_avg_growth.png")
by.period
dev.off()

by.model <- ggplot(small.proj[!small.proj$year == 2000, ], aes(year, RWI, fill =factor( scenario, labels = c("constant", "taper P 35%", "taper P & T 35%", "no taper P 50%", "no taper P & T 50%"))))+geom_boxplot(position = "dodge2")+geom_hline(yintercept = mean(small.proj[small.proj$year ==2000 & small.proj$scenario %in% "constant",]$RWI, na.rm=TRUE), color = "black", linetype = "dashed")+ylab("Predicted tree growth (mm)")+xlab("Time period")+scale_fill_manual(values = c('#fee090','#91bfdb','#fc8d59','#4575b4','#d73027'
))+theme(legend.title = element_blank())+facet_grid(rows = vars(scenario), cols = vars(model))



png(height = 5, width = 6, units = "in", res = 300, "outputs/growth_model/lag2_reg_struct_x_cohort_re_t_pr_dry_yrs_site_rs_inter/fut_growth_by_model_26cm_avg_growth.png")
by.model
dev.off()

small.proj$model <- factor(small.proj$model, levels = c("INMCM4", "HadGEM-ES2", "MIROC"))
model_names <- c(
  'INMCM4' = "INMCM4 \n low Tmax - low Precip",
  'HadGEM-ES2' = "HadGEM-ES2 \n high Tmax - low Precip",
  'MIROC' = "MIROC \n high Tmax - high Precip"
  
)


model_names <- c("INMCM4 \n low Tmax - low Precip",
                 "HadGEM-ES2 \n high Tmax - low Precip",
                 "MIROC \n high Tmax - high Precip")

by.model.2050 <- ggplot(small.proj[!small.proj$year == 2000 , ], aes(year, RWI, fill =factor( scenario, labels = c("constant", "taper P 8%", "taper P & T 8%", "no taper P 15%", "no taper P & T 15%"))))+geom_boxplot(position = "dodge2")+geom_hline(yintercept = mean(small.proj[small.proj$year ==2000 & small.proj$scenario %in% "constant",]$RWI, na.rm=TRUE), color = "black", linetype = "dashed")+ylab("Predicted tree growth (mm)")+xlab("Time period")+scale_fill_manual(values = c('#fee090','#91bfdb','#fc8d59','#4575b4','#d73027'
))+theme(legend.title = element_blank())+facet_grid(~model)


png(height = 5, width = 9, units = "in", res = 300, "outputs/growth_model/lag2_reg_struct_x_cohort_re_t_pr_dry_yrs_site_rs_inter/fut_growth_by_model_2050_26cm_avg_growth.png")
by.model.2050
dev.off()

ggplot(small.proj[!small.proj$year == 2000 & !small.proj$year %in% "2070-2099", ], aes(year, RWI, fill =factor( scenario, labels = c("constant", "taper P 35%", "taper P & T 35%", "no taper P 50%", "no taper P & T 50%"))))+geom_boxplot(position = "dodge2")+geom_hline(yintercept = mean(small.proj[small.proj$year ==2000 & small.proj$scenario %in% "constant",]$RWI, na.rm=TRUE), color = "black", linetype = "dashed")+ylab("Predicted tree growth (mm)")+xlab("Time period")+scale_fill_manual(values = c('#fee090','#91bfdb','#fc8d59','#4575b4','#d73027'
))+theme(legend.title = element_blank())+facet_grid(rows=vars(model), cols= vars(site))



# ------------------------ Plot posterior response to climate ------------------------------------
# get together a dataframe to probe with:

Tmax.range <- round(seq(range(train.dry.pair$T.scaled)[1], range(train.dry.pair$T.scaled)[2], by = 0.1), 10)

MAP.range <- round(seq(range(train.dry.pair$MAP.scaled)[1], range(train.dry.pair$MAP.scaled)[2], by = 0.1), 10)

prob.vals <- expand.grid(T.scaled = Tmax.range, MAP.scaled = MAP.range, site.num = unique(train.dry.pair$site.num))

prob.vals <- left_join(unique(train.dry.pair[,c("struct.cohort.code", "site", "site.num", "ageclass", "structure")]), prob.vals, by = "site.num")

prob.vals$DBH.scaled <- mean(train.dry.pair$DBH.scaled)
prob.vals$RWI_1 <- log(mean(train.dry.pair$RWI_1))
prob.vals$RWI_2 <- log(mean(train.dry.pair$RWI_2))

# now lets use the betasamps to predict posterior growth for all these conditions:

#int.mcmc <- as.mcmc(samp.structure.cohort.re[[1]])
int.mcmc <- as.mcmc(samps)
int.mcmc.mat <- as.matrix(int.mcmc)
int.mcmc.dat <- data.frame(int.mcmc.mat)

int.1 <- matrix(rep(NA, nrow(int.mcmc.dat)*length(prob.vals$T.scaled)), nrow = nrow(int.mcmc.dat))

# need to clean this up but this is the basic idea:


# simulate the effect of beta 1 conditional on Tmax
for(i in 1:length(prob.vals$T.scaled)){
  # for struct.cohort == 1
  int.1[,i] <- int.mcmc.dat[,paste0("beta1.", prob.vals[i,"site.num"], ".")]+
    int.mcmc.dat[,paste0("beta2.", prob.vals[i,"struct.cohort.code"], ".")]*prob.vals[i,]$MAP.scaled+    
    int.mcmc.dat[,paste0("beta3.", prob.vals[i,"struct.cohort.code"], ".")]*prob.vals[i,"DBH.scaled"] + 
    int.mcmc.dat[,paste0("beta4.", prob.vals[i,"struct.cohort.code"], ".")]*prob.vals[i,"RWI_1"]  + 
    int.mcmc.dat[,paste0("beta5.", prob.vals[i,"struct.cohort.code"], ".")]*prob.vals[i,"RWI_2"] +
    int.mcmc.dat[,paste0("beta6.", prob.vals[i,"struct.cohort.code"], ".")]*prob.vals[i,"T.scaled"] + 
    int.mcmc.dat[,paste0("beta7.", prob.vals[i,"struct.cohort.code"], ".")] * (prob.vals[i,"MAP.scaled"] *prob.vals[i,"T.scaled"])
  
  
}


# columns are the different degree-site scenario combinations
prob.vals$idval <- 1:length(prob.vals$T.scaled)
# rows are the mcmc values
colnames(int.1) <- 1:length(prob.vals$T.scaled)
test.m <- melt(int.1)
colnames(test.m) <- c("MCMC", "idval", "Ypred")
full.pred <- left_join(test.m, prob.vals, by = "idval")
full.pred$RWI <- exp(full.pred$Ypred)
full.pred$Tmax <-  round(DMwR::unscale(full.pred$T.scaled, T.scaled), digits = 3)
full.pred$MAP <-  round(DMwR::unscale(full.pred$MAP.scaled, MAP.scaled), digits = 3)

site.summary <- full.pred %>% group_by(site.num, Tmax, MAP, struct.cohort.code) %>% dplyr::summarise(mean = mean(exp(RWI)), Ci.low = quantile(exp(Ypred), 0.025),  Ci.high = quantile(exp(Ypred), 0.975))




ageclass.summary <- full.pred %>% group_by(site, ageclass, Tmax, MAP) %>% dplyr::summarise(mean = mean(RWI), Ci.low = quantile(exp(Ypred), 0.025),  Ci.high = quantile(exp(Ypred), 0.975))


ggplot(ageclass.summary[ageclass.summary$MAP == 259.116,], aes(x = Tmax, y =mean, color = ageclass))+geom_line()+geom_ribbon(data = ageclass.summary[ageclass.summary$MAP == 259.338,], aes(x = Tmax, ymin = Ci.low, ymax = Ci.high, fill = ageclass), alpha = 0.25, colour = NA)+facet_wrap(~site)


#ggplot(ageclass.summary[ageclass.summary$MAP == 516.814,], aes(x = Tmax, y =mean, color = ageclass))+geom_line()+geom_ribbon(data = ageclass.summary[ageclass.summary$MAP == 516.814,], aes(x = Tmax, ymin = Ci.low, ymax = Ci.high, fill = ageclass), alpha = 0.25, colour = NA)+facet_wrap(~site)



ageclass.only <- full.pred %>% group_by( ageclass, Tmax, MAP) %>% dplyr::summarise(mean = mean(RWI), Ci.low = quantile(exp(Ypred), 0.025),  Ci.high = quantile(exp(Ypred), 0.975))


#ggplot(ageclass.only[ageclass.only$MAP == 259.338,], aes(x = Tmax, y =mean, color = ageclass))+geom_line()+geom_ribbon(data = ageclass.only[ageclass.only$MAP == 259.338,], aes(x = Tmax, ymin = Ci.low, ymax = Ci.high, fill = ageclass), alpha = 0.25, colour = NA)


#ggplot(ageclass.only[ageclass.only$MAP == 516.814,], aes(x = Tmax, y =mean, color = ageclass))+geom_line()+geom_ribbon(data = ageclass.only[ageclass.only$MAP == 516.814,], aes(x = Tmax, ymin = Ci.low, ymax = Ci.high, fill = ageclass), alpha = 0.25, colour = NA)


ggplot(ageclass.only[ageclass.only$MAP == 945.942,], aes(x = Tmax, y =mean, color = ageclass))+geom_line()+geom_ribbon(data = ageclass.only[ageclass.only$MAP == 945.942,], aes(x = Tmax, ymin = Ci.low, ymax = Ci.high, fill = ageclass), alpha = 0.25, colour = NA)


ggplot(ageclass.only[ageclass.only$Tmax == 26.799,], aes(x = MAP, y =mean, color = ageclass))+geom_line()+geom_ribbon(data = ageclass.only[ageclass.only$Tmax == 26.799,], aes(x = MAP, ymin = Ci.low, ymax = Ci.high, fill = ageclass), alpha = 0.25, colour = NA)

# now by ageclass and stand structure:

ageclass.ss.only <- full.pred %>% group_by(structure, ageclass, Tmax, MAP) %>% dplyr::summarise(mean = mean(RWI), Ci.low = quantile(exp(Ypred), 0.025),  Ci.high = quantile(exp(Ypred), 0.975))


ag.ss.pred.200MAP <- ggplot(ageclass.ss.only[ageclass.ss.only$MAP == 259.116,], aes(x = Tmax, y =mean, color = ageclass))+geom_point(size = 0.5)+geom_line()+geom_ribbon(data = ageclass.ss.only[ageclass.ss.only$MAP == 259.338,], aes(x = Tmax, ymin = Ci.low, ymax = Ci.high, fill = ageclass), alpha = 0.25, colour = NA)+facet_wrap(~structure, ncol = 1)+theme_bw()+theme(panel.grid = element_blank())+ylab("Predicted Tree growth (mm)")+xlab(expression("June Mean Maximum Temperature (" * degree * "C)"))+xlim(20,40)+scale_fill_manual(values = c("Past"='#2166ac', 'Modern' = "#b2182b")) +scale_color_manual(values = c("Past"='#2166ac', 'Modern' = "#b2182b"))


ag.ss.pred.500MAP <- ggplot(ageclass.ss.only[ageclass.ss.only$MAP == 514.927,], aes(x = Tmax, y =mean, color = ageclass))+geom_line()+geom_ribbon(data = ageclass.ss.only[ageclass.ss.only$MAP == 516.814,], aes(x = Tmax, ymin = Ci.low, ymax = Ci.high, fill = ageclass), alpha = 0.25, colour = NA)+facet_wrap(~structure, ncol = 1)+theme_bw()+theme(panel.grid = element_blank())+ylab("Predicted Tree growth (mm)")+xlab(expression("June Mean Maximum Temperature (" * degree * "C)"))+xlim(20,40)+scale_fill_manual(values = c("Past"='#2166ac', 'Modern' = "#b2182b")) +scale_color_manual(values = c("Past"='#2166ac', 'Modern' = "#b2182b"))


ag.ss.pred.950MAP <- ggplot(ageclass.ss.only[ageclass.ss.only$MAP == 941.279,], aes(x = Tmax, y =mean, color = ageclass))+geom_line()+geom_ribbon(data = ageclass.ss.only[ageclass.ss.only$MAP == 945.942,], aes(x = Tmax, ymin = Ci.low, ymax = Ci.high, fill = ageclass), alpha = 0.25, colour = NA)+facet_wrap(~structure, ncol = 1)+theme_bw()+theme(panel.grid = element_blank())+ylab("Predicted Tree growth (mm)")+xlab(expression("June Mean Maximum Temperature (" * degree * "C)"))+xlim(20,40)+scale_fill_manual(values = c("Past"='#2166ac', 'Modern' = "#b2182b")) +scale_color_manual(values = c("Past"='#2166ac', 'Modern' = "#b2182b"))


ag.ss.pred.25.TMAX <- ggplot(ageclass.ss.only[ageclass.ss.only$Tmax == 25.541,], aes(x = MAP, y =mean, color = ageclass))+geom_line()+geom_ribbon(data = ageclass.ss.only[ageclass.ss.only$Tmax == 25.383,], aes(x = MAP, ymin = Ci.low, ymax = Ci.high, fill = ageclass), alpha = 0.25, colour = NA)+facet_wrap(~structure, ncol = 1)+theme_bw()+theme(panel.grid = element_blank())+ylab("Predicted Tree growth (mm)")+xlab("Total Annual Precipitation (mm)")+scale_fill_manual(values = c("Past"='#2166ac', 'Modern' = "#b2182b")) +scale_color_manual(values = c("Past"='#2166ac', 'Modern' = "#b2182b"))+facet_wrap(~structure, ncol = 1)

#------------- get pct change for 25.541 TMAX:
full.pred.df <- data.frame(full.pred)
full.pred.df$Tmax <- as.numeric(full.pred.df$Tmax)
full.pred.df$MAP <- as.numeric(full.pred.df$MAP)
Tmax25 <- full.pred.df %>% filter(Tmax == 25.541) %>% select(MCMC, idval, RWI,site, site.num, ageclass, structure, MAP) 

Tmax25.mod.forest <- Tmax25  %>% filter(ageclass %in% "Modern" & structure %in% "Forest") %>%dplyr::select(MCMC, idval, RWI,site,MAP)
Tmax25.past.forest <- Tmax25  %>% filter(ageclass %in% "Past" & structure %in% "Forest")%>%dplyr::select(MCMC, idval, RWI,site,MAP)
Tmax25.mod.savanna <- Tmax25  %>% filter(ageclass %in% "Modern" & structure %in% "Savanna")%>%dplyr::select(MCMC, idval, RWI,site,MAP)
Tmax25.past.savanna <- Tmax25  %>% filter(ageclass %in% "Past" & structure %in% "Savanna")%>%dplyr::select(MCMC, idval, RWI,site,MAP)

colnames(Tmax25.mod.forest)[3] <- "Modern_Forest"
colnames(Tmax25.past.forest)[3] <- "Past_Forest"
colnames(Tmax25.mod.savanna)[3] <- "Modern_Savanna"
colnames(Tmax25.past.savanna)[3] <- "Past_Savanna"

test.join.forest <- left_join(Tmax25.mod.forest, Tmax25.past.forest, by = c("MCMC", "site", "MAP"))
test.join.forest$growth.change <- test.join.forest$Modern_Forest - test.join.forest$Past_Forest 
test.join.forest$growth.pct.change <- ((test.join.forest$Modern_Forest - test.join.forest$Past_Forest)/ test.join.forest$Past_Forest)*100
test.join.forest$structure <- "Forest"

test.join.savanna <- left_join(Tmax25.mod.savanna, Tmax25.past.savanna, by = c("MCMC", "site", "MAP"))
test.join.savanna$growth.change <- test.join.savanna$Modern_Savanna - test.join.savanna$Past_Savanna 
test.join.savanna$growth.pct.change <- ((test.join.savanna$Modern_Savanna - test.join.savanna$Past_Savanna)/ test.join.savanna$Past_Savanna)*100
test.join.savanna$structure <- "Savanna"

# generate summaries before joining all together:
tmax25forest.change.summary <- test.join.forest %>% group_by(structure, MAP) %>% dplyr::summarise(mean.change = mean(growth.change),
                                                                                             Ci.high.change = quantile(growth.change, 0.975), 
                                                                                             Ci.low.change = quantile(growth.change, 0.025), 
                                                                                             mean.pct.change = mean(growth.pct.change),
                                                                                             Ci.high.pct.change = quantile(growth.pct.change, 0.975), 
                                                                                             Ci.low.pct.change = quantile(growth.pct.change, 0.025))


tmax25savanna.change.summary <- test.join.savanna %>% group_by(structure, MAP) %>% dplyr::summarise(mean.change = mean(growth.change),
                                                                                                  Ci.high.change = quantile(growth.change, 0.975), 
                                                                                                  Ci.low.change = quantile(growth.change, 0.025), 
                                                                                                  mean.pct.change = mean(growth.pct.change),
                                                                                                  Ci.high.pct.change = quantile(growth.pct.change, 0.975), 
                                                                                                  Ci.low.pct.change = quantile(growth.pct.change, 0.025))



#pct.change.tmax25 <- rbind(test.join.forest[,c("site","structure", "MAP", "growth.change", "growth.pct.change")], test.join.savanna[,c("site","structure", "MAP", "growth.change", "growth.pct.change")])

tmax25.change.summary <- rbind(tmax25savanna.change.summary, tmax25forest.change.summary)


Tmax.summary.pct.change <- ggplot(tmax25.change.summary, aes(x = MAP, y = mean.pct.change, color = structure))+geom_line()+geom_ribbon(data = tmax25.change.summary, aes(x = MAP, ymin = Ci.low.pct.change, ymax = Ci.high.pct.change, fill = structure), alpha = 0.25, colour = NA)+theme_bw()+theme(panel.grid = element_blank())+ylab("% change in growth \n relative to past cohort")+xlab("Total Annual Precipitation (mm)")+geom_hline(yintercept = 0, color = "darkgrey", linetype = "dashed")+scale_color_manual(values = c("Savanna"='#a6611a',"Forest"='#018571'))+scale_fill_manual(values = c("Savanna"='#a6611a',"Forest"='#018571'))


Tmax.summary.total.change <- ggplot(tmax25.change.summary, aes(x = MAP, y = mean.change, color = structure))+geom_line()+geom_ribbon(data = tmax25.change.summary, aes(x = MAP, ymin = Ci.low.change, ymax = Ci.high.change, fill = structure), alpha = 0.25, colour = NA)+theme_bw()+theme(panel.grid = element_blank())+ylab("Average change in growth \n relative to past cohort (mm)")+xlab("Total Annual Precipitation (mm)")+geom_hline(yintercept = 0, color = "darkgrey", linetype = "dashed")+scale_color_manual(values = c("Savanna"='#a6611a',"Forest"='#018571'))+scale_fill_manual(values = c("Savanna"='#a6611a',"Forest"='#018571'))

struct.legend <- get_legend(Tmax.summary.total.change)

png(height = 3, width = 7, units = "in" ,res = 300, "outputs/growth_model/lag2_reg_struct_x_cohort_re_t_pr_dry_yrs_site_rs_inter/Precipitation_difference_pce_diff_TMAX_25_mean_dbh_growth.png")
plot_grid(Tmax.summary.total.change + theme(legend.position = "none"), Tmax.summary.pct.change+ theme(legend.position = "none"), struct.legend, ncol = 3, rel_widths = c(1,1, 0.3), labels = c("A", "B", ""))
dev.off()



png(height = 4, width = 3, units = "in" ,res = 300, "outputs/growth_model/lag2_reg_struct_x_cohort_re_t_pr_dry_yrs_site_rs_inter/Precipitation_effect_TMAX_25_mean_dbh_growth_v4.png")
ag.ss.pred.25.TMAX 
dev.off()


# make a plot of modern only responses for high and low precipitaiton only:
modern.hi.low <- ageclass.ss.only %>% filter(MAP == 975.387 & ageclass %in% "Modern" | MAP == 514.927 & ageclass %in% "Modern")
modern.hi.low$MAP_scenario <- ifelse(modern.hi.low$MAP >= 900, "975 mm", "515 mm")

ag.ss.pred.MAP.modonly <- ggplot(modern.hi.low, aes(x = Tmax, y =mean, color = MAP_scenario))+geom_line()+geom_ribbon(data = modern.hi.low, aes(x = Tmax, ymin = Ci.low, ymax = Ci.high, fill = MAP_scenario), alpha = 0.25, colour = NA)+facet_wrap(~structure, ncol = 1)+theme_bw()+theme(panel.grid = element_blank())+ylab("Predicted Tree growth (mm)")+xlab(expression("June Mean Maximum Temperature (" * degree * "C)"))+xlim(20,40)+scale_fill_manual(values = c("975 mm"='#008837', '515 mm' = "#7b3294"), name = "Precipitation")+scale_color_manual(values = c("975 mm"='#008837', '515 mm' = "#7b3294"), name = "Precipitation")+theme()


# read in future climate for the region:
June.rcps <- read.csv("/Users/kah/Documents/TreeRings/outputs/June_summer_cmip5_preds_allyears_2025_2099.csv")
precip.means <- read.csv("/Users/kah/Documents/TreeRings/outputs/CCESM_rcp8.5_mean_Pr_TMAX_proj_sites.csv")


unique.clim <- unique(train.dry.pair[, c("site", "year", "JUNTmax", "ageclass")])


Jun.rcps.clim.full <- June.rcps[,c("year", "value", "fut.class", "rcp", "site")]

# subset by sites analyzed here:
Jun.rcps.clim <- Jun.rcps.clim.full %>% filter(site %in% unique(unique.clim$site))


unique.clim$fut.class <- ifelse(unique.clim$year <= 1950, "1895-1950", "1950-2015")
unique.clim2 <- unique.clim[,c("year", "JUNTmax", "fut.class", "ageclass", "site")]
colnames(unique.clim2) <- c("year", "value", "fut.class", "rcp",  "site")

clim.full <- rbind(unique.clim2, Jun.rcps.clim)

reclass <- data.frame(class = c(1.5,2.5, 4,4.5,5,5.5, 8,8.5, 9, 9.5), 
                      fut.class = c("1895-1950", "1950-2015", "2025-2060","2025-2060","2025-2060","2025-2060",
                                    "2060-2099","2060-2099","2060-2099","2060-2099" ),
                      rcp = c("Past", "Modern", "rcp2.6", "rcp4.5", "rcp6.0", "rcp8.5", "rcp2.6", "rcp4.5", "rcp6.0", "rcp8.5"))

clim.full2 <- merge( reclass,clim.full, by = c("fut.class", "rcp"))

ggplot(clim.full, aes(x = fut.class, y = value, fill = rcp))+geom_boxplot(width = 0.5, outlier.size = 0.05)+coord_flip()+xlab("Time Period")+ylab(expression("June Mean Maximum Temperature (" * degree * "C)"))


ggplot(clim.full, aes(value, fill = rcp))+geom_histogram()+facet_wrap(~fut.class)

Future.Tmax.summaries <- clim.full %>% group_by(fut.class, rcp) %>% dplyr::summarise(meanTmax = mean(value, na.rm=TRUE),
                                                                                     sd = sd(value, na.rm=TRUE),
                                                                                     Tmax.ci.low = quantile(value, 0.025, na.rm=TRUE),
                                                                                     Tmax.ci.high = quantile(value, 0.975, na.rm=TRUE))

# save summary table to output in the tables script
saveRDS(Future.Tmax.summaries, "/Users/kah/Documents/TreeRings/outputs/data/Tmax_future_climate.rds")

boxplot.tmax <- ggplot(clim.full2, aes(x = class, y = value, fill = rcp, group = class))+geom_boxplot(width = 0.5, outlier.size = 0.05)+coord_flip()+xlab("Time Period")+ylab(expression("June Mean Maximum Temperature (" * degree * "C)"))+ scale_x_continuous(name = "Time Period", breaks = c(1.5, 2.5, 4.5, 8.5 ), minor_breaks = NULL,labels = sort(unique(clim.full2$fut.class)))+theme_bw()+theme(legend.title = element_blank(), panel.grid = element_blank(), axis.title.y  = element_blank()) +scale_fill_manual(values = c("Past"='#2166ac', 'Modern' = "#b2182b", "rcp2.6" = "#ffffb2", "rcp4.5" = "#984ea3", "rcp6.0" = "#e0f3f8", "rcp8.5" ="#fc8d59" ))+ylim(20,40)

png(height = 8, width = 4, units = "in", res = 200, "outputs/growth_model/lag2_reg_struct_x_cohort_re_t_pr_dry_yrs_site_rs_inter/predicted_treegrowth_Tmax_marginal_fut_climate_boxes_v4.png")
plot_grid(ag.ss.pred.500MAP, ag.ss.pred.950MAP, boxplot.tmax, ncol=1, align = "hv", rel_heights = c(1,1,0.75), labels = "AUTO")
dev.off()



png(height = 6, width = 5, units = "in", res = 200, "outputs/growth_model/lag2_reg_struct_x_cohort_re_t_pr_dry_yrs_site_rs_inter/predicted_treegrowth_Tmax_modern_only_marginal_fut_climate_boxes_v4.png")
plot_grid( boxplot.tmax, ag.ss.pred.MAP.modonly,ncol=1, align = "hv", rel_heights = c(1,0.75), labels = "AUTO") 
dev.off()


# ---------make the same plots, but with % change in tree growth given MAP and Tmax:
# Make plots of average percent change in grwoth from regional average growth due to tempearut:
# 1. get range of modern Tmax:
# 2. Find mean growth response for each site at mean Tmax overall 26.34, with all else constant
# 3. generate PP response to range of temperatures 21.94 to 31.94
# 4. Calculate % change between mean growth and 

mod.mean.clim <-
  test.dry.pair %>% filter(ageclass %in% "Modern" & year > 1950) %>%
  #group_by(site) %>%
  dplyr::summarize(
    mod.mean.Tmax = mean(JUNTmax, na.rm = TRUE),
    one = mean(JUNTmax, na.rm = TRUE) + 1,
    two = mean(JUNTmax, na.rm = TRUE) + 2,
    three = mean(JUNTmax, na.rm = TRUE) + 3,
    four = mean(JUNTmax, na.rm = TRUE) + 4,
    five = mean(JUNTmax, na.rm = TRUE) + 5,
    minusone = mean(JUNTmax, na.rm = TRUE) - 1,
    minustwo =  mean(JUNTmax, na.rm = TRUE) - 2,
    minusthree =  mean(JUNTmax, na.rm = TRUE) - 3,
    minusfour = mean(JUNTmax, na.rm = TRUE) - 4,
    minusfive =  mean(JUNTmax, na.rm = TRUE) - 5,
    
    mod.mean.MAP = mean(MAP.prism, na.rm = TRUE),
    MAPminus100 = mean(MAP.prism, na.rm = TRUE) -
      100,
    MAPminus50 = mean(MAP.prism, na.rm = TRUE) -
      50,
    MAPplus50 = mean(MAP.prism, na.rm = TRUE) +
      50,
    
    MAP750 = 750,
    
    MAP515 = 516.814,
    MAP850 = 850,
    MAP950 = 945.942,
    MAPplus100 = mean(MAP.prism, na.rm = TRUE) +
      100
  )




# get summaries for MAP and Temperature scenarios & join
MAP.scenarios <- mod.mean.clim %>% dplyr::select(mod.mean.MAP:MAPplus100) %>% gather(MAP.Scenario, MAP, mod.mean.MAP:MAPplus100)

temp.scenarios <- mod.mean.clim %>% dplyr::select( mod.mean.Tmax:minusfive)  %>% gather(Temp.Scenario,Tmax, mod.mean.Tmax:minusfive)
site.scenarios <- expand.grid(Temp.Scenario = temp.scenarios$Temp.Scenario, MAP.Scenario = MAP.scenarios$MAP.Scenario, site = as.character(unique(test.dry.pair$site)))
test.MAP.scenarios <- merge(site.scenarios, MAP.scenarios, by = c("MAP.Scenario"))
all.scen <- merge(test.MAP.scenarios, temp.scenarios, by = "Temp.Scenario", all = TRUE)

# get the scaled values for MAP and TMAX to run through parameters
all.scen$MAP.scaled <- as.numeric(scale(all.scen$MAP, attr(MAP.scaled, "scaled:center"), attr(MAP.scaled, "scaled:scale")))

all.scen$T.scaled <- as.numeric(scale(all.scen$Tmax, attr(T.scaled, "scaled:center"), attr(T.scaled, "scaled:scale")))

# expand grid to add structure + cohort class - add 1 diameter, add 1 lag, add 2 lag, then add probe to the model....will need to re-run model
extras <- expand.grid(RWI_1 = 0.103, 
                      RWI_2 = 0.103, 
                      site = unique(all.scen$site), 
                      DBH.scaled = mean(test.dry.pair$DBH.scaled))

sites.unique <- unique(train.dry.pair[, c("site", "struct.cohort.code", "structure", "ageclass")])
extras <- merge(extras, sites.unique[, c("site", "struct.cohort.code")], by = "site")

degree.scenario <- merge(all.scen, extras, by = "site", all = TRUE)
degree.scenario <- merge(degree.scenario, site.num.df, by = "site")

# -------------use estimates of betas to generate estimates for predicted growth:
meanMAP.sim <- degree.scenario %>% filter( MAP.Scenario == "mod.mean.MAP")
meanMAP.sim <- degree.scenario

int.mcmc <- as.mcmc(samps)
int.mcmc.mat <- as.matrix(int.mcmc)
int.mcmc.dat <- data.frame(int.mcmc.mat)

int.1 <- matrix(rep(NA, nrow(int.mcmc.dat)*length(meanMAP.sim$T.scaled)), nrow = nrow(int.mcmc.dat))

# need to clean this up but this is the basic idea:


# use betas to generate pp given a value for site, structure, dbh, rwi1, rwi2, and varying T and MAP:
for(i in 1:length(meanMAP.sim$T.scaled)){
  # for struct.cohort == 1
  int.1[,i] <- int.mcmc.dat[,paste0("beta1.", meanMAP.sim[i,"site.num"], ".")]+
    int.mcmc.dat[,paste0("beta2.", meanMAP.sim[i,"struct.cohort.code"], ".")]*meanMAP.sim[i,]$MAP.scaled+    
    int.mcmc.dat[,paste0("beta3.", meanMAP.sim[i,"struct.cohort.code"], ".")]*meanMAP.sim[i,"DBH.scaled"] + 
    int.mcmc.dat[,paste0("beta4.", meanMAP.sim[i,"struct.cohort.code"], ".")]*meanMAP.sim[i,"RWI_1"]  + 
    int.mcmc.dat[,paste0("beta5.", meanMAP.sim[i,"struct.cohort.code"], ".")]*meanMAP.sim[i,"RWI_2"] +
    int.mcmc.dat[,paste0("beta6.", meanMAP.sim[i,"struct.cohort.code"], ".")]*meanMAP.sim[i,"T.scaled"] + 
    int.mcmc.dat[,paste0("beta7.", meanMAP.sim[i,"struct.cohort.code"], ".")] * (meanMAP.sim[i,"MAP.scaled"] *meanMAP.sim[i,"T.scaled"])
  
  
}


# columns are the different degree-site scenario combinations
meanMAP.sim$idval <- 1:length(meanMAP.sim$Tmax)
# rows are the mcmc values
colnames(int.1) <- 1:length(meanMAP.sim$Tmax)
test.m <- melt(int.1)
colnames(test.m) <- c("MCMC", "idval", "Ypred")
full.pred <- left_join(test.m, meanMAP.sim, by = "idval")
full.pred$RWI <- exp(full.pred$Ypred)



#  create dfs with RWI values for each map and temp scernario:

past.Temp <- full.pred %>% dplyr::select(site, MAP.Scenario, struct.cohort.code, MCMC, RWI, Temp.Scenario, Tmax, MAP) %>%
  filter(Temp.Scenario %in% "mod.mean.Tmax") %>% dplyr::rename(zero = RWI) %>% dplyr::select( -Temp.Scenario) %>% arrange(site, MAP.Scenario, struct.cohort.code, MCMC,zero)

one <- full.pred %>% dplyr::select(site, MAP.Scenario, struct.cohort.code, MCMC,RWI, Temp.Scenario, Tmax, MAP) %>% filter(Temp.Scenario %in% "one" ) %>% dplyr::rename(one = RWI) %>% dplyr::select( -Temp.Scenario)%>% arrange(site, MAP.Scenario, struct.cohort.code, MCMC,one)

two <- full.pred %>%  dplyr::select(site, MAP.Scenario, struct.cohort.code,MCMC, RWI, Temp.Scenario, Tmax, MAP) %>%filter(Temp.Scenario %in% "two" ) %>% dplyr::rename(two = RWI) %>% dplyr::select( -Temp.Scenario) %>% arrange(site, MAP.Scenario, struct.cohort.code,MCMC, two)

three <- full.pred %>% dplyr::select(site, MAP.Scenario, struct.cohort.code, MCMC,RWI, Temp.Scenario, Tmax, MAP) %>% filter(Temp.Scenario %in% "three" ) %>% dplyr::rename(three = RWI) %>% dplyr::select( -Temp.Scenario) %>% arrange(site, MAP.Scenario, struct.cohort.code,MCMC, three)

four <- full.pred %>%  dplyr::select(site, MAP.Scenario, struct.cohort.code, MCMC,RWI, Temp.Scenario, Tmax, MAP) %>%filter(Temp.Scenario %in% "four" ) %>% dplyr::rename(four = RWI) %>% dplyr::select( -Temp.Scenario) %>% arrange(site, MAP.Scenario, struct.cohort.code, MCMC,four)

five <-  full.pred %>% dplyr::select(site, MAP.Scenario, struct.cohort.code, MCMC,RWI, Temp.Scenario, Tmax, MAP) %>% filter(Temp.Scenario %in% "five" ) %>% dplyr::rename(five = RWI) %>% dplyr::select( -Temp.Scenario) %>% arrange(site, MAP.Scenario, struct.cohort.code,MCMC, five)

minusone <-  full.pred %>%  dplyr::select(site, MAP.Scenario, struct.cohort.code, MCMC,RWI, Temp.Scenario, Tmax, MAP) %>%filter(Temp.Scenario %in% "minusone" ) %>% dplyr::rename(minusone = RWI) %>% dplyr::select( -Temp.Scenario) %>% arrange(site, MAP.Scenario, struct.cohort.code,MCMC, minusone)

minustwo <- full.pred %>% dplyr::select(site, MAP.Scenario, struct.cohort.code, MCMC,RWI, Temp.Scenario, Tmax, MAP) %>% filter(Temp.Scenario %in% "minustwo" ) %>% dplyr::rename(minustwo = RWI) %>% dplyr::select( -Temp.Scenario) %>% arrange(site, MAP.Scenario, struct.cohort.code,MCMC, minustwo)

minusthree<-  full.pred %>% dplyr::select(site, MAP.Scenario, struct.cohort.code, MCMC,RWI, Temp.Scenario, Tmax, MAP) %>% filter(Temp.Scenario %in% "minusthree" ) %>% dplyr::rename(minusthree = RWI) %>% dplyr::select( -Temp.Scenario) %>% arrange(site, MAP.Scenario, struct.cohort.code, MCMC, minusthree)

minusfour <- full.pred %>% dplyr::select(site, MAP.Scenario, struct.cohort.code, MCMC,RWI, Temp.Scenario, Tmax, MAP) %>% filter(Temp.Scenario %in% "minusfour" ) %>% dplyr::rename(minusfour = RWI) %>% dplyr::select( -Temp.Scenario) %>% arrange(site, MAP.Scenario, struct.cohort.code,MCMC, minusfour)

minusfive <-  full.pred %>% dplyr::select(site, MAP.Scenario, struct.cohort.code, MCMC,RWI, Temp.Scenario, Tmax, MAP) %>% filter(Temp.Scenario %in% "minusfive" ) %>% dplyr::rename(minusfive = RWI) %>% dplyr::select( -Temp.Scenario) %>% arrange(site, MAP.Scenario, struct.cohort.code, MCMC, minusfive)


newdf <- cbind( past.Temp, one[,c("one")], two[,c("two")], three[,c("three")], four[,c("four")], five[, c("five")], minusone[, c("minusone")], minustwo[, c("minustwo")], minusthree[, c("minusthree")], minusfour[, c("minusfour")], minusfive[, c("minusfive")])       
colnames(newdf) <- c("site", "MAP.Scenario", "struct.cohort.code","MCMC", "base","Tmax", "MAP", "one", "two", "three", "four", "five", "minusone", "minustwo", "minusthree", "minusfour", "minusfive")


# get means and CI for each temperature and MAP scenario:

pct.change.temp <- newdf %>% group_by(site, struct.cohort.code, MAP.Scenario, Tmax, MAP)  %>%
  dplyr::summarise(zero = mean(((base-base)/base)*100, na.rm = TRUE),
                   one = mean(((one-base)/base)*100, na.rm = TRUE),
                   two = mean(((two-base)/base)*100, na.rm = TRUE),
                   three = mean(((three-base)/base)*100, na.rm = TRUE),
                   four = mean(((four-base)/base)*100, na.rm = TRUE),
                   five = mean(((five-base)/base)*100, na.rm = TRUE),
                   minusone = mean(((minusone-base)/base)*100, na.rm = TRUE),
                   minustwo = mean(((minustwo-base)/base)*100, na.rm = TRUE),
                   minusthree = mean(((minusthree-base)/base)*100, na.rm = TRUE),
                   minusfour = mean(((minusfour-base)/base)*100, na.rm = TRUE),
                   minusfive = mean(((minusfive-base)/base)*100, na.rm = TRUE)
  ) %>% 
  gather(key = incT, pct_change, zero:minusfive)


pct.change.hi.ci <- newdf %>% group_by(site, struct.cohort.code, MAP.Scenario, Tmax, MAP)  %>%
  dplyr::summarise(zero = quantile(((base-base)/base)*100, 0.975 ,na.rm = TRUE),
                   one = quantile(((one-base)/base)*100, 0.975 ,na.rm = TRUE),
                   two = quantile(((two-base)/base)*100, 0.975 ,na.rm = TRUE),
                   three = quantile(((three-base)/base)*100, 0.975 ,na.rm = TRUE),
                   four = quantile(((four-base)/base)*100,0.975 , na.rm = TRUE),
                   five = quantile(((five-base)/base)*100,0.975 , na.rm = TRUE),
                   minusone = quantile((( minusone-base)/base)*100,0.975 , na.rm = TRUE),
                   minustwo = quantile((( minustwo-base)/base)*100,0.975 , na.rm = TRUE),
                   minusthree = quantile((( minusthree-base)/base)*100,0.975 , na.rm = TRUE),
                   minusfour = quantile((( minusfour-base)/base)*100,0.975 , na.rm = TRUE),
                   minusfive = quantile((( minusfive-base)/base)*100,0.975 , na.rm = TRUE)
  ) %>% 
  gather(key = incT, ci.hi, zero:minusfive)



pct.change.low.ci <- newdf %>% group_by(site, struct.cohort.code, MAP.Scenario, Tmax, MAP)  %>%
  dplyr::summarise(zero = quantile(((base-base)/base)*100, 0.025 ,na.rm = TRUE),
                   one = quantile(((one-base)/base)*100, 0.025 ,na.rm = TRUE),
                   two = quantile(((two-base)/base)*100, 0.025 ,na.rm = TRUE),
                   three = quantile(((three-base)/base)*100, 0.025 ,na.rm = TRUE),
                   four = quantile(((four-base)/base)*100, 0.025 , na.rm = TRUE),
                   five = quantile(((five-base)/base)*100, 0.025 , na.rm = TRUE),
                   minusone = quantile((( minusone-base)/base)*100,0.025 , na.rm = TRUE),
                   minustwo = quantile((( minustwo-base)/base)*100,0.025 , na.rm = TRUE),
                   minusthree = quantile((( minusthree-base)/base)*100,0.025 , na.rm = TRUE),
                   minusfour = quantile((( minusfour-base)/base)*100,0.025 , na.rm = TRUE),
                   minusfive = quantile((( minusfive-base)/base)*100,0.025 , na.rm = TRUE)
  ) %>% 
  gather(key = incT, ci.low, zero:minusfive)



cis <- merge(pct.change.hi.ci, pct.change.low.ci, by = c("site", "incT", "struct.cohort.code", "MAP.Scenario", "Tmax", "MAP", "MCMC") )
pct.change.temp <- merge(pct.change.temp, cis, by = c("site","incT", "struct.cohort.code", "MAP.Scenario", "Tmax", "MAP", "MCMC")) 

pct.change.temp <- merge(pct.change.temp , sites.unique, by = c("site", "struct.cohort.code"))


Tdf <- data.frame(incT = c("minusfive","minusfour", "minusthree", "minustwo", "minusone","zero", "one", "two", "three", "four", "five"), 
                  deltaT = -5:5)

pct.change.temp <- merge(pct.change.temp, Tdf, by = "incT")
pct.change.temp$struct.cohort.code <- as.character(pct.change.temp$struct.cohort.code)

locs.sites <- locs %>% filter(code %in% unique(pct.change.temp$site)) %>% arrange(pr30yr)
pct.change.temp$site <-factor(pct.change.temp$site, levels = c("BON", "GLL1", "GLL2", "GLL3", "ENG", "AVO", "UNC","MOU", "GLA"))

plus100 <- ggplot(pct.change.temp[pct.change.temp$MAP.Scenario %in% "MAPplus100",], aes(deltaT, pct_change, color = ageclass))+geom_point()+geom_line() + geom_ribbon(data = pct.change.temp[pct.change.temp$MAP.Scenario %in% "MAPplus100",], aes(x = deltaT, ymin = ci.low, ymax = ci.hi, fill = ageclass), alpha = 0.25, colour = NA)+facet_wrap(~site, ncol = 9)+geom_hline(yintercept = 0, linetype = "dashed")+ylab("+100mm ANNUAL PRECIPITATION \n % change in growth")+xlab("increase in Tmax (degC)")



# get regional responses:


pct.change.temp.reg <- newdf %>% group_by(struct.cohort.code, MAP.Scenario, Tmax, MAP)  %>%
  dplyr::summarise(zero = mean(((base-base)/base)*100, na.rm = TRUE),
                   one = mean(((one-base)/base)*100, na.rm = TRUE),
                   two = mean(((two-base)/base)*100, na.rm = TRUE),
                   three = mean(((three-base)/base)*100, na.rm = TRUE),
                   four = mean(((four-base)/base)*100, na.rm = TRUE),
                   five = mean(((five-base)/base)*100, na.rm = TRUE),
                   minusone = mean(((minusone-base)/base)*100, na.rm = TRUE),
                   minustwo = mean(((minustwo-base)/base)*100, na.rm = TRUE),
                   minusthree = mean(((minusthree-base)/base)*100, na.rm = TRUE),
                   minusfour = mean(((minusfour-base)/base)*100, na.rm = TRUE),
                   minusfive = mean(((minusfive-base)/base)*100, na.rm = TRUE)
  ) %>% 
  gather(key = incT, pct_change, zero:minusfive)


pct.change.hi.ci.reg <- newdf %>% group_by( struct.cohort.code, MAP.Scenario, Tmax, MAP)  %>%
  dplyr::summarise(zero = quantile(((base-base)/base)*100, 0.975 ,na.rm = TRUE),
                   one = quantile(((one-base)/base)*100, 0.975 ,na.rm = TRUE),
                   two = quantile(((two-base)/base)*100, 0.975 ,na.rm = TRUE),
                   three = quantile(((three-base)/base)*100, 0.975 ,na.rm = TRUE),
                   four = quantile(((four-base)/base)*100,0.975 , na.rm = TRUE),
                   five = quantile(((five-base)/base)*100,0.975 , na.rm = TRUE),
                   minusone = quantile((( minusone-base)/base)*100,0.975 , na.rm = TRUE),
                   minustwo = quantile((( minustwo-base)/base)*100,0.975 , na.rm = TRUE),
                   minusthree = quantile((( minusthree-base)/base)*100,0.975 , na.rm = TRUE),
                   minusfour = quantile((( minusfour-base)/base)*100,0.975 , na.rm = TRUE),
                   minusfive = quantile((( minusfive-base)/base)*100,0.975 , na.rm = TRUE)
  ) %>% 
  gather(key = incT, ci.hi, zero:minusfive)



pct.change.low.ci.reg <- newdf %>% group_by( struct.cohort.code, MAP.Scenario, Tmax, MAP)  %>%
  dplyr::summarise(zero = quantile(((base-base)/base)*100, 0.025 ,na.rm = TRUE),
                   one = quantile(((one-base)/base)*100, 0.025 ,na.rm = TRUE),
                   two = quantile(((two-base)/base)*100, 0.025 ,na.rm = TRUE),
                   three = quantile(((three-base)/base)*100, 0.025 ,na.rm = TRUE),
                   four = quantile(((four-base)/base)*100, 0.025 , na.rm = TRUE),
                   five = quantile(((five-base)/base)*100, 0.025 , na.rm = TRUE),
                   minusone = quantile((( minusone-base)/base)*100,0.025 , na.rm = TRUE),
                   minustwo = quantile((( minustwo-base)/base)*100,0.025 , na.rm = TRUE),
                   minusthree = quantile((( minusthree-base)/base)*100,0.025 , na.rm = TRUE),
                   minusfour = quantile((( minusfour-base)/base)*100,0.025 , na.rm = TRUE),
                   minusfive = quantile((( minusfive-base)/base)*100,0.025 , na.rm = TRUE)
  ) %>% 
  gather(key = incT, ci.low, zero:minusfive)


cis <- merge(pct.change.hi.ci.reg, pct.change.low.ci.reg, by = c("incT", "struct.cohort.code", "MAP.Scenario", "Tmax", "MAP") )
pct.change.temp.reg <- merge(pct.change.temp.reg, cis, by = c("incT", "struct.cohort.code", "MAP.Scenario", "Tmax", "MAP")) 

pct.change.temp.reg <- merge(pct.change.temp.reg , unique(sites.unique[,c("struct.cohort.code", "structure", "ageclass")]), by = c( "struct.cohort.code"))


Tdf <- data.frame(incT = c("minusfive","minusfour", "minusthree", "minustwo", "minusone","zero", "one", "two", "three", "four", "five"), 
                  deltaT = -5:5)
Tdf$Temperature <- Tdf$deltaT + 26.2

pct.change.temp.reg <- merge(pct.change.temp.reg, Tdf, by = "incT")
pct.change.temp.reg$struct.cohort.code <- as.character(pct.change.temp.reg$struct.cohort.code)


# select only the low and high map values & plot out
pct.change.low.high.MAP <- pct.change.temp.reg %>% filter(ageclass %in% "Modern" & MAP.Scenario %in% c("MAP950", "MAP515")) 

#1b9e77
#d95f02
#7570b3

# plot out pct change in growth estimated:
pct.change.low.high.MAP$Precipitation <- ifelse(pct.change.low.high.MAP$MAP.Scenario %in% "MAP950", "950 mm",
                                                ifelse(pct.change.low.high.MAP$MAP.Scenario %in% "MAP515","515 mm", "580 mm (current mean)"))

pct.change.plot <- ggplot(pct.change.low.high.MAP, aes(Temperature, pct_change, color = Precipitation, linetype = Precipitation))+geom_line() + geom_ribbon(data = pct.change.low.high.MAP, aes(x = Temperature, ymin = ci.low, ymax = ci.hi, fill = Precipitation), alpha = 0.25, colour = NA)+geom_hline(yintercept = 0, linetype = "dashed", color = "grey")+ylab("% change in growth")+facet_wrap(~structure, ncol = 1)+scale_linetype_manual(values = c("950 mm"= "solid","580 mm (current mean)"="dashed", '515 mm' = "solid"), name = "Precipitation")+scale_color_manual(values = c("950 mm"='#008837', '515 mm' = "#7b3294", "580 mm (current mean)" = "#d95f02"), name = "Precipitation")+scale_fill_manual(values = c("950 mm"='#008837', '515 mm' = "#7b3294", "580 mm (current mean)" = "#d95f02"), name = "Precipitation")+xlab(expression("June Mean Maximum Temperature (" * degree * "C)"))+xlim(20,40)+theme_bw()+theme(panel.grid = element_blank())


pct.change.plot


# put altogether in a single figure

png(height = 8, width = 5, units = "in", res = 300, "outputs/growth_model/lag2_reg_struct_x_cohort_re_t_pr_dry_yrs_site_rs_inter/predicted_treegrowth_pct_change_Tmax_modern_only_marginal_fut_climate_boxes_v4.png")
plot_grid( boxplot.tmax, ag.ss.pred.MAP.modonly, pct.change.plot, ncol=1, align = "v", rel_heights = c(0.65,1,1), labels = "AUTO") 
dev.off()

saveRDS(pct.change.low.high.MAP, "/Users/kah/Documents/TreeRings/outputs/data/pct_change_TMAX_precip_scenarios_v4.rds")


# -Get future growth at each site if we change temperature by 1-8 deg C given mean precip, DBH, and RWI---------

mod.mean.clim <- test.dry.pair %>% filter(ageclass %in% "Modern" & year > 1950) %>% group_by(site) %>% dplyr::summarize(mod.mean.Tmax = mean(JUNTmax, na.rm = TRUE), one = mean(JUNTmax, na.rm = TRUE) + 1,
                                                                                                                        two = mean(JUNTmax, na.rm = TRUE) + 2,
                                                                                                                        three = mean(JUNTmax, na.rm = TRUE) + 3,
                                                                                                                        four = mean(JUNTmax, na.rm = TRUE) + 4,
                                                                                                                        five = mean(JUNTmax, na.rm = TRUE) + 5,
                                                                                                                        six = mean(JUNTmax, na.rm = TRUE) + 6,
                                                                                                                        seven =  mean(JUNTmax, na.rm = TRUE) + 7,
                                                                                                                        eight =  mean(JUNTmax, na.rm = TRUE) + 8, 
                                                                                                                        mod.mean.MAP = mean(MAP.prism, na.rm = TRUE), 
                                                                                                                        MAPminus100 = mean(MAP.prism, na.rm = TRUE)-100,
                                                                                                                        MAPminus50 = mean(MAP.prism, na.rm = TRUE)-50,
                                                                                                                        MAPplus50 = mean(MAP.prism, na.rm = TRUE)+50,
                                                                                                                        MAPplus100 = mean(MAP.prism, na.rm = TRUE)+100
)

MAP.scenarios <- mod.mean.clim %>% dplyr::select(site, mod.mean.MAP:MAPplus100) %>% group_by(site) %>% gather(MAP.Scenario, MAP, mod.mean.MAP:MAPplus100)

temp.scenarios <- mod.mean.clim %>% dplyr::select(site, mod.mean.Tmax:eight) %>% group_by(site) %>% gather(Temp.Scenario,Tmax, mod.mean.Tmax:eight)

#next merge by site but keep all
all.scen <- merge(MAP.scenarios, temp.scenarios, by = "site", all = TRUE)

# get the scaled values for MAP and TMAX:
all.scen$MAP.scaled <- as.numeric(scale(all.scen$MAP, attr(MAP.scaled, "scaled:center"), attr(MAP.scaled, "scaled:scale")))

all.scen$T.scaled <- as.numeric(scale(all.scen$Tmax, attr(T.scaled, "scaled:center"), attr(T.scaled, "scaled:scale")))

# expand grid to add structure + cohort class - add 1 diameter, add 1 lag, add 2 lag, then add probe to the model....will need to re-run model

extras <- expand.grid(RWI_1 = 0.103, 
                      RWI_2 = 0.103, 
                      site = unique(all.scen$site), 
                      DBH.scaled = mean(test.dry.pair$DBH.scaled))

sites.unique <- unique(train.dry.pair[, c("site", "struct.cohort.code", "structure", "ageclass")])
extras <- merge(extras, sites.unique[, c("site", "struct.cohort.code")], by = "site")

degree.scenario <- merge(all.scen, extras, by = "site", all = TRUE)
degree.scenario <- merge(degree.scenario, site.num.df, by = "site")

# use estimates of betas to generate estimates for predicted growth:
meanMAP.sim <- degree.scenario %>% filter( MAP.Scenario == "mod.mean.MAP")
meanMAP.sim <- degree.scenario

int.mcmc <- as.mcmc(samp.structure.cohort.re[[1]])
int.mcmc.mat <- as.matrix(int.mcmc)
int.mcmc.dat <- data.frame(int.mcmc.mat)

int.1 <- matrix(rep(NA, nrow(int.mcmc.dat)*length(meanMAP.sim$T.scaled)), nrow = nrow(int.mcmc.dat))

# need to clean this up but this is the basic idea:


# simulate the effect of beta 1 conditional on Tmax
for(i in 1:length(meanMAP.sim$T.scaled)){
  # for struct.cohort == 1
  int.1[,i] <- int.mcmc.dat[,paste0("beta1.", meanMAP.sim[i,"site.num"], ".")]+
    int.mcmc.dat[,paste0("beta2.", meanMAP.sim[i,"struct.cohort.code"], ".")]*meanMAP.sim[i,]$MAP.scaled+    
    int.mcmc.dat[,paste0("beta3.", meanMAP.sim[i,"struct.cohort.code"], ".")]*meanMAP.sim[i,"DBH.scaled"] + 
    int.mcmc.dat[,paste0("beta4.", meanMAP.sim[i,"struct.cohort.code"], ".")]*meanMAP.sim[i,"RWI_1"]  + 
    int.mcmc.dat[,paste0("beta5.", meanMAP.sim[i,"struct.cohort.code"], ".")]*meanMAP.sim[i,"RWI_2"] +
    int.mcmc.dat[,paste0("beta6.", meanMAP.sim[i,"struct.cohort.code"], ".")]*meanMAP.sim[i,"T.scaled"] + 
    int.mcmc.dat[,paste0("beta7.", meanMAP.sim[i,"struct.cohort.code"], ".")] * (meanMAP.sim[i,"MAP.scaled"] *meanMAP.sim[i,"T.scaled"])
  
  
}


# columns are the different degree-site scenario combinations
meanMAP.sim$idval <- 1:length(meanMAP.sim$Tmax)
# rows are the mcmc values
colnames(int.1) <- 1:length(meanMAP.sim$Tmax)
test.m <- melt(int.1)
colnames(test.m) <- c("MCMC", "idval", "Ypred")
full.pred <- left_join(test.m, meanMAP.sim, by = "idval")
full.pred$RWI <- exp(full.pred$Ypred)

site.summary <- full.pred %>% group_by(site, Temp.Scenario,MAP.Scenario, struct.cohort.code) %>% dplyr::summarise(mean = mean(exp(RWI)), Ci.low = quantile(exp(Ypred), 0.025),  Ci.high = quantile(exp(Ypred), 0.975))

ggplot(site.summary[site.summary$MAP.Scenario %in% "MAPminus100",], aes(x = Temp.Scenario, y =mean, color = site))+geom_bar(stat = "identity", position = "dodge")+facet_wrap(~struct.cohort.code)



# get average growth increase relative to mod.mean:


pct.change.low.ci <- full.pred %>% dplyr::select(idval,MCMC,  site, MAP.Scenario, struct.cohort.code, Temp.Scenario, RWI) %>% group_by(site, MAP.Scenario, struct.cohort.code) %>% dplyr::select( -Temp.Scenario)

# spread function was adding nas so, create dfs with RWI values:

past.Temp <- full.pred %>% dplyr::select(site, MAP.Scenario, struct.cohort.code, MCMC, RWI, Temp.Scenario) %>% filter(Temp.Scenario %in% "mod.mean.Tmax") %>% dplyr::rename(zero = RWI) %>% dplyr::select( -Temp.Scenario) %>% arrange(site, MAP.Scenario, struct.cohort.code, MCMC,zero)

one <- full.pred %>% dplyr::select(site, MAP.Scenario, struct.cohort.code, MCMC,RWI, Temp.Scenario) %>% filter(Temp.Scenario %in% "one" ) %>% dplyr::rename(one = RWI) %>% dplyr::select( -Temp.Scenario)%>% arrange(site, MAP.Scenario, struct.cohort.code, MCMC,one)

two <- full.pred %>%  dplyr::select(site, MAP.Scenario, struct.cohort.code,MCMC, RWI, Temp.Scenario) %>%filter(Temp.Scenario %in% "two" ) %>% dplyr::rename(two = RWI) %>% dplyr::select( -Temp.Scenario) %>% arrange(site, MAP.Scenario, struct.cohort.code,MCMC, two)

three <- full.pred %>% dplyr::select(site, MAP.Scenario, struct.cohort.code, MCMC,RWI, Temp.Scenario) %>% filter(Temp.Scenario %in% "three" ) %>% dplyr::rename(three = RWI) %>% dplyr::select( -Temp.Scenario) %>% arrange(site, MAP.Scenario, struct.cohort.code,MCMC, three)

four <- full.pred %>%  dplyr::select(site, MAP.Scenario, struct.cohort.code, MCMC,RWI, Temp.Scenario) %>%filter(Temp.Scenario %in% "four" ) %>% dplyr::rename(four = RWI) %>% dplyr::select( -Temp.Scenario) %>% arrange(site, MAP.Scenario, struct.cohort.code, MCMC,four)

five <-  full.pred %>% dplyr::select(site, MAP.Scenario, struct.cohort.code, MCMC,RWI, Temp.Scenario) %>% filter(Temp.Scenario %in% "five" ) %>% dplyr::rename(five = RWI) %>% dplyr::select( -Temp.Scenario) %>% arrange(site, MAP.Scenario, struct.cohort.code,MCMC, five)

six <-  full.pred %>%  dplyr::select(site, MAP.Scenario, struct.cohort.code, MCMC,RWI, Temp.Scenario) %>%filter(Temp.Scenario %in% "six" ) %>% dplyr::rename(six = RWI) %>% dplyr::select( -Temp.Scenario) %>% arrange(site, MAP.Scenario, struct.cohort.code,MCMC, six)

seven <- full.pred %>% dplyr::select(site, MAP.Scenario, struct.cohort.code, MCMC,RWI, Temp.Scenario) %>% filter(Temp.Scenario %in% "seven" ) %>% dplyr::rename(seven = RWI) %>% dplyr::select( -Temp.Scenario) %>% arrange(site, MAP.Scenario, struct.cohort.code,MCMC, seven)

eight <-  full.pred %>% dplyr::select(site, MAP.Scenario, struct.cohort.code, MCMC,RWI, Temp.Scenario) %>% filter(Temp.Scenario %in% "eight" ) %>% dplyr::rename(eight = RWI) %>% dplyr::select( -Temp.Scenario) %>% arrange(site, MAP.Scenario, struct.cohort.code, MCMC,eight)



newdf <- cbind( past.Temp, one$one, two[,c("two")], three[,c("three")], four[,c("four")], five[, c("five")], six[, c("six")], seven[, c("seven")], eight[, c("eight")])       
colnames(newdf) <- c("site", "MAP.Scenario", "struct.cohort.code","MCMC", "base", "one", "two", "three", "four", "five", "six", "seven", "eight")




pct.change.temp <- newdf %>% group_by(site, struct.cohort.code, MAP.Scenario)  %>%
  dplyr::summarise(zero = mean(((base-base)/base)*100, na.rm = TRUE),
                   one = mean(((one-base)/base)*100, na.rm = TRUE),
                   two = mean(((two-base)/base)*100, na.rm = TRUE),
                   three = mean(((three-base)/base)*100, na.rm = TRUE),
                   four = mean(((four-base)/base)*100, na.rm = TRUE),
                   five = mean(((five-base)/base)*100, na.rm = TRUE),
                   six = mean(((six-base)/base)*100, na.rm = TRUE),
                   seven = mean(((seven-base)/base)*100, na.rm = TRUE),
                   eight = mean(((eight-base)/base)*100, na.rm = TRUE)
  ) %>% 
  gather(key = incT, pct_change, zero:eight)


pct.change.temp.all <- newdf %>% group_by(struct.cohort.code, MAP.Scenario)  %>%
  dplyr::summarise(zero = mean(((base-base)/base)*100, na.rm = TRUE),
                   one = mean(((one-base)/base)*100, na.rm = TRUE),
                   two = mean(((two-base)/base)*100, na.rm = TRUE),
                   three = mean(((three-base)/base)*100, na.rm = TRUE),
                   four = mean(((four-base)/base)*100, na.rm = TRUE),
                   five = mean(((five-base)/base)*100, na.rm = TRUE),
                   six = mean(((six-base)/base)*100, na.rm = TRUE),
                   seven = mean(((seven-base)/base)*100, na.rm = TRUE),
                   eight = mean(((eight-base)/base)*100, na.rm = TRUE)
  ) %>% 
  gather(key = incT, pct_change, zero:eight)


pct.change.hi.ci <- newdf %>% group_by(site, struct.cohort.code, MAP.Scenario)  %>%
  dplyr::summarise(zero = quantile(((base-base)/base)*100, 0.975 ,na.rm = TRUE),
                   one = quantile(((one-base)/base)*100, 0.975 ,na.rm = TRUE),
                   two = quantile(((two-base)/base)*100, 0.975 ,na.rm = TRUE),
                   three = quantile(((three-base)/base)*100, 0.975 ,na.rm = TRUE),
                   four = quantile(((four-base)/base)*100,0.975 , na.rm = TRUE),
                   five = quantile(((five-base)/base)*100,0.975 , na.rm = TRUE),
                   six = quantile(((six-base)/base)*100,0.975 , na.rm = TRUE),
                   seven = quantile(((seven-base)/base)*100,0.975 , na.rm = TRUE),
                   eight = quantile(((eight-base)/base)*100,0.975 , na.rm = TRUE)
  ) %>% 
  gather(key = incT, ci.hi, zero:eight)



pct.change.low.ci <- newdf %>% group_by(site, struct.cohort.code, MAP.Scenario)  %>%
  dplyr::summarise(zero = quantile(((base-base)/base)*100, 0.025 ,na.rm = TRUE),
                   one = quantile(((one-base)/base)*100, 0.025 ,na.rm = TRUE),
                   two = quantile(((two-base)/base)*100, 0.025 ,na.rm = TRUE),
                   three = quantile(((three-base)/base)*100, 0.025 ,na.rm = TRUE),
                   four = quantile(((four-base)/base)*100, 0.025 , na.rm = TRUE),
                   five = quantile(((five-base)/base)*100, 0.025 , na.rm = TRUE),
                   six = quantile(((six-base)/base)*100, 0.025 , na.rm = TRUE),
                   seven = quantile(((seven-base)/base)*100, 0.025 , na.rm = TRUE),
                   eight = quantile(((eight-base)/base)*100, 0.025 , na.rm = TRUE)
  ) %>% 
  gather(key = incT, ci.low, zero:eight)



cis <- merge(pct.change.hi.ci, pct.change.low.ci, by = c("site", "incT", "struct.cohort.code", "MAP.Scenario") )
pct.change.temp <- merge(pct.change.temp, cis, by = c("site","incT", "struct.cohort.code", "MAP.Scenario")) 

pct.change.temp <- merge(pct.change.temp , sites.unique, by = c("site", "struct.cohort.code"))


Tdf <- data.frame(incT = c("zero", "one", "two", "three", "four", "five","six","seven", "eight"), 
                  deltaT = 0:8)

pct.change.temp <- merge(pct.change.temp, Tdf, by = "incT")
pct.change.temp$struct.cohort.code <- as.character(pct.change.temp$struct.cohort.code)

locs.sites <- locs %>% filter(code %in% unique(pct.change.temp$site)) %>% arrange(pr30yr)
pct.change.temp$site <-factor(pct.change.temp$site, levels = c("BON", "GLL1", "GLL2", "GLL3", "ENG", "AVO", "UNC","MOU", "GLA"))

plus100<- ggplot(pct.change.temp[pct.change.temp$MAP.Scenario %in% "MAPplus100",], aes(deltaT, pct_change, color = ageclass))+geom_point()+geom_line() + geom_ribbon(data = pct.change.temp[pct.change.temp$MAP.Scenario %in% "MAPplus100",], aes(x = deltaT, ymin = ci.low, ymax = ci.hi, fill = ageclass), alpha = 0.25, colour = NA)+facet_wrap(~site, ncol = 9)+geom_hline(yintercept = 0, linetype = "dashed")+ylab("+100mm ANNUAL PRECIPITATION \n % change in growth")+xlab("increase in Tmax (degC)")

minus100 <- ggplot(pct.change.temp[pct.change.temp$MAP.Scenario %in% "MAPminus100",], aes(deltaT, pct_change, color = ageclass))+geom_point()+geom_line() + geom_ribbon(data = pct.change.temp[pct.change.temp$MAP.Scenario %in% "MAPminus100",], aes(x = deltaT, ymin = ci.low, ymax = ci.hi, fill = ageclass), alpha = 0.25, colour = NA)+facet_wrap(~site, ncol = 9)+geom_hline(yintercept = 0, linetype = "dashed")+ylab("-100mm ANNUAL PRECIPITATION \n % change in growth")+xlab("increase in Tmax (degC)")

minus0 <- ggplot(pct.change.temp[pct.change.temp$MAP.Scenario %in% "mod.mean.MAP",], aes(deltaT, pct_change, color = ageclass))+geom_line() + geom_ribbon(data = pct.change.temp[pct.change.temp$MAP.Scenario %in% "mod.mean.MAP",], aes(x = deltaT, ymin = ci.low, ymax = ci.hi, fill = ageclass), alpha = 0.25, colour = NA)+facet_wrap(~site, ncol = 9)+geom_hline(yintercept = 0, linetype = "dashed")+ylab("0mm ANNUAL PRECIPITATION \n % change in growth")+xlab("increase in Tmax (degC)")

png(height = 12, width = 12, units = "in", res = 300, "outputs/growth_model/lag2_reg_struct_x_cohort_re_t_pr_dry_yrs_site_rs_inter/future_change_ingrowth.png")
plot_grid( plus100, minus0, minus100, nrow = 3, align = "hv", labels = "AUTO")
dev.off()

# get mean differences between time periods:
summary.degrees <- full.pred %>% group_by(site, MCMC, idval,MAP.Scenario, struct.cohort.code) %>% spread(Temp.Scenario, Ypred, drop = TRUE) %>% dplyr::summarise(zero = 0, one.diff = mean(one - mod.mean.Tmax),
                                                                                                                                                                 two.diff = mean(two- mod.mean.Tmax), 
                                                                                                                                                                 three.diff = mean(three - mod.mean.Tmax), 
                                                                                                                                                                 four.diff = mean(four - mod.mean.Tmax),
                                                                                                                                                                 five.diff = mean(five - mod.mean.Tmax),
                                                                                                                                                                 six.diff = mean(six -  mod.mean.Tmax), 
                                                                                                                                                                 seven.diff = mean(seven- mod.mean.Tmax), 
                                                                                                                                                                 eight.diff = mean(eight - mod.mean.Tmax)
)


