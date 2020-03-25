
library(ggplot2)
library(rjags)
library(ggplot2)
library(caTools)
library(ggridges)
library(tidyr)
library(reshape2)
library(dplyr)
# preliminary models of tree growth for ITRDB data:

rwl.itrdb.clim.nona <- readRDS( "Data/ITRDB/full.clim.prism.rds")


# split training and testing data:


rwl.itrdb.clim.nona$Precip.scaled = as.vector(scale(rwl.itrdb.clim.nona$ppt_MAP.wy, center = TRUE, scale = TRUE))
rwl.itrdb.clim.nona.Precip.scaled = scale(rwl.itrdb.clim.nona$ppt_MAP.wy, center = TRUE, scale = TRUE)

rwl.itrdb.clim.nona$Temp.jun.scaled = as.vector(scale(rwl.itrdb.clim.nona$tmax_06, center = TRUE, scale = TRUE))
rwl.itrdb.clim.nona.jun.scaled = scale(rwl.itrdb.clim.nona$tmax_06, center = TRUE, scale = TRUE)


#splits <- unlist(strsplit(unique(ED.sort_lag$Site), "X"))
covert_site_codes <- data.frame(site_num = 1:length(unique(rwl.itrdb.clim.nona$studyCode)),
                                studyCode = unique(rwl.itrdb.clim.nona$studyCode))

covert_spec_codes <- data.frame(spec = 1:length(unique(rwl.itrdb.clim.nona$SPEC.CODE)),
                                SPEC.CODE = unique(rwl.itrdb.clim.nona$SPEC.CODE))


rwl.itrdb.clim.nona <- left_join(rwl.itrdb.clim.nona, covert_site_codes, by = "studyCode")
rwl.itrdb.clim.nona <- left_join(rwl.itrdb.clim.nona, covert_spec_codes, by = "SPEC.CODE")


# clean up the data and split testing and training:
rwl.full <- rwl.itrdb.clim.nona[!is.na(rwl.itrdb.clim.nona$RWI_1) & !is.na(rwl.itrdb.clim.nona$RWI_2)  ,]
rwl.full$RWI <- as.numeric(rwl.full$RWI)
rwl.full$RWI_1 <- as.numeric(rwl.full$RWI_1)
rwl.full$RWI_2 <- as.numeric(rwl.full$RWI_2)
rwl.full$Age <- as.numeric(rwl.full$Age)

# also get rid of 0 values??
rwl.full <- rwl.full[!rwl.full$RWI == 0, ]

QUMA <- rwl.full[rwl.full$SPEC.CODE %in% c("QUMA", "QUAL"),]

QUMA$spec <- ifelse(QUMA$SPEC.CODE %in% "QUMA", 1, 2)

covert_site_codes.QUMA <- data.frame(site_num.QUMA = 1:length(unique(QUMA$studyCode)),
                                studyCode = unique(QUMA$studyCode))

QUMA <- left_join(QUMA, covert_site_codes.QUMA, by = "studyCode")

msk <- caTools::sample.split( QUMA, SplitRatio = 3/4, group = NULL )

train.QUMA <- QUMA[msk,]
test.QUMA <- QUMA[!msk,]


ggplot(train.QUMA, aes(Temp.jun.scaled, RWI))+geom_point()
ggplot(train.QUMA, aes(Precip.scaled, RWI))+geom_point()
ggplot(train.QUMA, aes(Age, RWI))+geom_point()


# get training and testing for Hemlocks:
TSCA <- rwl.full[rwl.full$SPEC.CODE %in% c("TSCA", "TSCR"),]

TSCA$spec <- ifelse(TSCA$SPEC.CODE %in% "TSCA", 1, 2)

covert_site_codes.TSCA <- data.frame(site_num.TSCA = 1:length(unique(TSCA$studyCode)),
                                     studyCode = unique(TSCA$studyCode))

TSCA <- left_join(TSCA, covert_site_codes.TSCA, by = "studyCode")

msk <- caTools::sample.split( TSCA, SplitRatio = 3/4, group = NULL )

train.TSCA <- TSCA[msk,]
test.TSCA <- TSCA[!msk,]


ggplot(train.TSCA, aes(Temp.jun.scaled, RWI))+geom_point()
ggplot(train.TSCA, aes(Precip.scaled, RWI))+geom_point()
ggplot(train.TSCA, aes(Age, RWI))+geom_point()

# get training and testing for Beech:
FAGR <- rwl.full[rwl.full$SPEC.CODE %in% c("FAGR"),]

FAGR$spec <- ifelse(FAGR$SPEC.CODE %in% "FAGR", 1, 2)

covert_site_codes.FAGR <- data.frame(site_num.FAGR = 1:length(unique(FAGR$studyCode)),
                                     studyCode = unique(FAGR$studyCode))

FAGR <- left_join(FAGR, covert_site_codes.FAGR, by = "studyCode")

msk <- caTools::sample.split( FAGR, SplitRatio = 3/4, group = NULL )

train.FAGR <- FAGR[msk,]
test.FAGR <- FAGR[!msk,]


ggplot(train.FAGR, aes(Temp.jun.scaled, RWI))+geom_point()
ggplot(train.FAGR, aes(Precip.scaled, RWI))+geom_point()
ggplot(train.FAGR, aes(Age, RWI))+geom_point()


# get training and testing for Beech:
FAGR <- rwl.full[rwl.full$SPEC.CODE %in% c("FAGR"),]

FAGR$spec <- ifelse(FAGR$SPEC.CODE %in% "FAGR", 1, 2)

covert_site_codes.FAGR <- data.frame(site_num.FAGR = 1:length(unique(FAGR$studyCode)),
                                     studyCode = unique(FAGR$studyCode))

FAGR <- left_join(FAGR, covert_site_codes.FAGR, by = "studyCode")

msk <- caTools::sample.split( FAGR, SplitRatio = 3/4, group = NULL )

train.FAGR <- FAGR[msk,]
test.FAGR <- FAGR[!msk,]


ggplot(train.FAGR, aes(Temp.jun.scaled, RWI))+geom_point()
ggplot(train.FAGR, aes(Precip.scaled, RWI))+geom_point()
ggplot(train.FAGR, aes(Age, RWI))+geom_point()

# split training and testing data

msk <- caTools::sample.split( rwl.full, SplitRatio = 3/4, group = NULL )

train.full <- rwl.full[msk,]
test.full <- rwl.full[!msk,]


ggplot(train.full, aes(Temp.jun.scaled, RWI))+geom_point()
ggplot(train.full, aes(Precip.scaled, RWI))+geom_point()
ggplot(train.full, aes(Age, RWI))+geom_point()

# run a model with the full dataset available
#------------------------------------------------
# Develop bayesian mixed models for ITRDB data



mod <- lm(RWI ~ Precip.scaled + Temp.jun.scaled + RWI_1 + RWI_2 + Age + (Precip.scaled*Temp.jun.scaled), data = train.FAGR)
summary(mod)
#plot(mod)
preds <- predict(mod, test.full)
plot(test.full$RWI, preds)
abline(a = 0, b = 1, col= "red")

# basic lm with no re explais ~ 50 percent of variance

# model gwbi as a function of Temp, Precip, CO2, with random slops for time period & site random intercept
ITRDB_site_re_spec <- "model{

# for each the overall population include re for sites:

# Likelihood
for(i in 1:n){
# process model
Y[i]   ~ dnorm(gfunc[i], inv.var) # Y is agbi

# function g()
gfunc[i] <- alpha[sites[i]] + beta1[spec[i]]*Precip.scaled[i] + beta2[spec[i]]*Temp.jja.scaled[i] + beta3[spec[i]]*RWI_1[i] + beta4[spec[i]]*RWI_2[i] +  beta5[spec[i]]*Age[i] 

}


# Assume normal priors for betas, but generate a beta + alpha for each ageclass
for(c in 1:length(C)){
beta1[c] ~ dnorm(mu_beta1, inv_beta1)
beta2[c] ~ dnorm(mu_beta2, inv_beta2)
beta3[c] ~ dnorm(mu_beta3, inv_beta3)
beta4[c] ~ dnorm(mu_beta4, inv_beta4)
beta5[c] ~ dnorm(mu_beta5, inv_beta5)

}

for(s in 1:length(S)){
alpha[s] ~ dnorm(mu_alpha, inv_alpha)
}

# use normal hyperpriors for each hyperparamters 
mu_alpha ~ dunif(-2, 2)
mu_beta1 ~ dunif(-2, 2)
mu_beta2 ~ dunif(-2, 2)
mu_beta3 ~ dunif(-2, 2)
mu_beta4 ~ dunif(-2, 2)
mu_beta5 ~ dunif(-2, 2)


inv_alpha   ~ dgamma(0.001, 0.001)
sigma_alpha <- 1/sqrt(inv_alpha)
inv_beta1   ~ dgamma(0.001, 0.001)
sigma_beta1 <- 1/sqrt(inv_beta1)
inv_beta2   ~ dgamma(0.001, 0.001)
sigma_beta2 <- 1/sqrt(inv_beta2)
inv_beta3   ~ dgamma(0.001, 0.001)
sigma_beta3 <- 1/sqrt(inv_beta3)
inv_beta4   ~ dgamma(0.001, 0.001)
sigma_beta4 <- 1/sqrt(inv_beta4)
inv_beta5   ~ dgamma(0.001, 0.001)
sigma_beta5 <- 1/sqrt(inv_beta5)



# Non-informative Prior for the inverse population variances

#alpha_ref ~ dnorm(0,0.1)
inv.var   ~ dgamma(0.001, 0.001)
sigma     <- 1/sqrt(inv.var)


# Predictions
for(i in 1:np){
# process model
Ypred[i]   ~ dnorm(gfunc.p[i], inv.var) # Y is gwbbi

# function g()
gfunc.p[i] <- alpha[sites.p[i]] + beta1[spec.p[i]]*Precip.scaled.p[i] + beta2[spec.p[i]]*Temp.jja.scaled.p[i] + beta3[spec.p[i]]*RWI_1.p[i] + beta4[spec.p[i]]*RWI_2.p[i] + beta5[spec.p[i]]*Age.p[i] 

}


}"






reg.ITRDB.by_spec <- jags.model(textConnection(ITRDB_site_re_spec), 
                                 data = list(Y = log(train.QUMA$RWI), n=length(train.QUMA$RWI), Precip.scaled = train.QUMA$Precip.scaled, Temp.jja.scaled = train.QUMA$Temp.jun.scaled, RWI_1 = train.QUMA$RWI_1, RWI_2 = train.QUMA$RWI_2, Age= train.QUMA$Age, 
                                             spec = as.numeric(train.QUMA$spec), S = unique(train.QUMA$site_num.QUMA),  C = unique(train.QUMA$spec), sites = as.numeric(train.QUMA$site_num.QUMA), np=length(test.QUMA$spec), 
                                             sites.p = test.QUMA$site_num, Precip.scaled.p = test.QUMA$Precip.scaled, Temp.jja.scaled.p = test.QUMA$Temp.jun.scaled, RWI_1.p = test.QUMA$RWI_1, RWI_2.p = test.QUMA$RWI_2, 
                                             spec.p = as.numeric(test.QUMA$spec), Age.p = test.QUMA$Age),
                                             
                                             # nprobe=length(probe.ED$struct.cohort.code), 
                                             # sites.probe = probe.ED$site_num, Precip.scaled.probe = probe.ED$DI.scaled, Temp.jja.scaled.probe = probe.ED$T.scaled, agbi_1.probe = probe.ED$RWI_1, agbi_2.probe = probe.ED$RWI_2, agbi_3.probe = probe.ED$RWI_3, agbi_4.probe = probe.ED$RWI_4,
                                             # spec.probe = as.numeric(probe.ED$struct.cohort.code)), 
                               n.chains = 3, n.adapt = 100)


update(reg.ITRDB.by_spec, 1000); # Burnin for 1000 samples to start, then go higher later

#samp.ED.period <- coda.samples(reg.EDel.by_period, 
#                           variable.names=c("alpha","beta1", "beta2","beta3","beta3","sigma","sigma_alpha", "sigma_beta1", "sigma_beta2","sigma_beta3", "sigma_beta4"), 
#                          n.chains = 3, n.iter=2000, thin = 10)
samp.ITRDB.QUMA <- coda.samples(reg.ITRDB.by_spec, 
                               variable.names=c("alpha", "beta1", "beta2", "beta3", "beta4", "beta5"), 
                               n.chains = 3, n.iter=5000, thin = 1)

saveRDS(samp.ITRDB.QUMA, "outputs/ITRDB_models/ITRDB_site_species_re/samps_QUMA.rds")
samp.ITRDB.QUMA <- readRDS( "outputs/ITRDB_models/ITRDB_site_species_re/samps_QUMA.rds")

summary(samp.ITRDB.QUMA)
samps.ITRDB <- samp.ITRDB.QUMA[[1]]

traceplot(samps.ITRDB[,81:85])

ypred.ITRDB.QUMA <- coda.samples(reg.ITRDB.by_spec, 
                                variable.names=c("Ypred"), 
                                n.chains = 3, n.iter=500, thin = 1)

saveRDS(ypred.ITRDB.QUMA, "outputs/ITRDB_models/ITRDB_site_species_re/Ypred_samps_QUMA.rds")


Yp.samps <- ypred.ITRDB.QUMA[[1]]
Yp.samps <- data.frame(Yp.samps) 
Yp.m <- melt(Yp.samps)
Yp.summary <- Yp.m %>% group_by(variable) %>% dplyr::summarise(Predicted = mean(exp(value)),
                                                               ci.hi = quantile(exp(value),0.975),
                                                               ci.lo = quantile(exp(value),0.025))
Yp.summary$Observed <- test.QUMA$RWI
Yp.summary$SPEC.CODE <- test.QUMA$SPEC.CODE

pred.obs <- summary(lm(colMeans(exp(Yp.samps)) ~ test.QUMA$RWI))

p.o.plot <- ggplot(Yp.summary, aes(Observed, Predicted))+geom_point(color = "black", size = 0.5)+
  geom_errorbar(data = Yp.summary,aes(ymin=ci.lo, ymax=ci.hi), color = "grey", alpha = 0.5)+
  geom_point(data = Yp.summary, aes(Observed, Predicted, color = SPEC.CODE), size = 0.5)+
  geom_abline(aes(slope = 1, intercept = 0), color = "red", linetype = "dashed")+
  geom_text(data=data.frame(pred.obs$r.squared), aes( label = paste("R^2: ", pred.obs$r.squared, sep="")),parse=T,x=1, y=7)+ylim(-0.5, 12)+xlim(-0.5, 12)

# note poor model fit!
png(width = 6, height = 5, units = "in", res = 300, "outputs/ITRDB_models/ITRDB_site_species_re/pred_vs_obs_QUMA_QUAL.png")
p.o.plot
dev.off()

# calculate MSE & BIAS:

MSE1   <- mean((colMeans(Yp.samps)-test.QUMA$RWI)^2)
BIAS1  <- mean(colMeans(Yp.samps)-test.QUMA$RWI)

# write model summary output to a file!

model.summary <- data.frame(model = "mixed_effects_reg", 
                            MSE = MSE1, 
                            BIAS = BIAS1, 
                            Rsq = pred.obs$r.squared)



#---------- run the same model but with Fagus Grandifolia
reg.ITRDB.by_spec <- jags.model(textConnection(ITRDB_site_re_spec), 
                                data = list(Y = log(train.FAGR$RWI), n=length(train.FAGR$RWI), Precip.scaled = train.FAGR$Precip.scaled, Temp.jja.scaled = train.FAGR$Temp.jun.scaled, RWI_1 = train.FAGR$RWI_1, RWI_2 = train.FAGR$RWI_2, Age= train.FAGR$Age, 
                                            spec = as.numeric(train.FAGR$spec), S = unique(train.FAGR$site_num.FAGR),  C = unique(train.FAGR$spec), sites = as.numeric(train.FAGR$site_num.FAGR), np=length(test.FAGR$spec), 
                                            sites.p = test.FAGR$site_num.FAGR, Precip.scaled.p = test.FAGR$Precip.scaled, Temp.jja.scaled.p = test.FAGR$Temp.jun.scaled, RWI_1.p = test.FAGR$RWI_1, RWI_2.p = test.FAGR$RWI_2, 
                                            spec.p = as.numeric(test.FAGR$spec), Age.p = test.FAGR$Age),
                                
                                # nprobe=length(probe.ED$struct.cohort.code), 
                                # sites.probe = probe.ED$site_num, Precip.scaled.probe = probe.ED$DI.scaled, Temp.jja.scaled.probe = probe.ED$T.scaled, agbi_1.probe = probe.ED$RWI_1, agbi_2.probe = probe.ED$RWI_2, agbi_3.probe = probe.ED$RWI_3, agbi_4.probe = probe.ED$RWI_4,
                                # spec.probe = as.numeric(probe.ED$struct.cohort.code)), 
                                n.chains = 3, n.adapt = 100)


update(reg.ITRDB.by_spec, 1000); # Burnin for 1000 samples to start, then go higher later

#samp.ED.period <- coda.samples(reg.EDel.by_period, 
#                           variable.names=c("alpha","beta1", "beta2","beta3","beta3","sigma","sigma_alpha", "sigma_beta1", "sigma_beta2","sigma_beta3", "sigma_beta4"), 
#                          n.chains = 3, n.iter=2000, thin = 10)
samp.ITRDB.FAGR <- coda.samples(reg.ITRDB.by_spec, 
                                variable.names=c("alpha", "beta1", "beta2", "beta3", "beta4", "beta5"), 
                                n.chains = 3, n.iter=5000, thin = 1)

saveRDS(samp.ITRDB.FAGR, "outputs/ITRDB_models/ITRDB_site_species_re/samps_FAGR.rds")


samp.ITRDB.FAGR <- coda.samples(reg.ITRDB.by_spec, 
                                variable.names=c("alpha", "beta1", "beta2", "beta3", "beta4", "beta5"), 
                                n.chains = 3, n.iter=5000, thin = 1)

saveRDS(samp.ITRDB.FAGR, "outputs/ITRDB_models/ITRDB_site_species_re/samps_FAGR.rds")
samp.ITRDB.FAGR <- readRDS( "outputs/ITRDB_models/ITRDB_site_species_re/samps_FAGR.rds")

summary(samp.ITRDB.FAGR)
samps.ITRDB <- samp.ITRDB.FAGR[[1]]

traceplot(samps.ITRDB)

ypred.ITRDB.FAGR <- coda.samples(reg.ITRDB.by_spec, 
                                 variable.names=c("Ypred"), 
                                 n.chains = 3, n.iter=500, thin = 1)

saveRDS(ypred.ITRDB.FAGR, "outputs/ITRDB_models/ITRDB_site_species_re/Ypred_samps_FAGR.rds")


Yp.samps <- ypred.ITRDB.FAGR[[1]]
Yp.samps <- data.frame(Yp.samps) 
Yp.m <- melt(Yp.samps)
Yp.summary <- Yp.m %>% group_by(variable) %>% dplyr::summarise(Predicted = mean(exp(value)),
                                                               ci.hi = quantile(exp(value),0.975),
                                                               ci.lo = quantile(exp(value),0.025))
Yp.summary$Observed <- test.FAGR$RWI
Yp.summary$SPEC.CODE <- test.FAGR$SPEC.CODE

pred.obs <- summary(lm(colMeans(log(Yp.samps)) ~ test.FAGR$RWI))

p.o.plot <- ggplot(Yp.summary, aes(Observed, Predicted))+geom_point(color = "black", size = 0.5)+
  geom_errorbar(data = Yp.summary,aes(ymin=ci.lo, ymax=ci.hi), color = "grey", alpha = 0.5)+
  geom_point(data = Yp.summary, aes(Observed, Predicted, color = SPEC.CODE), size = 0.5)+
  geom_abline(aes(slope = 1, intercept = 0), color = "red", linetype = "dashed")+
  geom_text(data=data.frame(pred.obs$r.squared), aes( label = paste("R^2: ", pred.obs$r.squared, sep="")),parse=T,x=1, y=7)+ylim(-0.5, 12)+xlim(-0.5, 12)

# note poor model fit!
png(width = 6, height = 5, units = "in", res = 300, "outputs/ITRDB_models/ITRDB_site_species_re/pred_vs_obs_FAGR_QUAL.png")
p.o.plot
dev.off()

# calculate MSE & BIAS:

MSE1   <- mean((colMeans(Yp.samps)-test.FAGR$RWI)^2)
BIAS1  <- mean(colMeans(Yp.samps)-test.FAGR$RWI)

# write model summary output to a file!

model.summary <- data.frame(model = "mixed_effects_reg", 
                            MSE = MSE1, 
                            BIAS = BIAS1, 
                            Rsq = pred.obs$r.squared)



# ---------------------Lets try fitting a model without site specific intercepts-----------------

ITRDB_spec <- "model{

# for each the overall population include re for sites:

# Likelihood
for(i in 1:n){
# process model
Y[i]   ~ dnorm(gfunc[i], inv.var) # Y is agbi

# function g()
gfunc[i] <- alpha + beta1[spec[i]]*Precip.scaled[i] + beta2[spec[i]]*Temp.jja.scaled[i] + beta3[spec[i]]*RWI_1[i] + beta4[spec[i]]*RWI_2[i] +  beta5[spec[i]]*Age[i] 

}


# Assume normal priors for betas, but generate a beta + alpha for each ageclass
for(c in 1:length(C)){
beta1[c] ~ dnorm(mu_beta1, inv_beta1)
beta2[c] ~ dnorm(mu_beta2, inv_beta2)
beta3[c] ~ dnorm(mu_beta3, inv_beta3)
beta4[c] ~ dnorm(mu_beta4, inv_beta4)
beta5[c] ~ dnorm(mu_beta5, inv_beta5)

}

#for(s in 1:length(S)){
alpha ~ dnorm(0, 0.001)
#}

# use normal hyperpriors for each hyperparamters 
#mu_alpha ~ dunif(-2, 2)
mu_beta1 ~ dunif(-2, 2)
mu_beta2 ~ dunif(-2, 2)
mu_beta3 ~ dunif(-2, 2)
mu_beta4 ~ dunif(-2, 2)
mu_beta5 ~ dunif(-2, 2)


#inv_alpha   ~ dgamma(0.001, 0.001)
#sigma_alpha <- 1/sqrt(inv_alpha)
inv_beta1   ~ dgamma(0.001, 0.001)
sigma_beta1 <- 1/sqrt(inv_beta1)
inv_beta2   ~ dgamma(0.001, 0.001)
sigma_beta2 <- 1/sqrt(inv_beta2)
inv_beta3   ~ dgamma(0.001, 0.001)
sigma_beta3 <- 1/sqrt(inv_beta3)
inv_beta4   ~ dgamma(0.001, 0.001)
sigma_beta4 <- 1/sqrt(inv_beta4)
inv_beta5   ~ dgamma(0.001, 0.001)
sigma_beta5 <- 1/sqrt(inv_beta5)



# Non-informative Prior for the inverse population variances

#alpha_ref ~ dnorm(0,0.1)
inv.var   ~ dgamma(0.001, 0.001)
sigma     <- 1/sqrt(inv.var)


# Predictions
for(i in 1:np){
# process model
Ypred[i]   ~ dnorm(gfunc.p[i], inv.var) # Y is gwbbi

# function g()
gfunc.p[i] <- alpha + beta1[spec.p[i]]*Precip.scaled.p[i] + beta2[spec.p[i]]*Temp.jja.scaled.p[i] + beta3[spec.p[i]]*RWI_1.p[i] + beta4[spec.p[i]]*RWI_2.p[i] + beta5[spec.p[i]]*Age.p[i] 

}
}"



reg.ITRDB.by_spec_a <- jags.model(textConnection(ITRDB_spec), 
                                 data = list(Y = log(train.full$RWI), n=length(train.full$RWI), Precip.scaled = train.full$Precip.scaled, Temp.jja.scaled = train.full$Temp.jun.scaled, RWI_1 = train.full$RWI_1, RWI_2 = train.full$RWI_2, Age= train.full$Age, 
spec = as.numeric(train.full$spec), S = unique(train.full$site_num),  C = unique(train.full$spec), sites = as.numeric(train.full$site_num), np=length(test.full$spec), 
sites.p = test.full$site_num, Precip.scaled.p = test.full$Precip.scaled, Temp.jja.scaled.p = test.full$Temp.jun.scaled, RWI_1.p = test.full$RWI_1, RWI_2.p = test.full$RWI_2, 
spec.p = as.numeric(test.full$spec), Age.p = test.full$Age),

# nprobe=length(probe.ED$struct.cohort.code), 
# sites.probe = probe.ED$site_num, Precip.scaled.probe = probe.ED$DI.scaled, Temp.jja.scaled.probe = probe.ED$T.scaled, agbi_1.probe = probe.ED$RWI_1, agbi_2.probe = probe.ED$RWI_2, agbi_3.probe = probe.ED$RWI_3, agbi_4.probe = probe.ED$RWI_4,
# spec.probe = as.numeric(probe.ED$struct.cohort.code)), 
n.chains = 3, n.adapt = 100)


update(reg.ITRDB.by_spec_a, 1000); # Burnin for 1000 samples to start, then go higher later

#samp.ED.period <- coda.samples(reg.EDel.by_period, 
#                           variable.names=c("alpha","beta1", "beta2","beta3","beta3","sigma","sigma_alpha", "sigma_beta1", "sigma_beta2","sigma_beta3", "sigma_beta4"), 
#                          n.chains = 3, n.iter=2000, thin = 10)
samp.ITRDB.all <- coda.samples(reg.ITRDB.by_spec_a, 
variable.names=c("alpha", "beta1", "beta2", "beta3", "beta4", "beta5"), 
n.chains = 3, n.iter=5000, thin = 1)

saveRDS(samp.ITRDB.all, "outputs/ITRDB_models/ITRDB_site_species_re/samps_all_no_site_re.rds")
samp.ITRDB.all <- readRDS( "outputs/ITRDB_models/ITRDB_site_species_re/samps_all_no_site_re.rds")
