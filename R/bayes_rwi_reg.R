library(rjags)

# lets make a bayseian linear regression of the detrended RWI and JJA.PDSI:


##################

RWIS <- read.csv("outputs/data/full_det_ghcn_rwi.csv")

head(RWIS)

RWIS <- RWIS[!is.na(RWIS$RWI),]
fit <- lm(RWI~JJA.pdsi , data = RWIS)
summary(fit)

#### 
# need to specify a model with hyperparameters for tree size class

Y <- as.vector(RWIS$RWI) 
#Y     <- 100*dat[,2]
#Y     <- (Y-mean(Y))/sd(Y)
pdsi <- as.vector( RWIS$JJA.pdsi)
ageclass <- as.numeric( RWIS$ageclass)

n     <- length(RWIS$RWI)


model_string <- "model{

# Likelihood
for(i in 1:n){
Y[i]   ~ dnorm(mu[i],inv.var)
mu[i] <- beta[1] + beta[2]*pdsi[i] 
}

# Prior for beta
for(j in 1:2){
beta[j] ~ dnorm(0,0.0001)
}

# Prior for the inverse variance
inv.var   ~ dgamma(0.01, 0.01)
sigma     <- 1/sqrt(inv.var)

}"

reg.model.age <- jags.model(textConnection(model_string), 
                    data = list(Y=Y,n=n,pdsi=pdsi))

update(reg.model.age, 10000, progress.bar="none"); # Burnin for 10000 samples

samp <- coda.samples(reg.model.age, 
                     variable.names=c("beta","sigma"), 
                    n.chains = 3, n.iter=20000, progress.bar="none")

summary(samp)


# plot mcmc + the parameter distn
plot(samp)

# save a data frame with beta2 sensitivity and the 
samp.df <- summary(samp)
  
samps.df.sum<- data.frame(mean = samp.df$statistics[2,"Mean"], 
           ci.low = samp.df$quantiles[2,"2.5%"], 
           ci.high = samp.df$quantiles[2,"97.5%"])
  

ggplot(samps.df.sum, aes("sens",mean))+geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin=ci.low, ymax = ci.high), size = 0.2, width = 0.2)

# compare to the ols models
fit <- lm(Y ~ pdsi )
summary(fit)



lm1_mcmc <- as.mcmc(samp)
plot(lm1_mcmc)


nvalues <- 100
b_length_new <- seq(min(RWIS$JJA.pdsi), max(RWIS$JJA.pdsi), length.out = nvalues)
lm1_mcmc_combi <- as.mcmc(rbind(lm1_mcmc[,1], lm1_mcmc[,2], lm1_mcmc[,3]))

pred_mean_mean <- mean(lm1_mcmc_combi[, "alpha"]) + b_length_new * mean(lm1_mcmc_combi[, "beta"])

pred_mean_mean <- mean(lm1_mcmc[, "beta[1]"]) + b_length_new * mean(lm1_mcmc[, "beta[2]"])


pred_mean_dist <- matrix(NA, nrow = nrow(lm1_mcmc_combi), ncol = nvalues)
for (i in 1:nrow(pred_mean_dist)){
  pred_mean_dist[i,] <- lm1_mcmc[i,"beta[1]"] + b_length_new * lm1_mcmc[i,"beta[2]"]
}
credible_lower <- apply(pred_mean_dist, MARGIN = 2, quantile, prob = 0.025)
credible_upper <- apply(pred_mean_dist, MARGIN = 2, quantile, prob = 0.975)


lm1_mcmc_combi_rep <- do.call(rbind, rep(list(lm1_mcmc_combi), 50)) # replication

# Draw random values for all parameter combinations (rows) and body length values (columns):
pred_data_dist <- matrix(NA, nrow = nrow(lm1_mcmc), ncol = nvalues)
for (i in 1:nrow(pred_data_dist)){
  pred_data_dist[i,] <- lm1_mcmc[i,"beta[1]"] + b_length_new * lm1_mcmc[i,"beta[2]"] +
    rnorm(nvalues, mean = 0, sd = lm1_mcmc[i, "sigma"])
}

# Calculate quantiles:
uncertain_lower <- apply(pred_data_dist, MARGIN = 2, quantile, prob = 0.025)
uncertain_upper <- apply(pred_data_dist, MARGIN = 2, quantile, prob = 0.975)


plot(RWI ~ JJA.pdsi, data = RWIS)
lines(b_length_new, pred_mean_mean)
lines(b_length_new, credible_lower, lty = 2, col = "green")
lines(b_length_new, credible_upper, lty = 2, col = "green")
lines(b_length_new, uncertain_lower, lty = 2, col = "red")
lines(b_length_new, uncertain_upper, lty = 2, col = "red")



# Linear regression with fixed effects for age class:
# need to convert ageclass to 0 and 1's
RWIS.full <- RWIS
RWIS <- RWIS[RWIS$RWI <  3, ]
RWIS$ages <- ifelse(RWIS$ageclass %in% "Modern", 1, 0)
jagsdata_s2 <- with(RWIS, list(RWI = RWI, pdsi = JJA.pdsi, ageclass = ages, N = length(RWI)))

lm2_jags <- "model{
  # Likelihood:
  for (i in 1:N){
    RWI[i] ~ dnorm(mu[i], tau) # tau is precision (1 / variance)
    mu[i] <- alpha[1] + ageclass[i] * alpha[2] + (beta[1] + beta[2] * ageclass[i]) * pdsi[i]
  }
  # Priors:
  for (i in 1:2){
    alpha[i] ~ dnorm(0, 0.01)
    beta[i] ~ dnorm(0, 0.01)
  }
  sigma ~ dunif(0, 100)
  tau <- 1 / (sigma * sigma)
}"




init_values <- function(){
  list(alpha = rnorm(2), beta = rnorm(2), sigma = runif(1))
}




params <- c("alpha", "beta", "sigma")



reg.model.age <- jags.model(textConnection(lm2_jags), 
                            data = jagsdata_s2)

update(reg.model.age , 10000, progress.bar="none"); # Burnin for 10000 samples

samp <- coda.samples(reg.model.age , 
                     variable.names=c("beta","alpha","sigma"), 
                     n.chains = 3, n.iter=20000, progress.bar="none")

summary(samp)
plot(samp)

summary(lm(RWI ~ JJA.pdsi*ages, data = RWIS))

lm1_mcmc <- as.mcmc(samp)
plot(lm1_mcmc)


# lets plot relationshipw with credible and ci for age class == 0:

nvalues <- 100
b_length_new <- seq(min(RWIS$JJA.pdsi), max(RWIS$JJA.pdsi), length.out = nvalues)
age_length_new <- rep(0, length.out = nvalues)

lm1_mcmc_combi <- as.mcmc(rbind(lm1_mcmc[,1], lm1_mcmc[,2], lm1_mcmc[,3], lm1_mcmc[,4], lm1_mcmc[,5]))

#pred_mean_mean <- mean(lm1_mcmc_combi[, "alpha"]) + b_length_new * mean(lm1_mcmc_combi[, "beta"])
pred_mean_mean <- mean(lm1_mcmc[, "alpha[1]"]) + age_length_new*mean(lm1_mcmc[, "alpha[2]"]) + b_length_new *(mean(lm1_mcmc[, "beta[1]"]) + mean(lm1_mcmc[, "beta[2]"])*age_length_new)


pred_mean_dist <- matrix(NA, nrow = nrow(lm1_mcmc_combi), ncol = nvalues)
for (i in 1:nrow(pred_mean_dist)){
  pred_mean_dist[i,] <- lm1_mcmc[i, "alpha[1]"] + age_length_new*lm1_mcmc[i, "alpha[2]"] + b_length_new *(lm1_mcmc[i, "beta[1]"] + lm1_mcmc[i, "beta[2]"]*age_length_new)
}
credible_lower <- apply(pred_mean_dist, MARGIN = 2, quantile, prob = 0.025)
credible_upper <- apply(pred_mean_dist, MARGIN = 2, quantile, prob = 0.975)


lm1_mcmc_combi_rep <- do.call(rbind, rep(list(lm1_mcmc_combi), 50)) # replication

# Draw random values for all parameter combinations (rows) and body length values (columns):
pred_data_dist <- matrix(NA, nrow = nrow(lm1_mcmc), ncol = nvalues)
for (i in 1:nrow(pred_data_dist)){
  pred_data_dist[i,] <- lm1_mcmc[i, "alpha[1]"] + age_length_new*lm1_mcmc[i, "alpha[2]"] + b_length_new *(lm1_mcmc[i, "beta[1]"] + lm1_mcmc[i, "beta[2]"]*age_length_new) +
    rnorm(nvalues, mean = 0, sd = lm1_mcmc[i, "sigma"])
}

# Calculate quantiles:
uncertain_lower <- apply(pred_data_dist, MARGIN = 2, quantile, prob = 0.025)
uncertain_upper <- apply(pred_data_dist, MARGIN = 2, quantile, prob = 0.975)


# lets plot relationshipw with credible and ci for age class == 0:

nvalues <- 100
b_length_new.1 <- seq(min(RWIS$JJA.pdsi), max(RWIS$JJA.pdsi), length.out = nvalues)
age_length_new.1 <- rep(1, length.out = nvalues)

lm1_mcmc_combi.1 <- as.mcmc(rbind(lm1_mcmc[,1], lm1_mcmc[,2], lm1_mcmc[,3], lm1_mcmc[,4], lm1_mcmc[,5]))

#pred_mean_mean <- mean(lm1_mcmc_combi[, "alpha"]) + b_length_new * mean(lm1_mcmc_combi[, "beta"])
pred_mean_mean.1 <- mean(lm1_mcmc[, "alpha[1]"]) + age_length_new.1*mean(lm1_mcmc[, "alpha[2]"]) + b_length_new *(mean(lm1_mcmc[, "beta[1]"]) + mean(lm1_mcmc[, "beta[2]"])*age_length_new.1)


pred_mean_dist.1 <- matrix(NA, nrow = nrow(lm1_mcmc_combi), ncol = nvalues)
for (i in 1:nrow(pred_mean_dist.1)){
  pred_mean_dist.1[i,] <- lm1_mcmc[i, "alpha[1]"] + age_length_new.1*lm1_mcmc[i, "alpha[2]"] + b_length_new *(lm1_mcmc[i, "beta[1]"] + lm1_mcmc[i, "beta[2]"]*age_length_new.1)
}
credible_lower.1 <- apply(pred_mean_dist.1, MARGIN = 2, quantile, prob = 0.025)
credible_upper.1 <- apply(pred_mean_dist.1, MARGIN = 2, quantile, prob = 0.975)


lm1_mcmc_combi_rep.1 <- do.call(rbind, rep(list(lm1_mcmc_combi), 50)) # replication

# Draw random values for all parameter combinations (rows) and body length values (columns):
pred_data_dist.1 <- matrix(NA, nrow = nrow(lm1_mcmc), ncol = nvalues)
for (i in 1:nrow(pred_data_dist.1)){
  pred_data_dist.1[i,] <- lm1_mcmc[i, "alpha[1]"] + age_length_new.1*lm1_mcmc[i, "alpha[2]"] + b_length_new *(lm1_mcmc[i, "beta[1]"] + lm1_mcmc[i, "beta[2]"]*age_length_new.1) +
    rnorm(nvalues, mean = 0, sd = lm1_mcmc[i, "sigma"])
}

# Calculate quantiles:
uncertain_lower.1 <- apply(pred_data_dist.1, MARGIN = 2, quantile, prob = 0.025)
uncertain_upper.1 <- apply(pred_data_dist.1, MARGIN = 2, quantile, prob = 0.975)

png("outputs/growth_model/RWI_jja_pdsi_ageclass_predCI_no_outliers.png")
plot(RWI ~ JJA.pdsi, data = RWIS)
lines(b_length_new, pred_mean_mean)
lines(b_length_new, credible_lower, lty = 2, col = "green")
lines(b_length_new, credible_upper, lty = 2, col = "green")
lines(b_length_new, uncertain_lower, lty = 2, col = "red")
lines(b_length_new, uncertain_upper, lty = 2, col = "red")
# add lines for past trees
lines(b_length_new, pred_mean_mean.1, col = "orange")
lines(b_length_new, credible_lower.1, lty = 2, col = "purple")
lines(b_length_new, credible_upper.1, lty = 2, col = "purple")
lines(b_length_new, uncertain_lower.1, lty = 2, col = "blue")
lines(b_length_new, uncertain_upper.1, lty = 2, col = "blue")
dev.off()


plot(b_length_new, pred_mean_mean, ylim=c(0,2))
plot(b_length_new, pred_mean_mean)

plot(b_length_new, credible_lower,"l", lty = 2, col = "green")
lines(b_length_new, credible_upper, lty = 2, col = "green")
lines(b_length_new, credible_lower.1, lty = 2, col = "purple")
lines(b_length_new, credible_upper.1, lty = 2, col = "purple")

# need to genereate predictions above for all ages == 0, then all ages == 1

# >>>>>>>>>>>>>>>>>>>now for a mixed effects model w/ random effects for tree size classes: <<<<<<<<<<<<<<<<<

RWIS <- read.csv("outputs/det.age.clim.ghcn.sizes.csv")
#"outputs/det.age.clim.ghcn.sizes.csv"
head(RWIS)

RWIS <- RWIS[!is.na(RWIS$RWI),]
RWIS <- RWIS[!is.na(RWIS$dbhclass),]
fit <- lm(RWI~JJA.pdsi*dbhclass , data = RWIS)
summary(fit)

#### 
# need to specify a model with hyperparameters for tree size class

Y <- as.vector(RWIS$RWI) 
#Y     <- 100*dat[,2]
#Y     <- (Y-mean(Y))/sd(Y)
pdsi <- as.vector( RWIS$JJA.pdsi)
dbhclass <- as.numeric( RWIS$dbhclass)

dbhs <- as.numeric( RWIS$dbhclass)
n     <- length(RWIS$RWI)

jagsdata_s2 <- with(RWIS, list(RWI = RWI, pdsi = JJA.pdsi, dbhclass = dbhs, N = length(RWI)))

lm2_jags <- "model{
# Likelihood:
for (i in 1:N){
RWI[i] ~ dnorm(mu[i], tau) # tau is precision (1 / variance)
mu[i] <- alpha[1] + dbhclass[i] * alpha[2] + (beta[1] + beta[2] * dbhclass[i]) * pdsi[i]
}
# Priors:
for (i in 1:2){
alpha[i] ~ dnorm(0, 0.01)
beta[i] ~ dnorm(0, 0.01)
}
sigma ~ dunif(0, 100)
tau <- 1 / (sigma * sigma)
}"




init_values <- function(){
  list(alpha = rnorm(2), beta = rnorm(2), sigma = runif(1))
}




params <- c("alpha", "beta", "sigma")



reg.model.age <- jags.model(textConnection(lm2_jags), 
                            data = jagsdata_s2)

update(reg.model.age , 10000, progress.bar="none"); # Burnin for 10000 samples

samp <- coda.samples(reg.model.age , 
                     variable.names=c("beta","alpha","sigma"), 
                     n.chains = 3, n.iter=20000, progress.bar="none")

summary(samp)
plot(samp)

summary(lm(RWI ~ JJA.pdsi*dbhs, data = RWIS))

lm1_mcmc <- as.mcmc(samp)
plot(lm1_mcmc)


# now lets plot the different estimates + credible intervals:

# lets plot relationshipw with credible and ci for age class == 0:

nvalues <- 100
b_length_new <- seq(min(RWIS$JJA.pdsi), max(RWIS$JJA.pdsi), length.out = nvalues)
dbh_length_new <- rep(0, length.out = nvalues)

lm1_mcmc_combi <- as.mcmc(rbind(lm1_mcmc[,1], lm1_mcmc[,2], lm1_mcmc[,3], lm1_mcmc[,4], lm1_mcmc[,5]))

#pred_mean_mean <- mean(lm1_mcmc_combi[, "alpha"]) + b_length_new * mean(lm1_mcmc_combi[, "beta"])
pred_mean_mean <- mean(lm1_mcmc[, "alpha[1]"]) + dbh_length_new*mean(lm1_mcmc[, "alpha[2]"]) + b_length_new *(mean(lm1_mcmc[, "beta[1]"]) + mean(lm1_mcmc[, "beta[2]"])*dbh_length_new)


pred_mean_dist <- matrix(NA, nrow = nrow(lm1_mcmc_combi), ncol = nvalues)
for (i in 1:nrow(pred_mean_dist)){
  pred_mean_dist[i,] <- lm1_mcmc[i, "alpha[1]"] + dbh_length_new*lm1_mcmc[i, "alpha[2]"] + b_length_new *(lm1_mcmc[i, "beta[1]"] + lm1_mcmc[i, "beta[2]"]*dbh_length_new)
}
credible_lower <- apply(pred_mean_dist, MARGIN = 2, quantile, prob = 0.025)
credible_upper <- apply(pred_mean_dist, MARGIN = 2, quantile, prob = 0.975)


lm1_mcmc_combi_rep <- do.call(rbind, rep(list(lm1_mcmc_combi), 50)) # replication

# Draw random values for all parameter combinations (rows) and body length values (columns):
pred_data_dist <- matrix(NA, nrow = nrow(lm1_mcmc), ncol = nvalues)
for (i in 1:nrow(pred_data_dist)){
  pred_data_dist[i,] <- lm1_mcmc[i, "alpha[1]"] + dbh_length_new*lm1_mcmc[i, "alpha[2]"] + b_length_new *(lm1_mcmc[i, "beta[1]"] + lm1_mcmc[i, "beta[2]"]*dbh_length_new) +
    rnorm(nvalues, mean = 0, sd = lm1_mcmc[i, "sigma"])
}

# Calculate quantiles:
uncertain_lower <- apply(pred_data_dist, MARGIN = 2, quantile, prob = 0.025)
uncertain_upper <- apply(pred_data_dist, MARGIN = 2, quantile, prob = 0.975)


# lets plot relationshipw with credible and ci for dbh class == 0:

nvalues <- 100
b_length_new.1 <- seq(min(RWIS$JJA.pdsi), max(RWIS$JJA.pdsi), length.out = nvalues)
dbh_length_new.1 <- rep(1, length.out = nvalues)

lm1_mcmc_combi.1 <- as.mcmc(rbind(lm1_mcmc[,1], lm1_mcmc[,2], lm1_mcmc[,3], lm1_mcmc[,4], lm1_mcmc[,5]))

#pred_mean_mean <- mean(lm1_mcmc_combi[, "alpha"]) + b_length_new * mean(lm1_mcmc_combi[, "beta"])
pred_mean_mean.1 <- mean(lm1_mcmc[, "alpha[1]"]) + dbh_length_new.1*mean(lm1_mcmc[, "alpha[2]"]) + b_length_new *(mean(lm1_mcmc[, "beta[1]"]) + mean(lm1_mcmc[, "beta[2]"])*dbh_length_new.1)


pred_mean_dist.1 <- matrix(NA, nrow = nrow(lm1_mcmc_combi), ncol = nvalues)
for (i in 1:nrow(pred_mean_dist.1)){
  pred_mean_dist.1[i,] <- lm1_mcmc[i, "alpha[1]"] + dbh_length_new.1*lm1_mcmc[i, "alpha[2]"] + b_length_new *(lm1_mcmc[i, "beta[1]"] + lm1_mcmc[i, "beta[2]"]*dbh_length_new.1)
}
credible_lower.1 <- apply(pred_mean_dist.1, MARGIN = 2, quantile, prob = 0.025)
credible_upper.1 <- apply(pred_mean_dist.1, MARGIN = 2, quantile, prob = 0.975)


lm1_mcmc_combi_rep.1 <- do.call(rbind, rep(list(lm1_mcmc_combi), 50)) # replication

# Draw random values for all parameter combinations (rows) and body length values (columns):
pred_data_dist.1 <- matrix(NA, nrow = nrow(lm1_mcmc), ncol = nvalues)
for (i in 1:nrow(pred_data_dist.1)){
  pred_data_dist.1[i,] <- lm1_mcmc[i, "alpha[1]"] + dbh_length_new.1*lm1_mcmc[i, "alpha[2]"] + b_length_new *(lm1_mcmc[i, "beta[1]"] + lm1_mcmc[i, "beta[2]"]*dbh_length_new.1) +
    rnorm(nvalues, mean = 0, sd = lm1_mcmc[i, "sigma"])
}

# Calculate quantiles:
uncertain_lower.1 <- apply(pred_data_dist.1, MARGIN = 2, quantile, prob = 0.025)
uncertain_upper.1 <- apply(pred_data_dist.1, MARGIN = 2, quantile, prob = 0.975)


plot(RWI ~ JJA.pdsi, data = RWIS)
lines(b_length_new, pred_mean_mean)
lines(b_length_new, credible_lower, lty = 2, col = "green")
lines(b_length_new, credible_upper, lty = 2, col = "green")
lines(b_length_new, uncertain_lower, lty = 2, col = "red")
lines(b_length_new, uncertain_upper, lty = 2, col = "red")
# add lines for past trees
lines(b_length_new, pred_mean_mean.1, col = "orange")
lines(b_length_new, credible_lower.1, lty = 2, col = "purple")
lines(b_length_new, credible_upper.1, lty = 2, col = "purple")
lines(b_length_new, uncertain_lower.1, lty = 2, col = "blue")
lines(b_length_new, uncertain_upper.1, lty = 2, col = "blue")
dev.off()


plot(b_length_new,pred_mean_mean, ylim=c(0,2))
plot(b_length_new, pred_mean_mean)

plot(b_length_new, credible_lower,"l", lty = 2, col = "green")
lines(b_length_new, credible_upper, lty = 2, col = "green")
lines(b_length_new, credible_lower.1, lty = 2, col = "purple")
lines(b_length_new, credible_upper.1, lty = 2, col = "purple")






#>>>>>>>>>>>>>>>>>>>>>>>>>>>> Models of response to drought by dbh class >>>>>>>>>>>>>>>>>>>>>>>>>>.

library(rjags)

# lets make a bayseian linear regression of the detrended RWI and JJA.PDSI:


##################

RWIS <- read.csv("outputs/det.age.clim.ghcn.sizes.csv")

head(RWIS)

RWIS <- RWIS[!is.na(RWIS$RWI),]
fit <- lm(RWI~JJA.pdsi , data = RWIS)
summary(fit)

#### 
# need to specify a model with hyperparameters for tree size class
# < 20 
Y.20 <- as.vector(RWIS[RWIS$dbhclass %in% "< 20",]$RWI) 
pdsi.20 <- as.vector( RWIS[RWIS$dbhclass %in% "< 20",]$JJA.pdsi)
n.20     <- length(RWIS[RWIS$dbhclass %in% "< 20",]$RWI)

#20 - 40
Y.20.40 <- as.vector(RWIS[RWIS$dbhclass %in% "20 - 40",]$RWI) 
pdsi.20.40 <- as.vector( RWIS[RWIS$dbhclass %in% "20 - 40",]$JJA.pdsi)
n.20.40     <- length(RWIS[RWIS$dbhclass %in% "20 - 40",]$RWI)

# 40 - 60
Y.40.60 <- as.vector(RWIS[RWIS$dbhclass %in% "40 - 60",]$RWI) 
pdsi.40.60 <- as.vector( RWIS[RWIS$dbhclass %in% "40 - 60",]$JJA.pdsi)
n.40.60     <- length(RWIS[RWIS$dbhclass %in% "40 - 60",]$RWI)

#60 - 80
Y.60.80 <- as.vector(RWIS[RWIS$dbhclass %in% "60 - 80",]$RWI) 
pdsi.60.80 <- as.vector( RWIS[RWIS$dbhclass %in% "60 - 80",]$JJA.pdsi)
n.60.80     <- length(RWIS[RWIS$dbhclass %in% "60 - 80",]$RWI)

#80 + 
Y.80 <- as.vector(RWIS[RWIS$dbhclass %in% ">80",]$RWI) 
pdsi.80 <- as.vector( RWIS[RWIS$dbhclass %in% ">80",]$JJA.pdsi)
n.80     <- length(RWIS[RWIS$dbhclass %in% ">80",]$RWI)


model_string <- "model{

# Likelihood
for(i in 1:n){
Y[i]   ~ dnorm(mu[i],inv.var)
mu[i] <- beta[1] + beta[2]*pdsi[i] 
}

# Prior for beta
for(j in 1:2){
beta[j] ~ dnorm(0,0.0001)
}

# Prior for the inverse variance
inv.var   ~ dgamma(0.01, 0.01)
sigma     <- 1/sqrt(inv.var)

}"


# now run the regression models on these datasets---Ideally we do them all together and account for individual facters, but here we want to get a basic idea of slopes
# model for <20 cm diameter
reg.20 <- jags.model(textConnection(model_string), 
                            data = list(Y=Y.20,n=n.20,pdsi=pdsi.20))

update(reg.20, 10000, progress.bar="none"); # Burnin for 10000 samples

samp.20 <- coda.samples(reg.20, 
                     variable.names=c("beta","sigma"), 
                     n.chains = 3, n.iter=20000, progress.bar="none")

summary(samp.20)

# model for 20-40 cm diameter
reg.20.40 <- jags.model(textConnection(model_string), 
                     data = list(Y=Y.20.40, n=n.20.40, pdsi=pdsi.20.40))

update(reg.20.40, 10000, progress.bar="none"); # Burnin for 10000 samples

samp.20.40 <- coda.samples(reg.20.40, 
                        variable.names=c("beta","sigma"), 
                        n.chains = 3, n.iter=20000, progress.bar="none")
summary(samp.20.40 )

# model for 40-60 cm diameter
reg.40.60 <- jags.model(textConnection(model_string), 
                        data = list(Y=Y.40.60, n=n.40.60, pdsi=pdsi.40.60))

update(reg.40.60, 10000, progress.bar="none"); # Burnin for 10000 samples

samp.40.60 <- coda.samples(reg.40.60, 
                           variable.names=c("beta","sigma"), 
                           n.chains = 3, n.iter=20000, progress.bar="none")
summary(samp.40.60 )
plot(samp.40.60)

# model for 40-60 cm diameter
reg.60.80 <- jags.model(textConnection(model_string), 
                        data = list(Y=Y.60.80, n=n.60.80, pdsi=pdsi.60.80))

update(reg.60.80, 10000, progress.bar="none"); # Burnin for 10000 samples

samp.60.80 <- coda.samples(reg.60.80, 
                           variable.names=c("beta","sigma"), 
                           n.chains = 3, n.iter=20000, progress.bar="none")
summary(samp.60.80 )


# model for 40-60 cm diameter
reg.80 <- jags.model(textConnection(model_string), 
                        data = list(Y=Y.80, n=n.80, pdsi=pdsi.80))

update(reg.80, 10000, progress.bar="none"); # Burnin for 10000 samples

samp.80 <- coda.samples(reg.80, 
                           variable.names=c("beta","sigma"), 
                           n.chains = 3, n.iter=20000, progress.bar="none")
summary(samp.80)



# plot mcmc + the parameter distn
plot(samp.80)

# save a data frame with beta2 sensitivity and the 
samp.df <- summary(samp)

samps.df.sum <- data.frame(dbhclass = c(">20", "20 - 40", "40 - 60", "60 - 80", ">80"),
                           mean = c(summary(samp.20)$statistics[2,"Mean"],
                                   summary(samp.20.40)$statistics[2,"Mean"],
                                   summary(samp.40.60)$statistics[2,"Mean"],
                                   summary(samp.60.80)$statistics[2,"Mean"],
                                   summary(samp.80)$statistics[2,"Mean"] ),
                          ci.low = c(summary(samp.20)$quantiles[2,"2.5%"],
                                     summary(samp.20.40)$quantiles[2,"2.5%"],
                                     summary(samp.40.60)$quantiles[2,"2.5%"],
                                     summary(samp.60.80)$quantiles[2,"2.5%"],
                                     summary(samp.80)$quantiles[2,"2.5%"] ), 
                          ci.high = c(summary(samp.20)$quantiles[2,"97.5%"],
                                      summary(samp.20.40)$quantiles[2,"97.5%"],
                                      summary(samp.40.60)$quantiles[2,"97.5%"],
                                      summary(samp.60.80)$quantiles[2,"97.5%"],
                                      summary(samp.80)$quantiles[2,"97.5%"] ))

samps.df.sum$dbhclass <- factor(x = samps.df.sum$dbhclass, levels = c(">20", "20 - 40", "40 - 60", "60 - 80", ">80"))

# now plot all of these together in a barplot:
png(height = 4, width = 4, units = "in", res = 300, "outputs/barplots/all_sensitivity_jja_pdsi_by_dbhclass_bw.png")
ggplot(samps.df.sum, aes(dbhclass, mean, fill = dbhclass))+geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin=ci.low, ymax = ci.high), size = 0.2, width = 0.2, color = "white") +xlab("Diameter class (cm)") + ylab("Sensitivity to Summer PDSI")+theme_black(base_size = 15)
dev.off()


# now plot all of these together in a barplot:
png(height = 3, width = 4, units = "in", res = 300, "outputs/barplots/0-80_sensitivity_jja_pdsi_by_dbhclass_bw.png")
ggplot(samps.df.sum[!samps.df.sum$dbhclass %in% ">80",], aes(dbhclass, mean, fill = dbhclass))+geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin=ci.low, ymax = ci.high), size = 0.2, width = 0.2, color = "white") +xlab("Diameter class (cm)") + ylab("Sensitivity to Summer PDSI")+theme_black(base_size = 10)
dev.off()



# now do the same thing here for forest vs. savanna:
RWIS <- read.csv("outputs/det.age.clim.ghcn.sizes.covclass.csv")
head(RWIS)

RWIS <- RWIS[!is.na(RWIS$RWI),]
fit <- lm(RWI~JJA.pdsi , data = RWIS)
summary(fit)

# fore forest
Y.F <- as.vector(RWIS[RWIS$Description %in% "Forest",]$RWI) 
pdsi.F <- as.vector( RWIS[RWIS$Description %in% "Forest",]$JJA.pdsi)
n.F     <- length(RWIS[RWIS$Description %in% "Forest",]$RWI)
reg.F <- jags.model(textConnection(model_string), 
                     data = list(Y=Y.F,n=n.F,pdsi=pdsi.F))

update(reg.F, 10000, progress.bar="none"); # Burnin for 10000 samples

samp.F <- coda.samples(reg.F, 
                        variable.names=c("beta","sigma"), 
                        n.chains = 3, n.iter=20000, progress.bar="none")

summary(samp.F)

# for savanna
Y.S <- as.vector(RWIS[RWIS$Description %in% "Savanna",]$RWI) 
pdsi.S <- as.vector( RWIS[RWIS$Description %in% "Savanna",]$JJA.pdsi)
n.S    <- length(RWIS[RWIS$Description %in% "Savanna",]$RWI)
reg.S <- jags.model(textConnection(model_string), 
                    data = list(Y=Y.S,n=n.S,pdsi=pdsi.S))

update(reg.S, 10000, progress.bar="none"); # Burnin for 10000 samples

samp.S <- coda.samples(reg.S, 
                       variable.names=c("beta","sigma"), 
                       n.chains = 3, n.iter=20000, progress.bar="none")

summary(samp.S)

samps.df.cover.class <- data.frame(coverclass = c("Forest", "Savanna"),
                           mean = c(summary(samp.F)$statistics[2,"Mean"],
                                    summary(samp.S)$statistics[2,"Mean"]
                                    ),
                           ci.low = c(summary(samp.F)$quantiles[2,"2.5%"],
                                      summary(samp.S)$quantiles[2,"2.5%"]), 
                           ci.high = c(summary(samp.F)$quantiles[2,"97.5%"],
                                       summary(samp.S)$quantiles[2,"97.5%"]))
                                  
png(height = 4, width = 4, units = "in", res = 300, "outputs/barplots/all_sensitivity_jja_pdsi_by_coverclass_bw.png")
ggplot(samps.df.cover.class, aes(coverclass, mean, fill = coverclass))+geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin=ci.low, ymax = ci.high), size = 0.2, width = 0.2, color = "white")+scale_fill_manual(values = c("Savanna"="#7b3294", 'Forest' ="#006837")) +xlab("Stand Structure") + ylab("Sensitivity to Summer PDSI")+theme_black(base_size = 15)
dev.off()
