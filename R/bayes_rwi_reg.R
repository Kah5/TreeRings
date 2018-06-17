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

#80 + 
Y.80 <- as.vector(RWIS[RWIS$dbhclass %in% c(">80", "60 - 80"),]$RWI) 
pdsi.80 <- as.vector( RWIS[RWIS$dbhclass %in% c(">80", "60 - 80"),]$JJA.pdsi)
n.80     <- length(RWIS[RWIS$dbhclass %in% c(">80", "60 - 80"),]$RWI)

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

samps.df.sum <- data.frame(dbhclass = c(">20", "20 - 40", "40 - 60", "60 - 80", ">60"),
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

samps.df.sum$dbhclass <- factor(x = samps.df.sum$dbhclass, levels = c(">20", "20 - 40", "40 - 60", "60 - 80", ">60"))

# now plot all of these together in a barplot:
png(height = 4, width = 4, units = "in", res = 300, "outputs/barplots/all_sensitivity_jja_pdsi_by_dbhclass_bw.png")
ggplot(samps.df.sum, aes(dbhclass, mean, fill = dbhclass))+geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin=ci.low, ymax = ci.high), size = 0.2, width = 0.2, color = "white") +xlab("Diameter class (cm)") + ylab("Sensitivity to Summer PDSI")+theme_black(base_size = 15)
dev.off()


# now plot all of these together in a barplot:
png(height = 3, width = 4, units = "in", res = 300, "outputs/barplots/0-80_sensitivity_jja_pdsi_by_dbhclass_bw.png")
ggplot(samps.df.sum[!samps.df.sum$dbhclass %in% "60 - 80",], aes(dbhclass, mean, fill = dbhclass))+geom_bar(stat = "identity")+
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



# do the same for each species:
# note: now this is the dominant oak species at the site--need to fix species code matching @ 
RWIS <- read.csv("outputs/det.age.clim.ghcn.sizes.covclass.csv")
head(RWIS)

RWIS <- RWIS[!is.na(RWIS$RWI),]
fit <- lm(RWI~JJA.pdsi , data = RWIS)
summary(fit)

# for QURA
Y.QURA <- as.vector(RWIS[RWIS$species %in% "QURA",]$RWI) 
pdsi.QURA <- as.vector( RWIS[RWIS$species %in% "QURA",]$JJA.pdsi)
n.QURA     <- length(RWIS[RWIS$species %in% "QURA",]$RWI)
reg.QURA <- jags.model(textConnection(model_string), 
                    data = list(Y=Y.QURA,n=n.QURA,pdsi=pdsi.QURA))

update(reg.QURA, 10000, progress.bar="none"); # Burnin for 10000 samples

samp.QURA <- coda.samples(reg.QURA, 
                       variable.names=c("beta","sigma"), 
                       n.chains = 3, n.iter=20000, progress.bar="none")

summary(samp.QURA)

# for QUMA
Y.QUMA <- as.vector(RWIS[RWIS$species %in% "QUMA",]$RWI) 
pdsi.QUMA <- as.vector( RWIS[RWIS$species %in% "QUMA",]$JJA.pdsi)
n.QUMA     <- length(RWIS[RWIS$species %in% "QUMA",]$RWI)
reg.QUMA <- jags.model(textConnection(model_string), 
                       data = list(Y=Y.QUMA,n=n.QUMA,pdsi=pdsi.QUMA))

update(reg.QURA, 10000, progress.bar="none"); # Burnin for 10000 samples

samp.QUMA <- coda.samples(reg.QUMA, 
                          variable.names=c("beta","sigma"), 
                          n.chains = 3, n.iter=20000, progress.bar="none")

summary(samp.QUMA)

# for QUAL
Y.QUAL <- as.vector(RWIS[RWIS$species %in% "QUAL/QUMA",]$RWI) 
pdsi.QUAL <- as.vector( RWIS[RWIS$species %in% "QUAL/QUMA",]$JJA.pdsi)
n.QUAL     <- length(RWIS[RWIS$species %in% "QUAL/QUMA",]$RWI)
reg.QUAL <- jags.model(textConnection(model_string), 
                       data = list(Y=Y.QUAL,n=n.QUAL,pdsi=pdsi.QUAL))

update(reg.QUAL, 10000, progress.bar="none"); # Burnin for 10000 samples

samp.QUAL <- coda.samples(reg.QUAL, 
                          variable.names=c("beta","sigma"), 
                          n.chains = 3, n.iter=20000, progress.bar="none")

summary(samp.QUMA)



samps.df.spec <- data.frame(species = c("Red Oak", "Bur Oak","White Oak"),
                                   mean = c(summary(samp.QURA)$statistics[2,"Mean"],
                                            summary(samp.QUMA)$statistics[2,"Mean"],
                                            summary(samp.QUAL)$statistics[2,"Mean"]
                                   ),
                                   ci.low = c(summary(samp.QURA)$quantiles[2,"2.5%"],
                                              summary(samp.QUMA)$quantiles[2,"2.5%"],
                                              summary(samp.QUAL)$quantiles[2,"2.5%"]), 
                                   ci.high = c(summary(samp.QURA)$quantiles[2,"97.5%"],
                                               summary(samp.QUMA)$quantiles[2,"97.5%"],
                                               summary(samp.QUAL)$quantiles[2,"97.5%"]))

samps.df.spec$species <- factor(x = samps.df.spec$species, levels = c("Bur Oak", "White Oak", "Red Oak"))

png(height = 4, width = 6, units = "in", res = 300, "outputs/barplots/all_sensitivity_jja_pdsi_by_species_bw.png")
ggplot(samps.df.spec, aes(species, mean, fill = species))+geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin=ci.low, ymax = ci.high), size = 0.2, width = 0.2, color = "white")+scale_fill_manual(values = c("Red Oak"="#ca0020", 'Bur Oak' ="#1b9e77", "White Oak" = "#7570b3")) +xlab(" ") + ylab("Sensitivity to Summer PDSI")+theme_black(base_size = 15)
dev.off()
#


# >>>>>>>>>>>>>>>>>>>> now run the model for each individual site: <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# for AVO
Y.AVO <- as.vector(RWIS[RWIS$site %in% "AVO",]$RWI) 
pdsi.AVO <- as.vector( RWIS[RWIS$site %in% "AVO",]$JJA.pdsi)
n.AVO    <- length(RWIS[RWIS$site %in% "AVO",]$RWI)
reg.AVO <- jags.model(textConnection(model_string), 
                     data = list(Y=Y.AVO,n=n.AVO,pdsi=pdsi.AVO))

update(reg.AVO, 10000, progress.bar="none"); # Burnin for 10000 samples

samp.AVO <- coda.samples(reg.AVO, 
                        variable.names=c("beta","sigma"), 
                        n.chains = 3, n.iter=20000, progress.bar="none")

summary(samp.AVO)

# for BON
Y.BON <- as.vector(RWIS[RWIS$site %in% "BON",]$RWI) 
pdsi.BON <- as.vector( RWIS[RWIS$site %in% "BON",]$JJA.pdsi)
n.BON    <- length(RWIS[RWIS$site %in% "BON",]$RWI)
reg.BON <- jags.model(textConnection(model_string), 
                      data = list(Y=Y.BON,n=n.BON,pdsi=pdsi.BON))

update(reg.BON, 10000, progress.bar="none"); # Burnin for 10000 samples

samp.BON <- coda.samples(reg.BON, 
                         variable.names=c("beta","sigma"), 
                         n.chains = 3, n.iter=20000, progress.bar="none")

summary(samp.BON)


# for HIC
Y.HIC <- as.vector(RWIS[RWIS$site %in% "HIC",]$RWI) 
pdsi.HIC <- as.vector( RWIS[RWIS$site %in% "HIC",]$JJA.pdsi)
n.HIC    <- length(RWIS[RWIS$site %in% "HIC",]$RWI)
reg.HIC <- jags.model(textConnection(model_string), 
                      data = list(Y=Y.HIC,n=n.HIC,pdsi=pdsi.HIC))

update(reg.HIC, 10000, progress.bar="none"); # Burnin for 10000 samples

samp.HIC <- coda.samples(reg.HIC, 
                         variable.names=c("beta","sigma"), 
                         n.chains = 3, n.iter=20000, progress.bar="none")

summary(samp.HIC)

# for MOU
Y.MOU <- as.vector(RWIS[RWIS$site %in% "MOU",]$RWI) 
pdsi.MOU <- as.vector( RWIS[RWIS$site %in% "MOU",]$JJA.pdsi)
n.MOU    <- length(RWIS[RWIS$site %in% "MOU",]$RWI)
reg.MOU <- jags.model(textConnection(model_string), 
                      data = list(Y=Y.MOU,n=n.MOU,pdsi=pdsi.MOU))

update(reg.MOU, 10000, progress.bar="none"); # Burnin for 10000 samples

samp.MOU <- coda.samples(reg.MOU, 
                         variable.names=c("beta","sigma"), 
                         n.chains = 3, n.iter=20000, progress.bar="none")

summary(samp.MOU)


# for PLE
Y.PLE <- as.vector(RWIS[RWIS$site %in% "PLE",]$RWI) 
pdsi.PLE <- as.vector( RWIS[RWIS$site %in% "PLE",]$JJA.pdsi)
n.PLE    <- length(RWIS[RWIS$site %in% "PLE",]$RWI)
reg.PLE <- jags.model(textConnection(model_string), 
                      data = list(Y=Y.PLE,n=n.PLE,pdsi=pdsi.PLE))

update(reg.PLE, 10000, progress.bar="none"); # Burnin for 10000 samples

samp.PLE <- coda.samples(reg.PLE, 
                         variable.names=c("beta","sigma"), 
                         n.chains = 3, n.iter=20000, progress.bar="none")

summary(samp.PLE)

# for PVC
Y.PVC <- as.vector(RWIS[RWIS$site %in% "PVC",]$RWI) 
pdsi.PVC <- as.vector( RWIS[RWIS$site %in% "PVC",]$JJA.pdsi)
n.PVC    <- length(RWIS[RWIS$site %in% "PVC",]$RWI)
reg.PVC <- jags.model(textConnection(model_string), 
                      data = list(Y=Y.PVC,n=n.PVC,pdsi=pdsi.PVC))

update(reg.PVC, 10000, progress.bar="none"); # Burnin for 10000 samples

samp.PVC <- coda.samples(reg.PVC, 
                         variable.names=c("beta","sigma"), 
                         n.chains = 3, n.iter=20000, progress.bar="none")

summary(samp.PVC)

# for STC
Y.STC <- as.vector(RWIS[RWIS$site %in% "STC",]$RWI) 
pdsi.STC <- as.vector( RWIS[RWIS$site %in% "STC",]$JJA.pdsi)
n.STC    <- length(RWIS[RWIS$site %in% "STC",]$RWI)
reg.STC <- jags.model(textConnection(model_string), 
                      data = list(Y=Y.STC,n=n.STC,pdsi=pdsi.STC))

update(reg.STC, 10000, progress.bar="none"); # Burnin for 10000 samples

samp.STC <- coda.samples(reg.STC, 
                         variable.names=c("beta","sigma"), 
                         n.chains = 3, n.iter=20000, progress.bar="none")

summary(samp.STC)


# for TOW
Y.TOW <- as.vector(RWIS[RWIS$site %in% "TOW",]$RWI) 
pdsi.TOW <- as.vector( RWIS[RWIS$site %in% "TOW",]$JJA.pdsi)
n.TOW    <- length(RWIS[RWIS$site %in% "TOW",]$RWI)
reg.TOW <- jags.model(textConnection(model_string), 
                      data = list(Y=Y.TOW,n=n.TOW,pdsi=pdsi.TOW))

update(reg.TOW, 10000, progress.bar="none"); # Burnin for 10000 samples

samp.TOW <- coda.samples(reg.TOW, 
                         variable.names=c("beta","sigma"), 
                         n.chains = 3, n.iter=20000, progress.bar="none")

summary(samp.TOW)


# for UNC
Y.UNC <- as.vector(RWIS[RWIS$site %in% "UNC",]$RWI) 
pdsi.UNC <- as.vector( RWIS[RWIS$site %in% "UNC",]$JJA.pdsi)
n.UNC    <- length(RWIS[RWIS$site %in% "UNC",]$RWI)
reg.UNC <- jags.model(textConnection(model_string), 
                      data = list(Y=Y.UNC,n=n.UNC,pdsi=pdsi.UNC))

update(reg.UNC, 10000, progress.bar="none"); # Burnin for 10000 samples

samp.UNC <- coda.samples(reg.UNC, 
                         variable.names=c("beta","sigma"), 
                         n.chains = 3, n.iter=20000, progress.bar="none")

summary(samp.UNC)


# save a data frame with beta2 sensitivity and the 
samp.df <- summary(samp.UNC)

samps.df.site <- data.frame(site = unique(RWIS$site),
                           mean = c(summary(samp.AVO)$statistics[2,"Mean"],
                                    summary(samp.BON)$statistics[2,"Mean"],
                                    summary(samp.HIC)$statistics[2,"Mean"],
                                    summary(samp.MOU)$statistics[2,"Mean"],
                                    summary(samp.PLE)$statistics[2,"Mean"],
                                    summary(samp.PVC)$statistics[2,"Mean"],
                                    summary(samp.STC)$statistics[2,"Mean"],
                                    summary(samp.TOW)$statistics[2,"Mean"],
                                    summary(samp.UNC)$statistics[2,"Mean"]),
                           ci.low = c(summary(samp.AVO)$quantiles[2,"2.5%"],
                                      summary(samp.BON)$quantiles[2,"2.5%"],
                                      summary(samp.HIC)$quantiles[2,"2.5%"],
                                      summary(samp.MOU)$quantiles[2,"2.5%"],
                                      summary(samp.PLE)$quantiles[2,"2.5%"],
                                      summary(samp.PVC)$quantiles[2,"2.5%"],
                                      summary(samp.STC)$quantiles[2,"2.5%"],
                                      summary(samp.TOW)$quantiles[2,"2.5%"],
                                      summary(samp.UNC)$quantiles[2,"2.5%"]), 
                           ci.high = c(summary(samp.AVO)$quantiles[2,"97.5%"],
                                       summary(samp.BON)$quantiles[2,"97.5%"],
                                       summary(samp.HIC)$quantiles[2,"97.5%"],
                                       summary(samp.MOU)$quantiles[2,"97.5%"],
                                       summary(samp.PLE)$quantiles[2,"97.5%"],
                                       summary(samp.PVC)$quantiles[2,"97.5%"],
                                       summary(samp.STC)$quantiles[2,"97.5%"],
                                       summary(samp.TOW)$quantiles[2,"97.5%"],
                                       summary(samp.UNC)$quantiles[2,"97.5%"]))

#samps.df.sum$dbhclass <- factor(x = samps.df.sum$dbhclass, levels = c(">20", "20 - 40", "40 - 60", "60 - 80", ">80"))

# now plot all of these together in a barplot:
png(height = 4, width = 4, units = "in", res = 300, "outputs/barplots/all_sensitivity_jja_pdsi_by_site_bw.png")
ggplot(samps.df.site, aes(site, mean, fill = site))+geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin=ci.low, ymax = ci.high), size = 0.2, width = 0.2, color = "white") +xlab("Diameter class (cm)") + ylab("Sensitivity to Summer PDSI")+theme_black(base_size = 15)
dev.off()

# merge with site level data:

head(RWIS$sand)
new.df <- merge(samps.df.site, locs[,c("code", "sand", "awc", "pr30yr", "tm30yr")], by.x = "site", by.y = "code")
ggplot(new.df[!new.df$site %in% c("AVO"),], aes(tm30yr, mean, color = site))+geom_point()+ylim(0,0.05)
