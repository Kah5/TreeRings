library(rjags)

# lets make a bayseian linear regression of the detrended RWI and JJA.PDSI:


dat   <- read.csv("http://www4.stat.ncsu.edu/~reich/ST590/assignments/Obama2012.csv")
Y     <- 100*dat[,2]
Y     <- (Y-mean(Y))/sd(Y)
white <- dat[,7]
white <- (white-mean(white))/sd(white)
unemp <- dat[,18]
unemp <- (unemp-mean(unemp))/sd(unemp)
n     <- 100



model_string <- "model{

# Likelihood
for(i in 1:n){
Y[i]   ~ dnorm(mu[i],inv.var)
mu[i] <- beta[1] + beta[2]*white[i] + beta[3]*unemp[i]
}

# Prior for beta
for(j in 1:3){
beta[j] ~ dnorm(0,0.0001)
}

# Prior for the inverse variance
inv.var   ~ dgamma(0.01, 0.01)
sigma     <- 1/sqrt(inv.var)

}"

model <- jags.model(textConnection(model_string), 
                    data = list(Y=Y,n=n,white=white,unemp=unemp))

update(model, 10000, progress.bar="none"); # Burnin for 10000 samples

samp <- coda.samples(model, 
                     variable.names=c("beta","sigma"), 
                     n.chains = 3,n.iter=20000, progress.bar="none")

summary(samp)



plot(samp)


fit <- lm(Y~white+unemp)
summary(fit)






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

update(reg.model, 10000, progress.bar="none"); # Burnin for 10000 samples

samp <- coda.samples(reg.model, 
                     variable.names=c("beta","sigma"), 
                    n.chains = 3, n.iter=20000, progress.bar="none")

summary(samp)


# plot mcmc + the parameter distn
plot(samp)

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

# now for a mixed effects model w/ random effects for tree size classes:
