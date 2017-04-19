library(simr)
library(lme4)

model1 <- glmer(z ~ x + (1|g), family="poisson", data=simdata)

fixef(model1)["x"]

# changes the desired fixed effect size to 0.05
fixef(model1)["x"] <- -0.05

# run the power analysis
powerSim(model1)



#The power to reject the null hypothesis of zero trend in x is
#about 33%, given this particular setup. This would almost
#always be considered insufficient; traditionally 80% power is
#considered adequate (although this arbitrary threshold is not
#                     always appropriate – see e.g. Field et al. 2007).


#The pilot study had observations at 10 values of x, representing
#for example study years 1 through 10. In this step, we will
#calculate the effect of increasing this to 20 years.
model2 <- extend(model1, along="x", n=20)
powerSim(model2)

# exploring tradeoffs bewteen size and power
pc2 <- powerCurve(model2)
print(pc2)
plot(pc2)


# adding more sites or groups to dataset:
model3 <- extend(model1, along="g", n=15)
pc3 <- powerCurve(model3, along="g")
plot(pc3)

# increasing samples within each group/site:
model4 <- extend(model1, within="x+g", n=5)
pc4 <- powerCurve(model4, within="x+g", breaks=1:5)
print(pc4)


#-----------------------------------------------------------
# Power analysis for tree ring growth model
#-----------------------------------------------------------

# we do a power analysis with the actual data from hic and stc
# use the data from 

model1 <- lmer(RWI ~  PDSI + WUE.fake + (1|site), data = all)

pc1 <- powerCurve(model1)
plot(pc1)

# lets make some dummy data:

PDSI <- all[25:50,]$PDSI # pdsi ranges from -5 to 5
WUE <- all[25:50,]$WUE.fake # WUE increase
Age <- all[25:50,]$Age
RWI <- all[25:50,]$RWI
# three site
site <- c('a', 'b')

#X <- expand.grid(PDSI=PDSI,WUE=WUE, site = site, RWI=RWI)

# randomly sample some rows from the all dataframe:
X <- all[sample(nrow(all), 25), c("WUE.fake", "RWI", "PDSI", 'site')]
#Specify some fixed and random parameters.

b <- c(1, 0.5,0.5) # fixed intercept and slope
V1 <- 0.5 # random intercept variance
V2 <- matrix(c(0.5,0.05,0.05,0.10), 2) # random intercept and slope variance-covariance matrix
s <- 1 # residual variance


# make a model object as a lmer or glmer:
#model1 <- makeLmer(RWI ~ PDSI  + WUE + (1|site) , fixef=b, VarCorr=V1, sigma=s, data=X)
model1 <- glmer(RWI ~   WUE.fake + PDSI + (1|site), data = X)

print(model1)

pc1 <- powerCurve(model1)
plot(pc1)


# make desired observed power 0.15
fixef(model1)["WUE.fake"] <- 0.15

powerSim(model1, nsim=20)
#powerSim(model2, nsim=20)


# power is only 85%:

model2 <- extend(model1, along="RWI", n=20)
powerSim(model2)

# exploring tradeoffs bewteen size and power
pc2 <- powerCurve(model2)
print(pc2)
plot(pc2)


# adding more sites or groups to dataset:
model3 <- extend(model1, along="site", n=3)
pc3 <- powerCurve(model3, along="site")
plot(pc3)

# increasing samples within each group/site:
model4 <- extend(model1, within="PDSI+site", n=5)
pc4 <- powerCurve(model4, within="PDSI+site", breaks=1:5)
print(pc4)

#---------------------------------------------------------
# basic power analysis
#-------------------------------------------------------

rm(list=ls())
possible.ns <- seq(from=100, to=2000, by=50)
powers <- rep(NA, length(possible.ns))
powers.cov <- rep(NA, length(possible.ns))        # Need a second empty vector
alpha <- 0.05
sims <- 500
for (j in 1:length(possible.ns)){
  N <- possible.ns[j]
  
  significant.experiments <- rep(NA, sims)
  significant.experiments.cov <- rep(NA, sims)      # Need a second empty vector here too
  
  for (i in 1:sims){
    gender <- c(rep("F", N/2), rep("M", N/2))       # Generate "gender" covariate
    age <- sample(x=18:65, size=N, replace=TRUE)    # Generate "age" covariate
    effectofgender <- 10                            # Hypothesize the "effect" of gender on income
    effectofage <- 2                                # Hypothesize the "effect" of age on income
    
    ## Hypothesize Control Outcome as a function of gender, age, and error
    Y0 <- effectofgender*(gender=="M") + effectofage*age + rnorm(n=N, mean=100, sd=20)
    
    ## This is all the same ##
    tau <- 5
    Y1 <- Y0 + tau
    Z.sim <- rbinom(n=N, size=1, prob=.5)
    Y.sim <- Y1*Z.sim + Y0*(1-Z.sim)
    fit.sim <- lm(Y.sim ~ Z.sim)
    
    ## This is the novel analysis -- including two covariates to increase precision ##
    fit.sim.cov <- lm(Y.sim ~ Z.sim + (gender=="M") + age)
    
    ## extract p-values and calculate significance ##
    p.value <- summary(fit.sim)$coefficients[2,4]
    p.value.cov <- summary(fit.sim.cov)$coefficients[2,4]
    significant.experiments[i] <- (p.value <= alpha)
    significant.experiments.cov[i] <- (p.value.cov <= alpha)
  }
  
  powers[j] <- mean(significant.experiments)
  powers.cov[j] <- mean(significant.experiments.cov)
}

plot(possible.ns, powers, ylim=c(0,1))
points(possible.ns, powers.cov, col="red")







glm(RWI ~ WUE.fake + PDSI + year, data = all)


# for TREE ring model
possible.ns <- seq(from=10, to=100, by=10) # needs to be an even no
powers <- rep(NA, length(possible.ns))
powers.cov <- rep(NA, length(possible.ns)) 
powers.covPDSI <- rep(NA, length(possible.ns))# Need a second empty vector
powers.covsite <- rep(NA, length(possible.ns))
powers.covforest <- rep(NA, length(possible.ns))
alpha <- 0.05
sims <- 500

for (j in 1:length(possible.ns)){
  N <- possible.ns[j]
  
  significant.experiments <- rep(NA, sims)
  significant.experiments.cov <- rep(NA, sims)      # Need a second empty vector here too
  significant.experiments.covPDSI <- rep(NA, sims) 
  significant.experiments.covsite <- rep(NA, sims)
  significant.experiments.covforest <- rep(NA, sims)
  
  for (i in 1:sims){
    forest <- c(rep("F", N/2), rep("S", N/2))       # Generate "gender" covariate
    site <- sample (rep (1:4, N), size = N)
    #site <- c(rep(1, N/4), rep(2, N/4), rep(3, N/4), rep(4, N/4))
    PDSI <- rnorm(n = N, mean = 0, sd = 2.5)
    WUE <- rnorm(n = N, mean = 10, sd = 2.5)
    #WUE <- sample(x=1:20, size=N, replace=TRUE)    # Generate "WUE" covariate
    #PDSI <- sample(x = -5:5, size = N, replace = TRUE) # generate PDSI vals
    effectofforest <- -0.5                            # Hypothesize the "effect" of gender on income
    effectofWUE <- 0.1                                # Hypothesize the "effect" of age on income
    effectofPDSI <- 0.05  # based on effect of PDSI on growth 
    effectofsite <- 0.01
    ## Hypothesize Control Outcome as a function of gender, age, and error
    Y0 <-  effectofforest*(forest =="S") + effectofPDSI*PDSI +  effectofWUE*WUE + effectofsite*site+ rnorm(n=N, mean=0.25, sd=0.5)
    
    ## This is all the same ##
    tau <- 0.25
    Y1 <- Y0 + tau
    Z.sim <- rbinom(n=N, size=1, prob=.5)
    Y.sim <- Y1*Z.sim + Y0*(1-Z.sim)
    fit.sim <- lm(Y.sim ~ Z.sim)
    
    ## This is the novel analysis -- including two covariates to increase precision ##
    fit.sim.cov <- lm(Y.sim ~ forest + PDSI +  WUE + site) 
    
    ## extract p-values and calculate significance ##
    p.value <- summary(fit.sim)$coefficients[4]
    p.value.cov <- summary(fit.sim.cov)$coefficients[4,4] # for WUE
    p.value.covPDSI <- summary(fit.sim.cov)$coefficients[3,4]
    p.value.covsite <- summary(fit.sim.cov)$coefficients[5,4]
    p.value.covforest <- summary(fit.sim.cov)$coefficients[2,4]
    
    significant.experiments[i] <- (p.value <= alpha)
    significant.experiments.cov[i] <- (p.value.cov <= alpha)
    significant.experiments.covPDSI[i] <- (p.value.covPDSI <= alpha)
    significant.experiments.covsite[i] <- (p.value.covsite <= alpha)
    significant.experiments.covforest[i] <- (p.value.covforest <= alpha)
    
  }
  
  powers[j] <- mean(significant.experiments)
  powers.cov[j] <- mean(significant.experiments.cov)
  powers.covPDSI[j] <- mean(significant.experiments.covPDSI)
  powers.covsite[j] <- mean(significant.experiments.covsite)
  powers.covforest[j] <- mean(significant.experiments.covforest)
}


pdf("outputs/hyp_effects_pwr_analysis.R")
plot(possible.ns, powers.cov, ylim=c(0,1), 
     main = "Number of samples for hyp WUE effect of 0.1", xlab="Total number samples", ylab = "Power")
#points(possible.ns, powers.cov, col="red")
#points(possible.ns, powers.covPDSI, col="blue")
abline(a = 0.8, b = 0, col = 'red')

plot(possible.ns, powers.covforest, ylim=c(0,1), 
     main = "Number of samples for hyp forest/sav effect of 0.1", xlab="Total number samples", ylab = "Power")
#points(possible.ns, powers.cov, col="red")
#points(possible.ns, powers.covPDSI, col="blue")
abline(a = 0.8, b = 0, col = 'red')

plot(possible.ns, powers.covsite, ylim=c(0,1), 
     main = "Number of samples for hyp site effect of 0.01", xlab="Total number samples", ylab = "Power")
#points(possible.ns, powers.cov, col="red")
#points(possible.ns, powers.covPDSI, col="blue")
abline(a = 0.8, b = 0, col = 'red')

dev.off()

# do the sam with gam model fits:
# for TREE ring model
possible.ns <- seq(from=10, to=100, by=10) # needs to be an even no
powers <- rep(NA, length(possible.ns))
powers.cov <- rep(NA, length(possible.ns)) 
powers.covPDSI <- rep(NA, length(possible.ns))# Need a second empty vector
powers.covsite <- rep(NA, length(possible.ns))
powers.covforest <- rep(NA, length(possible.ns))
alpha <- 0.05
sims <- 500

for (j in 1:length(possible.ns)){
  N <- possible.ns[j]
  
  significant.experiments <- rep(NA, sims)
  significant.experiments.cov <- rep(NA, sims)      # Need a second empty vector here too
  significant.experiments.covPDSI <- rep(NA, sims) 
  significant.experiments.covsite <- rep(NA, sims)
  significant.experiments.covforest <- rep(NA, sims)
  
  for (i in 1:sims){
    forest <- c(rep("F", N/2), rep("S", N/2))       # Generate "gender" covariate
    site <- sample (rep (1:4, N), size = N)
    #site <- c(rep(1, N/4), rep(2, N/4), rep(3, N/4), rep(4, N/4))
    PDSI <- rnorm(n = N, mean = 0, sd = 2.5)
    WUE <- rnorm(n = N, mean = 10, sd = 2.5)
    #WUE <- sample(x=1:20, size=N, replace=TRUE)    # Generate "WUE" covariate
    #PDSI <- sample(x = -5:5, size = N, replace = TRUE) # generate PDSI vals
    effectofforest <- -0.5                            # Hypothesize the "effect" of gender on income
    effectofWUE <- 0.1                                # Hypothesize the "effect" of age on income
    effectofPDSI <- 0.05  # based on effect of PDSI on growth 
    effectofsite <- 0.01
    ## Hypothesize Control Outcome as a function of gender, age, and error
    Y0 <-  effectofforest*(forest =="S") + effectofPDSI*PDSI +  effectofWUE*WUE + effectofsite*site+ rnorm(n=N, mean=0.25, sd=0.5)
    
    ## This is all the same ##
    tau <- 0.25
    Y1 <- Y0 + tau
    Z.sim <- rbinom(n=N, size=1, prob=.5)
    Y.sim <- Y1*Z.sim + Y0*(1-Z.sim)
    fit.sim <- lm(Y.sim ~ Z.sim)
    
    ## This is the novel analysis -- including two covariates to increase precision ##
    fit.sim.cov <- gam(Y.sim ~ forest + PDSI +  WUE + WUE*site) 
    
    ## extract p-values and calculate significance ##
    p.value <- summary(fit.sim)$coefficients[4]
    p.value.cov <- summary(fit.sim.cov)$p.coeff[4]
    p.value.covPDSI <- summary(fit.sim.cov)$p.coeff[3]
    p.value.covsite <- summary(fit.sim.cov)$p.coeff[5]
    p.value.covforest <- summary(fit.sim.cov)$p.coeff[2]
    
    significant.experiments[i] <- (p.value <= alpha)
    significant.experiments.cov[i] <- (p.value.cov <= alpha)
    significant.experiments.covPDSI[i] <- (p.value.covPDSI <= alpha)
    significant.experiments.covsite[i] <- (p.value.covsite <= alpha)
    significant.experiments.covforest[i] <- (p.value.covforest <= alpha)
    
  }
  
  powers[j] <- mean(significant.experiments)
  powers.cov[j] <- mean(significant.experiments.cov)
  powers.covPDSI[j] <- mean(significant.experiments.covPDSI)
  powers.covsite[j] <- mean(significant.experiments.covsite)
  powers.covforest[j] <- mean(significant.experiments.covforest)
}



plot(possible.ns, powers.cov, ylim=c(0,1), 
     main = "Number of samples for hyp WUE effect of 0.1", xlab="Total number samples", ylab = "Power")
#points(possible.ns, powers.cov, col="red")
#points(possible.ns, powers.covPDSI, col="blue")
abline(a = 0.8, b = 0, col = 'red')

plot(possible.ns, powers.covforest, ylim=c(0,1), 
     main = "Number of samples for hyp WUE effect of 0.1", xlab="Total number samples", ylab = "Power")
#points(possible.ns, powers.cov, col="red")
#points(possible.ns, powers.covPDSI, col="blue")
abline(a = 0.8, b = 0, col = 'red')

plot(possible.ns, powers.covsite, ylim=c(0,1), 
     main = "Number of samples for hyp WUE effect of 0.1", xlab="Total number samples", ylab = "Power")
#points(possible.ns, powers.cov, col="red")
#points(possible.ns, powers.covPDSI, col="blue")
abline(a = 0.8, b = 0, col = 'red')





######################

simfunc <- function( effectofPDSI, effectofWUE, n=10 ) {
  
  RWI <- rnorm(n = n, mean = 1, sd = 0.25)
  PDSI <- rnorm(n = n, mean = 0, sd = 2.5)
  WUE <- rnorm(n = n, mean = 10, sd = 2.5)
  forest <- rbinom(n = n, 1, 0.5)
  
  RWI <- PDSI*effectofPDSI + WUE*effectofWUE + rnorm(1, mean = 0.1, sd=0.1)
  
  mydat2 <- data.frame(RWI=RWI, PDSI=PDSI, WUE=WUE)
  
  
  fit1 <- glm( RWI ~ PDSI, data=mydat2,
               family=gaussian(link='identity'))
  fit2 <- update(fit1, .~PDSI+ WUE )
  summary(fit2)[3,4]
}

out <- replicate(100, simfunc(0.05, 0.1, 10))
mean( out <= 0.05 )
hist(out)
abline(v=0.05, col='lightgrey')


