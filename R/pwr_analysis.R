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


# Power analysis for overall TREE ring model (not time varying)
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


pdf("outputs/hyp_effects_pwr_analysis.pdf")
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

#-----------------------------------------------------------
# Power for wue and time on RWI
#-----------------------------------------------------------
possible.ns <- seq(from=10, to=100, by=10) # needs to be an even no
possible.times <- seq(from=1, to=100, by=10)
powers <- rep(NA, length(possible.ns))
powers.cov <- rep(NA, length(possible.ns)) 
powers.covyear <- rep(NA, length(possible.ns))# Need a second empty vector
powers.covsite <- rep(NA, length(possible.ns))
powers.covforest <- rep(NA, length(possible.ns))
alpha <- 0.05
sims <- 50

for (j in 1:length(possible.times)){
  T <- possible.times[j]
  
  #significant.experiments <- rep(NA, sims)
  significant.experiments.cov <- rep(NA, sims)      # Need a second empty vector here too
  significant.experiments.covyear <- rep(NA, sims) 
  #significant.experiments.covsite <- rep(NA, sims)
  #significant.experiments.covforest <- rep(NA, sims)
  
  
  for (i in 1:sims){
    #forest <- c(rep("F", N/2), rep("S", N/2))       # Generate "gender" covariate
    #site <- sample (rep (1:4, N), size = N)
    #site <- c(rep(1, N/4), rep(2, N/4), rep(3, N/4), rep(4, N/4))
    
    N <- 5 # five samples at a site
    
    RWI <- matrix(0, nrow = N, ncol = T)
    WUE <- matrix(0, nrow = N, ncol = T)
    year <- matrix(0, nrow = N, ncol = T)
    PDSI <- matrix(0, nrow = N, ncol = T)
    #site <- matrix(0, nrow = N, ncol = 1)
   
    # create time series of WUE, RWI, etc from fake and real data distns  
   
  for(t in 1:T){
     for(n in 1:N){
      WUE[n,t] <- sample(all[all$year == (2014 - t),]$WUE.fake, size = 1) + rnorm(n = 1, mean = 0.15, sd = 1)
      RWI[n,t] <- sample(all[all$year == (2014 - t),]$RWI, size = 1) #RWI[t+1] <- rnorm(n = T, mean = 1, sd = 0.25)#WUE <- rnorm(n = N, mean = 10, sd = 2.5)
      PDSI[n,t] <- sample(all[all$year == (2014 - t),]$PDSI, size = 1) #RWI[t+1] <- rnorm(n = T, mean = 1, sd = 0.25)#WUE <- rnorm(n = N, mean = 10, sd = 2.5)
      year[n,t] <- 2014-t
      }
   }
   # for(n in 1:N){
    #  site[n] <- sample(c("a", "b", "c", "d"), size = 1, replace = T) 
      
    #}
    # melt the datframes to have only years and values
    WUE <- data.frame(WUE)
   # WUE$sample <- 1:N
    RWI <- data.frame(RWI)
    PDSI <- data.frame(PDSI)
    #RWI$sample <- 1:N
    WUE <- data.frame(t(WUE))
    WUE$year <- c(year[1,])
    RWI <- data.frame(t(RWI))
    RWI$year<-  c(year[1,])
    PDSI <- data.frame(t(PDSI))
    PDSI$year<-  c(year[1,])
    
    WUE.m <- melt(WUE, id.vars = c("year"))
    RWI.m <- melt(RWI, id.vars = c("year"))
    PDSI.m <- melt(PDSI, id.vars = c("year"))
    
    df <- data.frame(year = as.numeric(WUE.m$year),
                     RWI = RWI.m$value, 
                     WUE = WUE.m$value, 
                     PDSI = PDSI.m$value)
    ## This is the novel analysis -- including two covariates to increase precision ##
    fit.sim.cov <- gam(RWI ~ PDSI +  WUE , data = df) 
    
    ## extract p-values and calculate significance ##
    
    #p.value <- summary(fit.sim)$coefficients[4]
    p.value.cov <- summary(fit.sim.cov)$p.coeff[3] # for WUE
    p.value.covyear <- summary(fit.sim.cov)$p.coeff[2]
    #p.value.covsite <- summary(fit.sim.cov)$coefficients[5,4]
    #p.value.covforest <- summary(fit.sim.cov)$coefficients[2,4]
    
    #significant.experiments[i] <- (p.value <= alpha)
    significant.experiments.cov[i] <- (p.value.cov <= alpha)
    significant.experiments.covyear[i] <- (p.value.covyear <= alpha)
    #significant.experiments.covsite[i] <- (p.value.covsite <= alpha)
    #significant.experiments.covforest[i] <- (p.value.covforest <= alpha)
    
  }
  
  #powers[j] <- mean(significant.experiments)
  powers.cov[j] <- mean(significant.experiments.cov)
  powers.covyear[j] <- mean(significant.experiments.covyear)
  #powers.covsite[j] <- mean(significant.experiments.covsite)
  #powers.covforest[j] <- mean(significant.experiments.covforest)
}

png("outputs/pwr_years_for_WUE_sig.png")
plot(possible.times, powers.cov, ylim=c(0,1), 
     main = "Number of years needed for hyp WUE significance", xlab="Total number years", ylab = "Power")
#points(possible.ns, powers.cov, col="red")
#points(possible.ns, powers.covPDSI, col="blue")
abline(a = 0.8, b = 0, col = 'red')
dev.off()

#################################################################
# detecting a change in WUE with time:
############################################################
possible.ns <- seq(from=10, to=100, by=10) # needs to be an even no
possible.times <- seq(from=1, to=100, by=5)
powers <- rep(NA, length(possible.ns))
powers.cov <- rep(NA, length(possible.ns)) 
powers.covyear <- rep(NA, length(possible.ns))# Need a second empty vector
powers.covsite <- rep(NA, length(possible.ns))
powers.covforest <- rep(NA, length(possible.ns))
alpha <- 0.05
sims <- 100


for (j in 1:length(possible.times)){
  T <- possible.times[j]
  
  #significant.experiments <- rep(NA, sims)
  significant.experiments.cov <- rep(NA, sims)      # Need a second empty vector here too
  significant.experiments.covyear <- rep(NA, sims) 
  #significant.experiments.covsite <- rep(NA, sims)
  #significant.experiments.covforest <- rep(NA, sims)
  
  
  for (i in 1:sims){
    #forest <- c(rep("F", N/2), rep("S", N/2))       # Generate "gender" covariate
    #site <- sample (rep (1:4, N), size = N)
    #site <- c(rep(1, N/4), rep(2, N/4), rep(3, N/4), rep(4, N/4))
    
    N <- 5 # five samples at a site
    
   # RWI <- matrix(0, nrow = N, ncol = T)
    WUE <- matrix(0, nrow = N, ncol = T)
    year <- matrix(0, nrow = N, ncol = T)
    #site <- matrix(0, nrow = N, ncol = 1)
    
    # create time series of WUE, RWI, etc from fake and real data distns  
    for(t in 1:T){
      for(n in 1:N){
        WUE[n,t] <- sample(all[all$year == (2014 - t),]$WUE.fake, size = 1) + rnorm(n = 1, mean = 0.15, sd = 1)
        #RWI[n,t] <- sample(all[all$year == (2014 - t),]$RWI, size = 1) #RWI[t+1] <- rnorm(n = T, mean = 1, sd = 0.25)#WUE <- rnorm(n = N, mean = 10, sd = 2.5)
        #PDSI[n,t] <- sample(all[all$year == (2014 - t),]$PDSI, size = 1) #RWI[t+1] <- rnorm(n = T, mean = 1, sd = 0.25)#WUE <- rnorm(n = N, mean = 10, sd = 2.5)
        year[n,t] <- 2014-t
      }
    }
    
    # for(n in 1:N){
    #  site[n] <- sample(c("a", "b", "c", "d"), size = 1, replace = T) 
    
    #}
    # melt the datframes to have only years and values
    WUE <- data.frame(WUE)
    # WUE$sample <- 1:N
    #RWI <- data.frame(RWI)
    #RWI$sample <- 1:N
    WUE <- data.frame(t(WUE))
    WUE$year <- c(year[1,])
    #RWI <- data.frame(t(RWI))
    #RWI$year<-  c(year[1,])
    
    WUE.m <- melt(WUE, id.vars = c("year"))
    #RWI.m <- melt(RWI, id.vars = c("year"))
    
    df <- data.frame(year = as.numeric(WUE.m$year),
                     #RWI = RWI.m$value, 
                     WUE = WUE.m$value)
    ## This is the novel analysis -- including two covariates to increase precision ##
    fit.sim.cov <- gam(WUE ~ year , data = df) 
    
    ## extract p-values and calculate significance ##
    
    #p.value <- summary(fit.sim)$coefficients[4]
    p.value.cov <- summary(fit.sim.cov)$p.pv[2] # for WUE
    #p.value.covyear <- summary(fit.sim.cov)$p.coeff[2]
    #p.value.covsite <- summary(fit.sim.cov)$coefficients[5,4]
    #p.value.covforest <- summary(fit.sim.cov)$coefficients[2,4]
    
    #significant.experiments[i] <- (p.value <= alpha)
    significant.experiments.cov[i] <- (p.value.cov <= alpha)
    #significant.experiments.covyear[i] <- (p.value.covyear <= alpha)
    #significant.experiments.covsite[i] <- (p.value.covsite <= alpha)
    #significant.experiments.covforest[i] <- (p.value.covforest <= alpha)
    
  }
  
  #powers[j] <- mean(significant.experiments)
  powers.cov[j] <- mean(significant.experiments.cov)
  #powers.covyear[j] <- mean(significant.experiments.covyear)
  #powers.covsite[j] <- mean(significant.experiments.covsite)
  #powers.covforest[j] <- mean(significant.experiments.covforest)
}


png("outputs/pwr_year_for_WUE_v_time.png")
plot(possible.times, powers.cov, ylim=c(0,1), 
     main = "Number of years needed for sig trend in WUE", xlab="Total number years", ylab = "Power")
#points(possible.ns, powers.cov, col="red")
#points(possible.ns, powers.covPDSI, col="blue")
abline(a = 0.8, b = 0, col = 'red')
dev.off()












########################################################
Model <- function(x = all,
                  type = c("normal","log","logit")){
  ## Transforms
  if (type[1] == "log")
    x$RWI <- log(x$RWI)
  else if (type[1] == "logit")
    x$RWI <- log(x$RWI / (1 - x$RWI))
  
  mod <- lme(RWI ~ PDSI + WUE.fake  ,
             data = x,
             random = ~ 1 | ID/site,
             na.action = na.omit,
             control = lmeControl(opt = "optim",
                                  maxIter = 800, msMaxIter = 800)
  )
  mod$type <- type[1]
  
  return(mod)
}

mod <- Model(all, "normal")

factoredDesign <- function(Elevs = 0.2/c(1,5,10,20),
                           Nlevs = seq(5, 100, by = 5),
                           Jlevs = seq(4, 10, by = 2),
                           Klevs = c(2, 4, 6), ...){
  ## Generates factored series of sampling designs for simulation
  ## of data that follow a particular model.
  ## Inputs:
  ##   Elevs - vector of effect sizes for the slope parameter.
  ##   Nlevs - vector of number of years to sample.
  ##   Jlevs - vector of number of trees to sample in each site.
  ##   Klevs - vector of number of sites to sample.
  ## Results:
  ##   Data frame with where columns are the factors and
  ##   rows are the designs.
  
  # Level lengths
  lE <- length(Elevs)
  lN <- length(Nlevs)
  lJ <- length(Jlevs)
  lK <- length(Klevs)
  
  # Generate repeated vectors for each factor
  E <- rep(Elevs, each = lN*lJ*lK)
  N <- rep(rep(Nlevs, each = lJ*lK), times = lE)
  J <- rep(rep(Jlevs, each = lK), times = lE*lN)
  K <- rep(Klevs, times = lE*lN*lJ)
  
  return(data.frame(E, N, J, K))
}
factoredDesign()

######################

GetHyperparam<-function(x,b=NULL){
  ## Get the hyperparameters from the mixed effect model
  fe <- fixef(x)
  
  if(is.null(b))
    b<-fe[2] # use the data effect size if not supplied
  
  mu.a <- fe[1] 
  
  vc <- VarCorr(x)
  sigma.y <- as.numeric(vc[5, 2]) # Residual StdDev
  sigma.a <- as.numeric(vc[2, 2]) # Cobblebar StdDev
  sigma.g <- as.numeric(vc[4, 2]) # Cobblebar:transect StdDev
  
  hp<-c(b, mu.a, sigma.y, sigma.a, sigma.g)
  names(hp)<-c("b", "mu.a", "sigma.y", "sigma.a", "sigma.g")
  return(hp)
}


fake <- function(N = 2, J = 6, K = 5, b = NULL, m.orig = mod,
                 transform = TRUE, ...){
  ## Simulated Data for power analysis
  ## N = Number of years
  ## J = Number of cobblebars
  ## K = Number of transects within cobblebars
  year <- rep(0:(N-1), each = J*K)
  tree <- factor(rep(rep(1:J, each = K), times = N))
  site <- factor(rep(1:K, times = N*J))
  #WUE.fake <- rnorm(J*K, mean(all$WUE.fake)
  
  ## Simulated parameters
  hp<-GetHyperparam(x=m.orig)
  if(is.null(b))
    b <- hp['b']
  g <- rnorm(J*K, 0, hp['sigma.g'])
  a <- rnorm(J*K, hp['mu.a'] + g, hp['sigma.a'])
  
  ## Simulated responses
  eta <- rnorm(J*K*N, a + b * year , hp['sigma.y'])
  #if (transform){
   # if (m.orig$type == "normal"){
    #  y <- eta
     # y[y > 1] <- 1 # Fix any boundary problems.
      #y[y < 0] <- 0
    #}
    #else if (m.orig$type == "log"){
     # y <- exp(eta)
      #y[y > 1] <- 1
    #}
    #else if (m.orig$type == "logit")
     # y <- exp(eta) / (1 + exp(eta))
  #}
  
  #else{
    y <- eta
  #}
  
  return(data.frame(RWI = y,  site, tree))
}



fake()




fakeModWithRestarts <- function(m.o, n = 100,  ...){
  ## A Fake Model
  withCallingHandlers({
    i <- 0
    mod <- NULL
    while (i < n & is.null(mod)){
      mod <- withRestarts({
        f <- fake(m.orig = m.o, transform = F, ...)
        return(update(m.o, data = f))
      },
      rs = function(){
        i <<- i + 1
        return(NULL)
      })
    }
    if(is.null(mod))
      warning("ExceededIterations")
    return(mod)
  },
  error = function(e){
    invokeRestart("rs")
  },
  warning = function(w){
    if(w$message == "ExceededIterations")
      cat("\n", w$message, "\n")
    else
      invokeRestart("rs")
  })
}

dt.power <- function (m, n.sims = 1000, alpha=0.05, ...){
  ## Calculate power for a particular sampling design
  signif<-rep(NA, n.sims)
  for(i in 1:n.sims){
    lme.power <- fakeModWithRestarts(m.o = m, ...)
    if(!is.null(lme.power))
      signif[i] <- summary(lme.power)$tTable[2, 5] < alpha
  }
  power <- mean(signif, na.rm = T)
  return(power)
}

powerAnalysis <- function(parallel = T, ...){
  ## Full Power Analysis
  
  ## Parallel
  if(parallel){
    closeAllConnections()
    cl <- makeCluster(7, type = "SOCK")
    on.exit(closeAllConnections())
    clusterEvalQ(cl, source("cobblebars2.r"))
  }
  
  ## The simulations
  dat <- factoredDesign(...)
  
  if (parallel){
    dat$power <- parRapply(cl, dat, function(x,...){
      dt.power(N = x[2], J = x[3], K = x[4], b = x[1], ...)
    }, ...)
  } else {
    dat$power <- apply(dat, 1, function(x, ...){
      dt.power(N = x[2], J = x[3], K = x[4], b = x[1], ...)
    }, ...)
  }
  
  return(dat)
}

dt <- powerAnalysis(parallel = F)

plotPower <- function(dt){
  xyplot(power~N|J*K, data = dt, groups = E,
         panel = function(...){panel.xyplot(...)
           panel.abline(h = 0.8, lty = 2)},
         type = c("p", "l"),
         xlab = "sampling years",
         ylab = "power",
         strip = strip.custom(var.name = c("C", "T"),
                              strip.levels = c(T, T)),
         auto.key = T
  )
}



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


