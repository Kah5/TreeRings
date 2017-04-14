library(dplR)
library(ggplot2)
library(R2jags)
# lets look at relationship to climate with age:
setwd("C:/Users/JMac/Documents/Kelly/TreeRings")

#####################################
#read in rwl & add site + year codes#
#####################################

# quick function to read detrend and add the year as a column:
# this function will also just calculate BAI instead
read_detrend_year <- function( filename, method , rwiorbai){
  newseries <- read.tucson( filename )
  ifelse(rwiorbai == "rwi", 
          detrended <- detrend(rwl = newseries, method = method),
          detrended <- bai.out(rwl = newseries))
  
  detrended$year <- rownames(detrended)
  detrended
}

#calculate BAI or the detrended RWI: switch the rwiorbai argument 
Hickory.bai <- read_detrend_year("./cofecha/HICww.rwl", method = "ModNegExp", rwiorbai = "rwi")
StCroix.bai <- read_detrend_year("./cofecha/STCww.rwl", method = "ModNegExp", rwiorbai = "bai")
Bonanza.bai <- read_detrend_year("./cofecha/BONww.rwl", method = "ModNegExp", rwiorbai = "bai")
#Hickory.bai <- read_detrend_year ("./cofecha/HICww.rwl", method = "ModNegExp", rwiorbai = "bai")
#PleasantWolf.bai <- read_detrend_year('data/wi006.rwl', method = "ModNegExp", rwiorbai = "bai") #Pleasant prairie in southeast WI, from ITRDB
#Sand.bai <- read_detrend_year("data/il001.rwl", method = "ModNegExp", rwiorbai = "bai") #Sandwich, il. Cook tree rings from the 1980's
#Pulaski <- read_detrend_year("./in001.rwl", method = "ModNegExp", rwiorbai = "bai")
Townsend.bai <- read_detrend_year('./cofecha/tow/TOWww.rwl', method = "ModNegExp", rwiorbai = "bai")#townsedn woods
#YellowRiver <- read_detrend_year('data/ia029.rwl', method = "ModNegExp", rwiorbai = "bai") # had to fix a wrong year
Pleasant.bai <- read_detrend_year('./cofecha/PLEww.rwl', method = "ModNegExp", rwiorbai = "bai") #Pleasant valley conservency
#Desouix <- read_detrend_year('data/mn029.rwl', method = "ModNegExp", rwiorbai = "bai") #close to BONanza
Coral.bai <- read_detrend_year('C:/Users/JMac/Documents/Kelly/crossdating/data/cofecha/COR.rwl', method = "ModNegExp", rwiorbai = "bai")
Uncas.bai <- read_detrend_year("C:/Users/JMac/Documents/Kelly/crossdating/data/cofecha/UNC.rwl", method = "ModNegExp", rwiorbai = "bai")
Glacial.bai <- read_detrend_year("C:/Users/JMac/Documents/Kelly/crossdating/data/cofecha/GLA.rwl", method = "ModNegExp", rwiorbai = "bai")
Englund.bai <- read_detrend_year("C:/Users/JMac/Documents/Kelly/crossdating/data/cofecha/ENG.rwl", method = "ModNegExp", rwiorbai = "bai")
Mound.bai <- read_detrend_year("C:/Users/JMac/Documents/Kelly/crossdating/data/cofecha/MOU.rwl", method = "ModNegExp", rwiorbai = "bai")

##########################################################
# tree age_agg adds on the ages of the trees at each year
# can do this with BAI or detrended RWI
source("R/tree_age_agg.R")

Hic <- tree_age_agg(rwiorbai = Hickory.bai, sampleyear = 2015, site.code= "HIC", age1950 = 50,type = "RWI_ModNegExp_detrended")
Stc <- tree_age_agg(StCroix.bai, 2015, "STC", 50,"RWI_ModNegExp_detrended")
Bon <- tree_age_agg(Bonanza.bai, 2015, "BON", 50,"RWI_ModNegExp_detrended")
Tow <- tree_age_agg(Townsend.bai, 2015, "TOW", 50,"RWI_ModNegExp_detrended")
Ple <- tree_age_agg(Pleasant.bai, 2015, "PLE", 50,"RWI_ModNegExp_detrended")
Cor <- tree_age_agg(Coral.bai, 2015, "COR", 50,"RWI_ModNegExp_detrended")
Unc <- tree_age_agg(Uncas.bai, 2015, "UNC", 50,"RWI_ModNegExp_detrended")
Eng <- tree_age_agg(Englund.bai, 2015, "ENG", 50,"RWI_ModNegExp_detrended")
Mou <- tree_age_agg(Mound.bai, 2015, "MOU", 50,"RWI_ModNegExp_detrended")


###################################
#add climate onto the age trends
####################################

# read in the climate for each site
IL.clim <- read.csv("data/NE_illinois_climdiv.csv") #Hickory Grove, Sandwich, Glacial park
EC_MN.clim <- read.csv("data/East_Central_MN_CDODiv5039587215503.csv")

get.clim <- function(site.code, site.df){
  if(site.code == "BON"){
    MNcd.clim <- read.csv("data/West_central_MN_nclimdiv.csv")
  } else{ if(site.code == "HIC" ){
    MNcd.clim <- read.csv("data/NE_illinois_climdiv.csv")
  } else{ if(site.code == "GLA" ){
    MNcd.clim <- read.csv("data/NE_illinois_climdiv.csv")
  } else{ if(site.code == "COR" ){
    MNcd.clim <- read.csv("data/NE_illinois_climdiv.csv")
  } else{ if(site.code == "W-R" ){
    MNcd.clim <- read.csv("data/West_central_MN_nclimdiv.csv")
  } else{ if(site.code == 'SAW'){
    MNcd.clim <- read.csv("data/NE_illinois_climdiv.csv")
  }else{ if(site.code == "STC"){
    MNcd.clim <- read.csv("data/East_Central_MN_CDODiv5039587215503.csv")
  }else{ if(site.code == "ENG"){
    MNcd.clim <- read.csv("data/Central_MN_CDO.csv")
  }else{ if(site.code == "TOW"){
    MNcd.clim <- read.csv("data/South_central_MN_CDO.csv")
  }else{ if(site.code == "MOU"){
    MNcd.clim <- read.csv("data/South_East_MN_CDO.csv")
  }else{ if(site.code == "UNC"){
    MNcd.clim <- read.csv("data/East_Central_MN_CDODiv5039587215503.csv")
  }else { if(site.code == 'PLE'){
    MNcd.clim <- read.csv('data/south_central_WI_climdiv.csv')
  }else { if(site.code == 'YRF'){
    MNcd.clim <- read.csv('IA_nclim_div_northeast.csv')}
    #MNcd.clim <-read.csv('data/CDODiv2154347072867.csv')}
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  
  MNcd.clim$PCP <- MNcd.clim$PCP*25.54
  
  keeps <- c("Year", "Month",  "PCP")
  keepstavg <- c("Year", "Month", "TAVG")
  keepst <- c("Year", "Month",  "TMAX")
  keepstmin <- c("Year", "Month",  "TMIN")
  keepspdsi <- c("Year", "Month",  "PDSI")
  #create a dataset for Precip
  MNp.df <- MNcd.clim[,keeps]
  MNp.df[MNp.df == -9999]<- NA
  
  #for tmax
  MNt.df <- MNcd.clim[,keepst]
  MNt.df[MNt.df == -9999]<- NA
  
  #for tmin
  MNtmin.df<- MNcd.clim[,keepstmin]
  MNtmin.df[MNtmin.df == -9999]<- NA
  
  #for tavg
  MNtavg.df <- MNcd.clim[,keepstavg]
  MNtavg.df[MNtavg.df == -9999]<- NA
  
  MNpdsi.df<- MNcd.clim[,keepspdsi]
  MNpdsi.df[MNpdsi.df == -9999]<- NA
  #for precipitation
  
  
  total.p <- aggregate(PCP ~ Year + Month, data=MNp.df, FUN=sum, na.rm = T) 
  months <- 6:9
  MNpjja.df <- MNp.df[MNp.df$Month %in% months,]
  jja.p <- aggregate(PCP ~ Year, data = MNpjja.df, FUN = sum, na.rm = T)
  
  total.p <- aggregate(PCP ~ Year + Month, data=MNp.df, FUN=sum, na.rm = T) 
  may.p <- total.p[total.p$Month == 5, ]
  
  tavg.m <- aggregate(TAVG ~ Year + Month, data=MNtavg.df, FUN=sum, na.rm = T) 
  jun.tavg <- tavg.m[tavg.m$Month == 6,]
  
  tmin.m <- aggregate(TMIN ~ Year + Month, data = MNtmin.df, FUN = sum, na.rm = T)
  jun.tmin <- tmin.m[tmin.m$Month == 6, ]
  
  tmax.m <- aggregate(TMAX ~ Year + Month, data = MNt.df, FUN = sum, na.rm = T)
  jun.tmax <- tmax.m[tmax.m$Month == 6, ]
  
  
  
  
  #pr.yr <- aggregate(PCP ~ Year , data=MNp.df, FUN=sum, na.rm = T) 
  #plot(pr.yr[1:120,1], pr.yr[1:120,2], type = "l", xlab = "Year", ylab = "Annual Precip (mm)")
  
  
  #precip <- dcast(total.p, Year  ~ Month)
  annual.p <- aggregate(PCP~Year, data = MNp.df[1:1440,], FUN = sum, na.rm=T)
  annual.t <- aggregate(TAVG ~ Year, data = MNtavg.df[1:1440,], FUN = 'mean', na.rm=T)
  annual.mint <- aggregate(TMIN ~Year, data = MNtmin.df[1:1440,], FUN = 'mean', na.rm = T)
  annual.pdsi <- aggregate(PDSI ~ Year, data = MNpdsi.df[1:1440,], FUN = 'mean', na.rm = T)
  annual.pdsi.m <- aggregate(PDSI ~ Year + Month, data = MNpdsi.df[1:1440,], FUN = 'mean', na.rm = T)
  jul.pdsi <- annual.pdsi.m[annual.pdsi.m$Month == 7,] 
  
  annuals <- data.frame(year = annual.p$Year, 
                        PCP = annual.p$PCP,
                        TMIN = annual.mint$TMIN,
                        TAVG = annual.t$TAVG,
                        PDSI = annual.pdsi$PDSI,
                        MAY.p = may.p[1:120,]$PCP,
                        JJA.p = jja.p[1:120,]$PCP,
                        JUNTmin = jun.tmin[1:120,]$TMIN,
                        JUNTavg = jun.tavg[1:120,]$TAVG, 
                        JUNTmax = jun.tmax[1:120,]$TMAX,
                        Jul.pdsi = jul.pdsi[1:120,]$PDSI)
  
  #merge annuals with rwl
  #annuals.crn <- merge(annuals, chron, by = "Year")
  #melt(annuals.crn, id = c('ear','Site', 'PCP', "TMIN", "TAVG", "PDSI","MAY.p","JJA.p", 
   #                        "JUNTmin","JUNTavg", 'JUNTmax',"Jul.pdsi"))
  df<- merge(site.df, annuals, by = "year")
  df
}

# get climate and merge with the existing dataframes:
HIC_clim <- get.clim("HIC", Hic)
STC_clim <- get.clim("STC", Stc)
BON_clim <- get.clim("BON", Bon)
TOW_clim <- get.clim("TOW", Tow)
PLE_clim <- get.clim("PLE", Ple)
COR_clim <- get.clim("COR", Cor)
UNC_clim <- get.clim("UNC", Unc)
ENG_clim <- get.clim("ENG", Eng)
MOU_clim <- get.clim("MOU", Mou)

#ggplot(HIC_clim, aes(x = Jul.pdsi, y = RWI, color = ageclass))+geom_point()+stat_smooth(method = 'lm')
#ggplot(STC_clim, aes(x = Jul.pdsi, y = RWI, color = ageclass))+geom_point()+stat_smooth(method = 'lm')

STC_clim$site <- "STC"
HIC_clim$site <- "HIC"

lm(HIC_clim$RWI~ HIC_clim$PDSI)


# this function plots a scatter plot of a climate param vs. growth (RWI)
# with two separate slopes for the "young" and the "old" trees
plot.young.old <- function(x, Climate, xlab, ylab,Site){
  
  if(length(unique(x$ageclass)) >= 1){
  #create dummy variable
  x$group <- 0
  ifelse(x$ageclass %in% "old", x$group <- 1, x$group <- 0)
  
  #if the dummy variable is significant, then the two slopes are different
  print(summary(aov(x$RWI ~ x[,c(Climate)] * x$group)))
  #print(summary(lm(value ~ Climate:group, data = x)))
  #print(summary(aov(value~Climate*class, data=x)))
  print(anova(lm(x$RWI ~ x[,c(Climate)] * x$group), lm(x$RWI ~ x[,c(Climate)])))
  #print(summary(lm(value~Climate/group-1, data=x)))
  #print(summary(aov(value~Climate/group, data = x)))
  # Extend the regression lines beyond the domain of the data
  
  p<- ggplot(x, aes(x=x[,Climate], y=x$RWI, colour=x$ageclass)) + geom_point(shape=1) +
    #scale_colour_hue(l=50) +
    #+ylim(-1.0,1.0)
    #+xlim(-4,4)# Use a slightly darker palette than normal
    geom_smooth(method='lm',   # Add linear regression lines
                se=TRUE,    # add shaded confidence region
                fullrange=FALSE)+# Extend regression lines
    
    scale_color_manual(values=c('old'="red",'young'="blue"))+
    #xlim(-8, 8)+
    #ylim(0.5, 1.5) +
    theme_bw()+
    theme(text = element_text(size = 10), plot.title = element_text(hjust = 0.5))+
    ylab(ylab) +
    xlab( xlab ) +
    ggtitle(Site)
  }else{
    print(anova(lm(x$RWI ~ x[,c(Climate)])))
    #print(summary(lm(value~Climate/group-1, data=x)))
    #print(summary(aov(value~Climate/group, data = x)))
    # Extend the regression lines beyond the domain of the data
    
    p<- ggplot(x, aes(x=x[,Climate], y=x$RWI, colour=x$ageclass)) + geom_point(shape=1) +
      #scale_colour_hue(l=50) +
      #+ylim(-1.0,1.0)
      #+xlim(-4,4)# Use a slightly darker palette than normal
      geom_smooth(method='lm',   # Add linear regression lines
                  se=TRUE,    # add shaded confidence region
                  fullrange=FALSE)+# Extend regression lines
      
      scale_color_manual(values=c('young'="blue"))+
      #xlim(-8, 8)+
      #ylim(0.5, 1.5) +
      theme_bw()+
      theme(text = element_text(size = 10), plot.title = element_text(hjust = 0.5))+
      ylab(ylab) +
      xlab( xlab ) +
      ggtitle(Site)
  }
  p
  #ggsave(filename = paste0('outputs/correlations/pre_post_jul_pdsi_',Site,".png"), plot = p, width = 10, height = 7 )
}

pdf("outputs/correlations/BAI_young_old_figs.pdf")
plot.young.old(STC_clim, "PDSI", "PDSI","BAI", "STC")
plot.young.old(HIC_clim, "PDSI", "PDSI","BAI", "HIC")
plot.young.old(TOW_clim, "PDSI", "PDSI","BAI", "TOW")
plot.young.old(BON_clim, "PDSI", "PDSI","BAI", "BON")
plot.young.old(PLE_clim, "PDSI", "PDSI","BAI", "PLE")
plot.young.old(COR_clim, "PDSI", "PDSI","BAI", "COR")
plot.young.old(UNC_clim, "PDSI", "PDSI","BAI", "UNC")
plot.young.old(ENG_clim, "PDSI", "PDSI","BAI", "ENG")
plot.young.old(x = MOU_clim, Climate = "PDSI", xlab = "PDSI", ylab = "BAI",Site = "MOU")
dev.off()

# should create PNGS but that is for a later date




all <- rbind(STC_clim, HIC_clim)
ggplot(HIC_climate, aes(x = RWI, y = PCP, color = age.class))+geom_point()
ggplot(all, aes(x = PDSI, y = RWI, color = site))+geom_point()+stat_smooth()


summary(lm(RWI~PDSI, data = all))
summary(lm(RWI~PDSI:site, data = all))
summary(lm(RWI~year, data = all))
summary(lm(RWI~year:site, data = all))

ggplot(all, aes(x = year, y = RWI, color = site))+geom_point()+stat_smooth(method = "lm")

# typical tree ring model of growth has precip, temp, pdsi, ages, and sites
gam1 <- gam(RWI~ s(PCP)+
              s(TMIN) +
              s(TAVG) +
              s(PDSI) + s(Age) + s(site),
            data=all)


gam1
plot(gam1)

summary(gam1)$r.sq # R-squared
summary(gam1)$dev.expl # explained deviance
anova(gam1)
AIC(gam1)


scaledlinear <- "model{
  for(i in 1:Ntotal){
    zy[i] ~ dt(mu[i], 1/zsigma^2, nu)
    mu[i] <- zbeta0 + zbeta * zx[i]
  }

#priors
  zbeta0 ~ dnorm(0,1/(10)^2)
  zbeta1 ~ dnorm(0,1/(10)^2)
  zsigma ~ dunif(1.0E-3,1.0E-3)
  nu <- nuMinusOne+1
  nuMinusOne ~ dexp(1/29)
  
  }
}"


# data for linear reg
HIC_clim <- HIC_clim[!is.na(HIC_clim$RWI),]
myData = HIC_clim
xName = "PDSI" ; yName = "RWI"

#function to generate mcmc chains in the linear regression model
genMCMC = function( data , xName="x" , yName="y" , 
                    numSavedSteps=50000 , saveName=NULL ) { 
  require(rjags)
  #-----------------------------------------------------------------------------
  # THE DATA.
  y = data[,yName]
  x = data[,xName]
  # Do some checking that data make sense:
  if ( any( !is.finite(y) ) ) { stop("All y values must be finite.") }
  if ( any( !is.finite(x) ) ) { stop("All x values must be finite.") }
  #Ntotal = length(y)
  # Specify the data in a list, for later shipment to JAGS:
  dataList = list(
    x = x ,
    y = y 
  )
  #-----------------------------------------------------------------------------
  # THE MODEL.
  modelString = "
  # Standardize the data:
  data {
  Ntotal <- length(y)
  xm <- mean(x)
  ym <- mean(y)
  xsd <- sd(x)
  ysd <- sd(y)
  for ( i in 1:length(y) ) {
  zx[i] <- ( x[i] - xm ) / xsd
  zy[i] <- ( y[i] - ym ) / ysd
  }
  }
  # Specify the model for standardized data:
  model {
  for ( i in 1:Ntotal ) {
  zy[i] ~ dt( zbeta0 + zbeta1 * zx[i] , 1/zsigma^2 , nu )
  }
  # Priors vague on standardized scale:
  zbeta0 ~ dnorm( 0 , 1/(10)^2 )  
  zbeta1 ~ dnorm( 0 , 1/(10)^2 )
  zsigma ~ dunif( 1.0E-3 , 1.0E+3 )
  nu ~ dexp(1/30.0)
  # Transform to original scale:
  beta1 <- zbeta1 * ysd / xsd  
  beta0 <- zbeta0 * ysd  + ym - zbeta1 * xm * ysd / xsd 
  sigma <- zsigma * ysd
  }
  " # close quote for modelString
  # Write out modelString to a text file
  writeLines( modelString , con="TEMPmodel.txt" )
  #-----------------------------------------------------------------------------
  # INTIALIZE THE CHAINS.
  # Let JAGS do it...
  #-----------------------------------------------------------------------------
  # RUN THE CHAINS
  parameters = c( "beta0" ,  "beta1" ,  "sigma", 
                  "zbeta0" , "zbeta1" , "zsigma", "nu" )
  adaptSteps = 500  # Number of steps to "tune" the samplers
  burnInSteps = 1000
  nChains = 4 
  thinSteps = 1
  nIter = ceiling( ( numSavedSteps * thinSteps ) / nChains )
  # Create, initialize, and adapt the model:
  jagsModel = jags.model( "TEMPmodel.txt" , data=dataList , #inits=initsList , 
                          n.chains=nChains , n.adapt=adaptSteps )
  # Burn-in:
  cat( "Burning in the MCMC chain...\n" )
  update( jagsModel , n.iter=burnInSteps )
  # The saved MCMC chain:
  cat( "Sampling final MCMC chain...\n" )
  codaSamples = coda.samples( jagsModel , variable.names=parameters , 
                              n.iter=nIter , thin=thinSteps )
  # resulting codaSamples object has these indices: 
  #   codaSamples[[ chainIdx ]][ stepIdx , paramIdx ]
  if ( !is.null(saveName) ) {
    save( codaSamples , file=paste(saveName,"Mcmc.Rdata",sep="") )
  }
  return( codaSamples )
} # end function


startTime = proc.time()
mcmcCoda = genMCMC( data=myData , xName=xName , yName=yName , 
                    numSavedSteps=20000 , saveName=fileNameRoot )
stopTime = proc.time()
duration = stopTime - startTime
show(duration)

source("R/useful_jags_output_summary.R")
# Display diagnostics of chain, for specified parameters:
parameterNames = varnames(mcmcCoda) # get all parameter names
for ( parName in parameterNames ) {
  diagMCMC( codaObject=mcmcCoda , parName=parName , 
            saveName=fileNameRoot , saveType=graphFileType )
}
#------------------------------------------------------------------------------- 
# Get summary statistics of chain: functions from 
summaryInfo = smryMCMC( mcmcCoda , 
                        compValBeta1=0.0 , ropeBeta1=c(-0.5,0.5) ,
                        saveName=fileNameRoot )
show(summaryInfo)
# Display posterior information:
plotMCMC( mcmcCoda , data=myData , xName=xName , yName=yName , 
          compValBeta1=0.0 , ropeBeta1=c(-0.5,0.5) ,
          pairsPlot=TRUE , showCurve=FALSE ,
          saveName=fileNameRoot , saveType=graphFileType )
#------------------------------------------------------------------------------- 

#####################################################################################
# lets now add in a heirarchical regression that includes the subject/the tree:
#####################################################################################
# this code takes way too long ~ 2 hrs! but it eventually ran, I was getting plots, but strange errors occured
source("R/growth_PDSI_robust_heir_reg_by_tree.R")

HIC_clim <- HIC_clim[!is.na(HIC_clim$RWI),]
myData = HIC_clim # looking at hickory grove climate and TR growth

xName = "Jul.pdsi" ; yName = "RWI" ; sName="ID"
fileNameRoot = "HierLinRegressTree-Jags-" 

# myData = read.csv( file="IncomeFamszState.csv" )
# xName = "Famsz" ; yName = "Income" ; sName="State"
# fileNameRoot = "IncomeFamszState-Lin-Jags-" 

# myData = read.csv( file="BugsRatsData.csv" )
# xName = "Day" ; yName = "Weight" ; sName="Subj"
# fileNameRoot = "BugsRatsData-Jags-" 

graphFileType = "eps" 
#------------------------------------------------------------------------------- 
# Load the relevant model into R's working memory:
source("R/growth_PDSI_robust_heir_reg_by_tree.R")
#------------------------------------------------------------------------------- 
# Generate the MCMC chain:
#startTime = proc.time()
mcmcCoda = genMCMC( data=myData , xName=xName , yName=yName , sName=sName ,
                    numSavedSteps=20000 , thinSteps=15 , saveName=fileNameRoot )
#stopTime = proc.time()
#duration = stopTime - startTime
#show(duration)
# #------------------------------------------------------------------------------- 
# # Display diagnostics of chain, for specified parameters:
parameterNames = varnames(mcmcCoda) # get all parameter names
for ( parName in c("beta0mu","beta1mu","nu","sigma","beta0[1]","beta1[1]") ) {
  diagMCMC( codaObject=mcmcCoda , parName=parName , 
            saveName=fileNameRoot , saveType=graphFileType )
}
#------------------------------------------------------------------------------- 
# Get summary statistics of chain:
summaryInfo = smryMCMC( mcmcCoda , saveName=fileNameRoot )
show(summaryInfo)
# Display posterior information:
plotMCMC( mcmcCoda , data=myData , xName=xName , yName=yName , sName=sName ,
          compValBeta1=0.0 , ropeBeta1=c(-0.5,0.5) ,
          pairsPlot=TRUE , showCurve=FALSE ,
          saveName=fileNameRoot , saveType=graphFileType )
#------------------------------------------------------------------------------- 


