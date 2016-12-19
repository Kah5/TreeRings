library(lme4)
library(dplR)
library(ggplot2)
library(treeclim)
library(sp)  # classes for spatial data
library(raster)  # grids, rasters
library(rasterVis)  # raster visualisation
library(maptools)
library(rgeos)
library(ggplot2)
library(rgdal)
library(ggrepel)


#molten.full comes from climate_growth_reg_chron.R

#let's see if wyckoff and bower's findings of a decreased relationship between PDSI & growth are correct

# conduct f-test to see if the relationship pre-1950 is same as post 1950
#for Hickory Grove
yr <- 1895:1950
yr.post <- 1950:2014


plot.pre.post <- function(x, Climate, xlab, Site){
yr <- 1895:1950
yr.post <- 1950:2014
x$class <- '9999'
x[x$Year %in% yr,]$class <- 'Pre-1950'
x[x$Year %in% yr.post,]$class <- 'Post-1950'
#create dummy variable
x$group <- 0
x[x$Year %in% yr,]$group <- 1

#if the dummy variable is significant, then the two slopes are different
print(summary( lm(value ~ Climate:group, data = x)))
print(summary(lm(value ~ Climate:group, data = x)))
print(summary(aov(value~Climate*class, data=x)))
print(summary(lm(value~Climate/group-1, data=x)))
print(summary(aov(value~Climate/group, data = x)))
# Extend the regression lines beyond the domain of the data

ggplot(x, aes(x=Climate, y=value, colour=class)) + geom_point(shape=1) +
  scale_colour_hue(l=50) +
  #+ylim(-1.0,1.0)
  #+xlim(-4,4)# Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              se=TRUE,    # add shaded confidence region
              fullrange=FALSE)+# Extend regression lines
  
  scale_color_manual(values=c('Pre-1950'="red",'Post-1950'="blue"))+
  xlim(-8, 8)+
  ylim(0.5, 1.5) +
  theme_bw()+
  theme(text = element_text(size = 30))+
  ylab('Detrended Ring width Index') +
  xlab( xlab ) +
  ggtitle(Site)

}


pdf("outputs/pre_post_month_plots.pdf")
plot.pre.post(molten.HIC, molten.HIC$JJA.p, 'Summer Precipitation (mm)', "Hickory Grove, IL") #significant
plot.pre.post(molten.BON, molten.BON$JJA.p, 'Summer Precipitation (mm)', "Bonanza Prairie, MN") #significant
plot.pre.post(molten.PLE, molten.PLE$JJA.p, 'Summer Precipitation (mm)', "Pleasant Valley Conservancy, WI") #significant
plot.pre.post(molten.TOW, molten.TOW$JJA.p, 'Summer Precipitation (mm)', "Townsend Woods, MN") #not significant
plot.pre.post(molten.STC, molten.STC$JJA.p, 'Summer Precipitation (mm)', "St.Croix Savanna, MN") #not significant
plot.pre.post(molten.DES, molten.DES$JJA.p, 'Summer Precipitation (mm)', "Bois de Soix, MN") #significant
plot.pre.post(molten.SAN, molten.SAN$JJA.p, 'Summer Precipitation (mm)', "Sandwich, IL") #significant
plot.pre.post(molten.PLP, molten.PLP$JJA.p, 'Summer Precipitation (mm)', "Pleasant Prarie, WI") #significant


plot.pre.post(molten.HIC, molten.HIC$MAY.p, 'May Precipitation (mm)', "Hickory Grove, IL") #significant
plot.pre.post(molten.BON, molten.BON$MAY.p, 'May Precipitation (mm)', "Bonanza Prairie, MN") #significant
plot.pre.post(molten.PLE, molten.PLE$MAY.p, 'May Precipitation (mm)', "Pleasant Valley Conservancy, WI") #significant
plot.pre.post(molten.TOW, molten.TOW$MAY.p, 'May Precipitation (mm)', "Townsend Woods, MN") #not significant
plot.pre.post(molten.STC, molten.STC$MAY.p, 'May Precipitation (mm)', "St.Croix Savanna, MN") #not significant
plot.pre.post(molten.DES, molten.DES$MAY.p, 'May Precipitation (mm)', "Bois de Soix, MN") #significant
plot.pre.post(molten.SAN, molten.SAN$MAY.p, 'May Precipitation (mm)', "Sandwich, IL") #significant
plot.pre.post(molten.PLP, molten.PLP$MAY.p, 'May Precipitation (mm)', "Pleasant Prarie, WI") #significant


plot.pre.post(molten.HIC, molten.HIC$JUNTmin, 'June Minimum Temperature', "Hickory Grove, IL") #significant
plot.pre.post(molten.BON, molten.BON$JUNTmin, 'June Minimum Temperature', "Bonanza Prairie, MN") #significant
plot.pre.post(molten.PLE, molten.PLE$JUNTmin, 'June Minimum Temperature', "Pleasant Valley Conservancy, WI") #significant
plot.pre.post(molten.TOW, molten.TOW$JUNTmin, 'June Minimum Temperature', "Townsend Woods, MN") #not significant
plot.pre.post(molten.STC, molten.STC$JUNTmin, 'June Minimum Temperature', "St.Croix Savanna, MN") #not significant
plot.pre.post(molten.DES, molten.DES$JUNTmin, 'June Minimum Temperature', "Bois de Soix, MN") #significant
plot.pre.post(molten.SAN, molten.SAN$JUNTmin, 'June Minimum Temperature', "Sandwich, IL") #significant
plot.pre.post(molten.PLP, molten.PLP$JUNTmin, 'June Minimum Temperature', "Pleasant Prarie, WI") #significant


plot.pre.post(molten.HIC, molten.HIC$JUNTmax, 'June Maximum Temperature', "Hickory Grove, IL") #significant
plot.pre.post(molten.BON, molten.BON$JUNTmax, 'June Maximum Temperature', "Bonanza Prairie, MN") #significant
plot.pre.post(molten.PLE, molten.PLE$JUNTmax, 'June Maximum Temperature', "Pleasant Valley Conservancy, WI") #significant
plot.pre.post(molten.TOW, molten.TOW$JUNTmax, 'June Maximum Temperature', "Townsend Woods, MN") #not significant
plot.pre.post(molten.STC, molten.STC$JUNTmax, 'June Maximum Temperature', "St.Croix Savanna, MN") #not significant
plot.pre.post(molten.DES, molten.DES$JUNTmax, 'June Maximum Temperature', "Bois de Soix, MN") #significant
plot.pre.post(molten.SAN, molten.SAN$JUNTmax, 'June Maximum Temperature', "Sandwich, IL") #significant
plot.pre.post(molten.PLP, molten.PLP$JUNTmax, 'June Maximum Temperature', "Pleasant Prarie, WI") #significant


plot.pre.post(molten.HIC, molten.HIC$JUNTavg, 'June Average Temperature', "Hickory Grove, IL") #significant
plot.pre.post(molten.BON, molten.BON$JUNTavg, 'June Average Temperature', "Bonanza Prairie, MN") #significant
plot.pre.post(molten.PLE, molten.PLE$JUNTavg, 'June Average Temperature', "Pleasant Valley Conservancy, WI") #significant
plot.pre.post(molten.TOW, molten.TOW$JUNTavg, 'June Average Temperature', "Townsend Woods, MN") #not significant
plot.pre.post(molten.STC, molten.STC$JUNTavg, 'June Average Temperature', "St.Croix Savanna, MN") #not significant
plot.pre.post(molten.DES, molten.DES$JUNTavg, 'June Average Temperature', "Bois de Soix, MN") #significant
plot.pre.post(molten.SAN, molten.SAN$JUNTavg, 'June Average Temperature', "Sandwich, IL") #significant
plot.pre.post(molten.PLP, molten.PLP$JUNTavg, 'June Average Temperature', "Pleasant Prarie, WI") #significant

pdf('outputs/pdsi_pre_post_plots.pdf')
plot.pre.post(molten.HIC, molten.HIC$Jul.pdsi, 'July PDSI', "Hickory Grove, IL") #significant
plot.pre.post(molten.BON, molten.BON$Jul.pdsi, 'July PDSI', "Bonanza Prairie, MN") #significant
plot.pre.post(molten.PLE, molten.PLE$Jul.pdsi, 'July PDSI', "Pleasant Valley Conservancy, WI") #not significant
plot.pre.post(molten.TOW, molten.TOW$Jul.pdsi, 'July PDSI', "Townsend Woods, MN") #significant
plot.pre.post(molten.STC, molten.STC$Jul.pdsi, 'July PDSI', "St.Croix Savanna, MN") #significant
plot.pre.post(molten.DES, molten.DES$Jul.pdsi, 'July PDSI', "Bois de Soix, MN") #significant
plot.pre.post(molten.SAN, molten.SAN$Jul.pdsi, 'July PDSI', "Sandwich, IL") #significant
plot.pre.post(molten.PLP, molten.PLP$Jul.pdsi, 'July PDSI', "Pleasant Prarie, WI") #significant

dev.off()



plot.pre.post(molten.HIC, molten.HIC$JULTavg, 'July Average Temperature (DegF)', "Hickory Grove, IL") #significant




pdf("outputs/pre_post_1950_plots.pdf")
plot.pre.post(molten.HIC, molten.HIC$PDSI, 'PDSI', "Hickory Grove, IL") #significant
plot.pre.post(molten.HIC, molten.HIC$PCP, "Annual Precipitation", "Hickory Grove, IL") #sig
plot.pre.post(molten.HIC, molten.HIC$TMIN/10, "Mean Minimum Temperature", "Hickory Grove, IL") #sig
plot.pre.post(molten.HIC, molten.HIC$TAVG/10, "Mean Temperature", "Hickory Grove, IL") #sig
plot.pre.post(molten.BON, molten.BON$PDSI, "PDSI", "Bonanza Prairie, MN") #sig *
plot.pre.post(molten.BON, molten.BON$PCP, "Annual Precipitation", "Bonanza Prairie, MN") #sig 
plot.pre.post(molten.BON, molten.BON$TMIN, "Mean Minimum Temperature", "Bonanza Prairie, MN") #sig
plot.pre.post(molten.BON, molten.BON$TAVG, "Mean Temperature", "Bonanza Prairie, MN") #***
plot.pre.post(molten.PLE, molten.PLE$PDSI, "PDSI", "Pleasant Valley Conservancy, WI") #**
plot.pre.post(molten.PLE, molten.PLE$PCP, "Annual Precipitation", "Pleasant Valley Conservancy, WI") #***
plot.pre.post(molten.PLE, molten.PLE$TMIN,"Mean Minimum", "Pleasant Valley Conservancy, WI")#***
plot.pre.post(molten.PLE, molten.PLE$TAVG,"Mean Temperature","Pleasant Valley Conservancy, WI")#***
plot.pre.post(molten.TOW, molten.TOW$PDSI, "PDSI", "Townsend Woods, MN") #**
plot.pre.post(molten.TOW, molten.TOW$PCP, "Annual Precipitaiton", "Townsend Woods, MN") #not signficant
plot.pre.post(molten.TOW, molten.TOW$TMIN, "Minimum Temperature", "Townsend Woods, MN") #not significant
plot.pre.post(molten.TOW, molten.TOW$TAVG, "Average Temperature" ,"Townsend Woods, MN") #not significant
dev.off()




#####################################
#plot correlations against soil type#
#####################################'

#read in soil rasters from gssurgo data
library(raster)
ksat <- raster('C:/Users/JMac/Box Sync/GSSURGOtifs/8km_UMW_ksat1.tif')
ksat.alb <- projectRaster(ksat, crs='+init=epsg:3175')

awc <- raster('C:/Users/JMac/Box Sync/GSSURGOtifs/8km_UMW_awc1.tif')
awc.alb <- projectRaster(awc, crs = '+init=epsg:3175')


priority <- readOGR('data/Treecores.kml', layer = "NAPCsites")
priority <- spTransform(priority, CRSobj = CRS('+init=epsg:3175'))
priority <- data.frame(priority)
priority$code <- c("PVC", "STC", "TOW", "HIC", "BON")
priority$Names <- c('Pleasant Valley', 'St. Croix Savanna',"Townsend Woods", "Hickory Grove", "Bonanza Prairie")
places <- c('St. Croix Savanna',"Townsend Woods", "Hickory Grove", "Bonanza Prairie")



priority$ksat <- extract(ksat.alb, priority[,c("coords.x1", "coords.x2")])
priority$awc <- extract(awc.alb, priority[,c("coords.x1", "coords.x2")])

BON.pdsi <- read.csv("BON-WWPDSIcor.csv")
HIC.pdsi <- read.csv("HIC-WWPDSIcor.csv")
STC.pdsi <- read.csv("STC-WWPDSIcor.csv")
TOW.pdsi <- read.csv("TOW-WWPDSIcor.csv")
PLE.pdsi <- read.csv("PLE-WWPDSIcor.csv")

priority$pdsiJul <- 0
priority[priority$code %in% "BON", ]$pdsiJul <- BON.pdsi[19,2]
priority[priority$code %in% "HIC", ]$pdsiJul <- HIC.pdsi[19,2]
priority[priority$code %in% "STC", ]$pdsiJul <- STC.pdsi[19,2]
priority[priority$code %in% "TOW", ]$pdsiJul <- TOW.pdsi[19,2]
priority[priority$code %in% "PVC", ]$pdsiJul <- PLE.pdsi[19,2]

plot(priority$ksat, priority$pdsiJul)
plot(priority$awc, priority$pdsiJul)


#dataset from http://scrippsco2.ucsd.edu/data/atmospheric_co2
#CO2 <- read.csv('data/merged_ice_core_yearly.csv')

CO2 <- read.csv('data/spline_merged_ice_core_yearly2.csv', header = TRUE)

#colnames(CO2) <- c('YearCE', 'ppm', 'Year')

compare.CO2<- function(CO2, x){
  yr <- 1895:1950
  yr.post <- 1950:2014
  x$class <- '9999'
  x[x$Year %in% yr,]$class <- 'Pre-1950'
  x[x$Year %in% yr.post,]$class <- 'Post-1950'
  #create dummy variable
  x$group <- 0
  x[x$Year %in% yr,]$group <- 1
  
CO2.m <- merge(x, CO2, by = 'Year')
print(summary( lm(value ~ ppm, data = CO2.m)))


# Extend the regression lines beyond the domain of the data
ggplot(CO2.m, aes(x=ppm, y=value)) + geom_point(shape=1) +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=loess,   # Add linear regression lines
              se=TRUE,    # add shaded confidence region
              fullrange=FALSE)+# Extend regression lines
  ylab('Detrended Ring width Index') +
  xlab( 'ppm') +
  ggtitle('CO2 vs growth')
}

pdf('outputs/CO2_growth_splines.pdf')
compare.CO2(CO2, molten.BON)
compare.CO2(CO2, molten.HIC)
compare.CO2(CO2, molten.TOW)
compare.CO2(CO2, molten.PLE)
compare.CO2(CO2, molten.STC)
compare.CO2(CO2, molten.SAN)
compare.CO2(CO2, molten.DES)
dev.off()


compare.CO2.PDSI<- function(CO2, x){
  CO2.m <- merge(x, CO2, by = 'Year')
  plot(CO2.m$PDSI, CO2.m$ppm)
}



compare.CO2.PDSI(CO2, molten.BON)
compare.CO2.PDSI(CO2, molten.HIC)
compare.CO2.PDSI(CO2, molten.TOW)
compare.CO2.PDSI(CO2, molten.PLE)
compare.CO2.PDSI(CO2, molten.STC)

#plot theses
plot(molten.HIC[molten.HIC$Year== 1895:1950,]$PDSI, molten.HIC[molten.HIC$Year== 1895:1950,]$value)
abline(preHIClm)
points(molten.HIC[molten.HIC$Year== 1951:2014,]$PDSI, molten.HIC[molten.HIC$Year== 1951:2014,]$value, col = 'red')
abline(postHIClm, col = 'red')

var.test( lm(value ~ PDSI, data = molten.BON[molten.BON$Year %in% yr,]),
          lm(value ~ PDSI, data = molten.BON[molten.BON$Year %in% yr.post,]))
#F = 1.5127, num df = 222, denom df = 190, p-value = 0.003411

var.test( lm(value ~ PDSI, data = molten.PLE[molten.PLE$Year %in% yr,]),
          lm(value ~ PDSI, data = molten.PLE[molten.PLE$Year %in% yr.post,]))
# F = 0.82271, num df = 166, denom df = 126, p-value = 0.2389

var.test( lm(value ~ PDSI, data = molten.TOW[molten.TOW$Year %in% yr,]),
          lm(value ~ PDSI, data = molten.TOW[molten.TOW$Year %in% yr,]))

# F = 1.7999, num df = 173, denom df = 190, p-value = 8.101e-05



















MLexamp.6 <- lmer(value ~ Year + PCP + TAVG +(1| Year)+ (1 | Site ), data = molten.full)



#want a mixed effect model that treats climate data as fixed effects
#growth ~ B0 +B1*precip + B2* Temp + bsite + btree + E
model.re.lmer<-lmer(value ~ PDSI + (1| Site) , data = molten.full)

# If you want to see the equations for all the sites in this model:
coef(model.re.lmer)

# If you want to see the averaged (over all trees) equation:
fixef(model.re.lmer)
se.fixef(model.re.lmer)

# If you want to see the intercept shift away from the averaged intercept for each state:
ranef(model.re.lmer)
se.ranef(model.re.lmer)
# note: standard errors are all the same because the samples sizes are the same for each state.


  # make a CI plot of the intercepts
mean<-unlist(ranef(model.re.lmer)$Site)
sd<-unlist(se.ranef(model.re.lmer)$Site)

xlabels=rownames(ranef(model.re.lmer)$Site)
#xlabels=subset(molten.full ,year==2000, select="Site")
#par(mfrow=c(1,2))

#errbar(xlabels, mean, yplus=mean+1.96*sd, yminus=mean-1.96*sd, ylim=c(-5,5))	# this comes out of the Hmisc pkg

errbar(xlabels, mean[1:4], yplus=mean[1:4]+1.96*sd[1:4], yminus=mean[1:4]-1.96*sd[1:4], ylim=c(-1,1))	# this comes out of the Hmisc pkg
#errbar(xlabels$Site[26:50], mean[26:50], yplus=mean[26:50]+1.96*sd[26:50], yminus=mean[26:50]-1.96*sd[26:50], ylim=c(-10,10))



model1<-"
model{

#Likelihood
  for( i in 1:n)
    {
      value[i]~dnorm(mu[i], tau)
      mu[i]<-b0+b1*Year[i]
    }

#priors
b0~dnorm(0,.01)
b1~dnorm(0,.01)
tau<-pow(sd, -2)
sd~dunif(0,100)
#bayesian p-values for the regression coefficient using the step() function
#step() is an indicator fuction an evaluates to 1 if the argument is greater than 0, 0 otherwise
p1<-step(b1-1)
p2<-1-p1

}"

dat<-list(value=molten$value, Year = molten$Year,n=length(molten$Year))

#quick summary
lapply(dat, summary)

library(rjags)
mod<-jags.model(file=textConnection(model1), data=dat, n.chains=2, n.adapt=1000)

jags.samples(model= mod,variable.names=c("b0", "b1", "p1", "p2", "sd"), n.iter=1000 )



summary(lm(dat$value~dat$Year, family=gaussian))


samps<-coda.samples(mod, variable.names=c("b0", "b1", "sd"), n.iter=1000)

#Numerical summary of each parameter:
summary(samps)
densityplot.mcmc(samps)
densplot(samps)
#OLSexamp <- lm(extro ~ open + agree + social, data = lmm.data)
#summary(OLSexamp)

