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
library(reshape2)
library(RColorBrewer)

#read in growth crns and make individual barplot correlations for each site

cor.barplot <- function(site.code){
tavg <- read.csv(paste0(site.code, '-WWtavgcor.csv'))
tmin <- read.csv(paste0(site.code, '-WWtmincor.csv'))
tmax <- read.csv(paste0(site.code, '-WWtmaxcor.csv'))
precip <- read.csv(paste0(site.code, '-WWPrecipcor.csv'))
PDSI <- read.csv(paste0(site.code, '-WWPDSIcor.csv'))

months <- c("pJan", "pFeb", "pMar", "pApr", "pMay", "pJun", "pJul",
            "pAug", "pSep", "pOct", "pNov", "pDec",
            "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
            "Aug", "Sep", "Oct", "Nov", "Dec")

tavg$months <- months
colnames(tavg) <- c('mono', 'tavg', 'months')
full <- tavg
full$tmin <- tmin$V1
full$tmax <- tmax$V1
full$precip <- precip$V1
full$PDSI <- PDSI$V1
cors.melt <- melt(full, id.vars = c('months', 'mono'))
cors.melt$months <- factor(cors.melt$months, levels=full$months)
cors.melt[order(cors.melt$months),]
output<- ggplot(data = cors.melt, aes(months, value, fill = variable))+
  geom_bar(stat = 'identity', position = position_dodge()) + 
  facet_grid(variable~.)+theme_bw()+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1)) + ggtitle(paste0(site.code, " Correlations"))
output
}
pdf("outputs/site_barplots.pdf")
cor.barplot("COR")
cor.barplot('STC')
cor.barplot('BON')
cor.barplot('HIC')
cor.barplot('TOW')
cor.barplot('GLA')
cor.barplot('ENG')
cor.barplot('UNC')
dev.off()

#now make a barplot for each climate factors with the sites on it using the sites.barplot funciton

#This is a set up tocolor code sites by their total mean annual precip
sites <- c("COR", "HIC", "STC", "GLA", "TOW", "ENG", "UNC", "BON")
precip <- matrix(NA ,nrow = length(sites), ncol = 2)
for (i in 1:length(sites)){
  precip[i,1] <- sites[i]
  a <- read.csv(paste0(sites[i], "-annualP.csv"))
  precip[i,2] <- mean(a$PCP)
}
precip <- precip[order(as.numeric(precip[,2])),]
site.order <- rev(precip[,1])

tmax <- matrix(NA ,nrow = length(sites), ncol = 2)
for (i in 1:length(sites)){
  tmax[i,1] <- sites[i]
  a <- read.csv(paste0(sites[i], "-annualtmax.csv"))
  tmax[i,2] <- mean(a$TMAX)
}
tmax <- tmax[order(as.numeric(tmax[,2])),]
site.order <- rev(tmax[,1])

tmin <- matrix(NA ,nrow = length(sites), ncol = 2)
for (i in 1:length(sites)){
  tmin[i,1] <- sites[i]
  a <- read.csv(paste0(sites[i], "-annualtmin.csv"))
  tmin[i,2] <- mean(a$TMIN)
}
tmin <- tmin[order(as.numeric(tmin[,2])),]
site.order <- rev(tmin[,1])

PDSI <- matrix(NA ,nrow = length(sites), ncol = 2)
for (i in 1:length(sites)){
  PDSI[i,1] <- sites[i]
  a <- read.csv(paste0(sites[i], "-annualPDSI.csv"))
  PDSI[i,2] <- mean(a$PDSI)
}
PDSI <- PDSI[order(as.numeric(PDSI[,2])),]
site.order <- rev(PDSI[,1])


sites.barplot <- function(clim) {
COR <- read.csv(paste0('COR-WW', clim, 'cor.csv'))
HIC <- read.csv(paste0('HIC-WW', clim, 'cor.csv'))
GLA <- read.csv(paste0('GLA-WW', clim, 'cor.csv'))
STC <- read.csv(paste0('STC-WW', clim, 'cor.csv'))
TOW <- read.csv(paste0('TOW-WW', clim, 'cor.csv'))
ENG <- read.csv(paste0('ENG-WW', clim, 'cor.csv'))
UNC <- read.csv(paste0('UNC-WW', clim, 'cor.csv'))
BON <- read.csv(paste0('BON-WW', clim, 'cor.csv'))
months <- c("pJan", "pFeb", "pMar", "pApr", "pMay", "pJun", "pJul",
            "pAug", "pSep", "pOct", "pNov", "pDec",
            "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
            "Aug", "Sep", "Oct", "Nov", "Dec")

COR$months <- months
colnames(COR) <- c('mono', 'COR', 'months')
full <- COR
full$HIC <- HIC$V1
full$GLA <- GLA$V1
full$STC <- STC$V1
full$TOW <- TOW$V1
full$ENG <- ENG$V1
full$UNC <- UNC$V1
full$BON <- BON$V1


half <- full[13:24,]

cors.melt <- melt(half, id.vars = c('months', 'mono'))
cors.melt$months <- factor(cors.melt$months, levels=full$months)
cors.melt$variable <- factor(cors.melt$variable, levels = site.order)
cors.melt[order(cors.melt$months),]
output<- ggplot(data = cors.melt, aes(months, value, fill = variable))+
  geom_bar(stat = 'identity', position = position_dodge(width = 0.9)) + 
  #facet_grid(variable~.)+
  scale_fill_manual(values = rev(brewer.pal(n=8, "RdBu")))+
  theme_bw()+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1)) + ggtitle(paste0(clim, " site correlations"))
output
}
pdf("outputs/barplots_clim_v_allsites.pdf", width = 15, height = 7)
sites.barplot('tavg')
sites.barplot('tmax')
sites.barplot('tmin')
sites.barplot('Precip')
sites.barplot('PDSI')
dev.off()

#rank the correlations based on highest to lowest for each site
highest.cor <- function(site, i){
  precip <- read.csv(paste0(site,'-WW', 'Precip', 'cor.csv'))
  tavg <- read.csv(paste0(site,'-WW', 'tavg', 'cor.csv'))
  tmin <- read.csv(paste0(site,'-WW', 'tmin', 'cor.csv'))
  tmax <- read.csv(paste0(site,'-WW', 'tmax', 'cor.csv'))
  PDSI <- read.csv(paste0(site,'-WW', 'PDSI', 'cor.csv'))
  
  clim.cors <- cbind(precip, tavg[,2], tmin[,2], tmax[,2], PDSI[,2])
  colnames(clim.cors) <- c("mono", "Precip", "tavg", "tmin", "tmax", "PDSI")
  clim.cors$months <- c("pJan", "pFeb", "pMar", "pApr", "pMay", "pJun", "pJul",
                        "pAug", "pSep", "pOct", "pNov", "pDec",
                        "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                        "Aug", "Sep", "Oct", "Nov", "Dec")
  
  melted.cors <- melt(clim.cors, id.vars = c('mono', "months"))
  reorded.cors <- melted.cors[rev(order(abs(melted.cors[, "value"]))),]
  reorded.cors[i,]
}
highest.cor('HIC', 1)
highest.cor('BON', 1)
highest.cor('COR', 1)
highest.cor('GLA', 1)
highest.cor('STC', 1)
highest.cor('ENG', 1)
highest.cor('UNC', 1)
highest.cor('TOW', 1)



highest.cor('HIC', 2)
highest.cor('BON', 2)
highest.cor('COR', 2)
highest.cor('GLA', 2)
highest.cor('STC', 2)
highest.cor('ENG', 2)
highest.cor('UNC', 2)
highest.cor('TOW', 2)
#now plot the correlations against their mean annual precips:
cor.v.clim <- function(clim,mono, pre,var){
  locs <- read.csv("outputs/priority_sites_locs.csv")
  sites <- c("COR", "HIC", "STC", "GLA", "TOW", "ENG", "UNC", "BON")

  precip <- matrix(NA ,nrow = length(sites), ncol = 2)
  for (i in 1:length(sites)){
    precip[i,1] <- sites[i]
    a <- read.csv(paste0(sites[i], "-annualP.csv"))
    precip[i,2] <- mean(a$PCP)
  }
  precip <- precip[order(as.numeric(precip[,2])),]
  site.order <- rev(precip[,1])
  precip <- data.frame(precip)
  colnames(precip) <- c("site", "MAP")
  precip <- merge(precip, locs, by.x = 'site', by.y = 'code')
month.coef <- matrix(NA, nrow = length(precip[,1]), ncol = 1)
for(i in 1:length(precip[,1])){
cors <- read.csv(paste0(precip[i,1], "-WW", clim, "cor.csv"))
month.coef[i,] <- cors[mono,]$V1
}
x <- as.data.frame(precip)
x$cor <- as.vector(month.coef)
colnames(x[,1:3]) <- c('site', "MAP", "cor")
if(var %in% "MAP"){
  x$env <- as.numeric(as.character(x$MAP))
}else{if (var %in% "awc"){
  x$env <- pre$awc
}else{if (var %in% "ksat"){
x$env <- pre$ksat
}else{
   precip$env <- 8888
 }
}
}


ggplot(x, aes(env, cor, color = cor))+scale_color_continuous(low = 'red', high = 'blue')+geom_point(size = 5, aes(shape = Description))+theme_bw()+ggtitle(paste0(clim, " correlation with ", var))

}
pdf("outputs/cor_coef_v_MAP.pdf")
cor.v.clim("tavg", 18,precip, var = "MAP")
cor.v.clim("Precip",18, precip, var = "MAP")
cor.v.clim("tmin", 18,precip, var = "MAP")
cor.v.clim("tmax", 18,precip, var = "MAP")
cor.v.clim("PDSI", 18,precip, var = "MAP")
dev.off()
#can use the cor.v.clim function to plot correlations against soil characteristics
#read in site xys
locs <- read.csv("outputs/priority_sites_locs.csv")
sites <- c("COR", "HIC", "STC", "GLA", "TOW", "ENG", "UNC", "BON")
precip <- matrix(NA ,nrow = length(sites), ncol = 2)
for (i in 1:length(sites)){
  precip[i,1] <- sites[i]
  a <- read.csv(paste0(sites[i], "-annualP.csv"))
  precip[i,2] <- mean(a$PCP)
}
precip <- precip[order(as.numeric(precip[,2])),]
site.order <- rev(precip[,1])
precip <- data.frame(precip)
colnames(precip) <- c("site", "MAP")
test <- merge(precip, locs, by.x = 'site', by.y = 'code')

#plot correlation coefficients with July climate variables against ksat
pdf("outputs/cor_coef_v_ksat.pdf")
cor.v.clim("PDSI", 18,test, var = "ksat")
cor.v.clim("tavg", 18,test, var = 'ksat')
cor.v.clim("tmin", 18,test, var = 'ksat')
cor.v.clim("tmax", 18,test, var = 'ksat')
cor.v.clim("Precip", 18,test, var = 'ksat')
dev.off()

#plot correlation coefficients with July climate variabes with awc
pdf("outputs/cor_coef_v_awc.pdf")
cor.v.clim("PDSI", 18,test, var = "awc")
cor.v.clim("tavg", 18,test, var = 'awc')
cor.v.clim("tmin", 18,test, var = 'awc')
cor.v.clim("tmax", 18,test, var = 'awc')
cor.v.clim("Precip", 18,test, var = 'awc')
dev.off()

#molten.full comes from climate_growth_reg_chron.R
###################################################
#compare climate coreelaitons c
plot.cor.clim <- function(x, Climate, xlab, Site){
  yr <- 1895:1950
  x$class <- '9999'
  x[x$Year %in% yr,]$class <- 'clim_record'
  
  
  #if the dummy variable is significant, then the two slopes are different
  print(summary( cor( Climate, x$value)))
  
  # Extend the regression lines beyond the domain of the data
  
  ggplot(x, aes(x=Climate, y=value)) + geom_point(shape=1) +
    scale_colour_hue(l=50) +
    #+ylim(-1.0,1.0)
    #+xlim(-4,4)# Use a slightly darker palette than normal
    geom_smooth(method=lm,   # Add linear regression lines
                se=TRUE,    # add shaded confidence region
                fullrange=FALSE)+# Extend regression lines
    
    #scale_color_manual(values=c('Pre-1950'="red",'Post-1950'="blue"))+
    xlim(-8, 8)+
    ylim(0.5, 1.5) +
    theme_bw()+
    theme(text = element_text(size = 30))+
    ylab('Detrended Ring width Index') +
    xlab( xlab ) +
    ggtitle(Site)
  
}
plot.cor.clim(molten.BON, molten.BON$PDSI, "PDSI", "Bonanza Prairie")
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
plot.pre.post(molten.GLA, molten.GLA$Jul.pdsi, 'July PDSI', "Glacial Park, IL") #significant
plot.pre.post(molten.COR, molten.COR$Jul.pdsi, 'July PDSI', "Coral Woods, IL") #significant
plot.pre.post(molten.UNC, molten.UNC$Jul.pdsi, 'July PDSI', "Uncas Dunes, MN") #significant
plot.pre.post(molten.ENG, molten.ENG$Jul.pdsi, 'July PDSI', "Englund Ecotone, MN") #significant

#plot.pre.post(molten.DES, molten.DES$Jul.pdsi, 'July PDSI', "Bois de Soix, MN") #significant
#plot.pre.post(molten.SAN, molten.SAN$Jul.pdsi, 'July PDSI', "Sandwich, IL") #significant
#plot.pre.post(molten.PLP, molten.PLP$Jul.pdsi, 'July PDSI', "Pleasant Prarie, WI") #significant

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

