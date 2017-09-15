library(dplR)
library(ggplot2)
library(R2jags)
library(plyr)
library(raster)
library(data.table)
library(rgdal)
library(mgcv)
# lets look at relationship to climate with age:
setwd("/Users/kah/Documents/TreeRings")

#####################################
#read in rwl & add site + year codes#
#####################################

# quick function to read detrend and add the year as a column:
# this function will also just calculate BAI instead
read_detrend_year <- function( filename, method , rwiorbai, site){
  newseries <- read.tucson( filename )
  rwl.stats(newseries)
  # average the cores by tree (for the sites with multiple cores):
  
  #gp.ids <- read.ids(newseries, stc = autoread.ids(newseries))
  
  gp.treeMean <- treeMean(newseries, autoread.ids(newseries))
  gp.treeMean2 <- treeMean(newseries, autoread.ids(newseries), na.rm=TRUE)
  
  mean.rwl.stat <- rwl.stats(gp.treeMean2)
  write.csv(mean.rwl.stat, paste0("outputs/Stats/mean.rwl.stats.", site,".csv"))
  
  ifelse(rwiorbai == "rwi", 
          detrended <- detrend(rwl = newseries, method = method),
          detrended <- bai.out(rwl = newseries))
  
  
  detrended.mean <- treeMean(detrended, autoread.ids(detrended), na.rm=TRUE)
  
  mean.rwi.stat <- rwl.stats(detrended.mean)
  write.csv(mean.rwi.stat, paste0("outputs/Stats/mean.rwi.stats.", site,".csv"))
  
  # plot spag plots:
  png(paste0("outputs/spagplots/", site, "_", rwiorbai,"_mean_", method,"_detrended.png"))
  plot(detrended.mean, "spag")
  dev.off()
  
  detrended.mean$year <- rownames(detrended.mean)
  detrended.mean
}



#calculate BAI or the detrended RWI: switch the rwiorbai argument 
Hickory.bai <- read_detrend_year(filename = "cleanrwl/HICww.rwl", method = "Spline", rwiorbai = "rwi", site = "HIC")
StCroix.bai <- read_detrend_year("cleanrwl/STCww.rwl", method = "Spline", rwiorbai = "rwi", site = "STC")
Bonanza.bai <- read_detrend_year("cleanrwl/BONww.rwl", method = "Spline", rwiorbai = "rwi", site = "BON")
Townsend.bai <- read_detrend_year("cleanrwl/TOWww.rwl", method = "Spline", rwiorbai = "rwi", site = "TOW")#townsedn woods
Pleasant.bai <- read_detrend_year("cleanrwl/PLEww.rwl", method = "Spline", rwiorbai = "rwi", site = "PLE") #Pleasant valley conservency
Coral.bai <- read_detrend_year("cleanrwl/CORww.rwl", method = "Spline", rwiorbai = "rwi", site = "COR")
Uncas.bai <- read_detrend_year("cleanrwl/UNCww.rwl", method = "Spline", rwiorbai = "rwi", site = "UNC")
Glacial.bai <- read_detrend_year("cleanrwl/GLAww.rwl", method = "Spline", rwiorbai = "rwi", site = "GLA")
Englund.bai <- read_detrend_year("cleanrwl/ENGww.rwl", method = "Spline", rwiorbai = "rwi", site = "ENG")
Mound.bai <- read_detrend_year("cleanrwl/MOUww.rwl", method = "Spline", rwiorbai = "rwi", site = "MOU")
GLL1.bai <- read_detrend_year("cleanrwl/GLL1ww.rwl", method = "Spline", rwiorbai = "rwi", site = "GL1")
GLL2.bai <- read_detrend_year("cleanrwl/GLL2ww.rwl", method = "Spline", rwiorbai = "rwi", site = "GL2")
GLL3.bai <- read_detrend_year("cleanrwl/GLL3ww.rwl", method = "Spline", rwiorbai = "rwi", site = "GL3")
GLL4.bai <- read_detrend_year("cleanrwl/GLL4ww.rwl", method = "Spline", rwiorbai = "rwi", site = "GL4")
PVC.bai <- read_detrend_year("cleanrwl/PVCww.rwl", method = "Spline", rwiorbai = "rwi", site = "PVC")

##########################################################
# tree age_agg adds on the ages of the trees at each year
# can do this with BAI or detrended RWI
source("R/tree_age_agg.R")

Hic <- tree_age_agg(rwiorbai = Hickory.bai, sampleyear = 2015, site.code= "HIC", age1950 = 30,type = "RWI_Spline_detrended")
Stc <- tree_age_agg(StCroix.bai, 2015, "STC", 30,"RWI_Spline_detrended")
Bon <- tree_age_agg(Bonanza.bai, 2015, "BON", 30,"RWI_Spline_detrended")
Tow <- tree_age_agg(Townsend.bai, 2015, "TOW", 30,"RWI_Spline_detrended")
Ple <- tree_age_agg(Pleasant.bai, 2015, "PLE", 30,"RWI_Spline_detrended")
Cor <- tree_age_agg(Coral.bai, 2016, "COR", 30,"RWI_Spline_detrended")
Unc <- tree_age_agg(Uncas.bai, 2016, "UNC", 30,"RWI_Spline_detrended")
Eng <- tree_age_agg(Englund.bai, 2015, "ENG", 30,"RWI_Spline_detrended")
Mou <- tree_age_agg(Mound.bai, 2015, "MOU", 30,"RWI_Spline_detrended")
GLL1 <- tree_age_agg(GLL1.bai, 2016, "MOU", 30,"RWI_Spline_detrended")
GLL2 <- tree_age_agg(GLL2.bai, 2016, "MOU", 30,"RWI_Spline_detrended")
GLL3 <- tree_age_agg(GLL3.bai, 2016, "MOU", 30,"RWI_Spline_detrended")
GLL4 <- tree_age_agg(GLL4.bai, 2016, "MOU", 30,"RWI_Spline_detrended")
PVC <- tree_age_agg(PVC.bai, 2016, "MOU", 30,"RWI_Spline_detrended")


###################################
#add climate onto the age trends
####################################

# read in the climate for each site

get.clim <- function(site.code, site.df){
  if(site.code %in% c("BON", "GLL1", "GLL2", "GLL3", "GLL4")){
    MNcd.clim <- read.csv("data/West_central_MN_nclimdiv.csv")
  } else{ if(site.code %in% c("HIC", "COR","GLA", "PVC" )){
    MNcd.clim <- read.csv("data/NE_illinois_climdiv.csv")
  }  else{ if(site.code == "W-R" ){
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
                        Jul.pdsi = jul.pdsi[1:120,]$PDSI, 
                        WUE.fake = seq(0,15, by = 15/119))
  
  #merge annuals with rwl
  #annuals.crn <- merge(annuals, chron, by = "Year")
  #melt(annuals.crn, id = c('ear','Site', 'PCP', "TMIN", "TAVG", "PDSI","MAY.p","JJA.p", 
   #                        "JUNTmin","JUNTavg", 'JUNTmax',"Jul.pdsi"))
  df<- merge(site.df, annuals, by = "year")
  df$site <- site.code
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
GLL1_clim <- get.clim("GLL1", GLL1)
GLL2_clim <- get.clim("GLL2", GLL2)
GLL3_clim <- get.clim("GLL3", GLL3)
GLL4_clim <- get.clim("GLL4", GLL4)
PVC_clim <- get.clim("PVC", PVC)

#ggplot(HIC_clim, aes(x = Jul.pdsi, y = RWI, color = ageclass))+geom_point()+stat_smooth(method = 'lm')
ggplot(GLL1_clim, aes(x = Jul.pdsi, y = RWI, color = ageclass))+geom_point()+stat_smooth(method = 'lm')
ggplot(GLL2_clim, aes(x = Jul.pdsi, y = RWI, color = ageclass))+geom_point()+stat_smooth(method = 'lm')
ggplot(GLL3_clim, aes(x = Jul.pdsi, y = RWI, color = ageclass))+geom_point()+stat_smooth(method = 'lm')
ggplot(GLL4_clim, aes(x = Jul.pdsi, y = RWI, color = ageclass))+geom_point()+stat_smooth(method = 'lm')



# this function plots a scatter plot of a climate param vs. growth (RWI)
# with two separate slopes for the "young" and the "old" trees
plot.young.old <- function(x, Climate, xlab, ylab,Site){
  
  if(length(unique(x$ageclass)) >= 1){
  #create dummy variable
  x$group <- 0
  ifelse(x$ageclass %in% "old", x$group <- 1, x$group <- 0)
  co2.low.yr <- x[x$year < 1950 & x$ageclass %in% 'old',]
  co2.high.yr <- x[x$year >= 1950 & x$ageclass %in% 'young',]
  
  x <- rbind(co2.low.yr, co2.high.yr)
  
  #if the dummy variable is significant, then the two slopes are different
  print(summary(aov(x$RWI ~ x[,c(Climate)] * x$ageclass)))
  #print(summary(lm(value ~ Climate:group, data = x)))
  #print(summary(aov(value~Climate*class, data=x)))
  print(anova(lm(x$RWI ~ x[,c(Climate)] * x$ageclass), lm(x$RWI ~ x[,c(Climate)])))
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
      ylab("RWI") +
      xlab( xlab ) +
      ggtitle(Site)
  }
  p
  ggsave(filename = paste0('outputs/correlations/young_old_jul_pdsi_',Site,".png"), plot = p, width = 5, height = 3.5 )
}

#pdf("outputs/correlations/BAI_young_old_figs.pdf")
plot.young.old(STC_clim, "PDSI", "PDSI","RWI", "STC")
plot.young.old(HIC_clim, "PDSI", "PDSI","RWI", "HIC")
plot.young.old(TOW_clim, "PDSI", "PDSI","RWI", "TOW")
plot.young.old(BON_clim, "PDSI", "PDSI","RWI", "BON")
plot.young.old(PLE_clim, "PDSI", "PDSI","RWI", "PLE")
plot.young.old(COR_clim, "PDSI", "PDSI","RWI", "COR")
plot.young.old(UNC_clim, "PDSI", "PDSI","RWI", "UNC")
plot.young.old(ENG_clim, "PDSI", "PDSI","RWI", "ENG")
plot.young.old(PVC_clim, "PDSI", "PDSI","RWI", "PVC")
plot.young.old(GLL1_clim, "PDSI", "PDSI","RWI", "GLL1")
plot.young.old(GLL2_clim, "PDSI", "PDSI","RWI", "GLL2")
plot.young.old(GLL3_clim, "PDSI", "PDSI","RWI", "GLL3")
plot.young.old(GLL4_clim, "PDSI", "PDSI","RWI", "GLL4")
plot.young.old(x = MOU_clim, Climate = "PDSI", xlab = "PDSI", ylab = "RWI",Site = "MOU")
#dev.off()

# drought is really only important in the summer, so lest look at July pdsi

plot.young.old(STC_clim, "Jul.pdsi", "PDSI","BAI", "STC")
plot.young.old(HIC_clim, "Jul.pdsi", "PDSI","BAI", "HIC")
plot.young.old(TOW_clim, "Jul.pdsi", "PDSI","BAI", "TOW")
plot.young.old(BON_clim, "Jul.pdsi", "PDSI","BAI", "BON")
plot.young.old(PLE_clim, "Jul.pdsi", "PDSI","BAI", "PLE")
plot.young.old(COR_clim, "Jul.pdsi", "PDSI","BAI", "COR")
plot.young.old(UNC_clim, "Jul.pdsi", "PDSI","BAI", "UNC")
plot.young.old(ENG_clim, "Jul.pdsi", "PDSI","BAI", "ENG")
plot.young.old(PVC_clim, "Jul.pdsi", "PDSI","BAI", "PVC")
plot.young.old(GLL1_clim, "Jul.pdsi", "PDSI","BAI", "GLL1")
plot.young.old(GLL2_clim, "Jul.pdsi", "PDSI","BAI", "GLL2")
plot.young.old(GLL3_clim, "Jul.pdsi", "PDSI","BAI", "GLL3")
plot.young.old(GLL4_clim, "Jul.pdsi", "PDSI","BAI", "GLL4")
plot.young.old(x = MOU_clim, Climate = "PDSI", xlab = "PDSI", ylab = "BAI",Site = "MOU")


# should create PNGS but that is for a later date
plot.pre.post <- function(x, Climate, xlab, ylab,Site){
  
  
    #create dummy variable
    x$time <- 0
 
   x[x$year < 1950 ,]$time <- "Pre-1950"
    x[x$year >= 1950 ,]$time <- "Post-1950"
    
    #x <- rbind(co2.low.yr, co2.high.yr)
    
    #if the dummy variable is significant, then the two slopes are different
    print(summary(aov(x$RWI ~ x[,c(Climate)] * x$time)))
    #print(summary(lm(value ~ Climate:group, data = x)))
    #print(summary(aov(value~Climate*class, data=x)))
    print(anova(lm(x$RWI ~ x[,c(Climate)] * x$time), lm(x$RWI ~ x[,c(Climate)])))
    #print(summary(lm(value~Climate/group-1, data=x)))
    #print(summary(aov(value~Climate/group, data = x)))
    # Extend the regression lines beyond the domain of the data
    
    p<- ggplot(x, aes(x=x[,Climate], y=x$RWI, colour=x$time)) + geom_point(shape=1) +
      #scale_colour_hue(l=50) +
      #+ylim(-1.0,1.0)
      #+xlim(-4,4)# Use a slightly darker palette than normal
      geom_smooth(method='lm',   # Add linear regression lines
                  se=TRUE,    # add shaded confidence region
                  fullrange=FALSE)+# Extend regression lines
      
      scale_color_manual(values=c('Pre-1950'="red",'Post-1950'="blue"))+
      #xlim(-8, 8)+
      #ylim(0.5, 1.5) +
      theme_bw()+
      theme(text = element_text(size = 10), plot.title = element_text(hjust = 0.5), legend.title=element_blank())+
      ylab(ylab) +
      xlab( xlab ) +
      ggtitle(Site)
 
   

  p
  ggsave(filename = paste0('outputs/correlations/pre_post_jul_pdsi_',Site,".png"), plot = p, width = 5, height = 3.5 )
}

plot.pre.post(GLL4_clim, "Jul.pdsi", "PDSI","RWI", "GLL4")
plot.pre.post(STC_clim, "Jul.pdsi", "PDSI","RWI", "STC")
plot.pre.post(HIC_clim, "Jul.pdsi", "PDSI","RWI", "HIC")
plot.pre.post(TOW_clim, "Jul.pdsi", "PDSI","RWI", "TOW")
plot.pre.post(BON_clim, "Jul.pdsi", "PDSI","RWI", "BON")
plot.pre.post(PLE_clim, "Jul.pdsi", "PDSI","RWI", "PLE")
plot.pre.post(COR_clim, "Jul.pdsi", "PDSI","RWI", "COR")
plot.pre.post(UNC_clim, "Jul.pdsi", "PDSI","RWI", "UNC")
plot.pre.post(ENG_clim, "Jul.pdsi", "PDSI","RWI", "ENG")
plot.pre.post(PVC_clim, "Jul.pdsi", "PDSI","RWI", "PVC")
plot.pre.post(GLL1_clim, "Jul.pdsi", "PDSI","RWI", "GLL1")
plot.pre.post(GLL2_clim, "Jul.pdsi", "PDSI","RWI", "GLL2")
plot.pre.post(GLL3_clim, "Jul.pdsi", "PDSI","RWI", "GLL3")
plot.pre.post(GLL4_clim, "Jul.pdsi", "PDSI","RWI", "GLL4")
plot.pre.post(x = MOU_clim, Climate = "PDSI", xlab = "PDSI", ylab = "RWI",Site = "MOU")

all <- rbind(STC_clim, HIC_clim, TOW_clim, BON_clim, PLE_clim,
             COR_clim, UNC_clim, ENG_clim, MOU_clim, GLL1_clim,
             GLL2_clim, GLL3_clim, GLL4_clim, PVC_clim)


ggplot(all, aes(x = PDSI, y = RWI, color = site))+geom_point()+stat_smooth()


summary(lm(RWI~PDSI, data = all))
summary(lm(RWI~PDSI:site, data = all))
summary(lm(RWI~year, data = all))
summary(lm(RWI~year:site, data = all))

ggplot(all, aes(x = year, y = RWI, color = site))+geom_point()+stat_smooth(method = "lm")

###################################################################
# Lets directly compare old and young years with similar climates:
##################################################################

df <- aggregate(Jul.pdsi~year, data = BON_clim, FUN = mean )
#names(df) <- BON_clim$year
df.pre <- df[df$year < 1950,]
df.post <- df[df$year >=1950,]
sorted.pre1950 <- df.pre[order(df.pre$Jul.pdsi),]
colnames(sorted.pre1950) <- c("year_pre", "Jul.pdsi_pre")
sorted.post1950 <- df.post[order(df.post$Jul.pdsi),]
colnames(sorted.post1950) <- c("year_post", "Jul.pdsi_post")
comb <- cbind(sorted.pre1950, sorted.post1950)

ggplot(df, aes(year, Jul.pdsi))+geom_point()+stat_smooth(method = "lm")
test <- lm(Jul.pdsi~year, data = df)# no significant change


##########################################################################
# get a function to extract the senstivity of Growth-climate relationship of each site
##########################################################################

# function to extract whole time series slope of lm(RWI ~ PDSI)
get.clim.sensitivity <- function(df){
  
  coeffs <- matrix ( 0, length(unique(df$site)), 3 ) # set up matrix for coefficients
  
  # for loop
  for(s in 1: length(unique(all$site))){
    name <- unique(all$site)[s]  
    lmest <- lm(RWI ~ PDSI, data = all[all$site == name ,])
    coeffs[s,2:3] <- summary(lmest)$coefficients[2,1:2]
    coeffs[s , 1] <- name
  }
  
  coeffs <- data.frame(coeffs)
  colnames(coeffs) <- c("site", "slope.est", "std.err")
  coeffs$site <- as.character(coeffs$site)
  coeffs$slope.est <- as.numeric(as.character(coeffs$slope.est))
  coeffs$std.err <- as.numeric(as.character(coeffs$std.err))
  coeffs
  
}

# get all the sensitivities for pdsi:
pdsi.sens <- get.clim.sensitivity(all)

# function to extract slopes for young an old trees of lm(RWI~PDSI)
get.clim.sens.age <- function(df){
  
  coeffs <- matrix ( 0, length(unique(df$site))*2, 4 ) # set up matrix for coefficients
  
  
  for(s in 1: length(unique(all$site))){
     name <- unique(all$site)[s]  
     if(nrow(all[all$site == name & all$ageclass == "young" ,]) > 0){
     lmest <- lm(RWI ~ PDSI, data = all[all$site == name & all$ageclass == "young" & all$year >= 1950 ,])
     coeffs[s,2:3] <- summary(lmest)$coefficients[2,1:2]
     coeffs[s , 1] <-  "young"
     coeffs[s,4] <- name
     
    } else{
      #lmest <- lm(RWI ~ PDSI, data = all[all$site == name & all$ageclass == "young" ,])
      coeffs[s,2:3] <- c(NA,NA)
      coeffs[s , 1] <- "young"
      coeffs[s,4] <- name
    }
     
     if(nrow(all[all$site == name & all$ageclass == "old" ,]) > 0){
     lmest2 <- lm(RWI ~ PDSI, data = all[all$site == name & all$ageclass == "old" & all$year < 1950,])
     coeffs[s+14, 2:3] <- summary(lmest2)$coefficients[2,1:2]
     coeffs[s +14 , 1] <- 'old'
     coeffs[s+14,4] <- name
     }else{
     #lmest2 <- lm(RWI ~ PDSI, data = all[all$site == name & all$ageclass == "old" ,])
     coeffs[s+14, 2:3] <- c(NA,NA)
     coeffs[s+14 , 1] <- "old"
     coeffs[s+14,4] <- name
     }
  }
  
  coeffs <- data.frame(coeffs)
  colnames(coeffs) <- c("age", "slope.est", "std.err","site")
  coeffs$site <- as.character(coeffs$site)
  coeffs$slope.est <- as.numeric(as.character(coeffs$slope.est))
  coeffs$std.err <- as.numeric(as.character(coeffs$std.err))
  coeffs
  
}
pdsi.age.sens <- get.clim.sens.age(all)

# function to extrat the slope for all trees before and after 1950
get.clim.sens.year <- function(all){
  
  coeffs <- matrix ( 0, length(unique(all$site))*2, 4 ) # set up matrix for coefficients
  yr <- 1895:1950
  yr.post <- 1950:2014
  all$class <- '9999'
  all[all$year %in% yr,]$class <- 'Pre-1950'
  all[all$year %in% yr.post,]$class <- 'Post-1950'
  
  for(s in 1: length(unique(all$site))){
    name <- unique(all$site)[s]  
    if(nrow(all[all$site == name & all$class == "Pre-1950" ,]) > 0){
      lmest <- lm(RWI ~ PDSI, data = all[all$site == name & all$class == "Pre-1950" ,])
      coeffs[s,2:3] <- summary(lmest)$coefficients[2,1:2]
      coeffs[s , 1] <-  "Pre-1950"
      coeffs[s,4] <- name
      
    } else{
      #lmest <- lm(RWI ~ PDSI, data = all[all$site == name & all$ageclass == "young" ,])
      coeffs[s,2:3] <- c(NA,NA)
      coeffs[s , 1] <- "Pre-1950"
      coeffs[s,4] <- name
    }
    
    if(nrow(all[all$site == name & all$class == "Post-1950" ,]) > 0){
      lmest2 <- lm(RWI ~ PDSI, data = all[all$site == name & all$class == "Post-1950" ,])
      coeffs[s+14, 2:3] <- summary(lmest2)$coefficients[2,1:2]
      coeffs[s +14 , 1] <- 'Post-1950'
      coeffs[s+14,4] <- name
    }else{
      #lmest2 <- lm(RWI ~ PDSI, data = all[all$site == name & all$ageclass == "old" ,])
      coeffs[s+14, 2:3] <- c(NA,NA)
      coeffs[s+14 , 1] <- "Post-1950"
      coeffs[s+14,4] <- name
    }
  }
  
  coeffs <- data.frame(coeffs)
  colnames(coeffs) <- c("age", "slope.est", "std.err","site")
  coeffs$site <- as.character(coeffs$site)
  coeffs$slope.est <- as.numeric(as.character(coeffs$slope.est))
  coeffs$std.err <- as.numeric(as.character(coeffs$std.err))
  coeffs
  
}

pdsi.yr.sens <- get.clim.sens.year(all)


# read in soil, xy characteristics
locs <- read.csv("outputs/priority_sites.csv")
locs$code <- as.character(locs$code)
locs[24:27,]$code <- c("GLL4", "GLL3", "GLL2", "GLL1")
sites <- c("COR", "HIC", "STC", "GLA", "TOW", "ENG", "UNC", "BON", "MOU", "GLL4", "GLL3", "GLL2", "GLL1", "PVC")

site.df <- merge(pdsi.sens, locs, by.x = 'site', by.y = 'code')

# map out sensitivities in space:
all_states <- map_data("state")
states <- subset(all_states, region %in% c(  "illinois", "minnesota", "wisconsin", "iowa", "south dakota",
                                             "north dakota", 'michigan', 'missouri', 'indiana') )
coordinates(states)<-~long+lat
class(states)
proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
mapdata<-spTransform(states, CRS('+init=epsg:3175'))
mapdata<-data.frame(mapdata)

site.df <- merge(pdsi.sens, locs, by.x = 'site', by.y = 'code')
site.df.age <- merge(pdsi.age.sens, locs, by.x = 'site', by.y = 'code')
site.df.yr <- merge(pdsi.yr.sens, locs, by.x = 'site', by.y = 'code')

png("outputs/maps/Jul.pdsi_sensitivity.png")
ggplot(site.df, aes(coords.x1, coords.x2, color = slope.est))+geom_point()+scale_color_gradient(low = "blue", high = "red")+ geom_polygon(data=data.frame(mapdata), aes(x=long, y=lat, group=group),
            colour = "darkgrey", fill = NA)+theme_bw() + coord_cartesian(xlim = c(-59495.64, 724000), ylim=c(68821.43, 1480021))
dev.off()

png(width = 6, height = 4, units = 'in', res = 300,"outputs/maps/Jul.pdsi_sensitivity_age.png")
ggplot(site.df.age, aes(coords.x1, coords.x2, color = slope.est))+geom_point()+ geom_polygon(data=data.frame(mapdata), aes(x=long, y=lat, group=group),colour = "darkgrey", fill = NA)+theme_bw() +facet_wrap(~age)+scale_color_gradient(low = "blue", high = "red")+ coord_cartesian(xlim = c(-59495.64, 724000), ylim=c(68821.43, 1480021)) 
dev.off()

png(width = 6, height = 4, units = 'in', res = 300,"outputs/maps/Jul.pdsi_sensitivity_year.png")

ggplot(site.df.yr, aes(coords.x1, coords.x2, color = slope.est))+geom_point()+ geom_polygon(data=data.frame(mapdata), aes(x=long, y=lat, group=group),colour = "darkgrey", fill = NA)+theme_bw() +facet_wrap(~age)+scale_color_gradient(low = "blue", high = "red") + coord_cartesian(xlim = c(-59495.64, 724000), ylim=c(68821.43, 1480021))

dev.off()


#--------------------------------------------------------------
# extract relevant climate from PRSIM 30 year mean precip and temp data:
# dir where the precip data are
workingdir <- "./Users/kah/Documents/biomodality/data/"

# read in and average prism data (this is modern 30year normals)
prism<- raster(paste0(workingdir,"PRISM_ppt_30yr_normal_4kmM2_all_bil/PRISM_ppt_30yr_normal_4kmM2_annual_bil.bil"))
prism.alb<- projectRaster(prism, crs='+init=epsg:3175')

site.df$pr30yr <- extract(prism.alb, site.df[,c("coords.x1","coords.x2")])

workingdir <- "/Users/kah/Documents/biomodality/data/"

# read in and average prism temperature data (this is modern 30year normals)
prism.t<- raster(paste0(workingdir,'PRISM_tmean_30yr_normal_4kmM2_annual_bil/PRISM_tmean_30yr_normal_4kmM2_annual_bil.bil'))
prismt.alb<- projectRaster(prism.t, crs='+init=epsg:3175')

# extract temp
site.df$tm30yr <- extract(prismt.alb, site.df[,c("coords.x1","coords.x2")])


#merge overalll slope and envt with the young and old slopes:
a <- site.df
#colnames(a) <- c("site","full.slope.est", "full.std.err",  "Name","x",
 #                 "y", "PDSI_time", "sand","ksat","awc",      
  #               "pr30yr"  ,  "tm30yr")

sens.df <- merge(pdsi.age.sens, a, by = "site")
yr.sens.df <- merge(pdsi.yr.sens, a, by = "site")

#--------------------------------------------------------------
# how does sensitivity to drought vary by climate, envtl factors?
#---------------------------------------------------------------
# prelimnary plots sugges that higher precipitation and higher T places might be more sensitive to PDSI
ggplot(site.df, aes(pr30yr, slope.est))+geom_point()
ggplot(site.df, aes(tm30yr, slope.est))+geom_point()
ggplot(site.df, aes(sand, slope.est))+geom_point()

# fit a gam on the slope estimate
gam.sens <- gam(slope.est ~ pr30yr + tm30yr   , data = site.df)

summary(gam.sens) # explains 27.4% of deviance:


#################################################
#  make plots for young and old
# prelimnary plots sugges that higher precipitation and higher T places might be more sensitive to PDSI
ggplot(sens.df, aes(pr30yr, slope.est, color = age))+geom_point()
ggplot(sens.df, aes(tm30yr, slope.est, color = age))+geom_point()
ggplot(sens.df, aes(sand, slope.est, color = age))+geom_point()

sens.young <- gam(slope.est ~ pr30yr + tm30yr +sand  , data = sens.df[sens.df$age=="young",])
summary(sens.young) # explains 47.7% of deviance:

sens.old <- gam(slope.est ~ pr30yr + tm30yr +sand  , data = sens.df[sens.df$age=="old",])
summary(sens.old) # explains 90.5% of deviance:

##############################################################
# make prelimnary plots for pre- and post- 1950
###############################################################3
ggplot(yr.sens.df, aes(pr30yr, slope.est, color = age))+geom_point()
ggplot(yr.sens.df, aes(tm30yr, slope.est, color = age))+geom_point()
ggplot(yr.sens.df, aes(sand, slope.est, color = age))+geom_point()

sens.pre <- gam(slope.est ~ pr30yr + tm30yr +sand  , data = yr.sens.df[yr.sens.df$age=="Pre-1950",])
summary(sens.pre) # explains 33.4% of deviance:

sens.post <- gam(slope.est ~ pr30yr + tm30yr +sand  , data = yr.sens.df[yr.sens.df$age=="Post-1950",])
summary(sens.post) # explains 36.8% of deviance:



#install.packages("plot3D")
library(plot3D)

# created  a funciton that takes the data of interest, fits the gam model:
# gam(sensitivity ~ precip + temperature) and plots a 3d surface of it
plot3dsensitivity <- function(sens.df, age, class, col, add ){
  df <- sens.df[sens.df[,c(age)] == class,]
  df <- df[!is.na(df$slope.est.x),]
# x, y, z variables
  x <- df$pr30yr
  y <- df$tm30yr
  z <- df$slope.est.x
# Compute the linear regression (z = ax + by + d)
  fit <- lm(z ~ x + y)
# predict values on regular xy grid
  grid.lines = 25
  x.pred <- seq(min(x), max(x), length.out = grid.lines)
  y.pred <- seq(min(y), max(y), length.out = grid.lines)
  xy <- expand.grid( x = x.pred, y = y.pred)
  z.pred <- matrix(predict(fit, newdata = xy), 
                 nrow = grid.lines, ncol = grid.lines)
# fitted points for droplines to surface
  fitpoints <- predict(fit)
# scatter plot with regression plane
  scatter3D(x, y, z, pch = 18, cex = 2, col= col,
          theta = 50, phi = 25,  bty="u", lwd.panel= 2, space = 0.15,ticktype = "detailed",
          xlab = "\n\n\n\n Precip", ylab = "\n\n\n\n Temp", zlab = "\n\n\n\n drought sensitivity", add= add ,
          surf = list(x = x.pred, y = y.pred, z = z.pred,  
                      facets = NA, fit = fitpoints), main = paste("Drought Sensitivity by climate"),
          zlim=c(0,0.1))
 
}


# plot old and young predictive surfaces on the smae plot
png(height = 5, width = 9, units = 'in', res= 300, 'outputs/sensitivity_surface3d_age.png')
plot3dsensitivity(sens.df, "age","old", "red",FALSE)

plot3dsensitivity(sens.df, "age","young", "blue",TRUE)
legend(x = 0.5, y = 0 ,
       legend = c(expression(atop("Young pre-1950", "(low CO"[2]*")")), expression(atop("Young post-1950", "(high CO"[2]*")"))), 
       col = c("red", 
               "blue"), 
       pch = c(18, 18), 
       bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))

dev.off()

# plot the pre and post 1950 sensitivity surfaces:

png(height = 5, width = 9, units = 'in', res= 300,'outputs/sensitivity_surface3d_pre_post_1950.png')
plot3dsensitivity(yr.sens.df, "age","Pre-1950", "red",FALSE)
plot3dsensitivity(yr.sens.df, "age","Post-1950", "blue",TRUE)
legend(x = 0.5, y = 0 ,
       legend = c(expression(atop("All trees Pre-1950", "(low CO"[2]*")")), expression(atop("All trees Post-1950", "(high CO"[2]*")"))), 
       col = c("red", 
               "blue"), 
       pch = c(18, 18), 
       bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))

dev.off()


###########################################################################
# # Plot age vs. mean growth (across trees)
# read in all raw cleaned data:
require(plyr)
require(dostats)

files <- list.files("/Users/kah/Documents/TreeRings/cleanrwl/",pattern = ".rwl")

# read each rwl file and name the robject XXXww.rwl 
for (i in seq_along(files)) {
 
  assign(paste(files[i]), read.rwl(paste0("/Users/kah/Documents/TreeRings/cleanrwl/",files[i])))
  
}

# use tree_age_agg.R with raw RWI:
source("R/tree_age_agg_mean.R")

Hic <- tree_age_agg_mean(rwiorbai = HICww.rwl, sampleyear = 2015, site.code= "HIC", age1950 = 30,type = "RWI")
Stc <- tree_age_agg_mean(STCww.rwl, 2015, "STC", 30,"RWI_Spline_detrended")
Bon <- tree_age_agg_mean(BONww.rwl, 2015, "BON", 30,"RWI_Spline_detrended")
Tow <- tree_age_agg_mean(TOWww.rwl, 2015, "TOW", 30,"RWI_Spline_detrended")
Ple <- tree_age_agg_mean(PLEww.rwl, 2015, "PLE", 30,"RWI_Spline_detrended")
Cor <- tree_age_agg_mean(CORww.rwl, 2016, "COR", 30,"RWI_Spline_detrended")
Unc <- tree_age_agg_mean(UNCww.rwl, 2016, "UNC", 30,"RWI_Spline_detrended")
Eng <- tree_age_agg_mean(ENGww.rwl, 2015, "ENG", 30,"RWI_Spline_detrended")
Mou <- tree_age_agg_mean(MOUww.rwl, 2015, "MOU", 30,"RWI_Spline_detrended")
GLL1 <- tree_age_agg_mean(GLL1ww.rwl, 2016, "MOU", 30,"RWI_Spline_detrended")
GLL2 <- tree_age_agg_mean(GLL2ww.rwl, 2016, "MOU", 30,"RWI_Spline_detrended")
GLL3 <- tree_age_agg_mean(GLL3ww.rwl, 2016, "MOU", 30,"RWI_Spline_detrended")
GLL4 <- tree_age_agg_mean(GLL4ww.rwl, 2016, "MOU", 30,"RWI_Spline_detrended")
PVC <- tree_age_agg_mean(PVCww.rwl, 2016, "MOU", 30,"RWI_Spline_detrended")

# now plot mean with STDEV

ggplot(Hic, aes(Age, Mean))+geom_point()+ 
  geom_errorbar(aes(ymin=Mean-Std, ymax=Mean+Std), width=.1) 
ggplot(Stc, aes(Age, Mean))+geom_point()+ 
  geom_errorbar(aes(ymin=Mean-Std, ymax=Mean+Std), width=.1) 
ggplot(Tow, aes(Age, Mean))+geom_point()+ 
  geom_errorbar(aes(ymin=Mean-Std, ymax=Mean+Std), width=.1) 
ggplot(Unc, aes(Age, Mean))+geom_point()+ 
  geom_errorbar(aes(ymin=Mean-Std, ymax=Mean+Std), width=.1) 
ggplot(Bon, aes(Age, Mean))+geom_point()+ 
  geom_errorbar(aes(ymin=Mean-Std, ymax=Mean+Std), width=.1) 
ggplot(Ple, aes(Age, Mean))+geom_point()+ 
  geom_errorbar(aes(ymin=Mean-Std, ymax=Mean+Std), width=.1) 
ggplot(Cor, aes(Age, Mean))+geom_point()+ 
  geom_errorbar(aes(ymin=Mean-Std, ymax=Mean+Std), width=.1) 
ggplot(Eng, aes(Age, Mean))+geom_point()+ 
  geom_errorbar(aes(ymin=Mean-Std, ymax=Mean+Std), width=.1) 
ggplot(Mou, aes(Age, Mean))+geom_point()+ 
  geom_errorbar(aes(ymin=Mean-Std, ymax=Mean+Std), width=.1) 
ggplot(GLL1, aes(Age, Mean))+geom_point()+ 
  geom_errorbar(aes(ymin=Mean-Std, ymax=Mean+Std), width=.1) 
ggplot(GLL2, aes(Age, Mean))+geom_point()+ 
  geom_errorbar(aes(ymin=Mean-Std, ymax=Mean+Std), width=.1) 
ggplot(GLL3, aes(Age, Mean))+geom_point()+ 
  geom_errorbar(aes(ymin=Mean-Std, ymax=Mean+Std), width=.1) 
ggplot(GLL4, aes(Age, Mean))+geom_point()+ 
  geom_errorbar(aes(ymin=Mean-Std, ymax=Mean+Std), width=.1) 

#------------ find the means for trees established before before 1920 and those established after:
Hic.age <- tree_age_agg_mean_class(rwiorbai = HICww.rwl, sampleyear = 2015, site.code= "HIC", age1950 = 30,type = "RWI")
Stc.age <- tree_age_agg_mean_class(STCww.rwl, 2015, "STC", 30,"RWI_Spline_detrended")
Bon.age <- tree_age_agg_mean_class(BONww.rwl, 2015, "BON", 30,"RWI_Spline_detrended")
Tow.age <- tree_age_agg_mean_class(TOWww.rwl, 2015, "TOW", 30,"RWI_Spline_detrended")
Ple.age <- tree_age_agg_mean_class(PLEww.rwl, 2015, "PLE", 30,"RWI_Spline_detrended")
Cor.age <- tree_age_agg_mean_class(CORww.rwl, 2016, "COR", 30,"RWI_Spline_detrended")
Unc.age <- tree_age_agg_mean_class(UNCww.rwl, 2016, "UNC", 30,"RWI_Spline_detrended")
Eng.age <- tree_age_agg_mean_class(ENGww.rwl, 2015, "ENG", 30,"RWI_Spline_detrended")
Mou.age <- tree_age_agg_mean_class(MOUww.rwl, 2015, "MOU", 30,"RWI_Spline_detrended")
GLL1.age <- tree_age_agg_mean_class(GLL1ww.rwl, 2016, "MOU", 30,"RWI_Spline_detrended")
GLL2.age <- tree_age_agg_mean_class(GLL2ww.rwl, 2016, "MOU", 30,"RWI_Spline_detrended")
GLL3.age <- tree_age_agg_mean_class(GLL3ww.rwl, 2016, "MOU", 30,"RWI_Spline_detrended")
GLL4.age <- tree_age_agg_mean_class(GLL4ww.rwl, 2016, "MOU", 30,"RWI_Spline_detrended")
PVC.age <- tree_age_agg_mean_class(PVCww.rwl, 2016, "MOU", 30,"RWI_Spline_detrended")

# now plot mean with STDEV
# made a function to plot out mean RWI vs. age
rwi.age.class<- function(df, site){
  ggplot(df, aes(Age, Mean, color = Ageclass))+geom_point()+ 
    geom_errorbar(aes(ymin=Mean-Std, ymax=Mean+Std), width=.1) +ggtitle(site)
}

rwi.age.class(Hic.age, "HIC")
rwi.age.class(Stc.age, "STC")
rwi.age.class(Tow.age, "TOW")
rwi.age.class(Unc.age, "UNC")
rwi.age.class(Bon.age, "BON")
rwi.age.class(Ple.age, "PLE")
rwi.age.class(Cor.age, "COR")
rwi.age.class(Eng.age, "ENG")
rwi.age.class(Mou.age, "MOU")
rwi.age.class(GLL1.age, "GLL1")
rwi.age.class(GLL2.age, "GLL2")
rwi.age.class(GLL3.age, "GLL3")
rwi.age.class(GLL4.age, "GLL4")
rwi.age.class(PVC.age, "PVC")


#Plot pith date vs. mean growth (within each tree)




