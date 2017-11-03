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
  detrended.mean$site<- site
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

detrended.list <- list(Hickory.bai, StCroix.bai, Bonanza.bai,Townsend.bai,Pleasant.bai, Coral.bai,
                 Uncas.bai, Glacial.bai, Englund.bai, Mound.bai, GLL1.bai, GLL2.bai, 
                 GLL3.bai, GLL4.bai, PVC.bai)

##########################################################
# tree age_agg adds on the ages of the trees at each year
# can do this with BAI or detrended RWI
source("R/tree_age_agg.R")

# apply the tree_age_agg function on all of the detrended tree ring series
detrended.age <- lapply(detrended.list, FUN = tree_age_agg,   age1950 = 30,type = "RWI_Spline_detrended" )

# use do.calll to make these a dataframe
detrended.age.df <- do.call(rbind, detrended.age)

#Hic <- tree_age_agg(rwiorbai = Hickory.bai, age1950 = 30,type = "RWI_Spline_detrended")
#Stc <- tree_age_agg(rwiorbai = StCroix.bai, age1950=30,type = "RWI_Spline_detrended")
#Bon <- tree_age_agg(Bonanza.bai, 30,"RWI_Spline_detrended")
#Tow <- tree_age_agg(Townsend.bai,  30,"RWI_Spline_detrended")
#Ple <- tree_age_agg(Pleasant.bai,  30,"RWI_Spline_detrended")
#Cor <- tree_age_agg(Coral.bai,30,"RWI_Spline_detrended")
#Unc <- tree_age_agg(Uncas.bai, 30,"RWI_Spline_detrended")
#Eng <- tree_age_agg(Englund.bai, 30,"RWI_Spline_detrended")
#Mou <- tree_age_agg(Mound.bai,   30,"RWI_Spline_detrended")
#GLL1 <- tree_age_agg(GLL1.bai,   30,"RWI_Spline_detrended")
#GLL2 <- tree_age_agg(GLL2.bai,   30,"RWI_Spline_detrended")
#GLL3 <- tree_age_agg(GLL3.bai,   30,"RWI_Spline_detrended")
#GLL4 <- tree_age_agg(GLL4.bai,  30,"RWI_Spline_detrended")
#PVC <- tree_age_agg(PVC.bai,  30,"RWI_Spline_detrended")


###################################
# add climate data to the age trends
####################################


# this function reads in climate data from each site and adds it to the appropriate site
get.clim <- function(site.df, climatedata){
  site.code <- site.df[1,]$site
  
  if(climatedata == "GHCN"){
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
          
          MNpdsi.df <- MNcd.clim[,keepspdsi]
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
                                
          
          df <- merge(site.df, annuals, by = "year")
          
          df
  }else{
    
    MNcd.clim <- read.csv(paste0("data/PRISM/",list.files("data/PRISM/", pattern = site.code)), header = TRUE, skip = 10 )
    colnames(MNcd.clim) <- c("Date", "PCP", "TMIN", "TAVG", "TMAX", "TdAVG", "VPDmin", "VPDmax" )
    
    # get latitude (need for PET calculation):
    lat <- as.numeric(unlist(strsplit(list.files("data/PRISM/", pattern = site.code), split = "_"))[5])
    
    #split date into month and year:
    MNcd.clim <- MNcd.clim %>% separate(Date, c("Year", "Month"), "-")
    
    # conversions to metric b/c PRISM still uses Farenheit and inches \_O_/
    MNcd.clim$PCP <- MNcd.clim$PCP*25.54 # convert to mm
    # convert temperatures to celcius
    MNcd.clim$TMIN <- (MNcd.clim$TMIN - 32)/1.8
    MNcd.clim$TMAX <- (MNcd.clim$TMAX - 32)/1.8
    MNcd.clim$TAVG <- (MNcd.clim$TAVG - 32)/1.8
    MNcd.clim$TdAVG <- (MNcd.clim$TdAVG - 32)/1.8
    
    
    # calculate PET using thornthwaite method:
    
    MNcd.clim$PET <- as.numeric(thornthwaite(MNcd.clim$TAVG, lat))
    
    #calculate water balance for each month:
    MNcd.clim$BAL <- MNcd.clim$PCP - MNcd.clim$PET
    
    MNcd.clim$Month<- as.numeric(MNcd.clim$Month)
    # make separate DF for each of the variables:
    keeps <- c("Year", "Month",  "PCP")
    keepstavg <- c("Year", "Month", "TAVG")
    keepst <- c("Year", "Month",  "TMAX")
    keepstmin <- c("Year", "Month",  "TMIN")
    keepsvpdmin <- c("Year", "Month",  "VPDmin")
    keepsvpdmax <- c("Year", "Month",  "VPDmax")
    keepsPET <- c("Year", "Month",  "PET")
    keepsBAL <- c("Year", "Month", "BAL")
    
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
    
    # for vpdmin
    MNvpdmin.df<- MNcd.clim[,keepsvpdmin]
    MNvpdmin.df[MNvpdmin.df == -9999]<- NA
    
    # for vpdmax
    MNvpdmax.df<- MNcd.clim[,keepsvpdmax]
    MNvpdmax.df[MNvpdmax.df == -9999]<- NA
    
    #for PET (thornthwaite):
    MNPET.df<- MNcd.clim[,keepsPET]
    MNPET.df[MNPET.df == -9999]<- NA
    
    #for water balance (P- PET)
    MNBAL.df <- MNcd.clim[,keepsBAL]
    MNBAL.df[MNBAL.df == -9999] <- NA
    
    
    total.p <- aggregate(PCP ~ Year + Month, data=MNp.df, FUN=sum, na.rm = T) 
    months <- 6:9
    
    MNpjja.df <- MNp.df[as.numeric(MNp.df$Month) %in% months,]
    jja.p <- aggregate(PCP ~ Year, data = MNpjja.df, FUN = sum, na.rm = T)
    
    total.p <- aggregate(PCP ~ Year + Month, data=MNp.df, FUN=sum, na.rm = T) 
    may.p <- total.p[total.p$Month == 5, ]
    
    tavg.m <- aggregate(TAVG ~ Year + Month, data=MNtavg.df, FUN=sum, na.rm = T) 
    jun.tavg <- tavg.m[tavg.m$Month == 6,]
    
    tmin.m <- aggregate(TMIN ~ Year + Month, data = MNtmin.df, FUN = sum, na.rm = T)
    jun.tmin <- tmin.m[tmin.m$Month == 6, ]
    
    tmax.m <- aggregate(TMAX ~ Year + Month, data = MNt.df, FUN = sum, na.rm = T)
    jun.tmax <- tmax.m[tmax.m$Month == 6, ]
    
    VPDmax.m <- aggregate(VPDmax ~ Year + Month, data = MNvpdmax.df, FUN = sum, na.rm = T)
    jul.VPDmax <- VPDmax.m[VPDmax.m$Month == 7, ]
    
    BAL.m <- aggregate(BAL ~ Year + Month, data = MNBAL.df[1:1440,], FUN = sum, na.rm = T)
    jul.BAL <- BAL.m[BAL.m$Month == 7, ]
    
    annual.p <- aggregate(PCP~Year, data = MNp.df[1:1440,], FUN = sum, na.rm=T)
    annual.t <- aggregate(TAVG ~ Year, data = MNtavg.df[1:1440,], FUN = 'mean', na.rm=T)
    annual.mint <- aggregate(TMIN ~Year, data = MNtmin.df[1:1440,], FUN = 'mean', na.rm = T)
    annual.VPDmax <- aggregate(VPDmax ~ Year, data = MNvpdmax.df[1:1440,], FUN = 'mean', na.rm = T)
    annual.BAL <- aggregate(BAL ~ Year, data = MNBAL.df[1:1440,], FUN = 'sum', na.rm = T)
    
    
    annuals <- data.frame(year = annual.p$Year, 
                          PCP = annual.p$PCP,
                          TMIN = annual.mint$TMIN,
                          TAVG = annual.t$TAVG,
                          VPDmax = annual.VPDmax$VPDmax,
                          BAL = annual.BAL$BAL,
                          MAY.p = may.p[1:120,]$PCP,
                          JJA.p = jja.p[1:120,]$PCP,
                          JUNTmin = jun.tmin[1:120,]$TMIN,
                          JUNTavg = jun.tavg[1:120,]$TAVG, 
                          JUNTmax = jun.tmax[1:120,]$TMAX,
                          jul.VPDmax = jul.VPDmax[1:120,]$VPDmax, 
                          jul.BAL = jul.BAL[1:120,]$BAL) 
   
    
    df <- merge(site.df, annuals, by = "year")
    
    df
  
    
  }
}

det.age.clim.prism <-lapply(detrended.age, get.clim, climatedata = "PRISM")
det.age.clim.prism.df <- do.call(rbind,det.age.clim.prism)

det.age.clim.ghcn <-lapply(detrended.age, get.clim, climatedata = "GHCN")
det.age.clim.ghcn.df <- do.call(rbind, det.age.clim.ghcn)

# plot the RWI vs July.pdsi
ggplot(det.age.clim.prism.df, aes(x = jul.VPDmax, y = RWI, color = ageclass))+geom_point()+stat_smooth(method = 'lm')+facet_wrap(~site, ncol = 5)
ggplot(det.age.clim.ghcn.df, aes(x = Jul.pdsi, y = RWI, color = ageclass))+geom_point()+stat_smooth(method = 'lm')+facet_wrap(~site, ncol = 5)

# ------------------plot climate parameters vs growth for all the tree ring series

# this function plots a scatter plot of a climate param vs. growth (RWI)
# with two separate slopes for the "young" and the "old" trees
plot.young.old <- function(x, Climate, xlab, ylab){
  Site <- x[1,]$site
  if(length(unique(x$ageclass)) > 1){
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
    
    scale_color_manual(values=c('old'="red",'young'="blue"), name = "Tree Age")+
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
      
      scale_color_manual(values=c('young'="blue",'old'="red"), name = "Tree Age")+
      #xlim(-8, 8)+
      #ylim(0.5, 1.5) +
      theme_bw()+
      theme(text = element_text(size = 10), plot.title = element_text(hjust = 0.5))+
      ylab("RWI") +
      xlab( xlab ) +
      ggtitle(Site)
  }
  p
  #ggsave(filename = paste0('outputs/correlations/young_old_jul_pdsi_',Site,".png"), plot = p, width = 5, height = 3.5 )
}

# make all the plots for ghcn data: outputs to outputs/correlations/folder
allyoung.old.plots.pdsi <- lapply(det.age.clim.ghcn, plot.young.old, Climate = "PDSI",xlab = "PDSI", ylab = "RWI")

png(width = 10, height = 10, units = 'in', res = 300, "outputs/correlations/young_old_pdsi_allsite.png")
  n <- length(allyoung.old.plots.pdsi)
  nCol <- floor(sqrt(n))
  do.call("grid.arrange", c(allyoung.old.plots.pdsi, ncol=3))
dev.off()

# lets look at July VPDmax:
# make all the plots for ghcn data: outputs to outputs/correlations/folder
allyoung.old.plots.julvpdmax <- lapply(det.age.clim.prism, plot.young.old, Climate = "jul.VPDmax",xlab = "July VPDmax", ylab = "RWI")

png(width = 10, height = 10, units = 'in', res = 300, "outputs/correlations/young_old_jul_VPDmax_allsite.png")
n <- length(allyoung.old.plots.julvpdmax)
nCol <- floor(sqrt(n))
do.call("grid.arrange", c(allyoung.old.plots.julvpdmax, ncol=3))
dev.off()

# looking at July moisture balance:
allyoung.old.plots.julBAL <- lapply(det.age.clim.prism, plot.young.old, Climate = "jul.BAL",xlab = "July P - PET", ylab = "RWI")

png(width = 10, height = 10, units = 'in', res = 300, "outputs/correlations/young_old_jul_BAL_allsite.png")
n <- length(allyoung.old.plots.julBAL)
nCol <- floor(sqrt(n))
do.call("grid.arrange", c(allyoung.old.plots.julBAL, ncol=3))
dev.off()


# can do this for the remaining climate variables:


# the previous plost were showing differences in responses across tree ages, but are there differences before and after 1950 in general?
plot.pre.post <- function(x, Climate, xlab, ylab){
  
    Site <- x[1,]$site # assign site name
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
  #ggsave(filename = paste0('outputs/correlations/pre_post_jul_pdsi_',Site,".png"), plot = p, width = 5, height = 3.5 )
}


# for PDSI (mean):
allpre.post.plots.PDSI <- lapply(det.age.clim.ghcn, plot.pre.post, Climate = "PDSI", xlab = "PDSI", ylab = "RWI")

png(width = 10, height = 10, units = 'in', res = 300, "outputs/correlations/pre_post1950_PDSI_allsite.png")
n <- length(allpre.post.plots.PDSI)
nCol <- floor(sqrt(n))
do.call("grid.arrange", c(allpre.post.plots.PDSI, ncol=3))
dev.off()

# for PDSI (July):
allpre.post.plots.JulPDSI <- lapply(det.age.clim.ghcn, plot.pre.post, Climate = "Jul.pdsi", xlab = "July PDSI", ylab = "RWI")

png(width = 10, height = 10, units = 'in', res = 300, "outputs/correlations/pre_post1950_jul_PDSI_allsite.png")
n <- length(allpre.post.plots.JulPDSI)
nCol <- floor(sqrt(n))
do.call("grid.arrange", c(allpre.post.plots.JulPDSI, ncol=3))
dev.off()

# pre-post July VPDmax:
allpre.post.plots.julvpdmax <- lapply(det.age.clim.prism, plot.pre.post, Climate = "jul.VPDmax",xlab = "July VPDmax", ylab = "RWI")

png(width = 10, height = 10, units = 'in', res = 300, "outputs/correlations/pre_post1950_jul_VPDmax_allsite.png")
  n <- length(allpre.post.plots.julvpdmax)
  nCol <- floor(sqrt(n))
  do.call("grid.arrange", c(allpre.post.plots.julvpdmax, ncol=3))
dev.off()

# pre-post July P-PET:
allpre.post.plots.julBAL <- lapply(det.age.clim.prism, plot.pre.post, Climate = "jul.BAL",xlab = "July P - PET", ylab = "RWI")

png(width = 10, height = 10, units = 'in', res = 300, "outputs/correlations/pre_post1950_jul_BAL_allsite.png")
n <- length(allpre.post.plots.julBAL)
nCol <- floor(sqrt(n))
do.call("grid.arrange", c(allpre.post.plots.julBAL, ncol=3))
dev.off()


# a look at most of the sites altogether
ggplot(det.age.clim.df, aes(x = PDSI, y = RWI, color = site))+geom_point()+stat_smooth()


summary(lm(RWI~PDSI, data = det.age.clim.df))
summary(lm(RWI~PDSI:site, data = det.age.clim.df))
summary(lm(RWI~year, data = det.age.clim.df))
summary(lm(RWI~year:site, data = det.age.clim.df))

ggplot(det.age.clim.df, aes(x = year, y = RWI, color = site))+geom_point()+stat_smooth(method = "lm")

###################################################################
# Lets directly compare old and young years with similar climates:
##################################################################

df <- aggregate(Jul.pdsi~year, data = det.age.clim.df, FUN = mean )
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
get.clim.sensitivity <- function(df, model.func){
  
  coeffs <- matrix ( 0, length(unique(df$site)), 7 ) # set up matrix for coefficients
  
  # for loop
  for(s in 1: length(unique(df$site))){
    
    
      name <- unique(df$site)[s]  
      site.data<- na.omit(df[df$site == name ,])
  
    
    bs <- function(formula, data, indices) {
      d <- data[indices,] # allows boot to select sample 
      fit <- lm(formula, data=d)
      return(coef(fit)) 
    } 
    
    
      results <- boot(data=site.data, statistic=bs, 
                    R=2000, formula=model.func)
      
      int.cis <- boot.ci(boot.out = results, type = "norm", index = 1)# intercept 
      slope.cis <- boot.ci(boot.out = results, type = "norm", index = 2)
    coeffs[s,2:3] <- results$t0
    coeffs[s , 1] <- name
    coeffs[s,4] <- as.data.frame(int.cis$normal)$V2
    coeffs[s,5] <- as.data.frame(int.cis$normal)$V3
    coeffs[s,6] <- as.data.frame(slope.cis$normal)$V2
    coeffs[s,7] <- as.data.frame(slope.cis$normal)$V3
  }
  
  coeffs <- data.frame(coeffs)
  colnames(coeffs) <- c("site",'int.est', "slope.est", "int.min","int.max", "slope.min", "slope.max")
  coeffs$site <- as.character(coeffs$site)
  coeffs$slope.est <- as.numeric(as.character(coeffs$slope.est))
  coeffs$int.est <- as.numeric(as.character(coeffs$int.est))
  coeffs$int.min <- as.numeric(as.character(coeffs$int.min))
  coeffs$int.max <- as.numeric(as.character(coeffs$int.max))
  coeffs$slope.min <- as.numeric(as.character(coeffs$slope.min))
  coeffs$slope.max <- as.numeric(as.character(coeffs$slope.max))
  coeffs
  
}

# get all the sensitivities for pdsi:

pdsi.sens <- get.clim.sensitivity(df = det.age.clim.ghcn.df, model.func = "RWI ~ PDSI")
Julpdsi.sens <- get.clim.sensitivity(df = det.age.clim.ghcn.df, model.func = "RWI ~ Jul.pdsi")
TMIN.sens <- get.clim.sensitivity(df = det.age.clim.ghcn.df, model.func = "RWI ~ TMIN")
May.pr.sens <- get.clim.sensitivity(df = det.age.clim.ghcn.df, model.func = "RWI ~ MAY.p")


# make a plot with error bars
ggplot(pdsi.sens, aes(site, slope.est))+geom_bar(stat = "identity")+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), size = 0.2, width = 0.5)
ggplot(Julpdsi.sens, aes(site, slope.est))+geom_bar(stat = "identity")+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), size = 0.2, width = 0.5)
ggplot(TMIN.sens, aes(site, slope.est))+geom_bar(stat = "identity")+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), size = 0.2, width = 0.5)
ggplot(May.pr.sens, aes(site, slope.est))+geom_bar(stat = "identity")+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), size = 0.2, width = 0.5)

# for prism data:
VPDmax.sens <- get.clim.sensitivity(df = det.age.clim.prism.df, model.func = "RWI ~ VPDmax")
JulVPDmax.sens <- get.clim.sensitivity(df = det.age.clim.prism.df, model.func = "RWI ~ jul.VPDmax")
TMIN.sens <- get.clim.sensitivity(df = det.age.clim.prism.df, model.func = "RWI ~ TMIN")
BAL.sens <- get.clim.sensitivity(df = det.age.clim.prism.df, model.func = "RWI ~ BAL")


ggplot(VPDmax.sens, aes(site, slope.est))+geom_bar(stat = "identity")+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), size = 0.2, width = 0.5)
ggplot(JulVPDmax.sens, aes(site, slope.est))+geom_bar(stat = "identity")+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), size = 0.2, width = 0.5)
ggplot(TMIN.sens, aes(site, slope.est))+geom_bar(stat = "identity")+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), size = 0.2, width = 0.5)
ggplot(BAL.sens, aes(site, slope.est))+geom_bar(stat = "identity")+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), size = 0.2, width = 0.5)


# function to extract slopes for young an old trees of lm(RWI~PDSI)
get.clim.sens.age <- function(df, model.func){
  
  coeffs <- matrix ( 0, length(unique(df$site))*2, 8 ) # set up matrix for coefficients
  
  
  for(s in 1: length(unique(df$site))){
    name <- unique(df$site)[s]  
    site.data<- na.omit(df[df$site == name ,])
    
          # function used in boot strapping below
          bs <- function(formula, data, indices) {
            d <- data[indices,] # allows boot to select sample 
            fit <- lm(formula, data=d)
            return(coef(fit)) 
          } 
          
    # for the "young" class:
     if(nrow(site.data[site.data$site == name & site.data$ageclass == "young" ,]) > 0){
        # bootstrapping the linear regression model
        results <- boot(data=site.data[site.data$ageclass == "young" & site.data$year >= 1950 ,], statistic=bs, R=2000, formula=model.func)
    
        int.cis <- boot.ci(boot.out = results, type = "norm", index = 1)# intercept 
        slope.cis <- boot.ci(boot.out = results, type = "norm", index = 2)
        coeffs[s,3:4] <- results$t0
        coeffs[s , 1] <- name
        coeffs[s,2] <- "young"
        coeffs[s,5] <- as.data.frame(int.cis$normal)$V2
        coeffs[s,6] <- as.data.frame(int.cis$normal)$V3
        coeffs[s,7] <- as.data.frame(slope.cis$normal)$V2
        coeffs[s,8] <- as.data.frame(slope.cis$normal)$V3
     
    } else{
      #lmest <- lm(RWI ~ PDSI, data = df[df$site == name & df$ageclass == "young" ,])
      coeffs[s,3:8] <- c(NA,NA)
      coeffs[s , 2] <- "young"
      coeffs[s,1] <- name
    }
     
     
  # for the "old" class:  
     if(nrow(site.data[site.data$site == name & site.data$ageclass == "old" ,]) > 0){
       results <- boot(data=site.data[site.data$ageclass == "old" & site.data$year < 1950 ,], statistic=bs, R=2000, formula=model.func)
       
       int.cis <- boot.ci(boot.out = results, type = "norm", index = 1)# intercept 
       slope.cis <- boot.ci(boot.out = results, type = "norm", index = 2)
       coeffs[s+length(unique(df$site)),3:4] <- results$t0
       coeffs[s+length(unique(df$site)) , 1] <- name
       coeffs[s+length(unique(df$site)),2] <- "old"
       coeffs[s+length(unique(df$site)),5] <- as.data.frame(int.cis$normal)$V2
       coeffs[s+length(unique(df$site)),6] <- as.data.frame(int.cis$normal)$V3
       coeffs[s+length(unique(df$site)),7] <- as.data.frame(slope.cis$normal)$V2
       coeffs[s+length(unique(df$site)),8] <- as.data.frame(slope.cis$normal)$V3
       
       
     }else{
     #lmest2 <- lm(RWI ~ PDSI, data = df[df$site == name & df$ageclass == "old" ,])
       coeffs[s+length(unique(df$site)),3:8] <- c(NA,NA)
       coeffs[s +length(unique(df$site)), 2] <- "young"
       coeffs[s+length(unique(df$site)),1] <- name
     }
  }
  
  coeffs <- data.frame(coeffs)
  colnames(coeffs) <- c("site","age",'int.est', "slope.est", "int.min","int.max", "slope.min", "slope.max")
  coeffs$site <- as.character(coeffs$site)
  coeffs$slope.est <- as.numeric(as.character(coeffs$slope.est))
  coeffs$int.est <- as.numeric(as.character(coeffs$int.est))
  coeffs$int.min <- as.numeric(as.character(coeffs$int.min))
  coeffs$int.max <- as.numeric(as.character(coeffs$int.max))
  coeffs$slope.min <- as.numeric(as.character(coeffs$slope.min))
  coeffs$slope.max <- as.numeric(as.character(coeffs$slope.max))
  coeffs
  
}


pdsi.age.sens <- get.clim.sens.age(df = det.age.clim.ghcn.df, "RWI ~ PDSI")

# function to extrat the slope for all trees before and after 1950
get.clim.sens.year <- function(df, model.func){
  
  coeffs <- matrix ( 0, length(unique(df$site))*2, 8 ) # set up matrix for coefficients
  yr <- 1895:1950
  yr.post <- 1950:2014
  df$class <- '9999'
  df[df$year %in% yr,]$class <- 'Pre-1950'
  df[df$year %in% yr.post,]$class <- 'Post-1950'
  
  for(s in 1:length(unique(df$site))){
    name <- unique(df$site)[s]  
    site.data <- na.omit(df[df$site == name ,])
    
    # function used in boot strapping below
    bs <- function(formula, data, indices) {
      d <- data[indices,] # allows boot to select sample 
      fit <- lm(formula, data=d)
      return(coef(fit)) 
    } 
    
    # for the "Post-1950" class:
    if(nrow(site.data[ site.data$class == "Post-1950" ,]) > 0){
      # bootstrapping the linear regression model
      results <- boot(data=site.data[site.data$class == "Post-1950" & site.data$year >= 1950 ,], statistic=bs, R=2000, formula=model.func)
      
      int.cis <- boot.ci(boot.out = results, type = "norm", index = 1)# intercept 
      slope.cis <- boot.ci(boot.out = results, type = "norm", index = 2)
      coeffs[s,3:4] <- results$t0
      coeffs[s , 1] <- name
      coeffs[s,2] <- "Post-1950"
      coeffs[s,5] <- as.data.frame(int.cis$normal)$V2
      coeffs[s,6] <- as.data.frame(int.cis$normal)$V3
      coeffs[s,7] <- as.data.frame(slope.cis$normal)$V2
      coeffs[s,8] <- as.data.frame(slope.cis$normal)$V3
      
    } else{
      #lmest <- lm(RWI ~ PDSI, data = df[df$site == name & df$class == "Post-1950" ,])
      coeffs[s,3:8] <- c(NA,NA)
      coeffs[s , 2] <- "Post-1950"
      coeffs[s,1] <- name
    }
    
    
    # for the "Pre-1950" class:  
    if(nrow(site.data[ site.data$class == "Pre-1950" ,]) > 0){
      results <- boot(data=site.data[site.data$class == "Pre-1950" & site.data$year < 1950 ,], statistic=bs, R=2000, formula=model.func)
      
      int.cis <- boot.ci(boot.out = results, type = "norm", index = 1)# intercept 
      slope.cis <- boot.ci(boot.out = results, type = "norm", index = 2)
      coeffs[s+length(unique(df$site)),3:4] <- results$t0
      coeffs[s+length(unique(df$site)) , 1] <- name
      coeffs[s+length(unique(df$site)),2] <- "Pre-1950"
      coeffs[s+length(unique(df$site)),5] <- as.data.frame(int.cis$normal)$V2
      coeffs[s+length(unique(df$site)),6] <- as.data.frame(int.cis$normal)$V3
      coeffs[s+length(unique(df$site)),7] <- as.data.frame(slope.cis$normal)$V2
      coeffs[s+length(unique(df$site)),8] <- as.data.frame(slope.cis$normal)$V3
      
      
    }else{
      coeffs[s+length(unique(df$site)),3:8] <- c(NA,NA)
      coeffs[s +length(unique(df$site)), 2] <- "Pre-1950"
      coeffs[s+length(unique(df$site)),1] <- name
    }
  }

  coeffs <- data.frame(coeffs)
  colnames(coeffs) <- c("site","age",'int.est', "slope.est", "int.min","int.max", "slope.min", "slope.max")
  coeffs$site <- as.character(coeffs$site)
  coeffs$slope.est <- as.numeric(as.character(coeffs$slope.est))
  coeffs$int.est <- as.numeric(as.character(coeffs$int.est))
  coeffs$int.min <- as.numeric(as.character(coeffs$int.min))
  coeffs$int.max <- as.numeric(as.character(coeffs$int.max))
  coeffs$slope.min <- as.numeric(as.character(coeffs$slope.min))
  coeffs$slope.max <- as.numeric(as.character(coeffs$slope.max))
  coeffs
  
  
  }
  
  

pdsi.yr.sens <- get.clim.sens.year(df, "RWI ~ PDSI")


# read in soil, xy characteristics
locs <- read.csv("outputs/priority_sites.csv")
locs$code <- as.character(locs$code)
locs[24:27,]$code <- c("GLL4", "GLL3", "GLL2", "GLL1")
sites <- c("COR", "HIC", "STC", "GLA", "TOW", "ENG", "UNC", "BON", "MOU", "GLL4", "GLL3", "GLL2", "GLL1", "PVC")

# read in the N & S deposition data:
sdep.files <- list.files("data/total_Sdep/")
ndep.files <- list.files("data/total_Ndep/")
s.filenames <- paste0("data/total_Sdep/", sdep.files)
s <- stack(s.filenames) 

n.filenames <- paste0("data/total_Ndep/", ndep.files)
n <- stack(n.filenames)
plot(n[[2]])
plot(mapdata, add = TRUE)
projection(n) <- CRS('+init=epsg:4269')
n.alb <- projectRaster(n,CRS('+init=epsg:3175'))

site.df <- merge(pdsi.sens, locs, by.x = 'site', by.y = 'code')
sens.df <- merge(pdsi.age.sens, locs, by = "site")
yr.sens.df <- merge(pdsi.yr.sens, locs, by = "site")

# map out sensitivities in space:
df_states <- map_data("state")
states <- subset(df_states, region %in% c(  "illinois", "minnesota", "wisconsin", "iowa", "south dakota",
                                             "north dakota", 'michigan', 'missouri', 'indiana') )
coordinates(states) <- ~long+lat
class(states)
proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
mapdata<-spTransform(states, CRS('+init=epsg:3175'))
mapdata.h<-spTransform(states, CRS('+proj=aea +lat_1=0 +lat_2=29.5 +lat_0=45.5 +lon_0=0 +x_0=0 +y_0=-96 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0'))
mapdata<-data.frame(mapdata)

#site.df <- merge(pdsi.sens, locs, by.x = 'site', by.y = 'code')
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
workingdir <- "/Users/kah/Documents/bimodality/data/"

# read in and average prism data (this is modern 30year normals)
prism <- raster(paste0(workingdir,"PRISM_ppt_30yr_normal_4kmM2_all_bil/PRISM_ppt_30yr_normal_4kmM2_annual_bil.bil"))
prism.alb <- projectRaster(prism, crs='+init=epsg:3175')

site.df$pr30yr <- raster::extract(prism.alb, site.df[,c("coords.x1","coords.x2")])
site.df.age$pr30yr <- raster::extract(prism.alb, site.df.age[,c("coords.x1","coords.x2")])
site.df.yr$pr30yr <- raster::extract(prism.alb, site.df.yr[,c("coords.x1","coords.x2")])

workingdir <- "/Users/kah/Documents/bimodality/data/"

# read in and average prism temperature data (this is modern 30year normals)
prism.t<- raster(paste0(workingdir,'PRISM_tmean_30yr_normal_4kmM2_annual_bil/PRISM_tmean_30yr_normal_4kmM2_annual_bil.bil'))
prismt.alb<- projectRaster(prism.t, crs='+init=epsg:3175')

# extract temp
site.df$tm30yr <- raster::extract(prismt.alb, site.df[,c("coords.x1","coords.x2")])
site.df.age$tm30yr <- raster::extract(prismt.alb, site.df.age[,c("coords.x1","coords.x2")])
site.df.yr$tm30yr <- raster::extract(prismt.alb, site.df.yr[,c("coords.x1","coords.x2")])


#merge overalll slope and envt with the young and old slopes:
a <- site.df
#colnames(a) <- c("site","full.slope.est", "full.std.err",  "Name","x",
 #                 "y", "PDSI_time", "sand","ksat","awc",      
  #               "pr30yr"  ,  "tm30yr")

#sens.df <- merge(pdsi.age.sens, locs, by = "site")
#yr.sens.df <- merge(pdsi.yr.sens, locs, by = "site")

#--------------------------------------------------------------
# how does sensitivity to drought vary by climate, envtl factors?
#---------------------------------------------------------------
# prelimnary plots sugges that higher precipitation and higher T places might be more sensitive to PDSI
ggplot(site.df, aes(pr30yr, slope.est))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max))
ggplot(site.df, aes(tm30yr, slope.est))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max))
ggplot(site.df, aes(sand, slope.est))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max))
#ggplot(site.df, aes(Description, slope.est))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max))

# fit a gam on the slope estimate
#gam.sens <- mgcv::gam(slope.est ~ s(pr30yr) + s(tm30yr)   , data = site.df)

#summary(gam.sens) # explains 27.4% of deviance:


#################################################
#  make plots for young and old
# prelimnary plots sugges that higher precipitation and higher T places might be more sensitive to PDSI

ggplot(site.df.age, aes(pr30yr, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), width = 0.5)
ggplot(site.df.age, aes(tm30yr, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max))+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), width = 0.5)
ggplot(site.df.age, aes(sand, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max))+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), width = 0.5)

#sens.young <- gam(slope.est ~ pr30yr + tm30yr +sand  , data = site.df.age[site.df.age$age=="young",])
#summary(sens.young) # explains 47.7% of deviance:

#sens.old <- gam(slope.est ~ pr30yr + tm30yr +sand  , data = sens.df[sens.df$age=="old",])
#summary(sens.old) # explains 90.5% of deviance:

##############################################################
# make prelimnary plots for pre- and post- 1950
###############################################################3
ggplot(site.df.yr, aes(pr30yr, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), width = 0.5)
ggplot(site.df.yr, aes(tm30yr, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), width = 0.5)
ggplot(site.df.yr, aes(sand, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), width = 0.5)

#sens.pre <- gam(slope.est ~ pr30yr + tm30yr +sand  , data = yr.sens.df[yr.sens.df$age=="Pre-1950",])
#summary(sens.pre) # explains 33.4% of deviance:

#sens.post <- gam(slope.est ~ pr30yr + tm30yr +sand  , data = yr.sens.df[yr.sens.df$age=="Post-1950",])
#summary(sens.post) # explains 36.8% of deviance:

sens.df <- site.df.age

#install.packages("plot3D")
library(plot3D)

# created  a funciton that takes the data of interest, fits the gam model:
# gam(sensitivity ~ precip + temperature) and plots a 3d surface of it
plot3dsensitivity <- function(sens.df, age, class, col, add ){
  df <- sens.df[sens.df[,c(age)] == class,]
  df <- df[!is.na(df$slope.est),]
# x, y, z variables
  x <- df$pr30yr
  y <- df$tm30yr
  z <- df$slope.est
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
yr.sens.df <- site.df.yr 
  
png(height = 5, width = 9, units = 'in', res= 300,'outputs/sensitivity_surface3d_pre_post_1950.png')
#sens.df, age, class, col, add
plot3dsensitivity(sens.df = yr.sens.df, age = "age",class = "Pre-1950", col = "red",add = FALSE)
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


files <- list.files("/Users/kah/Documents/TreeRings/cleanrwl/",pattern = ".rwl")

# read each rwl file and name the robject XXXww.rwl 
for (i in seq_along(files)) {
 
  assign(paste(files[i]), read.rwl(paste0("/Users/kah/Documents/TreeRings/cleanrwl/",files[i])))
  
}

#list.rwls <- list(Hicww.rwl, STCww.rwl, Bon, Tow, Ple, Cor, Unc, Eng, Mou, GLL1, GLL2, GLL3, GLL4, GLL4,PVC)
#list.rwls <- list(HICww.rwl, STCww.rwl)
#age_agg_mean <- lapply(list.rwls, FUN = tree_age_agg_mean, sampleyear = 2015, site.code = "HIC", age1950 = 30,type = "RWI" )
#rwiorbai <- HICww.rwl

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
GLL1 <- tree_age_agg_mean(GLL1ww.rwl, 2016, "GLL1", 30,"RWI_Spline_detrended")
GLL2 <- tree_age_agg_mean(GLL2ww.rwl, 2016, "GLL2", 30,"RWI_Spline_detrended")
GLL3 <- tree_age_agg_mean(GLL3ww.rwl, 2016, "GLL3", 30,"RWI_Spline_detrended")
GLL4 <- tree_age_agg_mean(GLL4ww.rwl, 2016, "GLL4", 30,"RWI_Spline_detrended")
PVC <- tree_age_agg_mean(PVCww.rwl, 2016, "GLL5", 30,"RWI_Spline_detrended")


# now plot mean with STDEV
allsitesmean<- list(Hic, Stc, Bon, Tow, Ple, Cor, Unc, Eng, Mou, GLL1, GLL2, GLL3, GLL4, PVC)

plotmean.age<- function(df){
      ggplot(df, aes(Age, Mean))+geom_point()+ 
        geom_errorbar(aes(ymin=Mean-Std, ymax=Mean+Std), width=0.01)+xlim(0,250)+ggtitle(paste0(df$site, " Mean rwi by ageclass"))+theme_bw() 
}

mean.ages <- lapply(allsitesmean, plotmean.age)


png(width = 12, height = 12, units = "in", res = 300, "outputs/mean_age/mean_growth_vs_age.png")
do.call("grid.arrange", c(mean.ages, ncol = 3))
dev.off()

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


allsitesage<- list(Hic.age, Stc.age, Bon.age, Tow.age, Ple.age, Cor.age, Unc.age, Eng.age, Mou.age, GLL1.age, GLL2.age, GLL3.age, GLL4.age, PVC.age)

# now plot mean with STDEV
# made a function to plot out mean RWI vs. age
rwi.age.class<- function(df, site){
  ggplot(df, aes(Age, Mean, color = Ageclass))+geom_point()+ 
    geom_errorbar(aes(ymin=Mean-Std, ymax=Mean+Std), width=.1) +ggtitle(df$site)
}

allsites.ages <- lapply(allsitesage, rwi.age.class)


png(width = 12, height = 12, units = "in", res = 300, "outputs/mean_age/mean_growth_vs_age_by_ageclass.png")
do.call("grid.arrange", c(allsites.ages, ncol = 3))
dev.off()


#------------------Plot pith date vs. mean growth (within each tree)
Hic.pith <- tree_pith_agg_mean(rwiorbai = HICww.rwl, sampleyear = 2015, site.code= "HIC", age1950 = 30,type = "RWI")
Stc.pith <- tree_pith_agg_mean(STCww.rwl, 2015, "STC", 30,"RWI_Spline_detrended")
Bon.pith <- tree_pith_agg_mean(BONww.rwl, 2015, "BON", 30,"RWI_Spline_detrended")
Tow.pith <- tree_pith_agg_mean(TOWww.rwl, 2015, "TOW", 30,"RWI_Spline_detrended")
Ple.pith <- tree_pith_agg_mean(PLEww.rwl, 2015, "PLE", 30,"RWI_Spline_detrended")
Cor.pith <- tree_pith_agg_mean(CORww.rwl, 2016, "COR", 30,"RWI_Spline_detrended")
Unc.pith <- tree_pith_agg_mean(UNCww.rwl, 2016, "UNC", 30,"RWI_Spline_detrended")
Eng.pith <- tree_pith_agg_mean(ENGww.rwl, 2015, "ENG", 30,"RWI_Spline_detrended")
Mou.pith <- tree_pith_agg_mean(MOUww.rwl, 2015, "MOU", 30,"RWI_Spline_detrended")
GLL1.pith <- tree_pith_agg_mean(GLL1ww.rwl, 2016, "GLL1", 30,"RWI_Spline_detrended")
GLL2.pith <- tree_pith_agg_mean(GLL2ww.rwl, 2016, "GLL2", 30,"RWI_Spline_detrended")
GLL3.pith <- tree_pith_agg_mean(GLL3ww.rwl, 2016, "GLL3", 30,"RWI_Spline_detrended")
GLL4.pith <- tree_pith_agg_mean(GLL4ww.rwl, 2016, "GLL4", 30,"RWI_Spline_detrended")
PVC.pith <- tree_pith_agg_mean(PVCww.rwl, 2016, "PVC", 30,"RWI_Spline_detrended")

allsitespith<- list(Hic.pith, Stc.pith, Bon.pith, Tow.pith, Ple.pith, Cor.pith, Unc.pith, Eng.pith, Mou.pith, GLL1.pith, GLL2.pith, GLL3.pith, GLL4.pith, PVC.pith)


rwi.pith <- function(df, site){
  ggplot(df, aes(Pith, Mean, color = ageclass))+geom_point()+ 
    geom_errorbar(aes(ymin=Mean-Std, ymax=Mean+Std), width=.9) +ggtitle(df$site)+theme_bw()+ylab("Mean RWI (mm)")+xlab("Pith date")
}

allsites.pithplots <- lapply(allsitespith, rwi.pith)


png(width = 12, height = 12, units = "in", res = 300, "outputs/mean_age/mean_growth_vs_age_by_pithdate.png")
do.call("grid.arrange", c(allsites.pithplots, ncol = 3))
dev.off()



# left off here:
#------------------------Does growth climate response really vary by age???-----------------
# generate age classes and age-dependant climate response functions:
# use the "all" dataframe created on line 398
all <- det.age.clim.ghcn.df
summary(det.age.clim.ghcn.df$Age) # ages range from 0 to 246
label.breaks <- function(beg, end, splitby){
  labels.test <- data.frame(first = seq(beg, end, by = splitby), second = seq((beg + splitby), (end + splitby), by = splitby))
  labels.test <- paste (labels.test$first, '-' , labels.test$second)
  labels.test
}

# create classes of age groups by 25 years:
all$agebreaks <- cut(all$Age, breaks = seq(0, 250, by = 50), labels = label.breaks(0,200,50))

X11(width = 12)
ggplot(all, aes(Jul.pdsi, RWI))+geom_point()+stat_smooth(method = 'lm')+facet_grid(~agebreaks)

# 
# make a function to plot age based correlations with July PDSI climate of each site:
plot.cor.by.age.site <- function(df, site.names, clim){
    
  coef.list <- list()
    
    all <- df[df$site %in% site.names,]
    for(i in 1:length(unique(all$agebreaks))){
      lm.agebreak <- lm(all[all$agebreaks %in% unique(all$agebreaks)[i],]$RWI ~ all[all$agebreaks %in% unique(all$agebreaks)[i],c(clim)])
      coef.list[[i]] <- lm.agebreak$coefficients
    }
    
    coef <- do.call(rbind, coef.list)
    coef.df <- data.frame(agebreaks = as.character(unique(all$agebreaks)), 
                                intercept = coef[,1], 
                          slope = coef[,2])
    
    # get correlation coefficient for each group
    cor.list <- list()
    for(i in 1:length(unique(all$agebreaks))){
      cor.list[[i]] <- cor(all[all$agebreaks %in% unique(all$agebreaks)[i],]$RWI, all[all$agebreaks %in% unique(all$agebreaks)[i],c(clim)])
    
    }
    
    cors <- do.call(rbind, cor.list)
    cors.df <- data.frame(agebreaks = as.character(unique(all$agebreaks)), 
                          cor = cors[,1])
    
    
    cors.df$agebreaks_f <- factor(cors.df$agebreaks, levels = c("0 - 50", "50 - 100", "100 - 150", "150 - 200", "200 - 250"))
                                                               #"100 - 125", "125 - 150", "150 - 175","175 - 200", "200 - 225", 
                                                               #                                                    "225 - 250", "NA"))
    # plot based on correlation coefficient:
    ggplot(cors.df, aes(agebreaks_f, cor))+geom_bar(stat= "identity")+theme_bw()+ylab(paste("correlation with", clim))+xlab("Tree Age Classes")+ggtitle(site.names)
}

plot.cor.by.age.site(df = all, site.names = "BON", clim = "PDSI")
plot.cor.by.age.site(df = all, site.names = "HIC", clim = "PDSI")
plot.cor.by.age.site(df = all, site.names = "STC", clim = "PDSI")
plot.cor.by.age.site(df = all, site.names = "COR", clim = "PDSI")
plot.cor.by.age.site(df = all, site.names = "UNC", clim = "PDSI")
plot.cor.by.age.site(df = all, site.names = "GLL1", clim = "PDSI")
plot.cor.by.age.site(df = all, site.names = "GLL2", clim = "PDSI")
plot.cor.by.age.site(df = all, site.names = "GLL3", clim = "PDSI")
plot.cor.by.age.site(df = all, site.names = "GLL4", clim = "PDSI")
plot.cor.by.age.site(df = all, site.names = "PLE", clim = "PDSI")
plot.cor.by.age.site(df = all, site.names = "PVC", clim = "PDSI")
plot.cor.by.age.site(df = all, site.names = "TOW", clim = "PDSI")
plot.cor.by.age.site(df = all, site.names = "MOU", clim = "PDSI")
plot.cor.by.age.site(df = all, site.names = "ENG", clim = "PDSI")

plot.cor.by.age.site(df = all, site.names = "BON", clim = "TMIN")
plot.cor.by.age.site(df = all, site.names = "HIC", clim = "TMIN")
plot.cor.by.age.site(df = all, site.names = "STC", clim = "TMIN")
plot.cor.by.age.site(df = all, site.names = "COR", clim = "TMIN")
plot.cor.by.age.site(df = all, site.names = "UNC", clim = "TMIN")
plot.cor.by.age.site(df = all, site.names = "GLL1", clim = "TMIN")
plot.cor.by.age.site(df = all, site.names = "GLL2", clim = "TMIN")
plot.cor.by.age.site(df = all, site.names = "GLL3", clim = "TMIN")
plot.cor.by.age.site(df = all, site.names = "GLL4", clim = "TMIN")
plot.cor.by.age.site(df = all, site.names = "PLE", clim = "TMIN")
plot.cor.by.age.site(df = all, site.names = "PVC", clim = "TMIN")
plot.cor.by.age.site(df = all, site.names = "TOW", clim = "TMIN")
plot.cor.by.age.site(df = all, site.names = "MOU", clim = "TMIN")
plot.cor.by.age.site(df = all, site.names = "ENG", clim = "TMIN")
