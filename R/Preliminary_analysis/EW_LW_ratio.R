# This script calculate ratio of EW to LW for each year:

library(dplR)
library(ggplot2)
library(reshape2)
setwd("/Users/kah/Documents/TreeRings")

# this function does the following:
  # reads in the early wood and late wood files, 
  # takes the ration of EW:LW, 
  # plots tree level interannual fluctuationsin in EW:LW
  # calculates the trendline of EW:LW over time and saves output in "outputs/EWLW"

EWLW.ratio <- function(site, sampleyear, avg){
    TRseriesew <- read.tucson(paste0("/Users/kah/Documents/crossdating/data/cofecha/",site,"earlywood width.rwl"))
    TRserieslw <- read.tucson(paste0("/Users/kah/Documents/crossdating/data/cofecha/",site,"latewood width.rwl"))
    
    if(avg == TRUE){ # if avg true, the average all cores in each tree
        TRseriesew <- treeMean(TRseriesew, autoread.ids(TRseriesew), na.rm=TRUE)
        TRserieslw <- treeMean(TRserieslw, autoread.ids(TRserieslw), na.rm=TRUE)
      }else{ # if not, then don't avg
        TRseriesew <- TRseriesew
        TRserieslw <- TRserieslw
    }
    
    
    rat <- TRseriesew/TRserieslw
   
    
    rat$Year <- as.numeric(row.names(rat))
    rat.m <- melt(rat, id.vars = "Year")
    rat.m <- rat.m[rat.m$Year > 1700,] # gets rid of undated series (fix for now, but need to chech measurements)
    
    png(paste0("outputs/EWLW/", site, "_avg_EWLW_ts.png"))
    ggplot(rat.m[rat.m$Year < sampleyear ,], aes(Year, value, color = variable))+geom_line()+ggtitle(paste0(site, " average EW/LW in each tree"))
    dev.off()
    
    ggplot(rat.m[rat.m$Year < sampleyear,], aes(Year, value, color = variable))+geom_point()+stat_smooth()+ggtitle(paste0(site, " average EW/LW in each tree"))
    
    
    if(avg == TRUE){
        ggsave( file = paste0("outputs/EWLW/", site, "_avg_EWLW_ts.png"), ggplot(rat.m[rat.m$Year < sampleyear,], aes(Year, value, color = variable))+geom_line()+ggtitle(paste0(site, " average EW/LW in each tree"))
        )
      ggsave( file = paste0("outputs/EWLW/", site, "_avg_EWLW_ts_reg.png"), ggplot(rat.m[rat.m$Year < sampleyear & rat.m$Year > 1895,], aes(Year, value))+geom_point()+stat_smooth(method = "lm")+ggtitle(paste0(site, " average EW/LW trend 1895-sampleyr"))
      )
      # calculation regression coefficients:
      test <- summary(lm(value ~ Year, data = rat.m))$coefficients
      write.csv(test, paste0("outputs/EWLW/", site, "EWLW_time_reg_coef.csv"))
        #ggplot(rat.m[rat.m$Year < sampleyr,], aes(Year, value, color = variable))+geom_line()+ggtitle(paste0(site, " average EW/LW in each tree"))
        
        
        write.csv(rat, paste0("outputs/EWLW/",site, "EWLW_avg.csv"))
        
      }else{
          ggsave(paste0("outputs/EWLW/", site, "_EWLW_ts.png"), ggplot(rat.m[rat.m$Year < sampleyr,], aes(Year, value, color = variable))+geom_line()+ggtitle(paste0(site, " EW/LW in each core"))
          )
          #ggplot(rat.m[rat.m$Year < sampleyr,], aes(Year, value, color = variable))+geom_line()+ggtitle(paste0(site, " EW/LW in each core"))
          #dev.off()
          write.csv(rat, paste0("outputs/EWLW/",site, "EWLW.csv"))
    }
    
    rat
}

# apply this function to the different sites
BON.rat <- EWLW.ratio("BON", 2015, avg = TRUE)
#HIC.rat <- EWLW.ratio("HIC", 2015, avg = TRUE) # need to get hic ew and lw 
GLA.rat <- EWLW.ratio("GLA", 2015, avg = TRUE)
COR.rat <- EWLW.ratio("COR", 2016, avg = TRUE)
TOW.rat <- EWLW.ratio("TOW", 2015, avg = TRUE)
STC.rat <- EWLW.ratio("STC", 2015, avg = TRUE)
ENG.rat <- EWLW.ratio("ENG", 2015, avg = TRUE)
AVOo.rat <- EWLW.ratio("AVOo", 2016, avg = TRUE)
UNC.rat <- EWLW.ratio("UNC", 2015, avg = TRUE)
PAM.rat <- EWLW.ratio("PAM", 2015, avg = TRUE)
LED.rat <- EWLW.ratio("LED", 2015, avg = TRUE)
UNI.rat <- EWLW.ratio("UNI", 2015, avg = TRUE)
GLL1.rat <- EWLW.ratio("GLL1", 2016, avg = TRUE)
GLL2.rat <- EWLW.ratio("GLL2", 2016, avg = TRUE)
GLL3.rat <- EWLW.ratio("GLL3", 2016, avg = TRUE)
GLL4.rat <- EWLW.ratio("GLL4", 2016, avg = TRUE)
PVC.rat <- EWLW.ratio("PVC", 2016, avg = TRUE)

#------Are the sites with increasing EW:LW ratios also the ones with a change in drought sensitivity?-------

# read all the coefficient CSV's in the "outputs/EWLW/" folder:
file_names = list.files("outputs/EWLW")
file_names = file_names[ grepl("reg_coef.csv",file_names)]
full_filenames <- paste0("/Users/kah/Documents/TreeRings/outputs/EWLW/",file_names)
sites <- substr(file_names, 1,3)

coeffs <- lapply(full_filenames, read.csv, header=T) 
names(coeffs) <- sites
coeffs.m <- melt(coeffs)

coeffs.m[coeffs.m$X %in% "Year" & coeffs.m$variable %in% "Pr...t..", ]

# only BON and UNI dont have a significant trend in the ration of EW/LW

coeffs.m[coeffs.m$X %in% "Year", ]


#--------------------- are the EW and LW growth correlated?----------------------
# this function calculates the correlation coefficient between the EW and LW of each tree
EWLW.cor <- function(site,  avg){
  TRseriesew <- read.tucson(paste0("/Users/kah/Documents/crossdating/data/cofecha/",site,"earlywood width.rwl"))
  TRserieslw <- read.tucson(paste0("/Users/kah/Documents/crossdating/data/cofecha/",site,"latewood width.rwl"))
  
  if(avg == TRUE){ # if avg true, the average all cores in each tree
    TRseriesew <- treeMean(TRseriesew, autoread.ids(TRseriesew), na.rm=TRUE)
    TRserieslw <- treeMean(TRserieslw, autoread.ids(TRserieslw), na.rm=TRUE)
  }else{ # if not, then don't avg
    TRseriesew <- TRseriesew
    TRserieslw <- TRserieslw
  }
  
  
  
  corel <- TRseriesew[1,]
  for(i in 1:ncol(TRseriesew)){
    corel[,i] <- cor(TRseriesew[,i], TRserieslw[,i], use = "pairwise.complete.obs")
  }
  
  
  corel
  
}

PVC.cor <- EWLW.cor(site="PVC",  avg = TRUE)
BON.cor <- EWLW.cor("BON",  avg = TRUE)
#HIC.rat <- EWLW.ratio("HIC", 2015, avg = TRUE) # need to get hic ew and lw 
GLA.cor <- EWLW.cor("GLA",  avg = TRUE)
COR.cor <- EWLW.cor("COR",  avg = TRUE)
TOW.cor <- EWLW.cor("TOW",  avg = TRUE)
STC.cor <- EWLW.cor("STC",  avg = TRUE)
ENG.cor <- EWLW.cor("ENG",  avg = TRUE)
AVOo.cor <- EWLW.cor("AVOo",  avg = TRUE)
UNC.cor <- EWLW.cor("UNC",  avg = TRUE)
PAM.cor <- EWLW.cor("PAM",  avg = TRUE)
LED.cor <- EWLW.cor("LED",  avg = TRUE)
UNI.cor <- EWLW.cor("UNI",  avg = TRUE)
GLL1.cor <- EWLW.cor("GLL1",  avg = TRUE)
GLL2.cor <- EWLW.cor("GLL2",  avg = TRUE)
GLL3.cor <- EWLW.cor("GLL3",  avg = TRUE)
GLL4.cor <- EWLW.cor("GLL4",  avg = TRUE)
PVC.cor <- EWLW.cor("PVC",  avg = TRUE)


#------------------------ how correlated is LW to climate?---------------------

# detrend the lw series
read.detrend.lw<- function(site, avg = TRUE){
  
      TRseriesew <- read.tucson(paste0("/Users/kah/Documents/crossdating/data/cofecha/",site,"earlywood width.rwl"))
      TRserieslw <- read.tucson(paste0("/Users/kah/Documents/crossdating/data/cofecha/",site,"latewood width.rwl"))
      
      if(avg == TRUE){ # if avg true, the average all cores in each tree
        TRseriesew <- treeMean(TRseriesew, autoread.ids(TRseriesew), na.rm=TRUE)
        TRserieslw <- treeMean(TRserieslw, autoread.ids(TRserieslw), na.rm=TRUE)
      }else{ # if not, then don't avg
        TRseriesew <- TRseriesew
        TRserieslw <- TRserieslw
      }
      
      png(paste0("outputs/EWLW/",site,"_LW_chron.png"))
      TR.chron<- chron(TRserieslw)
      plot(chron(TRserieslw))
      dev.off()
      
      lw.det <- detrend(TR.chron, method = "Spline")
     
      
      lw.det$Year <- row.names(lw.det)
      lw.det$Site <- site
      ggplot(lw.det, aes(as.numeric(Year), xxxstd))+geom_line()
      lw.det

}

BON.lw <- read.detrend.lw("BON", avg = TRUE)
#HIC.rat <- read.detrend.lw("HIC", 2015, avg = TRUE) # need to get hic ew and lw 
GLA.lw <- read.detrend.lw("GLA", avg = TRUE)
COR.lw <- read.detrend.lw("COR",  avg = TRUE)
TOW.lw <- read.detrend.lw("TOW",  avg = TRUE)
STC.lw <- read.detrend.lw("STC",  avg = TRUE)
ENG.lw <- read.detrend.lw("ENG", avg = TRUE)
AVOo.lw <- read.detrend.lw("AVOo",  avg = TRUE)
UNC.lw <- read.detrend.lw("UNC",  avg = TRUE)
PAM.lw <- read.detrend.lw("PAM",  avg = TRUE)
LED.lw <- read.detrend.lw("LED",  avg = TRUE)
UNI.lw <- read.detrend.lw("UNI",  avg = TRUE)
GLL1.lw <- read.detrend.lw("GLL1",  avg = TRUE)
GLL2.lw <- read.detrend.lw("GLL2",  avg = TRUE)
GLL3.lw <- read.detrend.lw("GLL3",avg = TRUE)
GLL4.lw <- read.detrend.lw("GLL4",  avg = TRUE)
PVC.lw <- read.detrend.lw("PVC",  avg = TRUE)

#read climate files in:
IL.clim <- read.csv("data/NE_illinois_climdiv.csv") #Hickory Grove, Sandwich, Glacial park
WIse.clim <- read.csv("data/south_east_wi_climdiv.csv") #pleasant prairie 
MNwc.clim <- read.csv("data/West_central_MN_nclimdiv.csv") #Bonanza praire, Duboix
WIsc.clim <- read.csv("data/south_central_WI_climdiv.csv") #pleasant valley conservancy
MNec.clim <- read.csv("data/East_Central_MN_CDODiv5039587215503.csv") #townsend woods 
MNec.clim <- read.csv("data/East_Central_MN_CDODiv5039587215503.csv") #St. Croix savanna
MNse.clim <- read.csv("data/South_East_MN_CDO.csv") # for mound prairie


#this function merges relevant climate parameters with the lw site chronologies

merge.clim.lw <- function(MNcd.clim, chron){
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
  
  annuals <- data.frame(Year = annual.p$Year, 
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
  annuals.crn <- merge(annuals, chron[c('Year', 'xxxstd', 'Site')], by = "Year")
  melt(annuals.crn, id = c('Year','Site', 'PCP', "TMIN", "TAVG", "PDSI","MAY.p","JJA.p", 
                           "JUNTmin","JUNTavg", 'JUNTmax',"Jul.pdsi"))
}

#create molten dataframes with climate and rwi chronologies
#molten.HIC.lw <- merge.clim.chron(IL.clim, HIC.lw)
molten.COR.lw <- merge.clim.chron(IL.clim, COR.lw)
molten.GLA.lw<- merge.clim.chron(IL.clim, GLA.lw)
molten.BON.lw <- merge.clim.chron(MNwc.clim, BON.lw)
molten.PLE.lw <- merge.clim.chron(WIsc.clim, PLE.lw)
molten.TOW.lw <- merge.clim.chron(MNec.clim, TOW.lw)
molten.STC.lw <- merge.clim.chron(MNec.clim, STC.lw)
molten.UNC.lw <- merge.clim.chron(MNec.clim, UNC.lw)
molten.ENG.lw <- merge.clim.chron(MNec.clim, ENG.lw)
molten.MOU.lw <- merge.clim.chron(MNse.clim, MOU.lw)
molten.GL1.lw <- merge.clim.chron(MNwc.clim, GLL1.lw)
molten.GL2.lw <- merge.clim.chron(MNwc.clim, GLL2.lw)
molten.GL3.lw <- merge.clim.chron(MNwc.clim, GLL3.lw)
molten.GL4.lw <- merge.clim.chron(MNwc.clim, GLL4.lw)
molten.UNC.lw <- merge.clim.chron(MNec.clim, UNC.lw)
molten.PVC.lw <- merge.clim.chron(IL.clim, PVC.lw)


#want to plot all sites at once eventually, so merge these together 
molten.full <- rbind(molten.HIC.lw, molten.BON.lw, molten.STC.lw, #molten.PLE,
                     molten.TOW.lw, molten.ENG.lw, molten.UNC.lw, molten.COR.lw, molten.GLA.lw, molten.MOU.lw, molten.GL1.lw, molten.GL2.lw,
                     molten.GL3.lw, molten.GL4.lw, molten.PLE.lw, molten.PVC.lw)

write.csv(molten.full, "outputs/full_molten_chron_LW.csv")






# merge 


rat$Year <- as.numeric(row.names(rat))
rat.m <- melt(rat, id.vars = "Year")

#------------------------ how correlated is EW to climate?---------------------
# this function reads in the EW file for a site, averages (if avg == TRUE) the cores from the same tree, and 
# detrends the EW using splines. Before detrending, the function also plots the chronolgy of EW over time (saved under outputs/EWLW)
read.detrend.ew<- function(site, avg = TRUE){
  
  TRseriesew <- read.tucson(paste0("/Users/kah/Documents/crossdating/data/cofecha/",site,"earlywood width.rwl"))
  TRserieslw <- read.tucson(paste0("/Users/kah/Documents/crossdating/data/cofecha/",site,"latewood width.rwl"))
  
  if(avg == TRUE){ # if avg true, the average all cores in each tree
    TRseriesew <- treeMean(TRseriesew, autoread.ids(TRseriesew), na.rm=TRUE)
    TRserieslw <- treeMean(TRserieslw, autoread.ids(TRserieslw), na.rm=TRUE)
  }else{ # if not, then don't avg
    TRseriesew <- TRseriesew
    TRserieslw <- TRserieslw
  }
  
  png(paste0("outputs/EWLW/",site,"_EW_chron.png"))
  TR.chron<- chron(TRseriesew)
  plot(chron(TRseriesew))
  dev.off()
  
  ew.det <- detrend(TR.chron, method = "Spline")
  
  
  ew.det$Year <- row.names(ew.det)
  ew.det$Site <- site
  # save the plot of EW over time
  png(paste0("outputs/EWLW/", site, "-detrended_EW.png"))
  ggplot(ew.det, aes(as.numeric(Year), xxxstd))+geom_point()+geom_line(data =BON.ew, aes(x=as.numeric(Year), y=xxxstd))+xlab("Year")+ylab("Detrended EW")+ggtitle(site)
  dev.off()
  
  ew.det
  
}

BON.ew <- read.detrend.ew("BON", avg = TRUE)
GLA.ew <- read.detrend.ew("GLA", avg = TRUE)
COR.ew <- read.detrend.ew("COR",  avg = TRUE)
TOW.ew <- read.detrend.ew("TOW",  avg = TRUE)
STC.ew <- read.detrend.ew("STC",  avg = TRUE)
ENG.ew <- read.detrend.ew("ENG", avg = TRUE)
AVOo.ew <- read.detrend.ew("AVOo",  avg = TRUE)
UNC.ew <- read.detrend.ew("UNC",  avg = TRUE)
PAM.ew <- read.detrend.ew("PAM",  avg = TRUE)
LED.ew <- read.detrend.ew("LED",  avg = TRUE)
UNI.ew <- read.detrend.ew("UNI",  avg = TRUE)
GLL1.ew <- read.detrend.ew("GLL1",  avg = TRUE)
GLL2.ew <- read.detrend.ew("GLL2",  avg = TRUE)
GLL3.ew <- read.detrend.ew("GLL3",avg = TRUE)
GLL4.ew <- read.detrend.ew("GLL4",  avg = TRUE)
PVC.ew <- read.detrend.ew("PVC",  avg = TRUE)

# temporal trends in EW chronololgy vary across sites, but many show increasing trends over time
# some of these increases level off, but others do not

# Is EW correlated with climate? Use the merge.clim.chron function to get the data together

molten.COR.ew <- merge.clim.chron(IL.clim, COR.ew)
molten.GLA.ew<- merge.clim.chron(IL.clim, GLA.ew)
molten.BON.ew <- merge.clim.chron(MNwc.clim, BON.ew)
molten.PLE.ew <- merge.clim.chron(WIsc.clim, PLE.ew)
molten.TOW.ew <- merge.clim.chron(MNec.clim, TOW.ew)
molten.STC.ew <- merge.clim.chron(MNec.clim, STC.ew)
molten.UNC.ew <- merge.clim.chron(MNec.clim, UNC.ew)
molten.ENG.ew <- merge.clim.chron(MNec.clim, ENG.ew)
molten.MOU.ew <- merge.clim.chron(MNse.clim, MOU.ew)
molten.GL1.ew <- merge.clim.chron(MNwc.clim, GLL1.ew)
molten.GL2.ew <- merge.clim.chron(MNwc.clim, GLL2.ew)
molten.GL3.ew <- merge.clim.chron(MNwc.clim, GLL3.ew)
molten.GL4.ew <- merge.clim.chron(MNwc.clim, GLL4.ew)
molten.PVC.ew <- merge.clim.chron(IL.clim, PVC.ew)

# note that some of the EW files need to be fixed, so we don't have EW data for all sites yet
molten.full.ew <- rbind( molten.BON.ew, molten.UNC.ew,
                     molten.TOW.ew,   molten.COR.ew, molten.GLA.ew, molten.GL1.ew, molten.GL2.ew,
                     molten.GL3.ew, molten.GL4.ew,  molten.PVC.ew)

write.csv(molten.full.ew, "outputs/full_molten_chron_EW.csv")

#These are just plots of climate over time
ggplot(molten.full.ew, aes(Year, Jul.pdsi, color = Site))+geom_point()+geom_line(data = molten.full.ew, aes(Year, Jul.pdsi, color = Site))
ggplot(molten.full.ew, aes(Year, JUNTmax, color = Site))+geom_point()+geom_line(data = molten.full.ew, aes(Year, JUNTmax, color = Site))
ggplot(molten.full.ew, aes(Year, JUNTmax, color = Site))+geom_point()+geom_line(data = molten.full.ew, aes(Year, JUNTmax, color = Site))
ggplot(molten.full.ew, aes(Year, PCP, color = Site))+geom_point()+geom_line(data = molten.full.ew, aes(Year, PCP, color = Site))

# plot overall data (not by site)
ggplot(molten.full.ew, aes(Jul.pdsi, value))+geom_point()+stat_smooth(method = "lm")
ggplot(molten.full.ew, aes(PDSI, value))+geom_point()+stat_smooth(method = "lm")
ggplot(molten.full.ew, aes(PCP, value))+geom_point()+stat_smooth(method = "lm")
ggplot(molten.full.ew, aes(JJA.p, value))+geom_point()+stat_smooth(method = "lm")
ggplot(molten.full.ew, aes(JUNTmax, value))+geom_point()+stat_smooth(method = "lm")
ggplot(molten.full.ew, aes(JUNTmin, value))+geom_point()+stat_smooth(method = "lm")
ggplot(molten.full.ew, aes(JUNTavg, value))+geom_point()+stat_smooth(method = "lm")
ggplot(molten.full.ew, aes(MAY.p, value))+geom_point()+stat_smooth(method = "lm")


# plot overall data (colored by site)
ggplot(molten.full.ew, aes(Jul.pdsi, value, color = Site))+geom_point()+stat_smooth(method = "lm")+facet_wrap(~Site)+ylab("detrended EW")
ggplot(molten.full.ew, aes(PDSI, value, color = Site))+geom_point()+stat_smooth(method = "lm")+facet_wrap(~Site)+ylab("detrended EW")
ggplot(molten.full.ew, aes(PCP, value, color = Site))+geom_point()+stat_smooth(method = "lm")+facet_wrap(~Site)+ylab("detrended EW")
ggplot(molten.full.ew, aes(JJA.p, value, color = Site))+geom_point()+stat_smooth(method = "lm")+facet_wrap(~Site)+ylab("detrended EW")
ggplot(molten.full.ew, aes(JUNTmax, value, color = Site))+geom_point()+stat_smooth(method = "lm")+facet_wrap(~Site)+ylab("detrended EW")
ggplot(molten.full.ew, aes(JUNTmin, value, color = Site))+geom_point()+stat_smooth(method = "lm")+facet_wrap(~Site)+ylab("detrended EW")
ggplot(molten.full.ew, aes(JUNTavg, value, color = Site))+geom_point()+stat_smooth(method = "lm")+facet_wrap(~Site)+ylab("detrended EW")
ggplot(molten.full.ew, aes(MAY.p, value, color = Site))+geom_point()+stat_smooth(method = "lm")+facet_wrap(~Site)+ylab("detrended EW")


