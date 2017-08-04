# This script calculate ratio of EW to LW for each year:

# do i need to detrend?

library(dplR)
library(ggplot2)
setwd("/Users/kah/Documents/TreeRings")

EWLW.ratio<- function(site, sampleyear, avg){
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

png(paste0("outputs/EWLW/", site, "_avg_EWLW_ts.png"))
ggplot(rat.m[rat.m$Year < sampleyr,], aes(Year, value, color = variable))+geom_line()+ggtitle(paste0(site, " average EW/LW in each tree"))
dev.off()

ggplot(rat.m[rat.m$Year < sampleyr,], aes(Year, value, color = variable))+geom_point()+stat_smooth()+ggtitle(paste0(site, " average EW/LW in each tree"))


if(avg == TRUE){
    png(paste0("outputs/EWLW/", site, "_avg_EWLW_ts.png"))
    ggplot(rat.m[rat.m$Year < sampleyr,], aes(Year, value, color = variable))+geom_line()+ggtitle(paste0(site, " average EW/LW in each tree"))
    dev.off()
    
    write.csv(rat, paste0("outputs/EWLW/",site, "EWLW_avg.csv"))
    
  }else{
      png(paste0("outputs/EWLW/", site, "_EWLW_ts.png"))
      ggplot(rat.m[rat.m$Year < sampleyr,], aes(Year, value, color = variable))+geom_line()+ggtitle(paste0(site, " EW/LW in each core"))
      dev.off()
      write.csv(rat, paste0("outputs/EWLW/",site, "EWLW.csv"))
}

rat
}


BON.rat <- EWLW.ratio("BON", 2015, avg = TRUE)
#HIC.rat <- EWLW.ratio("HIC", 2015, avg = TRUE) # need to get hic ew and lw 
GLA.rat <- EWLW.ratio("GLA", 2015, avg = TRUE)
COR.rat <- EWLW.ratio("COR", 2015, avg = TRUE)
TOW.rat <- EWLW.ratio("TOW", 2015, avg = TRUE)
STC.rat <- EWLW.ratio("STC", 2015, avg = TRUE)
ENG.rat <- EWLW.ratio("ENG", 2015, avg = TRUE)
AVOo.rat <- EWLW.ratio("AVOo", 2015, avg = TRUE)
UNC.rat <- EWLW.ratio("UNC", 2015, avg = TRUE)
PAM.rat <- EWLW.ratio("PAM", 2015, avg = TRUE)
LED.rat <- EWLW.ratio("LED", 2015, avg = TRUE)
UNI.rat <- EWLW.ratio("UNI", 2015, avg = TRUE)
GLL1.rat <- EWLW.ratio("GLL1", 2015, avg = TRUE)
GLL2.rat <- EWLW.ratio("GLL2", 2015, avg = TRUE)
GLL3.rat <- EWLW.ratio("GLL3", 2015, avg = TRUE)
GLL4.rat <- EWLW.ratio("GLL4", 2015, avg = TRUE)
PVC.rat <- EWLW.ratio("PVC", 2015, avg = TRUE)
