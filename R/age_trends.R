library(dplR)
library(ggplot2)
library(plyr)
library(raster)
library(data.table)
library(rgdal)
library(mgcv)
library(tidyr)
library(SPEI)
library(boot)
library(dplyr)
# lets look atrelationship to climate with age:
setwd("/Users/kah/Documents/TreeRings")

#####################################
#read in rwl & add site + year codes#
#####################################

# quick function to read, detrend, and add the year as a column:
# this function will also just calculate BAI instead
read_detrend_year <- function( filename, method , rwiorbai, site){
  if(site %in% c("HIC", "AVO", "UNI", "GLL1", "GLL2", "GLL3")){
    newseries <- read.csv(paste0("cleanrwl/",site,"ww.csv"))
    rwl.stats(newseries)
    file.tuc <- read.tucson( filename )
    rownames(newseries) <- rownames(file.tuc)
    
  }else{if(site %in% "GLL4"){
      newseries <- read.csv(paste0("cleanrwl/",site,"ww.csv"))
      
      rownames(newseries) <- newseries$year
      newseries <- newseries[,1:(length(newseries)-1)] # remove yr column
      rwl.stats(newseries)
    }
  newseries <- read.tucson( filename )
  rwl.stats(newseries)
  }

  # average the cores by tree (for the sites with multiple cores):
  
  #gp.ids <- read.ids(newseries, stc = autoread.ids(newseries))
  
  gp.treeMean <- treeMean(newseries, autoread.ids(newseries))
  gp.treeMean2 <- treeMean(newseries, autoread.ids(newseries), na.rm=TRUE)
  
  mean.rwl.stat <- rwl.stats(gp.treeMean2)
  write.csv(mean.rwl.stat, paste0("outputs/Stats/mean.rwl.stats.", site,".csv"))
  
  ifelse(rwiorbai == "rwi", 
          detrended <- detrend(rwl = newseries, method = method),
          detrended <- bai.out(rwl = newseries))
  
  
 
  if(site %in% "HIC"){
    detrended.mean <- treeMean(detrended, read.ids(detrended, stc = c(3,4,1)), na.rm=TRUE)
    colnames(detrended.mean) <- paste0(site,colnames(detrended.mean))
  }else{
    if(site %in% "GLL4"){
      detrended.mean <- treeMean(detrended, read.ids(detrended, stc = c(4,7,1)), na.rm=TRUE)
      colnames(detrended.mean) <- paste0(site, colnames(detrended.mean))
      # quick fix for GLL4:
      
      colnames(detrended.mean) <- c("GLL41", "GLL413", "GLL414", "GLL415", "GLL42", "GLL45", "GLL47", "GLL48", "GLL49")
      
    }else{
    detrended.mean <- treeMean(detrended, autoread.ids(detrended), na.rm=TRUE)
  colnames(detrended.mean) <- paste0(site,colnames(detrended.mean))
    }
  }
  mean.rwi.stat <- rwl.stats(detrended.mean)
  write.csv(mean.rwi.stat, paste0("outputs/Stats/mean.rwi.stats.", site,".csv"))
  
  # plot spag plots:
  png(paste0("outputs/spagplots/", site, "_", rwiorbai,"_mean_", method,"_detrended.png"))
  plot(detrended.mean, "spag")
  dev.off()
  
  detrended.mean$year <- rownames(detrended.mean)
  detrended.mean$site<- site
  write.csv(detrended,paste0("cleanrwl/detrended_rwi_", site, ".csv"))
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
GLL1.bai <- read_detrend_year(filename = "cleanrwl/GLL1ww.rwl", method = "Spline", rwiorbai = "rwi", site = "GLL1")
GLL2.bai <- read_detrend_year("cleanrwl/GLL2ww.rwl", method = "Spline", rwiorbai = "rwi", site = "GLL2")
GLL3.bai <- read_detrend_year("cleanrwl/GLL3ww.rwl", method = "Spline", rwiorbai = "rwi", site = "GLL3")
GLL4.bai <- read_detrend_year("cleanrwl/GLL4ww.rwl", method = "Spline", rwiorbai = "rwi", site = "GLL4")
PVC.bai <- read_detrend_year("cleanrwl/PVCww.rwl", method = "Spline", rwiorbai = "rwi", site = "PVC")
AVO.bai <- read_detrend_year(filename = "cleanrwl/AVOww.rwl", method = "Spline", rwiorbai = "rwi", site = "AVO")
UNI.bai <- read_detrend_year("cleanrwl/UNIww.rwl", method = "Spline", rwiorbai = "rwi", site = "UNI")

detrended.list <- list(Hickory.bai, StCroix.bai, Bonanza.bai,Townsend.bai,Pleasant.bai, Coral.bai,
                 Uncas.bai, Glacial.bai, Englund.bai, Mound.bai, GLL1.bai, GLL2.bai, 
                 GLL3.bai, GLL4.bai, PVC.bai, AVO.bai)#, UNI.bai) # omitting UNI right now

# read in the site level data for each of these sites:
test <- read.csv("data/site_maps/stand_metadata/GLL1_full_xy.csv")

test$short %in% colnames(detrended.mean)


# make example chronology to plot out:
hic.raw <- read.rwl("cleanrwl/HICww.rwl")
hic.raw$year <- as.numeric(row.names( hic.raw))

png(width=6,height=4,units="in",res = 300,bg = "transparent","raw_rw_transparent.png")
ggplot(Hickory.bai, aes(hic.raw$year, hic.raw[,11]))+geom_line(color = "white")+theme_minimal()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text = element_text(colour = "white"), axis.title = element_text(color = "white"))+ylab("Raw Ring Width")+xlab("Year")
dev.off()

Hic.m<- melt(Hickory.bai, id.vars = c('year','site'))
Hic.m$year <- as.numeric(Hic.m$year)
Hickory.bai$year <- as.numeric(Hickory.bai$year)

png(width=6,height=4,units="in",res = 300,bg = "transparent","det_transparent.png")
ggplot(Hickory.bai, aes(Hickory.bai$year, Hickory.bai[,11]))+geom_line(color = "white")+theme_minimal()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text = element_text(colour = "white"), axis.title = element_text(color = "white"))+ylab("Detrended Ring Width Index")+xlab("Year")
dev.off()

hic.chron <- chron(Hickory.bai)
hic.chron$year <- as.numeric(row.names(hic.chron))

png(width=6,height=4,units="in",res = 300,bg = "transparent","transparent_chronology.png")
ggplot(hic.chron, aes(year, xxxstd))+xlim(1856, 2016) +ylim(0,2)+geom_line(color = "white")+theme_minimal()+xlab("Year")+ylab("Detrended Ring Width Index")+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text = element_text(colour = "white"), axis.title = element_text(color = "white"))
dev.off()

# make example PDSI reconstruction to plot out:
PDSImi <- read.table("/Users/kah/Documents/TreeRings/outputs/data/850w_425n_226.txt", header = TRUE)
png(width=6,height=4,units="in",res = 300,bg = "transparent","transparent_reconstruction.png")
ggplot(PDSImi, aes(YEAR, RECON))+#xlim(1500, 2016) +ylim(0,2)+
  geom_line(color = "white")+theme_minimal()+xlab("Year")+ylab("Reconstructed PDSI")+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text = element_text(colour = "white"), axis.title = element_text(color = "white"))
dev.off()
##########################################################
# tree age_agg adds on the ages of the trees at each year
# can do this with BAI or detrended RWI
source("R/tree_age_agg.R")

# apply the tree_age_agg function on all of the detrended tree ring series
detrended.age <- lapply(detrended.list, FUN = tree_age_agg,   age1950 = 10,type = "RWI_Spline_detrended" )

# use do.calll to make these a dataframe
detrended.age.df <- do.call(rbind, detrended.age)


age.classes <- detrended.age.df %>% group_by(site, ID)  %>% drop_na() %>% summarise(pre1800 = min(year) < 1880  , pre1950 = min(year, na.rm = TRUE) <1930 & min(year, na.rm = TRUE) >=1880 , post1950 = min(year, na.rm = TRUE) >1930)

age.classes  %>% group_by(site) %>% summarise(pre1800_n=sum(pre1800, na.rm=TRUE), pre1950_n = sum(pre1950, na.rm=TRUE), post1950_n = sum(post1950, na.rm=TRUE))

write.csv(age.classes, "data/site_stats/n_trees_ageclass_by_site.csv")

###################################
# add climate data to the age trends
####################################
# note about climate data: GHCN climate data provides PDSI esimtates, while PRISM is more commonly used and can be used to get VPD data.
# both GHCN and PRISM have Precip and temperature estimates, but PRSIM data should be used for this b/c GHCN is over the whole climate zone, PRISM is point estimates

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
          }else{ if(site.code %in% c("UNC", "AVO")){
            MNcd.clim <- read.csv("data/East_Central_MN_CDODiv5039587215503.csv")
          }else { if(site.code == 'PLE'){
            MNcd.clim <- read.csv('data/south_central_WI_climdiv.csv')
          }else { if(site.code == 'YRF'){
            MNcd.clim <- read.csv('IA_nclim_div_northeast.csv')
          }else{
            cat("missing climate data")}
          }
          }
          }
          }
          }
          }
          }
          }
          }}
          
          
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
          jja.pdsi <- aggregate(PDSI ~ Year, data = MNpdsi.df[MNpdsi.df$Month %in% 6:8 & MNpdsi.df$Year %in% 1895:2014,], FUN = 'mean', na.rm = T)
          jja.pdsi.m <- aggregate(PDSI ~ Year + Month, data = MNpdsi.df[MNpdsi.df$Month %in% 6:8 & MNpdsi.df$Year %in% 1895:2014,], FUN = 'mean', na.rm = T)
          
          
          
          annuals <- data.frame(year = annual.p$Year, 
                                PCP = annual.p$PCP,
                                TMIN = annual.mint$TMIN,
                                TAVG = annual.t$TAVG,
                                PDSI = annual.pdsi$PDSI,
                                JJA.pdsi = jja.pdsi$PDSI,
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
    
    jja.VPDmax <- aggregate(VPDmax ~ Year, data = MNvpdmax.df[MNvpdmax.df$Month %in% 6:8 & MNvpdmax.df$Year %in% 1895:2014,], FUN = 'mean', na.rm = T)
    
    annuals <- data.frame(year = annual.p$Year, 
                          PCP = annual.p$PCP,
                          TMIN = annual.mint$TMIN,
                          TAVG = annual.t$TAVG,
                          VPDmax = annual.VPDmax$VPDmax,
                          jja.VPDmax = jja.VPDmax$VPDmax,
                          BAL = annual.BAL$BAL,
                          MAY.p = may.p[1:120,]$PCP,
                          JJA.p = jja.p[1:120,]$PCP,
                          JUNTmin = jun.tmin[1:120,]$TMIN,
                          JUNTavg = jun.tavg[1:120,]$TAVG, 
                          JUNTmax = jun.tmax[1:120,]$TMAX,
                          jul.VPDmax = jul.VPDmax[1:120,]$VPDmax, 
                          jul.BAL = jul.BAL[1:120,]$BAL) 
   write.csv(annuals, paste0("data/climate/PRISM/", site.code, "full.clim.csv"))
    
    df <- merge(site.df, annuals, by = "year")
    
    df
  
    
  }
}

# get prism climate and merge for all:
det.age.clim.prism <- lapply(detrended.age, get.clim, climatedata = "PRISM")
det.age.clim.prism.df <- do.call(rbind,det.age.clim.prism)

# get GHCN climate and merge for all:
det.age.clim.ghcn <-  lapply(detrended.age, get.clim, climatedata = "GHCN")
det.age.clim.ghcn.df <- do.call(rbind, det.age.clim.ghcn)

# plot the RWI vs July.pdsi
ggplot(det.age.clim.prism.df, aes(x = jul.VPDmax, y = RWI, color = ageclass))+geom_point()+stat_smooth(method = 'lm')+facet_wrap(~site, ncol = 5)
ggplot(det.age.clim.ghcn.df, aes(x = Jul.pdsi, y = RWI, color = ageclass))+geom_point()+stat_smooth(method = 'lm')+facet_wrap(~site, ncol = 5)
ggplot(det.age.clim.ghcn.df, aes(x = JJA.pdsi, y = RWI, color = ageclass))+geom_point()+stat_smooth(method = 'lm')+facet_wrap(~site, ncol = 5)


#age.classes <- det.age.clim.ghcn.df %>% group_by(site, ID)  %>% summarise(pre1800 = min(year, na.rm = TRUE) < 1880, pre1950 = min(year, na.rm = TRUE) <1930 & min(year, na.rm = TRUE) >=1880 , post1950 = min(year, na.rm = TRUE) >1930)

#test <- age.classes %>% group_by(site) %>% summarise(pre1800_n=sum(pre1800 , na.rm=TRUE), pre1950_n = sum(pre1950, na.rm=TRUE), post1950_n = sum(post1950, na.rm=TRUE))


# write these dfs to a csv:
write.csv(det.age.clim.prism.df, "outputs/data/full_det_prism_rwi.csv", row.names = FALSE)
write.csv(det.age.clim.ghcn.df, "outputs/data/full_det_ghcn_rwi.csv", row.names = FALSE)

png(height = 3, width = 5, units = "in", res =300,"outputs/pdsi_over_time_bw.png")
ggplot(data = det.age.clim.ghcn.df[det.age.clim.ghcn.df$ID %in% "BON13", ], aes(year, Jul.pdsi))+geom_point()+stat_smooth(method = "lm", se = FALSE)+geom_line(color = "White")+theme_black(base_size = 20)+ylab("July PDSI")+geom_hline(yintercept = 0, color = "grey", linetype = "dashed")
dev.off()
# ------------------------- Get tree DBH at time of coring + put in DBH classes ----------------------------------
# This function uses DBH at time of coring and annual growth records to estimate Tree DBH over time
# based on the DBH at each time step, specify the DBH class over time. 
# for some reason these throw up alot of warnings now, but seem to be okay

read_DBH_year <- function( filename, site){
  
  if(site %in% c("HIC", "AVO", "UNI", "GLL1", "GLL2", "GLL3", "GLL4")){
    newseries <- read.csv(paste0("cleanrwl/",site,"ww.csv"))
    
    row.names(newseries) <- newseries$year
    newseries <- newseries[!names(newseries) %in% "year"]
  }else{
    newseries <- read.tucson( filename )
  }
 
  rwl.stats(newseries)
  # average the cores by tree (for the sites with multiple cores):
  
  
  
  gp.treeMean <- treeMean(newseries, read.ids(newseries, stc = c(3,1,2)))
  gp.treeMean2 <- treeMean(newseries, autoread.ids(newseries), na.rm=TRUE)
  
  # if multiple cores were sampled per each site, we need to average the widths of the cores before estimating diamters:
  mult.core.sites <- c("TOW", "COR", "HIC", "STC", "MOU", "ENG", "PVC", "HIC","UNI", "BON", "PLE",  "GLL1", "GLL2", "GLL3", "GLL4")
  if(site %in% mult.core.sites){
          if(site %in% "COR"){
            colnames(gp.treeMean2) <- paste0(site,19, colnames(gp.treeMean2))
          }else{
            if(site %in% "MOU"){
              gp.treeMean2 <- treeMean(newseries, read.ids(newseries, stc = c(3,1,2)))
              colnames(gp.treeMean2) <- paste0(site,colnames(gp.treeMean2))
            }else{
            if(site %in% "HIC"){
              
              gp.treeMean2 <- treeMean(newseries, read.ids(newseries, stc = c(3,4,1)), na.rm=TRUE)
              colnames(gp.treeMean2) <- paste0(site, colnames(gp.treeMean2))
            }else{
              if(site %in% "GLL4"){
              
              gp.treeMean2 <- treeMean(newseries, read.ids(newseries, stc = c(4,7,1)), na.rm=TRUE)
              colnames(gp.treeMean2) <- paste0(site, colnames(gp.treeMean2))
              # quick fix for GLL4:
              
              colnames(gp.treeMean2) <- c("GLL41", "GLL413", "GLL414", "GLL415", "GLL42", "GLL45", "GLL47", "GLL48", "GLL49")
              }else{
                if(site %in% "UNI"){
                colnames(gp.treeMean2) <- paste0(site, colnames(gp.treeMean))
              
            
            }else{
          colnames(gp.treeMean2) <- paste0(site, colnames(gp.treeMean2))
          
            }}}}}

          newseries <- gp.treeMean2
          
          site.data <- read.csv(paste0("/Users/kah/Documents/TreeRings/data/site_maps/all_metadata/", site, "_full_xy.csv"))
          if(site %in% "AVO"){
            diams <- site.data[complete.cases(site.data[c("full_tellervo", "DBH", "SpecCode")]), ]
            diams.agg <- aggregate(diams[,c("full_tellervo", "DBH")], list(diams$full_tellervo), mean, na.rm = TRUE)
            colnames(diams.agg) <- c("ID", "short", "DBH")
            #spec <- site.data[complete.cases(site.data[,c("full_tellervo", "SpecCode")]),c("full_tellervo", "SpecCode")]
            #diams.agg <- merge(diams.agg, spec, by.x = "ID", by.y = "full_tellervo")
            
            spec <- site.data[complete.cases(site.data[,c("full_tellervo", "SpecCode")]),c("full_tellervo", "SpecCode")]
            spec <- spec[!duplicated(spec),]
            diams.agg <- merge(diams.agg, spec, by.x = "ID", by.y = "full_tellervo")
            diams <- diams.agg[,c("ID", "DBH", "SpecCode")]
            diams$DBH <- c(diams$DBH) # subtract ~2cm for barkwidth and convert to mm
            colnames(diams) <- c("ID", "DBH", "SpecCode") 
            
            
            # only find records where we have both DBH and tellervo entries:
            # writecsv with tree rwl that are missing for each site so we can check these:
            not.in.rwl <- diams [!diams$ID %in% colnames(newseries),]
            if(length(not.in.rwl$ID) > 0){ # if there are any records missing, make a csv output
              write.csv(not.in.rwl, paste0("data/site_stats/", site, "-IDS_not_in_tellervo.csv"))
            }
            diams <- diams [diams$ID %in% colnames(newseries),]
            newseries <- newseries[,colnames(newseries) %in% diams$ID]
            write.csv(diams,paste0("outputs/DBH/species_codes_", sitecode, ".csv"))
            
            
          }else{
            diams <- site.data[c("short", "DBH",  "SpecCode")]
            #diams <- diams[2:length(diams$short),]
            diams$DBH <- as.numeric(as.character(diams$DBH))
            diams.agg <- aggregate(diams, list(diams$short), mean, na.rm = TRUE)
            colnames(diams.agg) <- c("ID", "short", "DBH")
            diams.agg<- diams.agg[!duplicated(diams.agg),]
            spec <- site.data[complete.cases(site.data[,c("short", "SpecCode")]),c("short", "SpecCode")]
            spec <- spec[!duplicated(spec),]
            diams.agg <- merge(diams.agg, spec, by.x = "ID", by.y = "short")
            diams <- diams.agg[,c("ID", "DBH", "SpecCode")]
            diams$DBH <- c(diams$DBH) # may need to subtract ~2cm for barkwidth 
            colnames(diams) <- c("ID", "DBH", "SpecCode") 
            
            
            # only find records where we have both DBH and tellervo entries:
            # writecsv with tree rwl that are missing for each site so we can check these:
            not.in.rwl <- diams [!diams$ID %in% colnames(newseries),]
            if(length(not.in.rwl$ID) > 0){ # if there are any records missing, make a csv output
              write.csv(not.in.rwl, paste0("data/site_stats/", site, "-IDS_not_in_tellervo.csv"))
            }
            
            
            diams <- diams [diams$ID %in% colnames(newseries),]
            newseries <- newseries[,colnames(newseries) %in% diams$ID]
            write.csv(diams ,paste0("outputs/DBH/species_codes_", sitecode, ".csv"))
          }
          
          rwl <- newseries*0.1 # convert measuremnts to CM:
          
          # below code is adapted from dplR function bai.out to just estimate tree diameter at this point:
          
          # if the data is messed up, send up some error warnings!
          if (!is.data.frame(newseries)) 
            stop("'rwl' must be a data.frame")
          if (!is.null(diams)) {
            if (ncol(newseries) != nrow(diams[!names(diams) %in% "SpecCode"])) 
              stop("dimension problem: ", "'ncol(rw)' != 'nrow(diam)'")
            if (!all(diams[, 1] %in% names(newseries))) 
              stop("series ids in 'diam' and 'rwl' do not match")
            diam.vec <- diams[, 2]
          }
          
          # setting up and reordering vectors to match diameters to the tellervo records:
          out <- rwl
          n.vec <- seq_len(nrow(rwl))
          diam <- diams[ order(match(diams$ID, colnames(rwl))), ] # reorder diameter vector to match trees
          diam.vec <- diam[, 2]
          
          # for each column and year, calculate the tree diameter:
          for (i in seq_len(ncol(rwl))) {
            dat <- rwl[[i]]
            dat2 <- na.omit(dat)
            #if (is.null(diams)) 
             # d <- sum(dat2) * 2
            #else 
            d <- diam.vec[i]
            r0 <- d/2 - c(0, cumsum(rev(dat2)))
            #bai <- -pi * rev(diff(r0 * r0))
            
            # add space for NA values in rwl style files:
            na <- attributes(dat2)$na.action
            if(min( n.vec[!n.vec %in% na]) == 1){
              no.na <- c( n.vec[!n.vec %in% na])
              out[no.na, i] <- rev(r0[1:length(r0)-1])*2 # only report back the diameters
            }else{
              
              no.na <- c(na[length(na)], n.vec[!n.vec %in% na])
              out[no.na, i] <- rev(r0[1:length(r0)])*2 # only report back the diameters
            }
            
          }
          
  

}else{ 
    
    # if sites only have one core per tree:
          site.data <- read.csv(paste0("/Users/kah/Documents/TreeRings/data/site_maps/all_metadata/", site, "_full_xy.csv"))
          
          diams <- site.data[c("full_tellervo", "DBH")]
          diams$DBH <- (diams$DBH) 
          colnames(diams) <- c("ID", "DBH") 
          spec <- site.data[complete.cases(site.data[,c("full_tellervo", "SpecCode")]),c("full_tellervo", "SpecCode")]
          spec <- spec[!duplicated(spec),]
          #diams.agg <- merge(diams.agg, spec, by.x = "ID", by.y = "full_tellervo")
          diams.agg <- merge(diams, spec, by.x = "ID", by.y = "full_tellervo")
          diams <- diams.agg[,c("ID", "DBH", "SpecCode")]
          diams$DBH <- c(diams$DBH) # subtract ~2cm for barkwidth and convert to mm
          colnames(diams) <- c("ID", "DBH", "SpecCode") 
          
          # writecsv with tree rwl that are missing for each site so we can check these:
          not.in.rwl <- diams [!diams$ID %in% colnames(newseries),]
          if(length(not.in.rwl$ID) > 0){ # if there are any records missing, make a csv output
            write.csv(not.in.rwl, paste0("data/site_stats/", site, "-IDS_not_in_tellervo.csv"))
          }
          # only find records where we have both DBH and tellervo entries:
          diams <- diams [diams$ID %in% colnames(newseries),]
          newseries <- newseries[,colnames(newseries) %in% diams$ID]
          write.csv(diams,paste0("outputs/DBH/species_codes_", site, ".csv"))
          
          rwl <- newseries*0.1 # convert measuremnts to CM:
          
          # below code is adapted from dplR function bai.out to just estimate tree diameter at this point:
          
          # if the data is messed up, send up some error warnings!
          if (!is.data.frame(rwl)) 
            stop("'rwl' must be a data.frame")
          if (!is.null(diam)) {
            if (ncol(rwl) != nrow(diams)) 
              stop("dimension problem: ", "'ncol(rwl)' != 'nrow(diam)'")
            if (!all(diams[, 1] %in% names(rwl))) 
              stop("series ids in 'diam' and 'rwl' do not match")
            diam.vec <- diams[, 2]
          }
          
          # setting up and reordering vectors to match diameters to the tellervo records:
          out <- rwl
          n.vec <- seq_len(nrow(rwl))
          diam <- diams[ order(match(diams$ID, colnames(rwl))), ] # reorder diameter vector to match trees
          diam.vec <- diam[, 2]
          
          # for each column and year, calculate the tree diameter:
          for (i in seq_len(ncol(rwl))) {
            dat <- rwl[[i]]
            dat2 <- na.omit(dat)
            if (is.null(diam)) 
              d <- sum(dat2) * 2
            else d <- diam.vec[i]
            r0 <- d/2 - c(0, cumsum(rev(dat2)))
            #bai <- -pi * rev(diff(r0 * r0))
            # add space for NA values in rwl style files:
            na <- attributes(dat2)$na.action
            if(min( n.vec[!n.vec %in% na]) == 1){
              no.na <- c( n.vec[!n.vec %in% na])
              out[no.na, i] <- rev(r0[1:length(r0)-1])*2 # only report back the diameters
            }else{
              
              no.na <- c(na[length(na)], n.vec[!n.vec %in% na])
              out[no.na, i] <- rev(r0[1:length(r0)])*2 # only report back the diameters
            }
            
          
  
          }
}
  # rename df
  yearly.diams <- out

  
  # add on year and site names
  yearly.diams$year <- row.names(yearly.diams)
  yearly.diams$site <- site
  
  # output yearly dataframe
  yearly.diams
}


Hickory.DBH <- read_DBH_year(filename = "cleanrwl/HICww.rwl",  site = "HIC")
StCroix.DBH <- read_DBH_year("cleanrwl/STCww.rwl",  site = "STC")
Bonanza.DBH <- read_DBH_year(filename = "cleanrwl/BONww.rwl", site = "BON") # missing 1 core
Townsend.DBH <- read_DBH_year(filename = "cleanrwl/TOWww.rwl",  site = "TOW") #missing 1 core
Pleasant.DBH <- read_DBH_year(filename = "cleanrwl/PLEww.rwl",  site = "PLE") #missing 3
Coral.DBH <- read_DBH_year(filename = "cleanrwl/CORww.rwl",  site = "COR") #bai needs the 19 in front of numbers
Uncas.DBH <- read_DBH_year(filename = "cleanrwl/UNCww.rwl",  site = "UNC") # bai is miisng full names...save as csv?
Glacial.DBH <- read_DBH_year("cleanrwl/GLAww.rwl",  site = "GLA") # messed up and DBH not averaged ring
Englund.DBH <- read_DBH_year(filename = "cleanrwl/ENGww.rwl",  site = "ENG")
Mound.DBH <- read_DBH_year(filename = "cleanrwl/MOUww.rwl", site = "MOU") # bai is messed up
GLL1.DBH <- read_DBH_year("cleanrwl/GLL1ww.rwl",  site = "GLL1")# bai removed extra ones
GLL2.DBH <- read_DBH_year("cleanrwl/GLL2ww.rwl",  site = "GLL2") #  bai removed extra onesi
GLL3.DBH <- read_DBH_year("cleanrwl/GLL3ww.rwl",  site = "GLL3")
GLL4.DBH <- read_DBH_year(filename = "cleanrwl/GLL4ww.rwl",  site = "GLL4") # error
PVC.DBH <- read_DBH_year("cleanrwl/PVCww.rwl",  site = "PVC")
AVO.DBH <- read_DBH_year(filename = "cleanrwl/AVOww.rwl",  site = "AVO") 
UNI.DBH <- read_DBH_year(filename = "cleanrwl/UNIww.rwl", site = "UNI") # DBH has multiple cores listed

dbh.list <- list(Hickory.DBH, StCroix.DBH, Bonanza.DBH,Townsend.DBH,Pleasant.DBH, Coral.DBH,
                       Uncas.DBH, Glacial.DBH, Englund.DBH, Mound.DBH, GLL1.DBH, GLL2.DBH, 
                       GLL3.DBH, GLL4.DBH, PVC.DBH, AVO.DBH) #, UNI.DBH)



# function to assign DBH class to all dataframes and make plots of DBH
DBH.classify <- function(dbh.df, n.classes){
        Hic <- dbh.df
        
        
        # plot rwi vs. tree age:
        DBH.m <- melt(Hic)
        colnames(DBH.m) <- c("year","site", "ID", "DBH")
        DBH.m$year <- as.numeric(DBH.m$year)
        site <- unique(DBH.m$site)
        # print out trajectory of DBH at each sites
        
        dbh.plot <- ggplot(DBH.m, aes(x = year, y = DBH, color = ID)) + geom_line()+theme_bw()
       ggsave(plot = dbh.plot, filename = paste0("outputs/DBH/",  site, "_DBH_time.png"))
        
        
        
        DBH.m$ID <- as.character(DBH.m$ID)
        DBH.m$dbhclass <- "small"
        site.code <- unique(DBH.m$site)
        
        # need to assign trees to age classes:
         
         if(n.classes == 9){
          class.dbh <- ifelse(is.na(DBH.m$DBH),  "NA",
                              ifelse(DBH.m$DBH <= 10,  "< 10", 
                 ifelse(DBH.m$DBH > 10 & DBH.m$DBH <= 20 ,  "10 - 20", 
                        ifelse(DBH.m$DBH > 20 & DBH.m$DBH <= 30 , "20 - 30",
                               ifelse(DBH.m$DBH > 30 & DBH.m$DBH <= 40 , "30 - 40",
                                      ifelse(DBH.m$DBH > 40 & DBH.m$DBH <= 50 ,  "40 - 50",
                                             ifelse(DBH.m$DBH > 50 & DBH.m$DBH <= 60 , "50 - 60",
                                                    ifelse(DBH.m$DBH > 60 & DBH.m$DBH <= 70 ,  "60 - 70",
                                                           ifelse(DBH.m$DBH > 70 & DBH.m$DBH <= 80 ,  "70 - 80", 
                                                                ">80")))))))))
          
         }else{ if(n.classes == 5){
           class.dbh <- ifelse(is.na(DBH.m$DBH),  "NA",
                               ifelse(DBH.m$DBH <= 20,  "< 20", 
                                      ifelse(DBH.m$DBH > 20 & DBH.m$DBH <= 40 ,  "20 - 40",
                                             ifelse(DBH.m$DBH > 40 & DBH.m$DBH <= 60 ,  "40 - 60",
                                                ifelse(DBH.m$DBH > 60 & DBH.m$DBH <= 80 , "60 - 80",">80")))))
         }else{
           class.dbh <- ifelse(is.na(DBH.m$DBH),  "NA",
                               ifelse(DBH.m$DBH <= 30,  "< 30", 
                                      ifelse(DBH.m$DBH > 30 & DBH.m$DBH <= 60 ,  "20 - 60", 
                                             ifelse(DBH.m$DBH > 60 & DBH.m$DBH <= 80 , "60 - 80",
                                                    ifelse(DBH.m$DBH > 80 , "> 80",">80")))))
         }}
        
        DBH.m$dbhclass <- class.dbh # output DBH dataframe
       # DBH.m$ID <- substr(DBH.m$ID, start = 4, 10)
        DBH.m
}

dbh.class <- lapply(dbh.list, DBH.classify, n.classes = 5)
dbh.class.df <- do.call(rbind, dbh.class) # make into df

# summarize # of cores each site has est before 1900, 1900-1950, and after 1950:

summary(dbh.class.df)

detach(package: plyr)
minyear.by.ID <- dbh.class.df %>% group_by(site, ID)  %>% summarise(min(year, na.rm = TRUE))
#group.by(site) %>% summarise()

age.classes <- dbh.class.df %>% group_by(site, ID)  %>% drop_na() %>% summarise(pre1800 = min(year, na.rm = TRUE) <1880, pre1950 = min(year, na.rm = TRUE) <1930 & min(year, na.rm = TRUE) >=1880 , post1950 = min(year, na.rm = TRUE) >1930)

age.classes %>% group_by(site) %>% summarise(pre1800_n=sum(pre1800, na.rm=TRUE), pre1950_n = sum(pre1950, na.rm=TRUE), post1950_n = sum(post1950, na.rm=TRUE))

# fixing some ID's with UNI:
test.uni<- det.age.clim.ghcn.df[det.age.clim.ghcn.df$site %in% "UNI",]$ID
det.age.clim.ghcn.df[det.age.clim.ghcn.df$site %in% "UNI",]$ID<- substr(test.uni, 1, 4)
det.age.clim.prism.df[det.age.clim.prism.df$site %in% "UNI",]$ID<- substr(test.uni, 1, 4)




# merge the diameter class df with the climate/growth dataframes:
det.age.clim.ghcn.df <- merge(det.age.clim.ghcn.df, dbh.class.df, by = c("year", "site", "ID"))
det.age.clim.prism.df <- merge(det.age.clim.prism.df, dbh.class.df, by = c("year", "site", "ID"))
test.ghcn.df <- merge(det.age.clim.ghcn.df, dbh.class.df, by = c("year", "site", "ID"))
test.prism.df <- merge(det.age.clim.prism.df, dbh.class.df, by = c("year", "site", "ID"))

# change factor order of dbhclass to make prettier plots:
det.age.clim.ghcn.df$dbhclass <- factor(det.age.clim.ghcn.df$dbhclass, levels = c("< 20", "20 - 40", "40 - 60", "60 - 80", ">80"))
png("outputs/DBH/July_clim_sens_by_dbh.png")
ggplot(na.omit(det.age.clim.ghcn.df), aes(Jul.pdsi, RWI, color = dbhclass))+stat_smooth(method = "lm", se = TRUE, aes(fill = dbhclass), alpha = 0.1)+theme_bw()+theme_black()
dev.off()

png("outputs/DBH/JJA_clim_sens_by_dbh.png")
ggplot(na.omit(det.age.clim.ghcn.df), aes(JJA.pdsi, RWI, color = dbhclass))+stat_smooth(method = "lm", se = TRUE, aes(fill = dbhclass), alpha = 0.1)+theme_bw()+theme_black(base_size = 20)+ylab("Detrended Ring Width Index")+xlab("Summer PDSI")
dev.off()


det.age.clim.ghcn.df$ageclass <- factor(det.age.clim.ghcn.df$ageclass, levels = c("Past", "Modern"))

png("outputs/DBH/July_clim_sens_by_ageclass.png")
ggplot(na.omit(det.age.clim.ghcn.df), aes(Jul.pdsi, RWI, color = dbhclass))+stat_smooth(method = "lm", se = TRUE, aes(fill = dbhclass), alpha = 0.1)+theme_bw()+theme_black()+facet_wrap(~ageclass)
dev.off()

png("outputs/DBH/JJA_clim_sens_by_ageclass.png")
ggplot(na.omit(det.age.clim.ghcn.df), aes(JJA.pdsi, RWI, color = dbhclass))+stat_smooth(method = "lm", se = TRUE, aes(fill = dbhclass), alpha = 0.1)+theme_bw()+theme_black(base_size = 12)+facet_wrap(~ageclass)
dev.off()

summary(aov(RWI~JJA.pdsi+ageclass, data=det.age.clim.class.ghcn.df))
summary(aov(RWI~JJA.pdsi*ageclass, data=det.age.clim.class.ghcn.df))

ggplot(na.omit(det.age.clim.ghcn.df), aes(JJA.pdsi, RWI, color = ageclass))+stat_smooth(method = "lm", se = TRUE, aes(fill = dbhclass), alpha = 0.1)+theme_bw()+theme_black()+facet_wrap(~ageclass)
det.age.clim.class.ghcn.df <- merge(det.age.clim.ghcn.df, locs, by.x = "site", by.y = "code")

png("outputs/DBH/JJA_clim_sens_by_coverclass.png")
ggplot(na.omit(det.age.clim.class.ghcn.df), aes(JJA.pdsi, RWI, color = Description))+geom_point(size = 0.8)+stat_smooth(method = "lm", se = TRUE, aes(fill = Description), alpha = 0.1)+theme_bw()+theme_black()+ylab("Detrended Ring Width Index")+xlab("Summer PDSI")
dev.off()

summary(lm(RWI ~ JJA.pdsi, data = det.age.clim.class.ghcn.df[det.age.clim.class.ghcn.df$Description %in% "Forest",]))
summary(lm(RWI ~ JJA.pdsi, data = det.age.clim.class.ghcn.df[det.age.clim.class.ghcn.df$Description %in% "Savanna",]))

summary(aov(RWI~JJA.pdsi+Description, data=det.age.clim.class.ghcn.df))
summary(aov(RWI~JJA.pdsi*Description, data=det.age.clim.class.ghcn.df))

png("outputs/DBH/July_clim_sens_by_site.png")
ggplot(na.omit(det.age.clim.ghcn.df), aes(Jul.pdsi, RWI, color = dbhclass))+stat_smooth(method = "lm", se = TRUE, aes(fill = ageclass), alpha = 0.1)+theme_bw()+theme_black()+facet_wrap(~site)
dev.off()

png("outputs/DBH/JJA_clim_sens_by_site.png")
ggplot(na.omit(det.age.clim.ghcn.df), aes(JJA.pdsi, RWI, color = dbhclass))+stat_smooth(method = "lm", se = TRUE, aes(fill = dbhclass), alpha = 0.1)+theme_bw()+theme_black()+facet_wrap(~site)
dev.off()

summary(lm(RWI ~ Jul.pdsi:dbhclass, data = na.omit(det.age.clim.ghcn.df)))
summary(lm(RWI ~ JJA.pdsi:dbhclass, data = na.omit(det.age.clim.ghcn.df)))

# make these plots with correlation coefficent, not linear relationships:
head(det.age.clim.ghcn.df)
require(plyr)
cor.func <- function(xx)
{
  return(data.frame(COR = cor(xx$RWI, xx$JJA.pdsi)))
}

nona.age.df <- det.age.clim.ghcn.df[!is.na(det.age.clim.ghcn.df$RWI) & !is.na(det.age.clim.ghcn.df$JJA.pdsi),]
# get correlation ceofficient with ages class
det.age.dbhclass.cor <- ddply(nona.age.df, .(dbhclass, site), cor.func)
ggplot(det.age.dbhclass.cor, aes(dbhclass, COR))+geom_bar(stat = "identity")+facet_wrap(~site)

cor.boot <- function(df){
dat <- df[,c("RWI", "JJA.pdsi")]
N <- nrow(dat)
R <- 2500

cor.orig <- cor(dat)[1,2]
cor.boot <- NULL

for (i in 1:R) {
  idx <- sample.int(N, N, replace = TRUE) 
  cor.boot[i] <- cor(dat[idx, ])[1,2] 
}
cor.boot
}

det.age.dbhclass.cor.b <- ddply(nona.age.df, .(dbhclass), cor.boot)
hist(cor.boot)
# lets find to cores that need to be checked (not sensitive to climate)
corrs <- data.frame(a = 1:length(unique(det.age.clim.ghcn.df$ID)),
                    id = unique(det.age.clim.ghcn.df$ID))
for(i in 1:length(unique(det.age.clim.ghcn.df$ID))){
  id <- unique(det.age.clim.ghcn.df$ID)[i]
 
  a <- cor(det.age.clim.ghcn.df[det.age.clim.ghcn.df$ID %in% id,]$RWI, det.age.clim.ghcn.df[det.age.clim.ghcn.df$ID %in% id,]$Jul.pdsi, use = "pairwise.complete.obs")
  corrs[i,]$a <- a
  corrs[i,]$id <- id
}

removes <- corrs[corrs$a <= 0.1 | is.na(corrs$a),]$id

det.age.clim.ghcn.df <- det.age.clim.ghcn.df[!det.age.clim.ghcn.df$ID %in% removes,]

write.csv(det.age.clim.ghcn.df, "outputs/det.age.clim.ghcn.sizes.csv", row.names = FALSE)
write.csv(det.age.clim.class.ghcn.df, "outputs/det.age.clim.ghcn.sizes.covclass.csv", row.names = FALSE)
# ------------------------How does growth vary over time:

#library(treeclim)

# we will us the dcc function in the tree clim package, but this funtion takes monthly data:
#test <- det.age.clim.ghcn.df


# moving correlations between climate and tree growth


#these funcitons print out plots time moving correlations for all of the climate parameters
# not run by default b/c they take along time to run:
clim.cor <- function(climate, chron, site.name){
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
    
  }
  climate <- MNcd.clim 
  PREC <- climate[,c('Year', 'Month', 'PCP')]
  PREC$Year <- as.numeric(PREC$Year)
  #PREC$PCP <- PREC$PCP*25.54
  PREC <- PREC[1:1452,]
  
  # PDSI
  PDSI <- climate[,c('Year', 'Month', 'PDSI')]
  PDSI$Year <- as.numeric(PDSI$Year)
  #PREC$PCP <- PREC$PCP*25.54
  PDSI <- PDSI[1:1452,]
  
  chron <- chron[chron$Year >=1895,]
  
  hic.pdsi.static <- dcc(chron, PREC, dynamic = 'static', win_size = 35, win_offset = 30)
  
  pdf(paste0('outputs/correlations/moving_site_cors/PREC_', site.name,'dynamic.pdf'))
  print(plot(hic.pdsi.static))
  #g_test(hic.pdsi.moving)
  #traceplot(hic.pdsi.moving)
  #plot(skills(hic.pdsi.moving))
  
  
  hic.prec.moving <- dcc(chron, PREC, dynamic = 'moving', win_size = 45, win_offset = 5, ci = 0.05, boot = "std")
  
  print(plot(hic.prec.moving))
  #g_test(hic.pdsi.moving)
  print(traceplot(hic.prec.moving))
  #plot(skills(hic.pdsi.moving))
  dev.off()
  write.csv(hic.prec.moving, paste0('outputs/correlations/moving_site_cors/PREC_', site.name,'dynamic.csv'))
  
  
  #PDSI
  PDSI <- climate[,c('Year', 'Month', 'PDSI')]
  
  PDSI <- PDSI[1:1452,]
  
  pdf(paste0('outputs/correlations/moving_site_cors/PDSI_', site.name,'dynamic.pdf'))
  hic.pdsi.static <- dcc(chron, PDSI, dynamic = 'static', win_size = 35, win_offset = 5)
  
  print(plot(hic.pdsi.static))
  #g_test(hic.pdsi.moving)
  #traceplot(hic.pdsi.moving)
  #plot(skills(hic.pdsi.moving))
  
  hic.pdsi.moving <- dcc(chron, PDSI, dynamic = 'moving', win_size = 35, win_offset = 5)
  
  print(plot(hic.pdsi.moving))
  #g_test(hic.pdsi.moving)
  print(traceplot(hic.pdsi.moving))
  #plot(skills(hic.pdsi.moving))
  dev.off()
  
  write.csv(hic.pdsi.moving, paste0('outputs/correlations/moving_site_cors/PDSI_', site.name,'dynamic.csv'))
  
  #TAVG
  TAVG <- climate[,c('Year', 'Month', 'TAVG')]
  
  TAVG <- TAVG[1:1452,]
  
  pdf(paste0('outputs/correlations/moving_site_cors/TAVG_', site.name,'dynamic.pdf'))
  hic.pdsi.static <- dcc(chron, TAVG, dynamic = 'static', win_size = 35, win_offset = 5)
  
  print(plot(hic.pdsi.static))
  
  
  hic.pdsi.moving <- dcc(chron, TAVG, dynamic = 'moving', win_size = 35, win_offset = 5)
  
  print(plot(hic.tavg.moving))
  #g_test(hic.pdsi.moving)
  print(traceplot(hic.pdsi.moving))
  dev.off()
  write.csv(hic.tavg.moving, paste0('outputs/correlations/moving_site_cors/PDSI_', site.name,'dynamic.csv'))
  
  
  #TMAX
  TMAX <- climate[,c('Year', 'Month', 'TMAX')]
  
  TMAX <- TMAX[1:1452,]
  
  pdf(paste0('outputs/correlations/moving_site_cors/TMAX_', site.name,'dynamic.pdf'))
  hic.pdsi.static <- dcc(chron, TMAX, dynamic = 'static', win_size = 35, win_offset = 5)
  
  print(plot(hic.pdsi.static))
  
  
  hic.pdsi.moving <- dcc(chron, TMAX, dynamic = 'moving', win_size = 35, win_offset = 5)
  
  print(plot(hic.pdsi.moving))
  #g_test(hic.pdsi.moving)
  print(traceplot(hic.pdsi.moving))
  #plot(skills(hic.pdsi.moving))
  dev.off()
  
  #TMIN
  TMIN <- climate[,c('Year', 'Month', 'TMIN')]
  
  TMIN <- TMIN[1:1452,]
  
  pdf(paste0('outputs/correlations/moving_site_cors/TMIN_', site.name,'dynamic.pdf'))
  hic.pdsi.static <- dcc(chron, TMIN, dynamic = 'static', win_size = 35, win_offset = 5)
  
  print(plot(hic.pdsi.static))
  
  
  hic.pdsi.moving <- dcc(chron, TMIN, dynamic = 'moving', win_size = 35, win_offset = 5)
  
  print(plot(hic.pdsi.moving))
  #g_test(hic.pdsi.moving)
  print(traceplot(hic.pdsi.moving))
  #plot(skills(hic.pdsi.moving))
  dev.off()
  dev.off()
}

#clim.cor(IL.clim, Hickory, 'Hickory_Grove_')
#clim.cor(MNwc.clim, Bonanza, 'Bonanza_Prairie_')
#clim.cor(MNwc.clim, Desoix, 'Desoix_')
#clim.cor(WIsc.clim, Pleasant, 'Pleasant_Valley_Conservancy_')
#clim.cor(MNec.clim, Townsend, 'Townsend_woods_')
#clim.cor(MNec.clim, StCroix, 'StCroix_savanna_')
#clim.cor(MNse.clim, Mound, 'Mound_prairie_')


# ------------------------What is the factor that affects growth-------------
# gam models:


gam1 <- gam(RWI~ s(TAVG, k = 6, by = year) +
              s(PCP, k = 6, by = year) + 
               site   + year,
            # random=list(Site=~1, PlotID=~1, TreeID=~1),
            data=det.age.clim.prism.df)

summary(gam1)$r.sq # R-squared
summary(gam1)$dev.expl # explained deviance
anova(gam1)
AIC(gam1)

# plot pred vs. obs:

preds <- predict(gam1, det.age.clim.prism.df)
plot(det.age.clim.prism.df$RWI, preds)

ggplot(det.age.clim.prism.df, aes(TAVG, RWI, color = site))+geom_point()+facet_wrap(~site)
ggplot(det.age.clim.prism.df, aes(PCP, RWI, color = site))+geom_point()+facet_wrap(~site)

# save climate + tree ring dfs detrened.age.clim.prism.df

write.csv(det.age.clim.prism.df, "outputs/data/Isotope_climate/detrened_age_rwi_PRISMclimate.df.csv", row.names = FALSE)
write.csv(det.age.clim.ghcn.df, "outputs/data/Isotope_climate/detrened_age_rwi_GHCNclimate.df.csv", row.names = FALSE)

# ------------------plot climate parameters vs growth for all the tree ring series

# this function plots a scatter plot of a climate param vs. growth (RWI)
# with two separate slopes for the "Modern" and the "Past" trees
plot.Modern.Past <- function(x, Climate, xlab, ylab){
  Site <- x[1,]$site
  if(length(unique(x$ageclass)) > 1){
  #create dummy variable
  x$group <- 0
  ifelse(x$ageclass %in% "Past", x$group <- 1, x$group <- 0)
  co2.low.yr <- x[x$year < 1950 & x$ageclass %in% 'Past',]
  co2.high.yr <- x[x$year >= 1950 & x$ageclass %in% 'Modern',]
  
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
    
    scale_color_manual(values=c('Past'="red",'Modern'="blue"), name = "Tree Age")+
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
      
      scale_color_manual(values=c('Modern'="blue",'Past'="red"), name = "Tree Age")+
      #xlim(-8, 8)+
      #ylim(0.5, 1.5) +
      theme_bw()+
      theme(text = element_text(size = 10), plot.title = element_text(hjust = 0.5))+
      ylab("RWI") +
      xlab( xlab ) +
      ggtitle(Site)
  }
  p
  #ggsave(filename = paste0('outputs/correlations/Modern_Past_jul_pdsi_',Site,".png"), plot = p, width = 5, height = 3.5 )
}

# make all the plots for ghcn data: outputs to outputs/correlations/fPaster
allModern.Past.plots.pdsi <- lapply(det.age.clim.ghcn, plot.Modern.Past, Climate = "PDSI",xlab = "PDSI", ylab = "RWI")

png(width = 10, height = 10, units = 'in', res = 300, "outputs/correlations/Modern_Past_pdsi_allsite.png")
  n <- length(allModern.Past.plots.pdsi)
  nCol <- floor(sqrt(n))
  do.call("grid.arrange", c(allModern.Past.plots.pdsi, ncol=3))
dev.off()



# lets look at July VPDmax:
# make all the plots for ghcn data: outputs to outputs/correlations/fPaster
allModern.Past.plots.julvpdmax <- lapply(det.age.clim.prism, plot.Modern.Past, Climate = "jul.VPDmax",xlab = "July VPDmax", ylab = "RWI")

png(width = 10, height = 10, units = 'in', res = 300, "outputs/correlations/Modern_Past_jul_VPDmax_allsite.png")
n <- length(allModern.Past.plots.julvpdmax)
nCol <- floor(sqrt(n))
do.call("grid.arrange", c(allModern.Past.plots.julvpdmax, ncol=3))
dev.off()

# looking at July moisture balance:
allModern.Past.plots.julBAL <- lapply(det.age.clim.prism, plot.Modern.Past, Climate = "jul.BAL",xlab = "July P - PET", ylab = "RWI")

png(width = 10, height = 10, units = 'in', res = 300, "outputs/correlations/Modern_Past_jul_BAL_allsite.png")
n <- length(allModern.Past.plots.julBAL)
nCol <- floor(sqrt(n))
do.call("grid.arrange", c(allModern.Past.plots.julBAL, ncol=3))
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
summary(lm(RWI~Jul.pdsi:dbhclass, data = det.age.clim.ghcn.df))
summary(lm(RWI~jul.VPDmax:dbhclass, data = det.age.clim.prism.df))
summary(lm(RWI~year, data = det.age.clim.df))
summary(lm(RWI~year:site, data = det.age.clim.df))

ggplot(det.age.clim.df, aes(x = year, y = RWI, color = site))+geom_point()+stat_smooth(method = "lm")


###################################################################
# Lets directly compare Past and Modern years with similar climates:
##################################################################

#df <- aggregate(Jul.pdsi~year, data = det.age.clim.ghcn.df[det.age.clim.ghcn.df$site %in% "HIC",], FUN = mean )
df <- aggregate(Jul.pdsi~year, data = det.age.clim.ghcn.df, FUN = mean )

df<- df[order(df$Jul.pdsi),]
df$order <- 1:length(df$Jul.pdsi)
df$deficit <- ifelse(df$Jul.pdsi < 0, "deficit" ,"surplus")

png(height = 4, width = 6, units = 'in', res = 300, "outputs/climate_25dry_percentile.png")
ggplot(df, aes(order,Jul.pdsi, fill = deficit))+geom_bar(stat = "identity", width = 0.75) + scale_fill_manual(values = c("red", "blue"))+ylab("July Drought")+xlab(" ")+theme_black(base_size = 25)+theme(legend.title = element_blank(), legend.position = "none", axis.text.x = element_blank(), axis.ticks.x = element_blank())+
  geom_vline(xintercept = 30, color = "grey", linetype = "dashed")+geom_vline(xintercept = 0, color = "grey", linetype = "dashed")
dev.off()

png(height = 4, width = 6, units = 'in', res = 300, "outputs/climate_75wet_percentile.png")
ggplot(df, aes(order, Jul.pdsi, fill = deficit))+geom_bar(stat = "identity", width = 0.75) + scale_fill_manual(values = c("red", "blue"))+ylab("July Drought")+xlab(" ")+theme_black(base_size = 25)+theme(legend.title = element_blank(), legend.position = "none", axis.text.x = element_blank(), axis.ticks.x = element_blank())+
  geom_vline(xintercept = 91, color = "grey", linetype = "dashed")+geom_vline(xintercept = 120, color = "grey", linetype = "dashed")
dev.off()

dry <- quantile(df$Jul.pdsi, 0.25) # value of the driest years
wet <- quantile(df$Jul.pdsi, 0.75) # value of the wettest years

pre.dry <- df[df$year < 1950 & df$Jul.pdsi <= dry,]
pre.dry$class <- "pre-1950"
pre.dry$climclass <- "Dry_0.25"
post.dry <- df[df$year >=1950 & df$Jul.pdsi <= dry,]
post.dry$class <- "post-1950"
post.dry$climclass <- "Dry_0.25"

pre.wet <- df[df$year < 1950 & df$Jul.pdsi >= wet,]
pre.wet$class <- "pre-1950"
pre.wet$climclass <- "Wet_0.25"
post.wet <- df[df$year >=1950 & df$Jul.pdsi >= wet,]
post.wet$class <- "post-1950"
post.wet$climclass <- "Wet_0.25"

similar.clims <- rbind(post.wet, pre.wet, pre.dry, post.dry)

dfs <- det.age.clim.ghcn.df[det.age.clim.ghcn.df$site %in% "HIC",]
dfs <- det.age.clim.ghcn.df
sim.df <- merge(dfs, similar.clims[,c("year", "class", "climclass")], by = c('year'))

ggplot(sim.df, aes(Jul.pdsi, RWI))+geom_point()+facet_wrap(~class + climclass)+stat_smooth(method = "lm")

#sign relationship with PDSI in wetter years:
summary(lm(Jul.pdsi ~ RWI, data = sim.df[sim.df$climclass %in% "Wet_0.25",]))
summary(lm(Jul.pdsi ~ RWI:class, data = sim.df[sim.df$climclass %in% "Wet_0.25",]))
# get an idea of the slopes both pre and post:
summary(lm(Jul.pdsi ~ RWI , data = sim.df[sim.df$climclass %in% "Wet_0.25" & sim.df$class %in% "pre-1950",]))
summary(lm(Jul.pdsi ~ RWI , data = sim.df[sim.df$climclass %in% "Wet_0.25" & sim.df$class %in% "post-1950",]))

#sign relationship with PDSI in drier years:
summary(lm(Jul.pdsi ~ RWI, data = sim.df[sim.df$climclass %in% "Dry_0.25",]))
summary(lm(Jul.pdsi ~ RWI , data = sim.df[sim.df$climclass %in% "Dry_0.25" & sim.df$class %in% "pre-1950",]))
summary(lm(Jul.pdsi ~ RWI , data = sim.df[sim.df$climclass %in% "Dry_0.25" & sim.df$class %in% "post-1950",]))




#---------------------Get Climate sensitivity for the two time periods in dry + wet------------------

# get bootstrapped estimates of climate sensitivity for dry years for wet years before and after 1950 :
get.clim.sens.by.dry <- function(df, model.func){
  
  df <- aggregate(Jul.pdsi~year , data = det.age.clim.ghcn.df, FUN = mean )
  
  dry <- quantile(df$Jul.pdsi, 0.25) # value of the driest years
  wet <- quantile(df$Jul.pdsi, 0.75) # value of the wettest years
  
  pre.dry <- df[df$year < 1950 & df$Jul.pdsi <= dry,]
  pre.dry$class <- "Pre-1950"
  pre.dry$climclass <- "Dry_0.25"
  post.dry <- df[df$year >=1950 & df$Jul.pdsi <= dry,]
  post.dry$class <- "Post-1950"
  post.dry$climclass <- "Dry_0.25"
  
  pre.wet <- df[df$year < 1950 & df$Jul.pdsi >= wet,]
  pre.wet$class <- "Pre-1950"
  pre.wet$climclass <- "Wet_0.25"
  post.wet <- df[df$year >=1950 & df$Jul.pdsi >= wet,]
  post.wet$class <- "Post-1950"
  post.wet$climclass <- "Wet_0.25"
  
  similar.clims <- rbind(post.wet, pre.wet, pre.dry, post.dry)
  
  #dfs <- det.age.clim.ghcn.df[det.age.clim.ghcn.df$site %in% "HIC",]
  dfs <- det.age.clim.ghcn.df
  sim.df <- merge(dfs, similar.clims[,c("year", "class", "climclass")], by = c('year'))
  
  sim.df <- sim.df[sim.df$climclass %in% "Dry_0.25",]
  coeffs <- matrix ( 0, length(unique(sim.df$site))*2, 8 ) # set up matrix for coefficients
  yr <- 1895:1950
  yr.post <- 1950:2014
  df$class <- '9999'
  df[df$year %in% yr,]$class <- 'Pre-1950'
  df[df$year %in% yr.post,]$class <- 'Post-1950'
  
  for(s in 1:length(unique(sim.df$site))){
    name <- unique(sim.df$site)[s]  
    site.data <- na.omit(sim.df[sim.df$site == name ,])
    
    # function used in boot strapping below
    bs <- function(formula, data, indices) {
      d <- data[indices,] # allows boot to select sample 
      fit <- lm(formula, data=d)
      return(coef(fit)) 
    } 
    
    # for the "Post-1950" class:
    if(nrow(site.data[ site.data$class %in% "Post-1950" ,]) > 0){
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
    if(nrow(site.data[ site.data$class %in% "Pre-1950" ,]) > 2){
      results <- boot(data=site.data[site.data$class == "Pre-1950" & site.data$year < 1950 ,], statistic=bs, R=2000, formula=model.func)
      
      int.cis <- boot.ci(boot.out = results, type = "norm", index = 1)# intercept 
      slope.cis <- boot.ci(boot.out = results, type = "norm", index = 2)
      coeffs[s+length(unique(sim.df$site)),3:4] <- results$t0
      coeffs[s+length(unique(sim.df$site)) , 1] <- name
      coeffs[s+length(unique(sim.df$site)),2] <- "Pre-1950"
      coeffs[s+length(unique(sim.df$site)),5] <- as.data.frame(int.cis$normal)$V2
      coeffs[s+length(unique(sim.df$site)),6] <- as.data.frame(int.cis$normal)$V3
      coeffs[s+length(unique(sim.df$site)),7] <- as.data.frame(slope.cis$normal)$V2
      coeffs[s+length(unique(sim.df$site)),8] <- as.data.frame(slope.cis$normal)$V3
      
      
    }else{
      coeffs[s+length(unique(sim.df$site)),3:8] <- c(NA,NA)
      coeffs[s +length(unique(sim.df$site)), 2] <- "Pre-1950"
      coeffs[s+length(unique(sim.df$site)),1] <- name
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
  coeffs$climclass <- "Dry_0.25"
  coeffs
  
  
}
sens.jul.pdsi_dry0.25 <- get.clim.sens.by.dry(df = det.age.clim.ghcn.df, model.func = "RWI ~ Jul.pdsi")
sens.jja.pdsi_dry0.25 <- get.clim.sens.by.dry(df = det.age.clim.ghcn.df, model.func = "RWI ~ JJA.pdsi")

# get bootstrapped estimates of climate sensitivity for wet years before and after 1950:
get.clim.sens.by.wet <- function(df,climateclass, model.func){
  
  df <- aggregate(Jul.pdsi~year, data = det.age.clim.ghcn.df, FUN = mean )
  
  dry <- quantile(df$Jul.pdsi, 0.25) # value of the driest years
  wet <- quantile(df$Jul.pdsi, 0.75) # value of the wettest years
  
  pre.dry <- df[df$year < 1950 & df$Jul.pdsi <= dry,]
  pre.dry$class <- "Pre-1950"
  pre.dry$climclass <- "Dry_0.25"
  post.dry <- df[df$year >=1950 & df$Jul.pdsi <= dry,]
  post.dry$class <- "Post-1950"
  post.dry$climclass <- "Dry_0.25"
  
  pre.wet <- df[df$year < 1950 & df$Jul.pdsi >= wet,]
  pre.wet$class <- "Pre-1950"
  pre.wet$climclass <- "Wet_0.25"
  post.wet <- df[df$year >=1950 & df$Jul.pdsi >= wet,]
  post.wet$class <- "Post-1950"
  post.wet$climclass <- "Wet_0.25"
  
  similar.clims <- rbind(post.wet, pre.wet, pre.dry, post.dry)
  
  #dfs <- det.age.clim.ghcn.df[det.age.clim.ghcn.df$site %in% "HIC",]
  dfs <- det.age.clim.ghcn.df
  sim.df <- merge(dfs, similar.clims[,c("year", "class", "climclass")], by = c('year'))
  
  # only use wet years across the region:
  sim.df <- sim.df[sim.df$climclass %in% climateclass,]
  coeffs <- matrix ( 0, length(unique(sim.df$site))*2, 8 ) # set up matrix for coefficients
  yr <- 1895:1950
  yr.post <- 1950:2014
  df$class <- '9999'
  df[df$year %in% yr,]$class <- 'Pre-1950'
  df[df$year %in% yr.post,]$class <- 'Post-1950'
  
  for(s in 1:length(unique(sim.df$site))){
    name <- unique(sim.df$site)[s]  
    site.data <- na.omit(sim.df[sim.df$site == name ,])
    
    # function used in boot strapping below
    bs <- function(formula, data, indices) {
      d <- data[indices,] # allows boot to select sample 
      fit <- lm(formula, data=d)
      return(coef(fit)) 
    } 
    
    # for the "Post-1950" class:
    if(nrow(site.data[ site.data$class %in% "Post-1950" ,]) > 2){
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
    if(nrow(site.data[ site.data$class %in% "Pre-1950" ,]) > 2){
      results <- boot(data=site.data[site.data$class == "Pre-1950" & site.data$year < 1950 ,], statistic=bs, R=2000, formula=model.func)
      
      int.cis <- boot.ci(boot.out = results, type = "norm", index = 1)# intercept 
      slope.cis <- boot.ci(boot.out = results, type = "norm", index = 2)
      coeffs[s+length(unique(sim.df$site)),3:4] <- results$t0
      coeffs[s+length(unique(sim.df$site)) , 1] <- name
      coeffs[s+length(unique(sim.df$site)),2] <- "Pre-1950"
      coeffs[s+length(unique(sim.df$site)),5] <- as.data.frame(int.cis$normal)$V2
      coeffs[s+length(unique(sim.df$site)),6] <- as.data.frame(int.cis$normal)$V3
      coeffs[s+length(unique(sim.df$site)),7] <- as.data.frame(slope.cis$normal)$V2
      coeffs[s+length(unique(sim.df$site)),8] <- as.data.frame(slope.cis$normal)$V3
      
      
    }else{
      coeffs[s+length(unique(sim.df$site)),3:8] <- c(NA,NA)
      coeffs[s +length(unique(sim.df$site)), 2] <- "Pre-1950"
      coeffs[s+length(unique(sim.df$site)),1] <- name
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
  coeffs$climclass <- "Dry_0.25"
  coeffs
  
  
}
sens.jul.pdsi_wet0.25 <- get.clim.sens.by.dry(df = det.age.clim.ghcn.df, model.func = "RWI ~ Jul.pdsi")
sens.jja.pdsi_wet0.25 <- get.clim.sens.by.dry(df = det.age.clim.ghcn.df, model.func = "RWI ~ JJA.pdsi")


ggplot(sens.jul.pdsi_wet0.25, aes(site, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), size = 0.2, width = 0.5)
ggplot(sens.jul.pdsi_dry0.25, aes(site, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), size = 0.2, width = 0.5)

ggplot(sens.jja.pdsi_wet0.25, aes(site, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), size = 0.2, width = 0.5)

ggplot(sens.jja.pdsi_dry0.25, aes(site, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), size = 0.2, width = 0.5)


#---------------------Get Climate sensitivity for the modern and past in dry + wet------------------

# get bootstrapped estimates of climate sensitivy for dry years Modern and Past, before + after 1950
get.clim.sens.age.by.moisture <- function(df, climateclass ,model.func){
 
  coeffs <- matrix ( 0, length(unique(df$site))*2, 9 ) # set up matrix for coefficients
  yr <- 1895:1950
  yr.post <- 1950:2014
  
  
  df$class <- '9999'
  df[df$year %in% yr,]$class <- 'Pre-1950'
  df[df$year %in% yr.post,]$class <- 'Post-1950'
  coeffs <- matrix ( 0, length(unique(df$site))*2, 9 ) # set up matrix for coefficients
  
  
  for(s in 1: length(unique(df$site))){
    
    name <- unique(df$site)[s]  
    site.data <- na.omit(df[df$site == name ,])
    
    sim.df <- aggregate(Jul.pdsi~year, data = site.data, FUN = mean )
    
    dry <- quantile(sim.df$Jul.pdsi, 0.25) # value of the driest years
    wet <- quantile(sim.df$Jul.pdsi, 0.75) # value of the wettest years
    
    pre.dry <- sim.df[sim.df$year < 1950 & sim.df$Jul.pdsi <= dry,]
    pre.dry$class <- "Pre-1950"
    pre.dry$climclass <- "Dry_0.25"
    post.dry <- sim.df[sim.df$year >=1950 & sim.df$Jul.pdsi <= dry,]
    post.dry$class <- "Post-1950"
    post.dry$climclass <- "Dry_0.25"
    
    pre.wet <- sim.df[sim.df$year < 1950 & sim.df$Jul.pdsi >= wet,]
    pre.wet$class <- "Pre-1950"
    pre.wet$climclass <- "Wet_0.25"
    post.wet <- sim.df[sim.df$year >=1950 & sim.df$Jul.pdsi >= wet,]
    post.wet$class <- "Post-1950"
    post.wet$climclass <- "Wet_0.25"
    
    similar.clims <- rbind(post.wet, pre.wet, pre.dry, post.dry)
    
    #dfs <- det.age.clim.ghcn.df[det.age.clim.ghcn.df$site %in% "HIC",]
    
    sim.df <- merge(site.data, similar.clims[,c("year", "class", "climclass")], by = c('year', "class"))
    
    # save the similar climates as a csv so we can pick trees to sample:
    
    write.csv(sim.df, paste0("outputs/data/Isotope_climate/",name, "_wet_dry_climate_age_class.csv"))
    # only use wet years across the region:
    sim.df <- sim.df[sim.df$climclass %in% climateclass,]
    
    
    
    # function used in boot strapping below
    bs <- function(formula, data, indices) {
      d <- data[indices,] # allows boot to select sample 
      fit <- lm(formula, data=d)
      return(coef(fit)) 
    } 
    
    # for the "Modern" class:
    if(nrow(sim.df[sim.df$site == name & sim.df$ageclass == "Modern" ,]) > 0){
      # bootstrapping the linear regression model
      results <- boot(data=sim.df[sim.df$ageclass == "Modern" & sim.df$year >= 1950 ,], statistic=bs, R=2000, formula=model.func)
      
      int.cis <- boot.ci(boot.out = results, type = "norm", index = 1)# intercept 
      slope.cis <- boot.ci(boot.out = results, type = "norm", index = 2)
      coeffs[s,3:4] <- results$t0
      coeffs[s , 1] <- name
      coeffs[s,2] <- "Modern"
      coeffs[s,5] <- as.data.frame(int.cis$normal)$V2
      coeffs[s,6] <- as.data.frame(int.cis$normal)$V3
      coeffs[s,7] <- as.data.frame(slope.cis$normal)$V2
      coeffs[s,8] <- as.data.frame(slope.cis$normal)$V3
      
    } else{
      #lmest <- lm(RWI ~ PDSI, data = df[df$site == name & df$ageclass == "Modern" ,])
      coeffs[s,3:8] <- c(NA,NA)
      coeffs[s , 2] <- "Modern"
      coeffs[s,1] <- name
    }
    
    
    # for the "Past" class:  
    if(nrow(sim.df[sim.df$site == name & sim.df$ageclass == "Past" ,]) > 0){
      results <- boot(data=sim.df[sim.df$ageclass == "Past" & sim.df$year < 1950 ,], statistic=bs, R=2000, formula=model.func)
      
      int.cis <- boot.ci(boot.out = results, type = "norm", index = 1)# intercept 
      slope.cis <- boot.ci(boot.out = results, type = "norm", index = 2)
      coeffs[s+length(unique(df$site)),3:4] <- results$t0
      coeffs[s+length(unique(df$site)) , 1] <- name
      coeffs[s+length(unique(df$site)),2] <- "Past"
      coeffs[s+length(unique(df$site)),5] <- as.data.frame(int.cis$normal)$V2
      coeffs[s+length(unique(df$site)),6] <- as.data.frame(int.cis$normal)$V3
      coeffs[s+length(unique(df$site)),7] <- as.data.frame(slope.cis$normal)$V2
      coeffs[s+length(unique(df$site)),8] <- as.data.frame(slope.cis$normal)$V3
      
      
    }else{
      #lmest2 <- lm(RWI ~ PDSI, data = df[df$site == name & df$ageclass == "Past" ,])
      coeffs[s+length(unique(df$site)),3:8] <- c(NA,NA)
      coeffs[s +length(unique(df$site)), 2] <- "Modern"
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

sens.jul.pdsi.age_wet.25 <- get.clim.sens.age.by.moisture(df =det.age.clim.ghcn.df, climateclass = "Wet_0.25", model.func = "RWI ~ Jul.pdsi" )
sens.jja.pdsi.age_wet.25 <- get.clim.sens.age.by.moisture(df =det.age.clim.ghcn.df, climateclass = "Wet_0.25", model.func = "RWI ~ JJA.pdsi" )

# get bootstrapped estimates of climate sensitivy for wet years Modern and Past, before + after 1950
sens.jul.pdsi.age_dry.25 <- get.clim.sens.age.by.moisture(df =det.age.clim.ghcn.df, climateclass = "Dry_0.25", model.func = "RWI ~ Jul.pdsi" )
sens.jja.pdsi.age_dry.25 <- get.clim.sens.age.by.moisture(df =det.age.clim.ghcn.df, climateclass = "Dry_0.25", model.func = "RWI ~ JJA.pdsi" )


# plot slope estimates
ggplot(sens.jul.pdsi.age_wet.25, aes(site, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), size = 0.2, width = 0.5)
ggplot(sens.jja.pdsi.age_wet.25, aes(site, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), size = 0.2, width = 0.5)

ggplot(sens.jul.pdsi.age_dry.25, aes(age, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), size = 0.2, width = 0.5)+facet_wrap(~site)

ggplot(sens.jul.pdsi.age_dry.25, aes(site, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), size = 0.2, width = 0.5)
ggplot(sens.jja.pdsi.age_dry.25, aes(site, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), size = 0.2, width = 0.5)

# get bootstrapped differences between slopes:
slope.diff.boot<- function(df, climateclass ,model.func){
  
  coeffs <- matrix ( 0, length(unique(df$site))*2, 8 ) # set up matrix for coefficients
  yr <- 1895:1950
  yr.post <- 1950:2014
  
  
  df$class <- '9999'
  df[df$year %in% yr,]$class <- 'Pre-1950'
  df[df$year %in% yr.post,]$class <- 'Post-1950'
  coeffs <- matrix ( 0, length(unique(df$site))*2, 8 ) # set up matrix for coefficients
  
  
  for(s in 1: length(unique(df$site))){
    
    name <- unique(df$site)[s]  
    site.data<- na.omit(df[df$site == name ,])
    
    sim.df <- aggregate(Jul.pdsi~year, data = site.data, FUN = mean )
    
    dry <- quantile(sim.df$Jul.pdsi, 0.25) # value of the driest years
    wet <- quantile(sim.df$Jul.pdsi, 0.75) # value of the wettest years
    
    pre.dry <- sim.df[sim.df$year < 1950 & sim.df$Jul.pdsi <= dry,]
    pre.dry$class <- "Pre-1950"
    pre.dry$climclass <- "Dry_0.25"
    post.dry <- sim.df[sim.df$year >=1950 & sim.df$Jul.pdsi <= dry,]
    post.dry$class <- "Post-1950"
    post.dry$climclass <- "Dry_0.25"
    
    pre.wet <- sim.df[sim.df$year < 1950 & sim.df$Jul.pdsi >= wet,]
    pre.wet$class <- "Pre-1950"
    pre.wet$climclass <- "Wet_0.25"
    post.wet <- sim.df[sim.df$year >=1950 & sim.df$Jul.pdsi >= wet,]
    post.wet$class <- "Post-1950"
    post.wet$climclass <- "Wet_0.25"
    
    similar.clims <- rbind(post.wet, pre.wet, pre.dry, post.dry)
    
    #dfs <- det.age.clim.ghcn.df[det.age.clim.ghcn.df$site %in% "HIC",]
    
    sim.df <- merge(site.data, similar.clims[,c("year", "class", "climclass")], by = c('year', "class"))
    
    # only use wet years across the region:
    sim.df <- sim.df[sim.df$climclass %in% climateclass,]
    formula <- "Jul.pdsi ~ class/RWI -1"
    fit <- lm(formula, data=site.data)
    print(unique(site.data$site))
    print(summary(fit))
    
  }
    # function used in boot strapping below
    bs <- function(formula, data, indices) {
      d <- data[indices,] # allows boot to select sample 
      fit <- lm(formula, data=d)
      return(coef(fit)) 
    } 
    
    # for the "Modern" class:
    if(nrow(site.data[site.data$site == name & site.data$ageclass == "Modern" ,]) > 0){
      # bootstrapping the linear regression model
      results <- boot(data=site.data[site.data$ageclass == "Modern" & site.data$year >= 1950 ,], statistic=bs, R=2000, formula=model.func)
      
      results <- boot(data=site.data,statistic=bs, R=2000, formula=model.func)
      int.cis <- boot.ci(boot.out = results, type = "norm", index = 1)# intercept 
      slope.cis <- boot.ci(boot.out = results, type = "norm", index = 2)
      coeffs[s,3:4] <- results$t0
      coeffs[s , 1] <- name
      coeffs[s,2] <- "Modern"
      coeffs[s,5] <- as.data.frame(int.cis$normal)$V2
      coeffs[s,6] <- as.data.frame(int.cis$normal)$V3
      coeffs[s,7] <- as.data.frame(slope.cis$normal)$V2
      coeffs[s,8] <- as.data.frame(slope.cis$normal)$V3
      
    } else{
      #lmest <- lm(RWI ~ PDSI, data = df[df$site == name & df$ageclass == "Modern" ,])
      coeffs[s,3:8] <- c(NA,NA)
      coeffs[s , 2] <- "Modern"
      coeffs[s,1] <- name
    }
    
    
    # for the "Past" class:  
    if(nrow(site.data[site.data$site == name & site.data$ageclass == "Past" ,]) > 0){
      results <- boot(data=site.data[site.data$ageclass == "Past" & site.data$year < 1950 ,], statistic=bs, R=2000, formula=model.func)
      
      int.cis <- boot.ci(boot.out = results, type = "norm", index = 1)# intercept 
      slope.cis <- boot.ci(boot.out = results, type = "norm", index = 2)
      coeffs[s+length(unique(df$site)),3:4] <- results$t0
      coeffs[s+length(unique(df$site)) , 1] <- name
      coeffs[s+length(unique(df$site)),2] <- "Past"
      coeffs[s+length(unique(df$site)),5] <- as.data.frame(int.cis$normal)$V2
      coeffs[s+length(unique(df$site)),6] <- as.data.frame(int.cis$normal)$V3
      coeffs[s+length(unique(df$site)),7] <- as.data.frame(slope.cis$normal)$V2
      coeffs[s+length(unique(df$site)),8] <- as.data.frame(slope.cis$normal)$V3
      
      
    }else{
      #lmest2 <- lm(RWI ~ PDSI, data = df[df$site == name & df$ageclass == "Past" ,])
      coeffs[s+length(unique(df$site)),3:8] <- c(NA,NA)
      coeffs[s +length(unique(df$site)), 2] <- "Modern"
      coeffs[s+length(unique(df$site)),1] <- name
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


# get the bootstrapped correlations for wet years and dry years:
get.clim.cor.age.by.moisture <- function(df, climateclass,clim){
  yr <- 1895:1950
  yr.post <- 1950:2014
  
  
  df$class <- '9999'
  df[df$year %in% yr,]$class <- 'Pre-1950'
  df[df$year %in% yr.post,]$class <- 'Post-1950'
  coeffs <- matrix ( 0, length(unique(df$site))*2, 8 ) # set u
  
  coeffs <- matrix ( 0, length(unique(df$site))*2, 5 ) # set up matrix for coefficients
  
  # function used in boot strapping below
  boot.cor <- function(data, ind, colno ){
    
    return(cor(data[ind,c(colno)], data[ind,]$RWI, use = "pairwise.complete.obs"))
  }
  
  
  for(s in 1: length(unique(df$site))) {
    
    
    
    name <- unique(df$site)[s]  
    site.data<- na.omit(df[df$site == name ,])
    
    sim.df <- aggregate(Jul.pdsi~year, data = site.data, FUN = mean )
    
    dry <- quantile(sim.df$Jul.pdsi, 0.25) # value of the driest years
    wet <- quantile(sim.df$Jul.pdsi, 0.75) # value of the wettest years
    
    pre.dry <- sim.df[sim.df$year < 1950 & sim.df$Jul.pdsi <= dry,]
    pre.dry$class <- "Pre-1950"
    pre.dry$climclass <- "Dry_0.25"
    post.dry <- sim.df[sim.df$year >=1950 & sim.df$Jul.pdsi <= dry,]
    post.dry$class <- "Post-1950"
    post.dry$climclass <- "Dry_0.25"
    
    pre.wet <- sim.df[sim.df$year < 1950 & sim.df$Jul.pdsi >= wet,]
    pre.wet$class <- "Pre-1950"
    pre.wet$climclass <- "Wet_0.25"
    post.wet <- sim.df[sim.df$year >=1950 & sim.df$Jul.pdsi >= wet,]
    post.wet$class <- "Post-1950"
    post.wet$climclass <- "Wet_0.25"
    
    similar.clims <- rbind(post.wet, pre.wet, pre.dry, post.dry)
    
    #dfs <- det.age.clim.ghcn.df[det.age.clim.ghcn.df$site %in% "HIC",]
    
    sim.df <- merge(site.data, similar.clims[,c("year", "class", "climclass")], by = c('year'))
    
    # only use wet years across the region:
    sim.df <- sim.df[sim.df$climclass %in% climateclass,]
    
    site.data <- sim.df
    
    
    # for the "Modern" class:
    if(nrow(site.data[site.data$site == name & site.data$ageclass == "Modern" ,]) > 0){
      
      # bootstrapping the correlation coefficients:
      results <- boot(data=site.data[site.data$ageclass == "Modern" & site.data$year >= 1950 ,], colno = clim, statistic=boot.cor, R=2000)
      
      #int.cis <- boot.ci(boot.out = results, type = "norm", index = 1)# intercept 
      #slope.cis <- boot.ci(boot.out = results, type = "norm", index = 2)
      cis <- boot.ci(boot.out = results, type = "norm")
      ci.mo <- cis$normal[2:3]
      t <- results$t0
      
      coeffs[s,3] <-t
      coeffs[s , 1] <- name
      coeffs[s,2] <- "Modern"
      coeffs[s,4] <- ci.mo[1]
      coeffs[s,5] <- ci.mo[2]
      
      
    } else{
      #lmest <- lm(RWI ~ PDSI, data = df[df$site == name & df$ageclass == "Modern" ,])
      coeffs[s,3:5] <- c(NA,NA, NA)
      coeffs[s , 2] <- "Modern"
      coeffs[s,1] <- name
    }
    
    
    # for the "Past" class:  
    if(nrow(site.data[site.data$site == name & site.data$ageclass == "Past" ,]) > 0){
      results <- boot(data=site.data[site.data$ageclass == "Past" & site.data$year < 1950 ,], colno = clim, statistic=boot.cor, R=2000)
      
      # bootstrapping the correlation coefficients:
      
      #int.cis <- boot.ci(boot.out = results, type = "norm", index = 1)# intercept 
      #slope.cis <- boot.ci(boot.out = results, type = "norm", index = 2)
      cis <- boot.ci(boot.out = results, type = "norm")
      ci.mo <- cis$normal[2:3]
      t <- results$t0
      
      coeffs[s+length(unique(df$site)),3] <-t
      coeffs[s+length(unique(df$site)) , 1] <- name
      coeffs[s+length(unique(df$site)),2] <- "Past"
      coeffs[s+length(unique(df$site)),4] <- ci.mo[1]
      coeffs[s+length(unique(df$site)),5] <- ci.mo[2]
      
      
    }else{
      #lmest2 <- lm(RWI ~ PDSI, data = df[df$site == name & df$ageclass == "Past" ,])
      coeffs[s+length(unique(df$site)),3:5] <- c(NA,NA, NA)
      coeffs[s +length(unique(df$site)), 2] <- "Past"
      coeffs[s+length(unique(df$site)),1] <- name
    }
  }
  
  coeffs <- data.frame(coeffs)
  colnames(coeffs) <- c("site","age",'cor.est', "ci.min", "ci.max")
  coeffs$site <- as.character(coeffs$site)
  coeffs$cor.est <- as.numeric(as.character(coeffs$cor.est))
  
  coeffs$ci.min <- as.numeric(as.character(coeffs$ci.min))
  coeffs$ci.max <- as.numeric(as.character(coeffs$ci.max))
  #coeffs$slope.min <- as.numeric(as.character(coeffs$slope.min))
  #coeffs$slope.max <- as.numeric(as.character(coeffs$slope.max))
  coeffs
  
}

cor.jul.pdsi.age_dry.25 <- get.clim.cor.age.by.moisture(df =det.age.clim.ghcn.df, climateclass = "Dry_0.25", clim = "Jul.pdsi" )
cor.jul.pdsi.age_wet.25 <- get.clim.cor.age.by.moisture(df =det.age.clim.ghcn.df, climateclass = "Wet_0.25", clim = "Jul.pdsi" )

cor.jja.pdsi.age_dry.25 <- get.clim.cor.age.by.moisture(df =det.age.clim.ghcn.df, climateclass = "Dry_0.25", clim = "JJA.pdsi" )
cor.jja.pdsi.age_wet.25 <- get.clim.cor.age.by.moisture(df =det.age.clim.ghcn.df, climateclass = "Wet_0.25", clim = "JJA.pdsi" )

ggplot(cor.jul.pdsi.age_dry.25, aes(site, cor.est, color = age))+geom_point()+geom_errorbar(aes(ymin=ci.min, ymax = ci.max), size = 0.2, width = 0.5)
ggplot(cor.jja.pdsi.age_dry.25, aes(site, cor.est, color = age))+geom_point()+geom_errorbar(aes(ymin=ci.min, ymax = ci.max), size = 0.2, width = 0.5)

#-------------------Get correlation by age class, using each tree------------------------
# get a correlation for each tree:
get.clim.cor.age.by.moist.ID <- function(df, climateclass,clim){
  yr <- 1895:1950
  yr.post <- 1950:2014
  
  
  df$class <- '9999'
  df[df$year %in% yr,]$class <- 'Pre-1950'
  df[df$year %in% yr.post,]$class <- 'Post-1950'
  
  coeffs <- matrix ( 0, length(unique(df$ID))*2, 8 ) # set u
  
  coeffs <- matrix ( 0, length(unique(df$ID))*2, 6 ) # set up matrix for coefficients
  
  # function used in boot strapping below
  boot.cor <- function(data, ind, colno ){
    
    return(cor(data[ind,c(colno)], data[ind,]$RWI, use = "pairwise.complete.obs"))
  }
  
  
  for(s in 1: length(unique(df$ID))) {
    
    
    
    IDname <- unique(df$ID)[s]  
    site.data <- df[df$ID == IDname & !is.na(df$RWI),] # for cases where we have missing DBH, but not RWI
    name <- unique(site.data$site)
    
    sim.df <- aggregate(Jul.pdsi~year, data = site.data, FUN = mean )
    
    dry <- quantile(sim.df$Jul.pdsi, 0.25) # value of the driest years
    wet <- quantile(sim.df$Jul.pdsi, 0.75) # value of the wettest years
    sim.df$class <- ifelse(sim.df$year < 1950, "Pre-1950", "Post-1950" )
    sim.df$climclass <- ifelse(sim.df$Jul.pdsi <= dry, "Dry_0.25", 
                               ifelse(sim.df$Jul.pdsi >= wet,"Wet_0.25", "NA" ))
    
    pre.dry <- sim.df[sim.df$class %in% "Pre-1950" & sim.df$climclass %in% "Dry_0.25", ]
    post.dry <- sim.df[sim.df$class %in% "Post-1950" & sim.df$climclass %in% "Dry_0.25", ]
    pre.wet <- sim.df[sim.df$class %in% "Pre-1950" & sim.df$climclass %in% "Wet_0.25", ]
    post.wet <- sim.df[sim.df$class %in% "Post-1950" & sim.df$climclass %in% "Wet_0.25", ]
    
    
    similar.clims <- rbind(post.wet, pre.wet, pre.dry, post.dry)
    
    #dfs <- det.age.clim.ghcn.df[det.age.clim.ghcn.df$site %in% "HIC",]
    
    sim.df <- merge(site.data, similar.clims[,c("year", "class", "climclass")], by = c('year'))
    
    # only use wet years across the region:
    sim.df <- sim.df[sim.df$climclass %in% climateclass,]
    
    site.data <- sim.df
    
    
    # for the "Modern" class:
    if(nrow(site.data[site.data$ID == IDname & site.data$ageclass == "Modern" ,]) > 2){
      
      # bootstrapping the correlation coefficients:
      results <- boot(data=site.data[site.data$ageclass == "Modern" & site.data$year >= 1950 ,], colno = clim, statistic=boot.cor, R=2000)
      
      #int.cis <- boot.ci(boot.out = results, type = "norm", index = 1)# intercept 
      #slope.cis <- boot.ci(boot.out = results, type = "norm", index = 2)
      cis <- boot.ci(boot.out = results, type = "norm")
      ci.mo <- cis$normal[2:3]
      t <- results$t0
      
      coeffs[s,4] <-t
      coeffs[s , 1] <- name
      coeffs[s, 2] <- IDname
      coeffs[s,3] <- "Modern"
      coeffs[s,5] <- ci.mo[1]
      coeffs[s,6] <- ci.mo[2]
      
      
    } else{
      #lmest <- lm(RWI ~ PDSI, data = df[df$site == name & df$ageclass == "Modern" ,])
      coeffs[s,4:6] <- c(NA,NA, NA)
      coeffs[s , 3] <- "Modern"
      coeffs[s,1] <- name
      coeffs[s,2]<- IDname
    }
    
    
    # for the "Past" class:  
    if(nrow(site.data[site.data$ID == IDname & site.data$ageclass == "Past" ,]) > 2){
      results <- boot(data=site.data[site.data$ageclass == "Past" & site.data$year < 1950 ,], colno = clim, statistic=boot.cor, R=2000)
      
      # bootstrapping the correlation coefficients:
      
      #int.cis <- boot.ci(boot.out = results, type = "norm", index = 1)# intercept 
      #slope.cis <- boot.ci(boot.out = results, type = "norm", index = 2)
      cis <- boot.ci(boot.out = results, type = "norm")
      ci.mo <- cis$normal[2:3]
      t <- results$t0
      
      coeffs[s+length(unique(df$ID)),4] <-t
      coeffs[s+length(unique(df$ID)) , 1] <- name
      coeffs[s + length(unique(df$ID)), 2] <- IDname
      coeffs[s+length(unique(df$ID)),3] <- "Past"
      coeffs[s+length(unique(df$ID)),5] <- ci.mo[1]
      coeffs[s+length(unique(df$ID)),6] <- ci.mo[2]
      
      
    }else{
      #lmest2 <- lm(RWI ~ PDSI, data = df[df$site == name & df$ageclass == "Past" ,])
      coeffs[s+length(unique(df$ID)), 4:6] <- c(NA,NA, NA)
      coeffs[s +length(unique(df$ID)), 3] <- "Past"
      coeffs[s+length(unique(df$ID)), 2] <- IDname
      coeffs[s+length(unique(df$ID)), 1] <- name
    }
  }
  
  coeffs <- data.frame(coeffs)
  colnames(coeffs) <- c("site","ID","age",'cor.est', "ci.min", "ci.max")
  coeffs$site <- as.character(coeffs$site)
  coeffs$cor.est <- as.numeric(as.character(coeffs$cor.est))
  
  coeffs$ci.min <- as.numeric(as.character(coeffs$ci.min))
  coeffs$ci.max <- as.numeric(as.character(coeffs$ci.max))
  #coeffs$slope.min <- as.numeric(as.character(coeffs$slope.min))
  #coeffs$slope.max <- as.numeric(as.character(coeffs$slope.max))
  coeffs
  
}
cor.jul.pdsi.age_dry.25.id <- get.clim.cor.age.by.moist.ID(df = det.age.clim.ghcn.df, climateclass = "Dry_0.25", clim = "Jul.pdsi" )

ggplot(cor.jul.pdsi.age_dry.25.id, aes(age, cor.est, color = age))+geom_boxplot()+facet_wrap(~site)#+geom_errorbar(aes(ymin=ci.min, ymax = ci.max), size = 0.2, width = 0.5)

ggplot(cor.jul.pdsi.age_dry.25.id, aes(age, cor.est, color = age))+geom_boxplot()+facet_wrap(~site)#+geom_errorbar(aes(ymin=ci.min, ymax = ci.max), size = 0.2, width = 0.5)

# get a senstivity for each tree at each site:
get.clim.sens.age.by.moisture.ID <- function(df, climateclass ,model.func){
  
  coeffs <- matrix ( 0, length(unique(df$ID))*2, 9 ) # set up matrix for coefficients
  yr <- 1895:1950
  yr.post <- 1950:2014
  
  
  df$class <- '9999'
  df[df$year %in% yr,]$class <- 'Pre-1950'
  df[df$year %in% yr.post,]$class <- 'Post-1950'
  coeffs <- matrix ( 0, length(unique(df$ID))*2, 9 ) # set up matrix for coefficients
  
  
  for(s in 1: length(unique(df$ID))){
    IDname <- unique(df$ID)[s]  
    site.data<- df[df$ID == IDname & !is.na(df$RWI),] 
    name <- unique(site.data$site)
    
    sim.df <- aggregate(Jul.pdsi~year, data = site.data, FUN = mean )
    
    dry <- quantile(sim.df$Jul.pdsi, 0.25) # value of the driest years
    wet <- quantile(sim.df$Jul.pdsi, 0.75) # value of the wettest years
    sim.df$class <- ifelse(sim.df$year < 1950, "Pre-1950", "Post-1950" )
    sim.df$climclass <- ifelse(sim.df$Jul.pdsi <= dry, "Dry_0.25", 
                               ifelse(sim.df$Jul.pdsi >= wet,"Wet_0.25", "NA" ))
    
    pre.dry <- sim.df[sim.df$class %in% "Pre-1950" & sim.df$climclass %in% "Dry_0.25", ]
    post.dry <- sim.df[sim.df$class %in% "Post-1950" & sim.df$climclass %in% "Dry_0.25", ]
    pre.wet <- sim.df[sim.df$class %in% "Pre-1950" & sim.df$climclass %in% "Wet_0.25", ]
    post.wet <- sim.df[sim.df$class %in% "Post-1950" & sim.df$climclass %in% "Wet_0.25", ]
    
    
    similar.clims <- rbind(post.wet, pre.wet, pre.dry, post.dry)
    
    #dfs <- det.age.clim.ghcn.df[det.age.clim.ghcn.df$site %in% "HIC",]
    
    sim.df <- merge(site.data, similar.clims[,c("year", "class", "climclass")], by = c('year'))
    
    # only use dry or wet years across the region:
    sim.df <- sim.df[sim.df$climclass %in% climateclass,]
    
    
    # function used in boot strapping below
    bs <- function(formula, data, indices) {
      d <- data[indices,] # allows boot to select sample 
      fit <- lm(formula, data=d)
      return(coef(fit)) 
    } 
    
    # for the "Modern" class:
    if(nrow(sim.df[sim.df$ID == IDname & sim.df$ageclass == "Modern" ,]) > 1){
      # bootstrapping the linear regression model
      results <- boot(data=sim.df[sim.df$ageclass == "Modern" & sim.df$year >= 1950 ,], statistic=bs, R=2000, formula=model.func)
      
      int.cis <- boot.ci(boot.out = results, type = "norm", index = 1)# intercept 
      slope.cis <- boot.ci(boot.out = results, type = "norm", index = 2)
      coeffs[s,4:5] <- results$t0
      coeffs[s , 2] <- name
      coeffs[s , 1] <- IDname
      coeffs[s,3] <- "Modern"
      coeffs[s,6] <- as.data.frame(int.cis$normal)$V2
      coeffs[s,7] <- as.data.frame(int.cis$normal)$V3
      coeffs[s,8] <- as.data.frame(slope.cis$normal)$V2
      coeffs[s,9] <- as.data.frame(slope.cis$normal)$V3
      
    } else{
      #lmest <- lm(RWI ~ PDSI, data = df[df$site == name & df$ageclass == "Modern" ,])
      coeffs[s,4:9] <- c(NA,NA)
      coeffs[s , 3] <- "Modern"
      coeffs[s,2] <- name
      coeffs[s,1] <- IDname
    }
    
    
    # for the "Past" class:  
    if(nrow(sim.df[sim.df$ID == IDname & sim.df$ageclass == "Past" ,]) > 1){
      results <- boot(data=sim.df[sim.df$ageclass == "Past" & sim.df$year < 1950 ,], statistic=bs, R=2000, formula=model.func)
      
      int.cis <- boot.ci(boot.out = results, type = "norm", index = 1)# intercept 
      slope.cis <- boot.ci(boot.out = results, type = "norm", index = 2)
      coeffs[s+length(unique(df$ID)),4:5] <- results$t0
      coeffs[s+length(unique(df$ID)) , 2] <- name
      coeffs[s+length(unique(df$ID)) , 1] <- IDname
      coeffs[s+length(unique(df$ID)),3] <- "Past"
      coeffs[s+length(unique(df$ID)),6] <- as.data.frame(int.cis$normal)$V2
      coeffs[s+length(unique(df$ID)),7] <- as.data.frame(int.cis$normal)$V3
      coeffs[s+length(unique(df$ID)),8] <- as.data.frame(slope.cis$normal)$V2
      coeffs[s+length(unique(df$ID)),9] <- as.data.frame(slope.cis$normal)$V3
      
      
    }else{
      #lmest2 <- lm(RWI ~ PDSI, data = df[df$site == name & df$ageclass == "Past" ,])
      coeffs[s+length(unique(df$ID)),4:9] <- c(NA,NA)
      coeffs[s +length(unique(df$ID)), 3] <- "Past"
      coeffs[s+length(unique(df$ID)),2] <- name
      coeffs[s+length(unique(df$ID)),1] <- IDname
    }
  }
  
  coeffs <- data.frame(coeffs)
  colnames(coeffs) <- c("site","ID","age",'int.est', "slope.est", "int.min","int.max", "slope.min", "slope.max")
  coeffs$site <- as.character(coeffs$site)
  
  coeffs$slope.est <- as.numeric(as.character(coeffs$slope.est))
  coeffs$int.est <- as.numeric(as.character(coeffs$int.est))
  coeffs$int.min <- as.numeric(as.character(coeffs$int.min))
  coeffs$int.max <- as.numeric(as.character(coeffs$int.max))
  coeffs$slope.min <- as.numeric(as.character(coeffs$slope.min))
  coeffs$slope.max <- as.numeric(as.character(coeffs$slope.max))
  coeffs
  
}
jul.pdsi.age_dry.25.id <- get.clim.sens.age.by.moisture.ID(df =det.age.clim.ghcn.df, climateclass = "Dry_0.25", model.func = "RWI ~ Jul.pdsi" )
colnames(jul.pdsi.age_dry.25.id)[1:2] <- c("ID", "site")

jja.pdsi.age_dry.25.id <- get.clim.sens.age.by.moisture.ID(df =det.age.clim.ghcn.df, climateclass = "Dry_0.25", model.func = "RWI ~ JJA.pdsi" )
colnames(jja.pdsi.age_dry.25.id)[1:2] <- c("ID", "site")


ggplot(jul.pdsi.age_dry.25.id, aes(age, slope.est, color = age))+geom_boxplot()+facet_wrap(~ID)#+geom_errorbar(aes(ymin=ci.min, ymax = ci.max), size = 0.2, width = 0.5)

dfnona<- jul.pdsi.age_dry.25.id[complete.cases(jul.pdsi.age_dry.25.id),]
t.test(dfnona[dfnona$age %in% "Modern",]$slope.est, dfnona[dfnona$age %in% "Past",]$slope.est)

# aesthetics off with this:
ggplot(cor.jul.pdsi.age_dry.25, aes(site, cor.est, fill = age))+geom_bar(stat="identity", position = position_dodge(width = 0.9))#+geom_errorbar(data = cor.jul.pdsi.age_dry.25,aes(ymin=ci.min, ymax = ci.max,fill = age, position = position_dodge(width = 0.5)), size = 0.2, width = 0.5)
ggplot(cor.jul.pdsi.age_wet.25, aes(site, cor.est, color = age))+geom_point()+geom_errorbar(aes(ymin=ci.min, ymax = ci.max), size = 0.2, width = 0.5)

ggplot(cor.jja.pdsi.age_dry.25, aes(site, cor.est, fill = age))+geom_bar(stat="identity", position = position_dodge(width = 0.9))#+geom_errorbar(data = cor.jul.pdsi.age_dry.25,aes(ymin=ci.min, ymax = ci.max,fill = age, position = position_dodge(width = 0.5)), size = 0.2, width = 0.5)
#ggplot(cor.jja.pdsi.age_wet.25, aes(site, cor.est, color = age))+geom_point()+geom_errorbar(aes(ymin=ci.min, ymax = ci.max), size = 0.2, width = 0.5)


get.clim.cor.age.by.moisture.dbh <- function(df, climateclass, clim){
  yr <- 1895:1950
  yr.post <- 1950:2014
  
  
  df$class <- '9999'
  df[df$year %in% yr,]$class <- 'Pre-1950'
  df[df$year %in% yr.post,]$class <- 'Post-1950'
  
  
  
  # function used in boot strapping below
  boot.cor <- function(data, ind, colno ){
    
    return(cor(data[ind,c(colno)], data[ind,]$RWI, use = "pairwise.complete.obs"))
  }
  
  bydbh <- list()
  # for each dbh class, lets get the sensitivy to climate:
  for(d in 1:length(unique(df$dbhclass))){
    
    sizeclass <- unique(df$dbhclass)[d]
   
    coeffs <- matrix ( 0, length(unique(df$site))*2, 5 ) # set up matrix for coefficients
  
        for(s in 1: length(unique(df$site))) {
          
          
          
          name <- unique(df$site)[s]  
          site.data<- na.omit(df[df$site == name,])
          
          sim.df <- aggregate(Jul.pdsi~year, data = site.data, FUN = mean )
          
          dry <- quantile(sim.df$Jul.pdsi, 0.25) # value of the driest years
          wet <- quantile(sim.df$Jul.pdsi, 0.75) # value of the wettest years
          
          pre.dry <- sim.df[sim.df$year < 1950 & sim.df$Jul.pdsi <= dry,]
          pre.dry$class <- "Pre-1950"
          pre.dry$climclass <- "Dry_0.25"
          
          post.dry <- sim.df[sim.df$year >=1950 & sim.df$Jul.pdsi <= dry,]
          post.dry$class <- "Post-1950"
          post.dry$climclass <- "Dry_0.25"
          
          pre.wet <- sim.df[sim.df$year < 1950 & sim.df$Jul.pdsi >= wet,]
          pre.wet$class <- "Pre-1950"
          pre.wet$climclass <- "Wet_0.25"
          post.wet <- sim.df[sim.df$year >=1950 & sim.df$Jul.pdsi >= wet,]
          post.wet$class <- "Post-1950"
          post.wet$climclass <- "Wet_0.25"
          
          similar.clims <- rbind(post.wet, pre.wet, pre.dry, post.dry)
          
          #dfs <- det.age.clim.ghcn.df[det.age.clim.ghcn.df$site %in% "HIC",]
          
          sim.df <- merge(site.data, similar.clims[,c("year", "class", "climclass")], by = c('year'))
          
          # only use wet years across the region:
          sim.df <- sim.df[sim.df$climclass %in% climateclass & sim.df$dbhclass %in% sizeclass,]
          
          
          
          
          # for the "Modern" class:
          if(nrow(sim.df[sim.df$site == name & sim.df$ageclass == "Modern" ,]) > 1){
            
            # bootstrapping the correlation coefficients:
            results <- boot(data=sim.df[sim.df$ageclass == "Modern" & sim.df$year >= 1950 ,], colno = clim, statistic=boot.cor, R=2000)
            
            #int.cis <- boot.ci(boot.out = results, type = "norm", index = 1)# intercept 
            #slope.cis <- boot.ci(boot.out = results, type = "norm", index = 2)
            cis <- boot.ci(boot.out = results, type = "norm")
            ci.mo <- cis$normal[2:3]
            t <- results$t0
            
            coeffs[s,3] <-t
            coeffs[s , 1] <- name
            coeffs[s,2] <- "Modern"
            coeffs[s,4] <- ci.mo[1]
            coeffs[s,5] <- ci.mo[2]
            
            
          } else{
            #lmest <- lm(RWI ~ PDSI, data = df[df$site == name & df$ageclass == "Modern" ,])
            coeffs[s,3:5] <- c(NA,NA, NA)
            coeffs[s , 2] <- "Modern"
            coeffs[s,1] <- name
          }
          
          
          # for the "Past" class:  
          if(nrow(sim.df[sim.df$site == name & sim.df$ageclass == "Past" & sim.df$year < 1950 ,]) > 2){
            results <- boot(data=sim.df[sim.df$ageclass == "Past" & sim.df$year < 1950 ,], colno = clim, statistic=boot.cor, R=2000)
            
            # bootstrapping the correlation coefficients:
            
            #int.cis <- boot.ci(boot.out = results, type = "norm", index = 1)# intercept 
            #slope.cis <- boot.ci(boot.out = results, type = "norm", index = 2)
            cis <- boot.ci(boot.out = results, type = "norm")
            ci.mo <- cis$normal[2:3]
            t <- results$t0
            
            coeffs[s+length(unique(df$site)),3] <-t
            coeffs[s+length(unique(df$site)) , 1] <- name
            coeffs[s+length(unique(df$site)),2] <- "Past"
            coeffs[s+length(unique(df$site)),4] <- ci.mo[1]
            coeffs[s+length(unique(df$site)),5] <- ci.mo[2]
            
            
          }else{
            #lmest2 <- lm(RWI ~ PDSI, data = df[df$site == name & df$ageclass == "Past" ,])
            coeffs[s+length(unique(df$site)),3:5] <- c(NA,NA, NA)
            coeffs[s +length(unique(df$site)), 2] <- "Past"
            coeffs[s+length(unique(df$site)),1] <- name
          }
        }
        
        coeffs <- data.frame(coeffs)
        colnames(coeffs) <- c("site","age",'cor.est', "ci.min", "ci.max")
        coeffs$site <- as.character(coeffs$site)
        coeffs$cor.est <- as.numeric(as.character(coeffs$cor.est))
        
        coeffs$ci.min <- as.numeric(as.character(coeffs$ci.min))
        coeffs$ci.max <- as.numeric(as.character(coeffs$ci.max))
        #coeffs$slope.min <- as.numeric(as.character(coeffs$slope.min))
        #coeffs$slope.max <- as.numeric(as.character(coeffs$slope.max))
        coeffs$dbhclass <- sizeclass
        bydbh[[d]]<- coeffs
        
  }  
  names(bydbh) <- unique(df$dbhclass)
  bydbh.df <- do.call(rbind, bydbh) # make list into a dataframe to output!
  bydbh.df
}

cor.jul.pdsi.age_wet.25.dbh <- get.clim.cor.age.by.moisture.dbh(df =det.age.clim.ghcn.df[!det.age.clim.ghcn.df$site %in% "MOU",], climateclass = "Wet_0.25", clim = "Jul.pdsi" )
cor.jul.pdsi.age_dry.25.dbh <- get.clim.cor.age.by.moisture.dbh(df =det.age.clim.ghcn.df[!det.age.clim.ghcn.df$site %in% "MOU",], climateclass = "Dry_0.25", clim = "Jul.pdsi" )

cor.jja.pdsi.age_wet.25.dbh <- get.clim.cor.age.by.moisture.dbh(df =det.age.clim.ghcn.df[!det.age.clim.ghcn.df$site %in% "MOU",], climateclass = "Wet_0.25", clim = "JJA.pdsi" )
cor.jja.pdsi.age_dry.25.dbh <- get.clim.cor.age.by.moisture.dbh(df =det.age.clim.ghcn.df[!det.age.clim.ghcn.df$site %in% "MOU",], climateclass = "Dry_0.25", clim = "JJA.pdsi" )

ggplot(cor.jul.pdsi.age_wet.25.dbh, aes(site, cor.est, color = age))+geom_point()+geom_errorbar(aes(ymin=ci.min, ymax = ci.max), size = 0.2, width = 0.5)+facet_wrap(~dbhclass)
ggplot(cor.jul.pdsi.age_dry.25.dbh, aes(site, cor.est, color = age))+geom_point()+geom_errorbar(aes(ymin=ci.min, ymax = ci.max), size = 0.2, width = 0.5)+facet_wrap(~dbhclass)
ggplot(cor.jja.pdsi.age_dry.25.dbh, aes(site, cor.est, color = age))+geom_point()+geom_errorbar(aes(ymin=ci.min, ymax = ci.max), size = 0.2, width = 0.5)+facet_wrap(~dbhclass)




# july pdsi is signicantly increaseing
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
df <- test.ghcn.df
pdsi.sens <- get.clim.sensitivity(df = det.age.clim.ghcn.df, model.func = "RWI ~ PDSI")
Julpdsi.sens <- get.clim.sensitivity(df = det.age.clim.ghcn.df, model.func = "RWI ~ Jul.pdsi")
JJApdsi.sens <- get.clim.sensitivity(df = det.age.clim.ghcn.df, model.func = "RWI ~ JJA.pdsi")
TMIN.sens <- get.clim.sensitivity(df = det.age.clim.ghcn.df, model.func = "RWI ~ TMIN")
May.pr.sens <- get.clim.sensitivity(df = det.age.clim.ghcn.df, model.func = "RWI ~ MAY.p")


# make a plot with error bars
ggplot(pdsi.sens, aes(site, slope.est))+geom_bar(stat = "identity")+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), size = 0.2, width = 0.5)
ggplot(Julpdsi.sens, aes(site, slope.est))+geom_bar(stat = "identity")+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), size = 0.2, width = 0.5)
ggplot(JJApdsi.sens, aes(site, slope.est))+geom_bar(stat = "identity")+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), size = 0.2, width = 0.5)

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


# function to extract slopes for Modern an Past trees of lm(RWI~PDSI)
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
          
    # for the "Modern" class:
     if(nrow(site.data[site.data$site == name & site.data$ageclass == "Modern" ,]) > 0){
        # bootstrapping the linear regression model
        results <- boot(data=site.data[site.data$ageclass == "Modern" & site.data$year >= 1950 ,], statistic=bs, R=2000, formula=model.func)
    
        int.cis <- boot.ci(boot.out = results, type = "norm", index = 1)# intercept 
        slope.cis <- boot.ci(boot.out = results, type = "norm", index = 2)
        coeffs[s,3:4] <- results$t0
        coeffs[s , 1] <- name
        coeffs[s,2] <- "Modern"
        coeffs[s,5] <- as.data.frame(int.cis$normal)$V2
        coeffs[s,6] <- as.data.frame(int.cis$normal)$V3
        coeffs[s,7] <- as.data.frame(slope.cis$normal)$V2
        coeffs[s,8] <- as.data.frame(slope.cis$normal)$V3
     
    } else{
      #lmest <- lm(RWI ~ PDSI, data = df[df$site == name & df$ageclass == "Modern" ,])
      coeffs[s,3:8] <- c(NA,NA)
      coeffs[s , 2] <- "Modern"
      coeffs[s,1] <- name
    }
     
     
  # for the "Past" class:  
     if(nrow(site.data[site.data$site == name & site.data$ageclass == "Past" ,]) > 0){
       results <- boot(data=site.data[site.data$ageclass == "Past" & site.data$year < 1950 ,], statistic=bs, R=2000, formula=model.func)
       
       int.cis <- boot.ci(boot.out = results, type = "norm", index = 1)# intercept 
       slope.cis <- boot.ci(boot.out = results, type = "norm", index = 2)
       coeffs[s+length(unique(df$site)),3:4] <- results$t0
       coeffs[s+length(unique(df$site)) , 1] <- name
       coeffs[s+length(unique(df$site)),2] <- "Past"
       coeffs[s+length(unique(df$site)),5] <- as.data.frame(int.cis$normal)$V2
       coeffs[s+length(unique(df$site)),6] <- as.data.frame(int.cis$normal)$V3
       coeffs[s+length(unique(df$site)),7] <- as.data.frame(slope.cis$normal)$V2
       coeffs[s+length(unique(df$site)),8] <- as.data.frame(slope.cis$normal)$V3
       
       
     }else{
     #lmest2 <- lm(RWI ~ PDSI, data = df[df$site == name & df$ageclass == "Past" ,])
       coeffs[s+length(unique(df$site)),3:8] <- c(NA,NA)
       coeffs[s +length(unique(df$site)), 2] <- "Modern"
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


julpdsi.age.sens <- get.clim.sens.age(df = det.age.clim.ghcn.df, model.func = "RWI ~ Jul.pdsi")
jjapdsi.age.sens <- get.clim.sens.age(df = det.age.clim.ghcn.df, model.func = "RWI ~ JJA.pdsi")
pdsi.age.sens <- get.clim.sens.age(df = det.age.clim.ghcn.df, "RWI ~ PDSI")
jjap.age.sens <- get.clim.sens.age(df = det.age.clim.prism.df, "RWI ~ JJA.p")


# function to get the bootstrapped correlation coefficients across ages:
get.clim.cor.age <- function(df, clim){
  
  coeffs <- matrix ( 0, length(unique(df$site))*2, 5 ) # set up matrix for coefficients
  
  # function used in boot strapping below
  boot.cor <- function(data, ind, colno ){
    
    return(cor(data[ind,c(colno)], data[ind,]$RWI, use = "pairwise.complete.obs"))
  }
  
  
  for(s in 1: length(unique(df$site))) {
    
    name <- unique(df$site)[s]  
    site.data <- na.omit(df[df$site == name ,])
    
   
  
    
    # for the "Modern" class:
    if(nrow(site.data[site.data$site == name & site.data$ageclass == "Modern" ,]) > 0){
      
      # bootstrapping the correlation coefficients:
      results <- boot(data=site.data[site.data$ageclass == "Modern" & site.data$year >= 1950 ,], colno = clim, statistic=boot.cor, R=2000)
      
      #int.cis <- boot.ci(boot.out = results, type = "norm", index = 1)# intercept 
      #slope.cis <- boot.ci(boot.out = results, type = "norm", index = 2)
      cis <- boot.ci(boot.out = results, type = "norm")
      ci.mo <- cis$normal[2:3]
      t <- results$t0
     
      coeffs[s,3] <-t
      coeffs[s , 1] <- name
      coeffs[s,2] <- "Modern"
      coeffs[s,4] <- ci.mo[1]
      coeffs[s,5] <- ci.mo[2]
      
      
    } else{
      #lmest <- lm(RWI ~ PDSI, data = df[df$site == name & df$ageclass == "Modern" ,])
      coeffs[s,3:5] <- c(NA,NA, NA)
      coeffs[s , 2] <- "Modern"
      coeffs[s,1] <- name
    }
    
    
    # for the "Past" class:  
    if(nrow(site.data[site.data$site == name & site.data$ageclass == "Past" ,]) > 0){
      results <- boot(data=site.data[site.data$ageclass == "Past" & site.data$year < 1950 ,], colno = clim, statistic=boot.cor, R=2000)
      
      # bootstrapping the correlation coefficients:
      
      #int.cis <- boot.ci(boot.out = results, type = "norm", index = 1)# intercept 
      #slope.cis <- boot.ci(boot.out = results, type = "norm", index = 2)
      cis <- boot.ci(boot.out = results, type = "norm")
      ci.mo <- cis$normal[2:3]
      t <- results$t0
      
      coeffs[s+length(unique(df$site)),3] <-t
      coeffs[s+length(unique(df$site)) , 1] <- name
      coeffs[s+length(unique(df$site)),2] <- "Past"
      coeffs[s+length(unique(df$site)),4] <- ci.mo[1]
      coeffs[s+length(unique(df$site)),5] <- ci.mo[2]
      
      
    }else{
      #lmest2 <- lm(RWI ~ PDSI, data = df[df$site == name & df$ageclass == "Past" ,])
      coeffs[s+length(unique(df$site)),3:5] <- c(NA,NA, NA)
      coeffs[s +length(unique(df$site)), 2] <- "Past"
      coeffs[s+length(unique(df$site)),1] <- name
    }
}
  
  coeffs <- data.frame(coeffs)
  colnames(coeffs) <- c("site","age",'cor.est', "ci.min", "ci.max")
  coeffs$site <- as.character(coeffs$site)
  coeffs$cor.est <- as.numeric(as.character(coeffs$cor.est))
  
  coeffs$ci.min <- as.numeric(as.character(coeffs$ci.min))
  coeffs$ci.max <- as.numeric(as.character(coeffs$ci.max))
  #coeffs$slope.min <- as.numeric(as.character(coeffs$slope.min))
  #coeffs$slope.max <- as.numeric(as.character(coeffs$slope.max))
  coeffs
  
}

# for ghcn
age.pdsi.rf.df<- get.clim.cor.age(df = det.age.clim.ghcn.df, clim = "PDSI")
age.julpdsi.rf.df <- get.clim.cor.age(df = det.age.clim.ghcn.df, clim = "Jul.pdsi")
age.jjapdsi.rf.df <- get.clim.cor.age(df = det.age.clim.ghcn.df, clim = "JJA.pdsi")
age.pcp.rf.df <- get.clim.cor.age(df = det.age.clim.ghcn.df, clim = "PCP")

# for prism
age.vpdmax.rf.df <- get.clim.cor.age(df = det.age.clim.prism.df, clim = "VPDmax")
age.BAL.rf.df <- get.clim.cor.age(df = det.age.clim.prism.df, clim = "BAL")
age.Prismpcp.rf.df <- get.clim.cor.age(df = det.age.clim.prism.df, clim = "PCP")

age.julvpdmax.rf.df <- get.clim.cor.age(df = det.age.clim.prism.df, clim = "VPDmax")
age.julBAL.rf.df <- get.clim.cor.age(df = det.age.clim.prism.df, clim = "jul.BAL")
age.jjaPrismpcp.rf.df <- get.clim.cor.age(df = det.age.clim.prism.df, clim = "JJA.p")
age.jjaPrismpcp.rf.df <- get.clim.cor.age(df = det.age.clim.prism.df, clim = "JJA.p")


ggplot(age.julpdsi.rf.df, aes(site, cor.est, color = age))+geom_point()+geom_errorbar(aes(ymin = ci.min, ymax=ci.max))
ggplot(age.jjaPrismpcp.rf.df, aes(site, cor.est, color = age))+geom_point()+geom_errorbar(aes(ymin = ci.min, ymax=ci.max))

ggplot(age.julvpdmax.rf.df, aes(site, cor.est, color = age))+geom_point()+geom_errorbar(aes(ymin = ci.min, ymax=ci.max))


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
  
  pdsi.yr.sens <- get.clim.sens.year(df, "RWI ~ Jul.pdsi")


# ---------------------------read in soil, xy characteristics
  
locs <- read.csv("outputs/priority_sites_locs.csv")
locs$code <- as.character(locs$code)
locs[9:12,]$code <- c( "GLL1", "GLL2", "GLL3", "GLL4")
sites <- c("COR", "HIC", "STC", "GLA", "TOW", "ENG", "UNC", "BON", "MOU", "GLL4", "GLL3", "GLL2", "GLL1", "PVC", "AVO", "PLE", "UNI")

speciesdf<- data.frame(code = c("BON", "COR", "GLA", "GLL1", "GLL2", "GLL3", "GLL4",
                                "HIC", "MOU", "PLE", "PVC", "STC", "TOW", "UNC", "AVO", "ENG", "PLE", "UNI"),
                       species =  c( "QUMA", "QUAL", "QUAL/QUMA", "QUMA","QUMA", "QUMA","QUMA",
                                  "QUAL/QUMA", "QURA/QUVE", "QUAL/QUMA", "QUMA", "QUMA", "QURA", "QUMA", "QURA", "QURA", "QUAL", "QUAL"))



#---------------------------- merge plot summary data with the locs and species df:
locs <- merge(locs, speciesdf, by = "code")

workingdir <- "/Users/kah/Documents/bimodality/data/"

# read in and average prism data (this is modern 30year normals)
prism <- raster(paste0(workingdir,"PRISM_ppt_30yr_normal_4kmM2_all_bil/PRISM_ppt_30yr_normal_4kmM2_annual_bil.bil"))
prism.alb <- projectRaster(prism, crs='+init=epsg:3175')

locs$pr30yr <- raster::extract(prism.alb, locs[,c("coords.x1","coords.x2")])

workingdir <- "/Users/kah/Documents/bimodality/data/"

# read in and average prism temperature data (this is modern 30year normals)
prism.t <- raster(paste0(workingdir,'PRISM_tmean_30yr_normal_4kmM2_annual_bil/PRISM_tmean_30yr_normal_4kmM2_annual_bil.bil'))
prismt.alb <- projectRaster(prism.t, crs='+init=epsg:3175')

# extract temp
locs$tm30yr <- raster::extract(prismt.alb, locs[,c("coords.x1","coords.x2")])

workingdir <- "/Users/kah/Documents/TreeRings"

# read in the N & S deposition data:
#sdep.files <- list.files("data/total_Sdep/")
#ndep.files <- list.files("data/total_Ndep/")
#s.filenames <- paste0("data/total_Sdep/", sdep.files)
#s <- stack(s.filenames) 

#n.filenames <- paste0("data/total_Ndep/", ndep.files)
#n <- stack(n.filenames)
#plot(n[[2]])
#plot(mapdata, add = TRUE)
#projection(n) <- CRS('+init=epsg:4269')
#n.alb <- projectRaster(n,CRS('+init=epsg:3175'))

# -------------------------merge sensitivitites with the location/site information------------------
site.df <- merge(Julpdsi.sens, locs, by.x = 'site', by.y = 'code')
sens.df <- merge(pdsi.age.sens, locs, by = "site",by.y = 'code')
yr.sens.df <- merge(pdsi.yr.sens, locs, by = "site",by.y = 'code')
jja.sens.df <- merge(JJApdsi.sens, locs, by = "site",by.y = 'code')

#site.df <- merge(pdsi.sens, locs, by.x = 'site', by.y = 'code')
site.df.age <- merge(julpdsi.age.sens, locs, by.x = 'site', by.y = 'code')
jja.site.df.age <- merge(jjapdsi.age.sens, locs, by.x = 'site', by.y = 'code')
site.df.yr <- merge(pdsi.yr.sens, locs, by.x = 'site', by.y = 'code')

site.df.age.dry <- merge(sens.jul.pdsi.age_dry.25, locs, by.x = 'site', by.y = 'code')
site.df.age.wet <- merge(sens.jul.pdsi.age_wet.25, locs, by.x = 'site', by.y = 'code')

jja.site.df.age.dry <- merge(sens.jja.pdsi.age_dry.25, locs, by.x = 'site', by.y = 'code')
jja.site.df.age.wet <- merge(sens.jja.pdsi.age_wet.25, locs, by.x = 'site', by.y = 'code')


site.df.age.dry.id <- merge(jul.pdsi.age_dry.25.id, locs, by.x = "site", by.y= "code")
jja.site.df.age.dry.id <- merge(jja.pdsi.age_dry.25.id, locs, by.x = "site", by.y= "code")


# -----------------------------------map out sensitivities in space: -----------------------------
df_states <- map_data("state")
states <- subset(df_states, region %in% c(  "illinois", "minnesota", "wisconsin", "iowa", "south dakota",
                                             "north dakota", 'michigan', 'missouri', 'indiana') )
coordinates(states) <- ~long+lat
class(states)
proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
mapdata<-spTransform(states, CRS('+init=epsg:3175'))
mapdata.h<-spTransform(states, CRS('+proj=aea +lat_1=0 +lat_2=29.5 +lat_0=45.5 +lon_0=0 +x_0=0 +y_0=-96 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0'))
mapdata<-data.frame(mapdata)



png("outputs/maps/JJA.pdsi_sensitivity.png")
ggplot(jja.sens.df, aes(coords.x1, coords.x2, color = slope.est))+geom_point()+scale_color_gradient(low = "blue", high = "red")+ geom_polygon(data=data.frame(mapdata), aes(x=long, y=lat, group=group),
            colour = "darkgrey", fill = NA)+theme_bw() + coord_cartesian(xlim = c(-59495.64, 724000), ylim=c(68821.43, 1480021))
dev.off()

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

png(width = 6, height = 4, units = 'in', res = 300,"outputs/maps/JJA.pdsi_sensitivity_age.png")
ggplot(jja.site.df.age, aes(coords.x1, coords.x2, color = slope.est))+geom_point()+ geom_polygon(data=data.frame(mapdata), aes(x=long, y=lat, group=group),colour = "darkgrey", fill = NA)+theme_bw() +facet_wrap(~age)+scale_color_gradient(low = "blue", high = "red")+ coord_cartesian(xlim = c(-59495.64, 724000), ylim=c(68821.43, 1480021)) 
dev.off()                                                                                                                                      



cor.age.df <- merge(age.julpdsi.rf.df, site.df, by = "site")
#yr.sens.df <- merge(s, site.df, by = "site")

#---------------------------------------------------------------------------------------------------------------------
# how does July PDSI sensitivity to drought vary by climate, envtl factors?
#----------------------------------------------------------------------------------------------------------------------


# prelimnary plots sugges that higher precipitation and higher T places might be more sensitive to PDSI
ggplot(site.df[!site.df$site %in% "PVC",], aes(slope.max, slope.est, color = site))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max))
ggplot(site.df, aes(tm30yr, slope.est))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max))
ggplot(site.df[!site.df$site %in% "UNC",], aes(sand, slope.est))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max))
ggplot(site.df, aes(Description, slope.est))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max))

png(height = 4, width = 6, units = "in", res = 300, "outputs/sensitivity_v_site_DBH.png")
ggplot(site.df, aes(DBH, slope.est))+geom_point(color = "white")+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), color = 'white')+theme_black(base_size = 20)+ylab("Sensitivity to July PDSI")+xlab("Diameter at Breast Height (cm)")+stat_smooth(method = "lm", se = FALSE)
dev.off()


png(height = 4, width = 6, units = "in", res = 300, "outputs/sensitivity_v_site_MAP.png")
ggplot(site.df, aes(pr30yr, slope.est))+geom_point(color = "white")+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), color = 'white')+theme_black(base_size = 20)+ylab("Sensitivity to July PDSI")+xlab("Mean Annual Precipitation (mm)")+stat_smooth(method = "lm", se = FALSE)
dev.off()

png(height = 4, width = 6, units = "in", res = 300, "outputs/sensitivity_v_site_sand.png")
ggplot(site.df, aes(sand, slope.est))+geom_point(color = "white")+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), color = 'white')+theme_black(base_size = 20)+ylab("Sensitivity to July PDSI")+xlab("% sand ")+stat_smooth(method = "lm", se = FALSE)
dev.off()

summary(lm(slope.est~sand, data =site.df[!site.df$site %in% "UNC",]))
summary(lm(slope.est~pr30yr, data =site.df[!site.df$site %in% "UNC",]))
summary(lm(slope.est~DBH, data =site.df[!site.df$site %in% "UNC",]))

ggplot(site.df, aes( BA, slope.est))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max))

# fit a gam on the slope estimate
gam.sens <- mgcv::gam(slope.est ~ pr30yr  + DBH , data = site.df)
site.df$gam_ypred <- predict.gam(gam.sens, newdata = site.df)
sand <- lm(slope.est ~ pr30yr + DBH, data = site.df) # outside of UNCAS dusnes, sesnsitivyt depends on soil type
summary(gam.sens) # explains 58.7% of deviance:
summary(sand)

library(plot3D)
# predict 3d sensitivity:
plot3dsensitivity.all <- function(sens.df, age, class, add ){
  df <- sens.df[sens.df[,c(age)] == class,]
  df <- df[!is.na(df$slope.est),]
  # x, y, z variables
  x <- df$pr30yr
  y <- df$DBH
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
  scatter3D(x, y, z, pch = 18, cex = 2, colvar = z,
            theta = 50, phi = 35,  bty="u", lwd.panel= 2, space = 0.15,ticktype = "detailed",
            xlab = "\n\n\n\n Precip (mm/yr)", ylab = "\n\n\n\n DBH (cm)", zlab = "\n\n\n\n drought sensitivity", add= add ,
            surf = list(x = x.pred, y = y.pred, z = z.pred,  
                        facets = NA, fit = fitpoints), main = paste("Drought Sensitivity by climate"),
            zlim=c(0,0.06))
  
}
site.df$age <- "all"
png(height = 4, width = 7, units = 'in', res = 300, "outputs/full_pdsi_sens_3dplot.png")
plot3dsensitivity.all(site.df, "age", class = "all", add =FALSE)
dev.off()

ggplot(site.df, aes(gam_ypred, slope.est))+geom_point()

png('outputs/modeled_sensitivity_Jul_PDSI_age_DBH_climate.png')
ggplot(site.df, aes(gam_ypred, slope.est)) + geom_point(color = "white") + geom_abline(color = "red", linetype = "dashed")+theme_black(base_size = 20)+ylab("Observed Sensitivity to July PDSI")+xlab("Predicted Sensitivity to July PDSI")
dev.off()



#---------------------------------------------------------------------------------------------------------------------
# how does SUMMER (JJA) PDSI sensitivity to drought vary by climate, envtl factors?
#----------------------------------------------------------------------------------------------------------------------


# prelimnary plots sugges that higher precipitation and higher T places might be more sensitive to PDSI
ggplot(jja.sens.df, aes(slope.max, slope.est, color = site))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max))
ggplot(jja.sens.df[!jja.sens.df$site %in% "PLE",], aes(tm30yr, slope.est))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max))+stat_smooth(method = "lm")
ggplot(jja.sens.df[!jja.sens.df$site %in% "PLE",], aes(sand, slope.est))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max))+stat_smooth(method = "lm")
ggplot(jja.sens.df, aes(Description, slope.est))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max))

png(height = 4, width = 6, units = "in", res = 300, "outputs/JJA_pdsi_sensitivity_v_site_DBH.png")
ggplot(jja.sens.df[!jja.sens.df$site %in% "PLE",], aes(DBH, slope.est))+geom_point(color = "white")+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), color = 'white')+theme_black(base_size = 20)+ylab("Sensitivity to July PDSI")+xlab("Diameter at Breast Height (cm)")+stat_smooth(method = "lm", se = FALSE)
dev.off()


png(height = 4, width = 6, units = "in", res = 300, "outputs/JJA_pdsi_sensitivity_v_site_MAP.png")
ggplot(jja.sens.df[!jja.sens.df$site %in% c("PLE"),], aes(pr30yr, slope.est))+geom_point(color = "white")+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), color = 'white')+theme_black(base_size = 20)+ylab("Sensitivity to July PDSI")+xlab("Mean Annual Precipitation (mm)")+stat_smooth(method = "lm", se = FALSE)
dev.off()

png(height = 4, width = 6, units = "in", res = 300, "outputs/JJA_pdsi_sensitivity_v_site_sand.png")
ggplot(jja.sens.df[!jja.sens.df$site %in% c("PLE"),], aes(sand, slope.est))+geom_point(color = "white")+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), color = 'white')+theme_black(base_size = 20)+ylab("Sensitivity to July PDSI")+xlab("% sand ")+stat_smooth(method = "lm", se = FALSE)
dev.off()

summary(lm(slope.est~sand, data =jja.sens.df[!jja.sens.df$site %in% "UNC",]))
summary(lm(slope.est~pr30yr, data =jja.sens.df[!jja.sens.df$site %in% "UNC",]))
summary(lm(slope.est~DBH, data =jja.sens.df[!jja.sens.df$site %in% "UNC",]))
summary(lm(slope.est~BA, data =jja.sens.df[!jja.sens.df$site %in% "UNC",]))

ggplot(jja.sens.df, aes( BA, slope.est))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max))

# fit a gam on the slope estimate
gam.sens <- mgcv::gam(slope.est ~ pr30yr  + DBH , data = jja.sens.df)
jja.sens.df$gam_ypred <- predict.gam(gam.sens, newdata = jja.sens.df)
sand <- lm(slope.est ~ pr30yr + DBH, data = jja.sens.df) # outside of UNCAS dusnes, sesnsitivyt depends on soil type
summary(gam.sens) # explains 58.7% of deviance:
summary(sand)

library(plot3D)
# predict 3d sensitivity:
plot3dsensitivity.all <- function(sens.df, age, class, add ){
  df <- sens.df[sens.df[,c(age)] == class,]
  df <- df[!is.na(df$slope.est),]
  # x, y, z variables
  x <- df$pr30yr
  y <- df$DBH
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
  scatter3D(x, y, z, pch = 18, cex = 2, colvar = z,
            theta = 50, phi = 35,  bty="u", lwd.panel= 2, space = 0.15,ticktype = "detailed",
            xlab = "\n\n\n\n Precip (mm/yr)", ylab = "\n\n\n\n DBH (cm)", zlab = "\n\n\n\n drought sensitivity", add= add ,
            surf = list(x = x.pred, y = y.pred, z = z.pred,  
                        facets = NA, fit = fitpoints), main = paste("Drought Sensitivity by climate"),
            zlim=c(0,0.06))
  
}
jja.sens.df$age <- "all"
png(height = 4, width = 7, units = 'in', res = 300, "outputs/full_JJApdsi_sens_3dplot.png")
plot3dsensitivity.all(jja.sens.df, "age", class = "all", add =FALSE)
dev.off()

# this model doesnt fit very well
ggplot(jja.sens.df, aes(gam_ypred, slope.est))+geom_point()

png('outputs/modeled_sensitivity_JJA_PDSI_age_DBH_climate.png')
ggplot(jja.sens.df, aes(gam_ypred, slope.est)) + geom_point(color = "white") + geom_abline(color = "red", linetype = "dashed")+theme_black(base_size = 20)+ylab("Observed Sensitivity to July PDSI")+xlab("Predicted Sensitivity to July PDSI")
dev.off()


###########################################################################################
#  make plots for Modern and Past trees sensitivity to Jul PDSI
###########################################################################################

# prelimnary plots sugges that higher precipitation and higher T places might be more sensitive to PDSI (though this is NS)


# specify color for modern and past trees, and order factors
ageColors <- c( "#009E73", "#D55E00")
#ageColors <- c( "blue", "#D55E00")
site.df.age$age <- factor(site.df.age$age, levels = c("Past", "Modern"))
names(ageColors) <- levels(site.df.age$age)

site.df.age.dry$age <- factor(site.df.age.dry$age, levels = c("Past", "Modern"))
names(ageColors) <- levels(site.df.age.dry$age)

site.df.age.wet$age <- factor(site.df.age.wet$age, levels = c("Past", "Modern"))
names(ageColors) <- levels(site.df.age.wet$age)

site.df.age.dry.id$age <- factor(site.df.age.dry.id$age, levels = c("Past", "Modern"))
names(ageColors) <- levels(site.df.age.dry.id$age)



# plot box plot differences and run t tests on them

png(height = 6.5, width = 8, units = "in", res =300, "outputs/boxplot_Past_Modern_sens.png")
ggplot(site.df.age, aes(age, slope.est, fill = age))+geom_boxplot()+
  theme_black(base_size = 25)+scale_fill_manual(values = ageColors)+ylab("Growth Sensitivity to Drought (PDSI)")+theme(legend.title = element_blank(), legend.position = c(0.8,0.9))
dev.off()

# find differences in means of the site.df.ages
t.out<- t.test(site.df.age[site.df.age$age %in% "Past" & !site.df.age$site %in% "UNI",]$slope.est, site.df.age[site.df.age$age %in% "Modern" & !site.df.age$site %in% "UNI",]$slope.est )
round(t.out$p.value, digits = 5)

png(height = 6.5, width = 8, units = "in", res =300,"outputs/boxplot_Past_Modern_sens_dry_0.25.png")
ggplot(site.df.age.dry[!site.df.age.dry$site %in% "UNI",], aes(age, slope.est, fill = age))+geom_boxplot()+
  theme_black(base_size = 25)+scale_fill_manual(values = ageColors)+ylab("Growth Sensitivity to Drought (PDSI) \n in dry years")+theme(legend.title = element_blank(), legend.position = c(0.8,0.9))
dev.off()

t.outdry<- t.test(site.df.age.dry[site.df.age.dry$age %in% "Past" & !site.df.age$site %in% c("UNI","AVO"),]$slope.est, site.df.age.dry[site.df.age.dry$age %in% "Modern" & !site.df.age$site %in% c("UNI","AVO"),]$slope.est )
round(t.outdry$p.value, digits = 5)


png(height = 6.5, width = 8, units = "in", res =300,"outputs/boxplot_Past_Modern_sens_wet_0.25.png")
ggplot(site.df.age.wet, aes(age, slope.est, fill = age))+geom_boxplot()+
  theme_black(base_size = 25)+scale_fill_manual(values = ageColors)+ylab("Growth Sensitivity to Drought (PDSI) \n in dry years")+theme(legend.title = element_blank(), legend.position = c(0.8,0.9))
dev.off()

t.outwet <- t.test(site.df.age.wet[site.df.age.wet$age %in% "Past",]$slope.est, site.df.age.wet[site.df.age.wet$age %in% "Modern",]$slope.est )
round(t.outwet$p.value, digits = 5)


png(height = 6.5, width = 8, units = "in", res =300,"outputs/boxplot_Past_Modern_sens_dry_0.25_id.png")
ggplot(site.df.age.dry.id[ !site.df.age$site %in% "UNI",], aes(age, slope.est, fill = age))+geom_boxplot()+
  theme_black(base_size = 25)+scale_fill_manual(values = ageColors)+ylab("Growth Sensitivity to Drought (PDSI) \n in dry years")+theme(legend.title = element_blank(), legend.position = c(0.8,0.9))+ylim(0,0.15)
dev.off()

t.outdryid <- t.test(site.df.age.dry.id[site.df.age.dry.id$age %in% "Past" & !site.df.age$site %in% "UNI",]$slope.est, site.df.age.dry.id[site.df.age.dry.id$age %in% "Modern" & !site.df.age$site %in% "UNI",]$slope.est )
round(t.outdryid$p.value, digits = 5)


site.df.age.wet[site.df.age.wet$site %in% c("AVO", "ENG", "UNI"),]$Description <- "Forest"
site.df.age.dry[site.df.age.dry$site %in% c("AVO", "ENG", "UNI"),]$Description <- "Forest"
site.df.age.dry.id[site.df.age.dry.id$site %in% c("AVO", "ENG", "UNI"),]$Description <- "Forest"

site.df.age.dry[site.df.age.dry$site %in% "",]$Description <- "Forest"


site.df.age.wet[site.df.age.wet$species %in% "QUAL/QUMA",]$species <- "QUAL"
site.df.age.wet[site.df.age.wet$species %in% "QURA/QUVE",]$species <- "QURA"

site.df.age.dry[site.df.age.dry$species %in% "QUAL/QUMA",]$species <- "QUAL"
site.df.age.dry[site.df.age.dry$species %in% "QURA/QUVE",]$species <- "QURA"

site.df.age.dry.id[site.df.age.dry.id$species %in% "QUAL/QUMA",]$species <- "QUAL"
site.df.age.dry.id[site.df.age.dry.id$species %in% "QURA/QUVE",]$species <- "QURA"

png(height = 6.5, width = 9, units = "in", res =300,"outputs/boxplot_Past_Modern_sens_wet_0.25_by_stand_type.png")
ggplot(site.df.age.wet, aes(age, slope.est, fill = age))+geom_boxplot()+
  theme_black(base_size = 20)+scale_fill_manual(values = ageColors)+ylab("Growth Sensitivity to Drought (PDSI) \n in dry years")+theme(legend.title = element_blank())+facet_wrap(~Description)
dev.off()

t.outwetsav <- t.test(site.df.age.wet[site.df.age.wet$age %in% "Past" & site.df.age.wet$Description %in% "Savanna",]$slope.est, site.df.age.wet[site.df.age.wet$age %in% "Modern" & site.df.age.wet$Description %in% "Savanna",]$slope.est )
round(t.outwetsav$p.value, digits = 5)

t.outwetfor <- t.test(site.df.age.wet[site.df.age.wet$age %in% "Past" & site.df.age.wet$Description %in% "Forest",]$slope.est, site.df.age.wet[site.df.age.wet$age %in% "Modern" & site.df.age.wet$Description %in% "Forest",]$slope.est )
round(t.outwetfor$p.value, digits = 5)


png(height = 6.5, width = 9, units = "in", res =300,"outputs/boxplot_Past_Modern_sens_dry_0.25_by_stand_type.png")
ggplot(site.df.age.dry, aes(age, slope.est, fill = age))+geom_boxplot()+
  theme_black(base_size = 20)+scale_fill_manual(values = ageColors)+ylab("Growth Sensitivity to Drought (PDSI) \n in dry years")+theme(legend.title = element_blank())+facet_wrap(~Description)
dev.off()

t.outdrysav <- t.test(site.df.age.dry[site.df.age.dry$age %in% "Past" & site.df.age.dry$Description %in% "Savanna",]$slope.est, site.df.age.wet[site.df.age.dry$age %in% "Modern" & site.df.age.dry$Description %in% "Savanna",]$slope.est )
round(t.outdrysav$p.value, digits = 5)

t.outdryfor <- t.test(site.df.age.dry[site.df.age.wet$age %in% "Past" & site.df.age.dry$Description %in% "Forest",]$slope.est, site.df.age.dry[site.df.age.dry$age %in% "Modern" & site.df.age.wet$Description %in% "Forest",]$slope.est )
round(t.outdryfor$p.value, digits = 5)

# for the sensitivity estimated by id:
png(height = 6.5, width = 9, units = "in", res =300,"outputs/boxplot_Past_Modern_sens_dry_0.25_by_stand_type_id.png")
ggplot(site.df.age.dry.id, aes(age, slope.est, fill = age))+geom_boxplot()+
  theme_black(base_size = 20)+scale_fill_manual(values = ageColors)+ylab("Growth Sensitivity to Drought (PDSI) \n in dry years")+theme(legend.title = element_blank())+facet_wrap(~Description)
dev.off()

t.outdrysav.id <- t.test(site.df.age.dry.id[site.df.age.dry.id$age %in% "Past" & site.df.age.dry.id$Description %in% "Savanna" & !site.df.age.dry.id$site %in% "UNI",]$slope.est, site.df.age.dry.id[site.df.age.dry.id$age %in% "Modern" & site.df.age.dry.id$Description %in% "Savanna" & !site.df.age.dry.id$site %in% "UNI",]$slope.est )
round(t.outdrysav.id$p.value, digits = 5)

t.outdryfor.id <- t.test(site.df.age.dry.id[site.df.age.dry.id$age %in% "Past" & site.df.age.dry.id$Description %in% "Forest",]$slope.est, site.df.age.dry.id[site.df.age.dry.id$age %in% "Modern" & site.df.age.dry.id$Description %in% "Forest",]$slope.est )
round(t.outdryfor.id$p.value, digits = 5)

nonas <- site.df.age.dry.id[complete.cases(site.df.age.dry.id$slope.est),]
nonas %>% group_by(Description) %>% summarise(mean = mean(slope.est, na.rm = TRUE), n = n())
nonas %>% group_by( species) %>% summarise(mean = mean(slope.est, na.rm = TRUE), n = n())
site.slope.table <- nonas %>% group_by(site) %>% summarise(mean = mean(slope.est, na.rm = TRUE), n = n())
site.slope.table.age <- nonas %>% group_by(site, age) %>% summarise(mean = mean(slope.est, na.rm = TRUE), n = n())

write.csv(site.slope.table, "outputs/site_n_trees_slope_table.csv")
write.csv(site.slope.table.age, "outputs/site_n_trees_slope_table_age.csv")

#-------- for the stisitivty by species:
png(width = 12, height = 6, units = "in", res =300,"outputs/boxplot_Past_Modern_sens_dry_0.25_by_species.png")
ggplot(site.df.age.dry, aes(age, slope.est, fill = age))+geom_boxplot()+
  theme_black(base_size = 20)+scale_fill_manual(values = ageColors)+ylab("Growth Sensitivity to Drought (PDSI) \n in dry years")+theme(legend.title = element_blank())+facet_wrap(~species)
dev.off()

# tests for differences between species
t.outdrywhite <- t.test(site.df.age.dry[site.df.age.wet$age %in% "Past" & site.df.age.dry$species %in% "QUAL",]$slope.est, site.df.age.dry[site.df.age.dry$age %in% "Modern" & site.df.age.dry$species %in% "QUAL",]$slope.est )

t.outdryred <- t.test(site.df.age.dry[site.df.age.wet$age %in% "Past" & site.df.age.dry$species %in% "QURA",]$slope.est, site.df.age.dry[site.df.age.dry$age %in% "Modern" & site.df.age.dry$species %in% "QURA",]$slope.est )


t.outdrybur <- t.test(site.df.age.dry[site.df.age.wet$age %in% "Past" & site.df.age.dry$species %in% "QUMA",]$slope.est, site.df.age.dry[site.df.age.dry$age %in% "Modern" & site.df.age.dry$species %in% "QUMA",]$slope.est )
round(t.outdrybur$p.value, digits = 5)

appends<- data.frame(x = 0.75, y = 0.06 ,label = c( as.character(round(t.outdrywhite$p.value, digits = 5)), 
             as.character(round(t.outdrybur$p.value, digits = 5)), 
             as.character(round(t.outdryred$p.value, digits = 5))), 
             color = c("QUAL", "QUMA", "QURA"))

png(width = 12, height = 6, units = "in", res =300,"outputs/boxplot_Past_Modern_sens_dry_0.25_by_species.png")
ggplot(site.df.age.dry, aes(age, slope.est, fill = age))+geom_boxplot()+
  theme_black(base_size = 20)+
  scale_fill_manual(values = ageColors)+ylab("Growth Sensitivity to Drought (PDSI) \n in dry years")+theme(legend.title = element_blank())+facet_wrap(~species)#+geom_text(data=appends, 
                                                                                            #                                                                              aes(x,y,label=label), inherit.aes=FALSE)
dev.off()


png(width = 12, height = 6, units = "in", res =300,"outputs/boxplot_Past_Modern_sens_wet_0.25_by_species.png")
ggplot(site.df.age.wet, aes(age, slope.est, fill = age))+geom_boxplot()+
  theme_black(base_size = 20)+scale_fill_manual(values = ageColors)+ylab("Growth Sensitivity to Drought (PDSI) \n in dry years")+theme(legend.title = element_blank())+facet_wrap(~species)
dev.off()

# for sensitivy by species, estimated by tree ID:
png(width = 12, height = 6, units = "in", res =300,"outputs/boxplot_Past_Modern_sens_dry_0.25_by_species_ID.png")
ggplot(site.df.age.dry.id, aes(age, slope.est, fill = age))+geom_boxplot()+
  theme_black(base_size = 20)+scale_fill_manual(values = ageColors)+ylab("Growth Sensitivity to Drought (PDSI) \n in dry years")+theme(legend.title = element_blank())+facet_wrap(~species)+ylim(0, 0.15)
dev.off()

# tests for differences between species
t.outdrywhite <- t.test(site.df.age.dry.id[site.df.age.dry.id$age %in% "Past" & site.df.age.dry.id$species %in% "QUAL",]$slope.est, site.df.age.dry.id[site.df.age.dry.id$age %in% "Modern" & site.df.age.dry.id$species %in% "QUAL",]$slope.est )

t.outdryred <- t.test(site.df.age.dry.id[site.df.age.dry.id$age %in% "Past" & site.df.age.dry.id$species %in% "QURA",]$slope.est, site.df.age.dry.id[site.df.age.dry.id$age %in% "Modern" & site.df.age.dry.id$species %in% "QURA",]$slope.est )


t.outdrybur <- t.test(site.df.age.dry.id[site.df.age.dry.id$age %in% "Past" & site.df.age.dry.id$species %in% "QUMA",]$slope.est, site.df.age.dry.id[site.df.age.dry.id$age %in% "Modern" & site.df.age.dry.id$species %in% "QUMA",]$slope.est )
round(t.outdrybur$p.value, digits = 5)


site.df.age.dry.id$site <- factor(site.df.age.dry.id$site, levels = c("BON", "GLL1", "GLL2", "GLA", 
                                                                      "GLL3", "UNC", "MOU", "HIC", 
                                                                      "GLL4","TOW", "AVO", "ENG", 
                                                                      "COR","STC", "PVC", "PLE", "UNI"))
# plot by site--using the sensitibity estimated by id:
png(width = 12, height = 4.5, units = "in", res = 300,"outputs/boxplot_Past_Modern_sens_wet_0.25_by_site_id_first.png")
ggplot(site.df.age.dry.id[!site.df.age.dry.id$site %in% c("UNI", NA, "GLL4","TOW", "AVO", "ENG", 
                                                          "COR","STC", "PVC", "PLE"),], aes(age, slope.est, fill = age))+geom_boxplot()+
  theme_black(base_size = 20)+scale_fill_manual(values = ageColors)+ylab("Growth Sensitivity to Drought (PDSI) \n in dry years")+theme(legend.title = element_blank(),strip.text = element_text(face="bold", size=9),
                                                                                                                                       strip.background = element_rect( colour="black",size=0.01))+facet_wrap(~site, scales = "free_y", ncol = 4)#+ylim(-0.01, 0.15)
dev.off()

png(width = 12, height = 4.5, units = "in", res = 300,"outputs/boxplot_Past_Modern_sens_wet_0.25_by_site_id_second.png")
ggplot(site.df.age.dry.id[!site.df.age.dry.id$site %in% c("UNI", NA, "BON", "GLL1", "GLL2", "GLA", 
                                                          "GLL3", "UNC", "MOU", "HIC"),], aes(age, slope.est, fill = age))+geom_boxplot()+
  theme_black(base_size = 20)+scale_fill_manual(values = ageColors)+ylab("Growth Sensitivity to Drought (PDSI) \n in dry years")+theme(legend.title = element_blank(),strip.text = element_text(face="bold", size=9),
                                                                                                                                       strip.background = element_rect( colour="black",size=0.01))+facet_wrap(~site, scales = "free_y", ncol = 4)#+ylim(-0.01, 0.15)
dev.off()

pairs <- c(  'GLA' , 'GLL1', 'GLL2', 'GLL3',  'MOU' , 'UNC')
for(i in 1:length(pairs)){
  sitei <- unique(pairs)[i]
  testresults<- t.test(site.df.age.dry.id[site.df.age.dry.id$age %in% "Past" & site.df.age.dry.id$site %in% sitei,]$slope.est, site.df.age.dry.id[site.df.age.dry.id$age %in% "Modern" & site.df.age.dry.id$site %in% sitei,]$slope.est )
  print(sitei)
  print(testresults)
  }

png(width = 12, height = 6, units = "in", res = 300, "outputs/boxplot_Past_Modern_sens_wet_0.25_by_site.png")
ggplot(site.df.age.dry, aes(age, slope.est, fill = age))+geom_bar(stat = "identity", position = "dodge")+geom_errorbar(aes(ymin = slope.min, ymax = slope.max ), color = "grey", width = 0.2)+
  theme_black(base_size = 20)+scale_fill_manual(values = ageColors)+ylab("Growth Sensitivity to Drought (PDSI) \n in dry years")+theme(legend.title = element_blank())+facet_wrap(~site)
dev.off()

ggplot(cor.jul.pdsi.age_dry.25.dbh[cor.jul.pdsi.age_dry.25.dbh$dbhclass %in% c("< 20", "20 - 40", "40 - 60", "60 - 80"),], aes(age, cor.est, fill = age))+geom_bar(stat="identity")+
  theme_black(base_size = 20)+scale_fill_manual(values = ageColors)+ylab("Growth Sensitivity to Drought (PDSI) \n in dry years")+theme(legend.title = element_blank())+facet_wrap(~site)


png("outputs/boxplot_Past_Modern_sens_dry_0.25_bydbh_class.png")
ggplot(cor.jul.pdsi.age_dry.25.dbh[cor.jul.pdsi.age_dry.25.dbh$dbhclass %in% c("< 20", "20 - 40", "40 - 60", "60 - 80"),], aes(age, cor.est, fill = age))+geom_boxplot()+
  theme_black(base_size = 20)+scale_fill_manual(values = ageColors)+ylab("Growth Sensitivity to Drought (PDSI) \n in dry years")+theme(legend.title = element_blank())+facet_wrap(~dbhclass)
dev.off()


png("outputs/boxplot_Past_Modern_sens.png")
ggplot(sens.jul.pdsi.age_dry.25, aes(age, slope.est, fill = age))+geom_boxplot()+
  theme_black(base_size = 20)+scale_fill_manual(values = ageColors)+ylab("Growth Sensitivity to Drought (PDSI)")+theme(legend.title = element_blank())
dev.off()

ggplot(site.df.age.wet, aes(pr30yr, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), width = 0.5)
ggplot(site.df.age.dry, aes(pr30yr, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), width = 0.5)

ggplot(site.df.age, aes(pr30yr, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), width = 0.5)
ggplot(site.df.age, aes(tm30yr, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max))+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), width = 0.5)
ggplot(site.df.age, aes(sand, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max))+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), width = 0.5)+stat_smooth(method = "lm")
ggplot(site.df.age, aes(DBH, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max))+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), width = 0.5)+stat_smooth(method = "lm")
#ggplot(site.df.age, aes(CW_avg, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max))+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), width = 0.5)+stat_smooth(method = "lm")


png("outputs/sensitivity_v_siteDBH_age.png")
ggplot(site.df.age, aes(DBH, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), width = 0.5)+scale_color_manual(values = ageColors)+stat_smooth(method = 'lm', se = FALSE)+theme_black(base_size = 20)+ylab("Growth Sensitivity to Drought (PDSI)")+xlab("Site Avg DBH")+theme(legend.title = element_blank())
dev.off()

ggplot(site.df.age.wet, aes(DBH, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), width = 0.5)+scale_color_manual(values = ageColors)+stat_smooth(method = 'lm', se = FALSE)+theme_black(base_size = 20)+ylab("Growth Sensitivity to Drought (PDSI)")+xlab("Site Avg DBH")+theme(legend.title = element_blank())


summary(lm(slope.est ~DBH + pr30yr + age, data = site.df.age))
gam.pr.dbh <- gam(slope.est ~ pr30yr+ DBH + age,data = site.df.age)
summary(gam.pr.dbh)

site.df.age$ypred <- predict(gam.pr.dbh, site.df.age)
summary(site.df.age)

png('outputs/modeled_sensitivity_v_DBH_age.png')
ggplot(site.df.age, aes(ypred, slope.est)) + geom_point(color = "white") + geom_abline(color = "red", linetype = "dashed")+theme_black(base_size = 20)+ylab("Observed Sensitivity to July PDSI")+xlab("Predicted Sensitivity to July PDSI")
dev.off()

png("outputs/sensitivity_v_DBH_age.png")
ggplot(site.df.age, aes(DBH, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), width = 0.5)+scale_color_manual(values = ageColors)+stat_smooth(method = 'lm', se = FALSE)+theme_black(base_size = 20)+ylab("Growth Sensitivity to Drought (PDSI)")+xlab("% Sand")+theme(legend.title = element_blank())
dev.off()

png("outputs/sensitivity_v_sand_age.png")
ggplot(site.df.age, aes(sand, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), width = 0.5)+scale_color_manual(values = ageColors)+stat_smooth(method = 'lm', se = FALSE)+theme_black(base_size = 20)+ylab("Growth Sensitivity to Drought (PDSI)")+xlab("% Sand")+theme(legend.title = element_blank())
dev.off()

png("outputs/sensitivity_v_sand_age.png")
ggplot(site.df.age.dry, aes(sand, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), width = 0.5)+scale_color_manual(values = ageColors)+stat_smooth(method = 'lm', se = FALSE)+theme_black(base_size = 20)+ylab("Growth Sensitivity to Drought (PDSI)")+xlab("% Sand")+theme(legend.title = element_blank())+ylim(0,0.2)
dev.off()

png("outputs/sensitivity_v_sand_age_dry_years.png")
ggplot(site.df.age.wet, aes(sand, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), width = 0.5)+scale_color_manual(values = ageColors)+stat_smooth(method = 'lm', se = FALSE)+theme_black(base_size = 20)+ylab("Growth Sensitivity to Drought (PDSI)")+xlab("% Sand")+theme(legend.title = element_blank())
dev.off()

png("outputs/sensitivity_v_MAP_age.png")
ggplot(site.df.age, aes(pr30yr, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), width = 0.5)+scale_color_manual(values = ageColors)+stat_smooth(method = 'lm', se = FALSE)+theme_black(base_size = 20)+ylab("Growth Sensitivity to Drought (PDSI)")+xlab("Mean Annual Precipitation")+theme(legend.title = element_blank())
dev.off()

png("outputs/sensitivity_v_MAP_age_dry_years.png")
ggplot(site.df.age.dry, aes(pr30yr, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), width = 0.5)+scale_color_manual(values = ageColors)+stat_smooth(method = 'lm', se = FALSE)+theme_black(base_size = 20)+ylab("Growth Sensitivity to Drought (PDSI)")+xlab("Mean Annual Precipitation")+theme(legend.title = element_blank())
dev.off()

png("outputs/sensitivity_v_TMEAN_age.png")
ggplot(site.df.age, aes(tm30yr, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), width = 0.1)+scale_color_manual(values = ageColors)+stat_smooth(method = 'lm', se = FALSE)+theme_black(base_size = 20)+ylab("Growth Sensitivity to Drought (PDSI)")+xlab("Mean Monthly Temperature (DegC)")+theme(legend.title = element_blank())
dev.off()

#ggplot(cor.age.df[!cor.age.df$site %in% "UNC",], aes(sand, cor.est, color = age))+geom_point()+geom_errorbar(aes(ymin=ci.min, ymax = ci.max))+stat_smooth(method = "lm")

#sens.Modern <- gam(slope.est ~ pr30yr + tm30yr +sand  , data = site.df.age[site.df.age$age=="Modern",])
#summary(sens.Modern) # explains 47.7% of deviance:

#sens.Past <- gam(slope.est ~ pr30yr + tm30yr +sand  , data = sens.df[sens.df$age=="Past",])
#summary(sens.Past) # explains 90.5% of deviance:

##############################################################
# make prelimnary plots for pre- and post- 1950
###############################################################3
site.df.yr$age <- factor(site.df.yr$age,levels = rev(levels(site.df.yr$age)),ordered = TRUE)
yrColors <- c( "#009E73", "#D55E00")
names(yrColors) <- levels(site.df.yr$age)
#colScale <- scale_colour_manual(name = "grp",values = myColors)

png("outputs/boxplot_pre_post_sens.png")
ggplot(site.df.yr, aes(age, slope.est, fill = age))+geom_boxplot()+theme_black(base_size = 20)+scale_fill_manual(values = yrColors)+ylab("Growth Sensitivity to Drought (PDSI)")+theme(legend.title = element_blank())
dev.off()

ggplot(site.df.yr, aes(pr30yr, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), width = 0.5)
ggplot(site.df.yr, aes(tm30yr, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), width = 0.5)

png("outputs/sensitivity_v_sand_pre_post.png")
ggplot(site.df.yr, aes(sand, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), width = 0.5)+stat_smooth(method = 'lm', se = FALSE)+scale_color_manual(values = yrColors)+theme_black(base_size = 20)+ylab("Growth Sensitivity to Drought (PDSI)")+xlab("% Sand")+theme(legend.title = element_blank())
dev.off()

png("outputs/sensitivity_v_MAP_pre_post.png")
ggplot(site.df.yr, aes(pr30yr, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), width = 0.5)+stat_smooth(method = 'lm', se = FALSE)+scale_color_manual(values = yrColors)+theme_black(base_size = 20)+ylab("Growth Sensitivity to Drought (PDSI)")+xlab("Mean Annual Precipitation")+theme(legend.title = element_blank())
dev.off()

png("outputs/sensitivity_v_TMEAN_pre_post.png")
ggplot(site.df.yr, aes(tm30yr, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), width = 0.05)+stat_smooth(method = 'lm', se = FALSE)+scale_color_manual(values = yrColors)+theme_black(base_size = 20)+ylab("Growth Sensitivity to Drought (PDSI)")+xlab("Mean Monthly Temperature (DegC)")+theme(legend.title = element_blank())
dev.off()

png("outputs/sensitivity_v_DBH_pre_post.png")
ggplot(site.df.yr, aes(DBH, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), width = 0.05)+stat_smooth(method = 'lm', se = FALSE)+scale_color_manual(values = yrColors)+theme_black(base_size = 20)+ylab("Growth Sensitivity to Drought (PDSI)")+xlab("DBH (cm)")+theme(legend.title = element_blank())
dev.off()

ggplot(site.df.yr, aes(sand, pr30yr,color = slope.est, shape = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), width = 0.5) + ylim(500, 1000)

summary(lm(slope.est ~ sand + age  ,data = site.df.yr))
summary(lm(slope.est ~ sand + pr30yr + age ,data = site.df.age))
summary(lm(slope.est ~  pr30yr + age +DBH,data = site.df.age))

reformed.df <- dcast(site.df.age[c("site", "age", "coords.x1", "coords.x2", 'slope.est', "DBH" , "pr30yr", "tm30yr",'sand')], coords.x1 + coords.x2+site+DBH+pr30yr+tm30yr+sand ~ age, mean, na.rm=TRUE, value.var = 'slope.est') 
reformed.df$diff <- reformed.df$Past - reformed.df$Modern
sens.dif <- gam(Modern ~  pr30yr + DBH   , data = reformed.df)
summary(sens.dif) #Deviance explained = 41.1%

gam.sens.age <- gam(slope.est ~  pr30yr + DBH   , data = site.df.age)
summary(gam.sens.age)
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
  y <- df$DBH
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
          xlab = "\n\n\n\n Precip", ylab = "\n\n\n\n DBH (cm)", zlab = "\n\n\n\n drought sensitivity", add= add ,
          surf = list(x = x.pred, y = y.pred, z = z.pred,  
                      facets = NA, fit = fitpoints), main = paste("Drought Sensitivity by climate"),
          zlim=c(0,0.1))
 
}


# plot Past and Modern predictive surfaces on the smae plot
png(height = 5, width = 9, units = 'in', res= 300, 'outputs/sensitivity_surface3d_age.png')
plot3dsensitivity(site.df.age, "age","Past", "#009E73",FALSE)

plot3dsensitivity(site.df.age, "age","Modern", "#D55E00",TRUE)
legend(x = 0.5, y = 0 ,
       legend = c(expression(atop("Modern pre-1950", "(low CO"[2]*")")), expression(atop("Modern post-1950", "(high CO"[2]*")"))), 
       col = c("#009E73", 
               "#D55E00"), 
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
  
png(height = 5, width = 9, units = 'in', res= 300,'outputs/sensitivity_surface3d_pre_post_1950_precip_DBH.png')
#sens.df, age, class, col, add
plot3dsensitivity(sens.df = site.df.yr, age = "age",class = "Pre-1950", col = "#009E73",add = FALSE)
plot3dsensitivity(site.df.yr, "age","Post-1950", "#D55E00",TRUE)
legend(x = 0.5, y = 0 ,
       legend = c(expression(atop("All trees Pre-1950", "(low CO"[2]*")")), expression(atop("All trees Post-1950", "(high CO"[2]*")"))), 
       col = c("#009E73", 
               "#D55E00"), 
       pch = c(18, 18), 
       bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))

dev.off()


###########################################################################################
#  make plots for Modern and Past trees sensitivity to JJA PDSI
###########################################################################################

# prelimnary plots sugges that higher precipitation and higher T places might be more sensitive to PDSI (though this is NS)


# specify color for modern and past trees, and order factors
ageColors <- c( "#009E73", "#D55E00")

# for JJApdsi responses:
# specify color for modern and past trees, and order factors
ageColors <- c( "#009E73", "#D55E00")
#ageColors <- c( "blue", "#D55E00")
jja.site.df.age$age <- factor(jja.site.df.age$age, levels = c("Past", "Modern"))
names(ageColors) <- levels(jja.site.df.age$age)

jja.site.df.age.dry$age <- factor(jja.site.df.age.dry$age, levels = c("Past", "Modern"))
names(ageColors) <- levels(jja.site.df.age.dry$age)

jja.site.df.age.wet$age <- factor(jja.site.df.age.wet$age, levels = c("Past", "Modern"))
names(ageColors) <- levels(jja.site.df.age.wet$age)

jja.site.df.age.dry.id$age <- factor(jja.site.df.age.dry.id$age, levels = c("Past", "Modern"))
names(ageColors) <- levels(jja.site.df.age.dry.id$age)



# specify color for modern and past trees, and order factors
ageColors <- c( "#009E73", "#D55E00")
#ageColors <- c( "blue", "#D55E00")
jja.site.df.age$age <- factor(jja.site.df.age$age, levels = c("Past", "Modern"))
names(ageColors) <- levels(jja.site.df.age$age)

jja.site.df.age.dry$age <- factor(jja.site.df.age.dry$age, levels = c("Past", "Modern"))
names(ageColors) <- levels(jja.site.df.age.dry$age)

jja.site.df.age.wet$age <- factor(jja.site.df.age.wet$age, levels = c("Past", "Modern"))
names(ageColors) <- levels(jja.site.df.age.wet$age)

jja.site.df.age.dry.id$age <- factor(jja.site.df.age.dry.id$age, levels = c("Past", "Modern"))
names(ageColors) <- levels(jja.site.df.age.dry.id$age)



# plot box plot differences and run t tests on them

png(height = 6.5, width = 8, units = "in", res =300, "outputs/boxplot_Past_Modern_sens.png")
ggplot(jja.site.df.age[!jja.site.df.age$site %in% "PLE",], aes(age, slope.est, fill = age))+geom_boxplot()+
  theme_black(base_size = 25)+scale_fill_manual(values = ageColors)+ylab("Growth Sensitivity to Drought (PDSI)")+theme(legend.title = element_blank(), legend.position = c(0.8,0.9))
dev.off()

# find differences in means of the jja.site.df.ages
t.out<- t.test(jja.site.df.age[jja.site.df.age$age %in% "Past" & !jja.site.df.age$site %in% "PLE",]$slope.est, jja.site.df.age[jja.site.df.age$age %in% "Modern" & !jja.site.df.age$site %in% "PLE",]$slope.est )
round(t.out$p.value, digits = 5)

png(height = 6.5, width = 8, units = "in", res =300,"outputs/boxplot_Past_Modern_sens_dry_0.25.png")
ggplot(jja.site.df.age.dry[!jja.site.df.age.dry$site %in% "PLE",], aes(age, slope.est, fill = age))+geom_boxplot()+
  theme_black(base_size = 25)+scale_fill_manual(values = ageColors)+ylab("Growth Sensitivity to Drought (PDSI) \n in dry years")+theme(legend.title = element_blank(), legend.position = c(0.8,0.9))
dev.off()

t.outdry<- t.test(jja.site.df.age.dry[jja.site.df.age.dry$age %in% "Past" & !jja.site.df.age$site %in% c("PLE"),]$slope.est, jja.site.df.age.dry[jja.site.df.age.dry$age %in% "Modern" & !jja.site.df.age$site %in% c("PLE"),]$slope.est )
round(t.outdry$p.value, digits = 5)


png(height = 6.5, width = 8, units = "in", res =300,"outputs/boxplot_Past_Modern_sens_wet_0.25.png")
ggplot(jja.site.df.age.wet[!jja.site.df.age.wet$site %in% "PLE",], aes(age, slope.est, fill = age))+geom_boxplot()+
  theme_black(base_size = 25)+scale_fill_manual(values = ageColors)+ylab("Growth Sensitivity to Drought (PDSI) \n in dry years")+theme(legend.title = element_blank(), legend.position = c(0.8,0.9))
dev.off()

t.outwet <- t.test(jja.site.df.age.wet[jja.site.df.age.wet$age %in% "Past" & !jja.site.df.age.wet$site %in% "PLE",]$slope.est, jja.site.df.age.wet[jja.site.df.age.wet$age %in% "Modern" & !jja.site.df.age.wet$site %in% "PLE",]$slope.est )
round(t.outwet$p.value, digits = 5)


png(height = 6.5, width = 8, units = "in", res =300,"outputs/boxplot_Past_Modern_sens_dry_0.25_id.png")
ggplot(jja.site.df.age.dry.id[ !jja.site.df.age$site %in% "PLE",], aes(age, slope.est, fill = age))+geom_boxplot()+
  theme_black(base_size = 25)+scale_fill_manual(values = ageColors)+ylab("Growth Sensitivity to Drought (PDSI) \n in dry years")+theme(legend.title = element_blank(), legend.position = c(0.8,0.9))+ylim(0,0.15)
dev.off()

t.outdryid <- t.test(jja.site.df.age.dry.id[jja.site.df.age.dry.id$age %in% "Past" & !jja.site.df.age$site %in% "PLE",]$slope.est, jja.site.df.age.dry.id[jja.site.df.age.dry.id$age %in% "Modern" & !jja.site.df.age$site %in% "PLE",]$slope.est )
round(t.outdryid$p.value, digits = 5)

colnames(jja.site.df.age)[27] <- c("species")
colnames(jja.site.df.age.dry)[28] <- c("species")
colnames(jja.site.df.age.wet)[28] <- c("species")

colnames(jja.site.df.age.dry.id)[29] <- c("species")


jja.site.df.age.wet[jja.site.df.age.wet$site %in% c("AVO", "ENG", "UNI"),]$Description <- "Forest"
jja.site.df.age.dry[jja.site.df.age.dry$site %in% c("AVO", "ENG", "UNI"),]$Description <- "Forest"
jja.site.df.age.dry.id[jja.site.df.age.dry.id$site %in% c("AVO", "ENG", "UNI"),]$Description <- "Forest"

jja.site.df.age.dry[jja.site.df.age.dry$site %in% "",]$Description <- "Forest"


jja.site.df.age.wet[jja.site.df.age.wet$species %in% "QUAL/QUMA",]$species <- "QUAL"
jja.site.df.age.wet[jja.site.df.age.wet$species %in% "QURA/QUVE",]$species <- "QURA"

jja.site.df.age.dry[jja.site.df.age.dry$species %in% "QUAL/QUMA",]$species <- "QUAL"
jja.site.df.age.dry[jja.site.df.age.dry$species %in% "QURA/QUVE",]$species <- "QURA"

jja.site.df.age.dry.id[jja.site.df.age.dry.id$species %in% "QUAL/QUMA",]$species <- "QUAL"
jja.site.df.age.dry.id[jja.site.df.age.dry.id$species %in% "QURA/QUVE",]$species <- "QURA"

png(height = 6.5, width = 9, units = "in", res =300,"outputs/boxplot_Past_Modern_sens_wet_0.25_by_stand_type.png")
ggplot(jja.site.df.age.wet, aes(age, slope.est, fill = age))+geom_boxplot()+
  theme_black(base_size = 20)+scale_fill_manual(values = ageColors)+ylab("Growth Sensitivity to Drought (PDSI) \n in dry years")+theme(legend.title = element_blank())+facet_wrap(~Description)
dev.off()

t.outwetsav <- t.test(jja.site.df.age.wet[jja.site.df.age.wet$age %in% "Past" & jja.site.df.age.wet$Description %in% "Savanna",]$slope.est, jja.site.df.age.wet[jja.site.df.age.wet$age %in% "Modern" & jja.site.df.age.wet$Description %in% "Savanna",]$slope.est )
round(t.outwetsav$p.value, digits = 5)

t.outwetfor <- t.test(jja.site.df.age.wet[jja.site.df.age.wet$age %in% "Past" & jja.site.df.age.wet$Description %in% "Forest",]$slope.est, jja.site.df.age.wet[jja.site.df.age.wet$age %in% "Modern" & jja.site.df.age.wet$Description %in% "Forest",]$slope.est )
round(t.outwetfor$p.value, digits = 5)


png(height = 6.5, width = 9, units = "in", res =300,"outputs/boxplot_Past_Modern_sens_dry_0.25_by_stand_type.png")
ggplot(jja.site.df.age.dry[!jja.site.df.age.dry$site %in% "PLE",], aes(age, slope.est, fill = age))+geom_boxplot()+
  theme_black(base_size = 20)+scale_fill_manual(values = ageColors)+ylab("Growth Sensitivity to Drought (PDSI) \n in dry years")+theme(legend.title = element_blank())+facet_wrap(~Description)
dev.off()

t.outdrysav <- t.test(jja.site.df.age.dry[jja.site.df.age.dry$age %in% "Past" & jja.site.df.age.dry$Description %in% "Savanna" & !jja.site.df.age.dry.id$site %in% "PLE",]$slope.est, jja.site.df.age.wet[jja.site.df.age.dry$age %in% "Modern" & jja.site.df.age.dry$Description %in% "Savanna" & !jja.site.df.age.dry.id$site %in% "PLE",]$slope.est )
round(t.outdrysav$p.value, digits = 5)

t.outdryfor <- t.test(jja.site.df.age.dry[jja.site.df.age.wet$age %in% "Past" & jja.site.df.age.dry$Description %in% "Forest"& !jja.site.df.age.dry.id$site %in% "PLE",]$slope.est, jja.site.df.age.dry[jja.site.df.age.dry$age %in% "Modern" & jja.site.df.age.wet$Description %in% "Forest" & !jja.site.df.age.dry.id$site %in% "PLE",]$slope.est )
round(t.outdryfor$p.value, digits = 5)

# for the sensitivity estimated by id:
png(height = 6.5, width = 9, units = "in", res =300,"outputs/boxplot_Past_Modern_sens_dry_0.25_by_stand_type_id.png")
ggplot(jja.site.df.age.dry.id[!jja.site.df.age.dry.id$site %in% "PLE",], aes(age, slope.est, fill = age))+geom_boxplot()+
  theme_black(base_size = 20)+scale_fill_manual(values = ageColors)+ylab("Growth Sensitivity to Drought (PDSI) \n in dry years")+theme(legend.title = element_blank())+facet_wrap(~Description)
dev.off()

t.outdrysav.id <- t.test(jja.site.df.age.dry.id[jja.site.df.age.dry.id$age %in% "Past" & jja.site.df.age.dry.id$Description %in% "Savanna" & !jja.site.df.age.dry.id$site %in% "PLE",]$slope.est, jja.site.df.age.dry.id[jja.site.df.age.dry.id$age %in% "Modern" & jja.site.df.age.dry.id$Description %in% "Savanna" & !jja.site.df.age.dry.id$site %in% "PLE",]$slope.est )
round(t.outdrysav.id$p.value, digits = 5)

t.outdryfor.id <- t.test(jja.site.df.age.dry.id[jja.site.df.age.dry.id$age %in% "Past" & jja.site.df.age.dry.id$Description %in% "Forest",]$slope.est, jja.site.df.age.dry.id[jja.site.df.age.dry.id$age %in% "Modern" & jja.site.df.age.dry.id$Description %in% "Forest",]$slope.est )
round(t.outdryfor.id$p.value, digits = 5)

nonas <- jja.site.df.age.dry.id[complete.cases(jja.site.df.age.dry.id$slope.est),]
nonas %>% group_by(Description) %>% summarise(mean = mean(slope.est, na.rm = TRUE), n = n())
nonas %>% group_by( species) %>% summarise(mean = mean(slope.est, na.rm = TRUE), n = n())
site.slope.table <- nonas %>% group_by(site) %>% summarise(mean = mean(slope.est, na.rm = TRUE), n = n())
site.slope.table.age <- nonas %>% group_by(site, age) %>% summarise(mean = mean(slope.est, na.rm = TRUE), n = n())

write.csv(site.slope.table, "outputs/site_n_trees_slope_table.csv")
write.csv(site.slope.table.age, "outputs/site_n_trees_slope_table_age.csv")

#-------- for the stisitivty by species:
png(width = 12, height = 6, units = "in", res =300,"outputs/boxplot_Past_Modern_sens_dry_0.25_by_species.png")
ggplot(jja.site.df.age.dry, aes(age, slope.est, fill = age))+geom_boxplot()+
  theme_black(base_size = 20)+scale_fill_manual(values = ageColors)+ylab("Growth Sensitivity to Drought (PDSI) \n in dry years")+theme(legend.title = element_blank())+facet_wrap(~species)
dev.off()

# tests for differences between species
t.outdrywhite <- t.test(jja.site.df.age.dry[jja.site.df.age.wet$age %in% "Past" & jja.site.df.age.dry$species %in% "QUAL",]$slope.est, jja.site.df.age.dry[jja.site.df.age.dry$age %in% "Modern" & jja.site.df.age.dry$species %in% "QUAL",]$slope.est )

t.outdryred <- t.test(jja.site.df.age.dry[jja.site.df.age.wet$age %in% "Past" & jja.site.df.age.dry$species %in% "QURA",]$slope.est, jja.site.df.age.dry[jja.site.df.age.dry$age %in% "Modern" & jja.site.df.age.dry$species %in% "QURA",]$slope.est )


t.outdrybur <- t.test(jja.site.df.age.dry[jja.site.df.age.wet$age %in% "Past" & jja.site.df.age.dry$species %in% "QUMA",]$slope.est, jja.site.df.age.dry[jja.site.df.age.dry$age %in% "Modern" & jja.site.df.age.dry$species %in% "QUMA",]$slope.est )
round(t.outdrybur$p.value, digits = 5)

appends<- data.frame(x = 0.75, y = 0.06 ,label = c( as.character(round(t.outdrywhite$p.value, digits = 5)), 
                                                    as.character(round(t.outdrybur$p.value, digits = 5)), 
                                                    as.character(round(t.outdryred$p.value, digits = 5))), 
                     color = c("QUAL", "QUMA", "QURA"))

png(width = 12, height = 6, units = "in", res =300,"outputs/boxplot_Past_Modern_sens_dry_0.25_by_species.png")
ggplot(jja.site.df.age.dry, aes(age, slope.est, fill = age))+geom_boxplot()+
  theme_black(base_size = 20)+
  scale_fill_manual(values = ageColors)+ylab("Growth Sensitivity to Drought (PDSI) \n in dry years")+theme(legend.title = element_blank())+facet_wrap(~species)#+geom_text(data=appends, 
#                                                                              aes(x,y,label=label), inherit.aes=FALSE)
dev.off()


png(width = 12, height = 6, units = "in", res =300,"outputs/boxplot_Past_Modern_sens_wet_0.25_by_species.png")
ggplot(jja.site.df.age.wet, aes(age, slope.est, fill = age))+geom_boxplot()+
  theme_black(base_size = 20)+scale_fill_manual(values = ageColors)+ylab("Growth Sensitivity to Drought (PDSI) \n in dry years")+theme(legend.title = element_blank())+facet_wrap(~species)
dev.off()

# for sensitivy by species, estimated by tree ID:
png(width = 12, height = 6, units = "in", res =300,"outputs/JJA_pdsi_boxplot_Past_Modern_sens_dry_0.25_by_species_ID.png")
ggplot(jja.site.df.age.dry.id[!jja.site.df.age.dry.id$site %in% "PLE",], aes(age, slope.est, fill = age))+geom_boxplot()+
  theme_black(base_size = 20)+scale_fill_manual(values = ageColors)+ylab("Growth Sensitivity to Drought (PDSI) \n in dry years")+theme(legend.title = element_blank())+facet_wrap(~species)+ylim(0, 0.15)
dev.off()

# tests for differences between species
t.outdrywhite <- t.test(jja.site.df.age.dry.id[jja.site.df.age.dry.id$age %in% "Past" & jja.site.df.age.dry.id$species %in% "QUAL" & !jja.site.df.age.dry.id$site %in% "PLE",]$slope.est, jja.site.df.age.dry.id[jja.site.df.age.dry.id$age %in% "Modern" & jja.site.df.age.dry.id$species %in% "QUAL" & !jja.site.df.age.dry.id$site %in% "PLE",]$slope.est )

t.outdryred <- t.test(jja.site.df.age.dry.id[jja.site.df.age.dry.id$age %in% "Past" & jja.site.df.age.dry.id$species %in% "QURA" & !jja.site.df.age.dry.id$site %in% "PLE",]$slope.est, jja.site.df.age.dry.id[jja.site.df.age.dry.id$age %in% "Modern" & jja.site.df.age.dry.id$species %in% "QURA" & !jja.site.df.age.dry.id$site %in% "PLE",]$slope.est )


t.outdrybur <- t.test(jja.site.df.age.dry.id[jja.site.df.age.dry.id$age %in% "Past" & jja.site.df.age.dry.id$species %in% "QUMA" & !jja.site.df.age.dry.id$site %in% "PLE",]$slope.est, jja.site.df.age.dry.id[jja.site.df.age.dry.id$age %in% "Modern" & jja.site.df.age.dry.id$species %in% "QUMA" & !jja.site.df.age.dry.id$site %in% "PLE",]$slope.est )
round(t.outdrybur$p.value, digits = 5)


jja.site.df.age.dry.id$site <- factor(jja.site.df.age.dry.id$site, levels = c("BON", "GLL1", "GLL2", "GLA", 
                                                                      "GLL3", "UNC", "MOU", "HIC", 
                                                                      "GLL4","TOW", "AVO", "ENG", 
                                                                      "COR","STC", "PVC", "PLE", "UNI"))
# plot by site--using the sensitibity estimated by id:
png(width = 12, height = 4.5, units = "in", res = 300,"outputs/JJA_pdsi_boxplot_Past_Modern_sens_dry_0.25_by_site_id_first.png")
ggplot(jja.site.df.age.dry.id[!jja.site.df.age.dry.id$site %in% c("UNI", NA, "GLL4","TOW", "AVO", "ENG", 
                                                          "COR","STC", "PVC", "PLE"),], aes(age, slope.est, fill = age))+geom_boxplot()+
  theme_black(base_size = 20)+scale_fill_manual(values = ageColors)+ylab("Growth Sensitivity to Drought (PDSI) \n in dry years")+theme(legend.title = element_blank(),strip.text = element_text(face="bold", size=9),
                                                                                                                                       strip.background = element_rect( colour="black",size=0.01))+facet_wrap(~site, scales = "free_y", ncol = 4)#+ylim(-0.01, 0.15)
dev.off()

png(width = 12, height = 4.5, units = "in", res = 300,"outputs/JJA_pdsi_boxplot_Past_Modern_sens_dry_0.25_by_site_id_second.png")
ggplot(jja.site.df.age.dry.id[!jja.site.df.age.dry.id$site %in% c("UNI", NA, "BON", "GLL1", "GLL2", "GLA", 
                                                          "GLL3", "UNC", "MOU", "HIC"),], aes(age, slope.est, fill = age))+geom_boxplot()+
  theme_black(base_size = 20)+scale_fill_manual(values = ageColors)+ylab("Growth Sensitivity to Drought (PDSI) \n in dry years")+theme(legend.title = element_blank(),strip.text = element_text(face="bold", size=9),
                                                                                                                                       strip.background = element_rect( colour="black",size=0.01))+facet_wrap(~site, scales = "free_y", ncol = 4)#+ylim(-0.01, 0.15)
dev.off()

pairs <- c(  'GLA' , 'GLL1', 'GLL2', 'GLL3',  'MOU' , 'UNC')
for(i in 1:length(pairs)){
  sitei <- unique(pairs)[i]
  testresults<- t.test(jja.site.df.age.dry.id[jja.site.df.age.dry.id$age %in% "Past" & jja.site.df.age.dry.id$site %in% sitei,]$slope.est, jja.site.df.age.dry.id[jja.site.df.age.dry.id$age %in% "Modern" & jja.site.df.age.dry.id$site %in% sitei,]$slope.est )
  print(sitei)
  print(testresults)
}

png(width = 12, height = 6, units = "in", res = 300, "outputs/JJA_pdsi_boxplot_Past_Modern_sens_dry_0.25_by_site.png")
ggplot(jja.site.df.age.dry, aes(age, slope.est, fill = age))+geom_bar(stat = "identity", position = "dodge")+geom_errorbar(aes(ymin = slope.min, ymax = slope.max ), color = "grey", width = 0.2)+
  theme_black(base_size = 20)+scale_fill_manual(values = ageColors)+ylab("Growth Sensitivity to Drought (PDSI) \n in dry years")+theme(legend.title = element_blank())+facet_wrap(~site, scales = "free_y")
dev.off()

ggplot(cor.jul.pdsi.age_dry.25.dbh[cor.jul.pdsi.age_dry.25.dbh$dbhclass %in% c("< 20", "20 - 40", "40 - 60", "60 - 80"),], aes(age, cor.est, fill = age))+geom_bar(stat="identity")+
  theme_black(base_size = 20)+scale_fill_manual(values = ageColors)+ylab("Growth Sensitivity to Drought (PDSI) \n in dry years")+theme(legend.title = element_blank())+facet_wrap(~site)


png("outputs/JJA_pdsi_boxplot_Past_Modern_sens_dry_0.25_bydbh_class.png")
ggplot(cor.jul.pdsi.age_dry.25.dbh[cor.jul.pdsi.age_dry.25.dbh$dbhclass %in% c("< 20", "20 - 40", "40 - 60", "60 - 80"),], aes(age, cor.est, fill = age))+geom_boxplot()+
  theme_black(base_size = 20)+scale_fill_manual(values = ageColors)+ylab("Growth Sensitivity to Drought (PDSI) \n in dry years")+theme(legend.title = element_blank())+facet_wrap(~dbhclass)
dev.off()


png("outputs/JJA_pdsi_boxplot_Past_Modern_sens.png")
ggplot(sens.jul.pdsi.age_dry.25, aes(age, slope.est, fill = age))+geom_boxplot()+
  theme_black(base_size = 20)+scale_fill_manual(values = ageColors)+ylab("Growth Sensitivity to Drought (PDSI)")+theme(legend.title = element_blank())
dev.off()

ggplot(jja.site.df.age.wet[!jja.site.df.age.wet$site %in% c("PLE", "AVO", "ENG"),], aes(pr30yr, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), width = 0.5)
ggplot(jja.site.df.age.dry[!jja.site.df.age.dry$site %in% c( "PLE", "AVO" ,"ENG"),], aes(pr30yr, slope.est, color = site))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), width = 0.5)

ggplot(jja.site.df.age, aes(pr30yr, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), width = 0.5)
ggplot(jja.site.df.age, aes(tm30yr, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max))+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), width = 0.5)
ggplot(jja.site.df.age, aes(sand, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max))+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), width = 0.5)+stat_smooth(method = "lm")
ggplot(jja.site.df.age, aes(DBH, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max))+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), width = 0.5)+stat_smooth(method = "lm")
#ggplot(jja.site.df.age, aes(CW_avg, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max))+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), width = 0.5)+stat_smooth(method = "lm")


png("outputs/JJA_pdsi_sensitivity_v_siteDBH_age.png")
ggplot(jja.site.df.age, aes(DBH, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), width = 0.5)+scale_color_manual(values = ageColors)+stat_smooth(method = 'lm', se = FALSE)+theme_black(base_size = 20)+ylab("Growth Sensitivity to Drought (PDSI)")+xlab("Site Avg DBH")+theme(legend.title = element_blank())
dev.off()

ggplot(jja.site.df.age[!jja.site.df.age$site %in% c("AVO", "PLE"),], aes(awc, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), width = 0.005)+scale_color_manual(values = ageColors)+stat_smooth(method = 'lm', se = FALSE)+theme_black(base_size = 20)+ylab("Growth Sensitivity to Drought (PDSI)")+xlab("AWC")+theme(legend.title = element_blank())


ggplot(jja.sens.df[!jja.sens.df$site %in% c("AVO", "PLE"),], aes(pr30yr, slope.est))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), width = 0.005)+stat_smooth(method = 'lm', se = FALSE)+theme_black(base_size = 20)+ylab("Growth Sensitivity to Drought (PDSI)")+xlab("AWC")+theme(legend.title = element_blank())

summary(lm(slope.est ~ pr30yr, data = jja.sens.df[!jja.sens.df$site %in% c("AVO", "PLE") ,]))


ggplot(jja.site.df.age[!jja.site.df.age$site %in% c("AVO", "PLE"),], aes(coords.x1, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), width = 0.005)+scale_color_manual(values = ageColors)+stat_smooth(method = 'lm', se = FALSE)+theme_black(base_size = 20)+ylab("Growth Sensitivity to Drought (PDSI)")+xlab("AWC")+theme(legend.title = element_blank())

summary(lm(slope.est ~ pr30yr, data = jja.site.df.age[!jja.site.df.age$site %in% c("AVO", "PLE") & jja.site.df.age$age %in% "Past",]))

ggplot(jja.site.df.age.wet, aes(DBH, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), width = 0.5)+scale_color_manual(values = ageColors)+stat_smooth(method = 'lm', se = FALSE)+theme_black(base_size = 20)+ylab("Growth Sensitivity to Drought (PDSI)")+xlab("Site Avg DBH")+theme(legend.title = element_blank())


summary(lm(slope.est ~awc + sand + age, data = jja.site.df.age))
gam.pr.dbh <- gam(slope.est ~ pr30yr+ DBH + age,data = jja.site.df.age[!jja.site.df.age$site %in% c("AVO", "PLE"),])
summary(gam.pr.dbh)

jja.site.df.age$ypred <- predict(gam.pr.dbh, jja.site.df.age)
summary(jja.site.df.age)

#png('outputs/JJA_pdsi_modeled_sensitivity_v_DBH_age.png')
#ggplot(jja.site.df.age, aes(ypred, slope.est)) + geom_point(color = "white") + geom_abline(color = "red", linetype = "dashed")+theme_black(base_size = 20)+ylab("Observed Sensitivity to July PDSI")+xlab("Predicted Sensitivity to July PDSI")
#dev.off()

png("outputs/JJA_pdsi_sensitivity_v_DBH_age.png")
ggplot(jja.site.df.age[!jja.site.df.age$site %in% c("PLE"),], aes(DBH, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), width = 0.5)+scale_color_manual(values = ageColors)+stat_smooth(method = 'lm', se = FALSE)+theme_black(base_size = 20)+ylab("Growth Sensitivity to Drought (PDSI)")+xlab("Mean Diameter of Site")+theme(legend.title = element_blank())
dev.off()

png("outputs/JJA_pdsi_sensitivity_v_sand_age.png")
ggplot(jja.site.df.age[!jja.site.df.age$site %in% c("PLE"),] , aes(sand, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), width = 0.5)+scale_color_manual(values = ageColors)+stat_smooth(method = 'lm', se = FALSE)+theme_black(base_size = 20)+ylab("Growth Sensitivity to Drought (PDSI)")+xlab("% Sand")+theme(legend.title = element_blank())
dev.off()

png("outputs/JJA_pdsi_dry_yrs_sensitivity_v_sand_age.png")
ggplot(jja.site.df.age.dry, aes(sand, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), width = 0.5)+scale_color_manual(values = ageColors)+stat_smooth(method = 'lm', se = FALSE)+theme_black(base_size = 20)+ylab("Growth Sensitivity to Drought (PDSI)")+xlab("% Sand")+theme(legend.title = element_blank())+ylim(-0.5,0.5)
dev.off()

png("outputs/JJA_pdsi_sensitivity_v_sand_age_wet_years.png")
ggplot(jja.site.df.age.wet, aes(sand, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), width = 0.5)+scale_color_manual(values = ageColors)+stat_smooth(method = 'lm', se = FALSE)+theme_black(base_size = 20)+ylab("Growth Sensitivity to Drought (PDSI)")+xlab("% Sand")+theme(legend.title = element_blank())
dev.off()

png("outputs/JJA_pdsi_sensitivity_v_MAP_age.png")
ggplot(jja.site.df.age, aes(pr30yr, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), width = 0.5)+scale_color_manual(values = ageColors)+stat_smooth(method = 'lm', se = FALSE)+theme_black(base_size = 20)+ylab("Growth Sensitivity to Drought (PDSI)")+xlab("Mean Annual Precipitation")+theme(legend.title = element_blank())
dev.off()

png("outputs/JJA_pdsi_sensitivity_v_MAP_age_dry_years.png")
ggplot(jja.site.df.age.dry[!jja.site.df.age.dry$site %in% c("PLE","AVO", "ENG"),], aes(pr30yr, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), width = 0.5)+scale_color_manual(values = ageColors)+stat_smooth(method = 'lm', se = FALSE)+theme_black(base_size = 20)+ylab("Growth Sensitivity to Drought (PDSI)")+xlab("Mean Annual Precipitation")+theme(legend.title = element_blank())
dev.off()

png("outputs/JJA_pdsi_sensitivity_v_TMEAN_age.png")
ggplot(jja.site.df.age[!jja.site.df.age.dry$site %in% c("PLE","AVO", "ENG"),], aes(tm30yr, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), width = 0.1)+scale_color_manual(values = ageColors)+stat_smooth(method = 'lm', se = FALSE)+theme_black(base_size = 20)+ylab("Growth Sensitivity to Drought (PDSI)")+xlab("Mean Monthly Temperature (DegC)")+theme(legend.title = element_blank())
dev.off()


#ggplot(jja.site.df.age.dry.id[!jja.site.df.age.dry.id$site %in% c("PLE","AVO", "ENG"),], aes(clay, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), width = 0.1)+scale_color_manual(values = ageColors)+stat_smooth(method = 'lm', se = FALSE)+theme_black(base_size = 20)+ylab("Growth Sensitivity to Drought (PDSI)")+xlab("Mean Monthly Temperature (DegC)")+theme(legend.title = element_blank())

#ggplot(cor.age.df[!cor.age.df$site %in% "UNC",], aes(sand, cor.est, color = age))+geom_point()+geom_errorbar(aes(ymin=ci.min, ymax = ci.max))+stat_smooth(method = "lm")

#sens.Modern <- gam(slope.est ~ pr30yr + tm30yr +sand  , data = jja.site.df.age[jja.site.df.age$age=="Modern",])
#summary(sens.Modern) # explains 47.7% of deviance:

#sens.Past <- gam(slope.est ~ pr30yr + tm30yr +sand  , data = sens.df[sens.df$age=="Past",])
#summary(sens.Past) # explains 90.5% of deviance:

##############################################################
# make prelimnary plots for pre- and post- 1950
###############################################################3
jja.site.df.yr$age <- factor(jja.site.df.yr$age,levels = rev(levels(jja.site.df.yr$age)),ordered = TRUE)
yrColors <- c( "#009E73", "#D55E00")
names(yrColors) <- levels(jja.site.df.yr$age)
#colScale <- scale_colour_manual(name = "grp",values = myColors)

png("outputs/boxplot_pre_post_sens.png")
ggplot(jja.site.df.yr, aes(age, slope.est, fill = age))+geom_boxplot()+theme_black(base_size = 20)+scale_fill_manual(values = yrColors)+ylab("Growth Sensitivity to Drought (PDSI)")+theme(legend.title = element_blank())
dev.off()

ggplot(jja.site.df.yr, aes(pr30yr, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), width = 0.5)
ggplot(jja.site.df.yr, aes(tm30yr, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), width = 0.5)

png("outputs/sensitivity_v_sand_pre_post.png")
ggplot(jja.site.df.yr, aes(sand, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), width = 0.5)+stat_smooth(method = 'lm', se = FALSE)+scale_color_manual(values = yrColors)+theme_black(base_size = 20)+ylab("Growth Sensitivity to Drought (PDSI)")+xlab("% Sand")+theme(legend.title = element_blank())
dev.off()

png("outputs/sensitivity_v_MAP_pre_post.png")
ggplot(jja.site.df.yr, aes(pr30yr, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), width = 0.5)+stat_smooth(method = 'lm', se = FALSE)+scale_color_manual(values = yrColors)+theme_black(base_size = 20)+ylab("Growth Sensitivity to Drought (PDSI)")+xlab("Mean Annual Precipitation")+theme(legend.title = element_blank())
dev.off()

png("outputs/sensitivity_v_TMEAN_pre_post.png")
ggplot(jja.site.df.yr, aes(tm30yr, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), width = 0.05)+stat_smooth(method = 'lm', se = FALSE)+scale_color_manual(values = yrColors)+theme_black(base_size = 20)+ylab("Growth Sensitivity to Drought (PDSI)")+xlab("Mean Monthly Temperature (DegC)")+theme(legend.title = element_blank())
dev.off()

png("outputs/sensitivity_v_DBH_pre_post.png")
ggplot(jja.site.df.yr, aes(DBH, slope.est, color = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), width = 0.05)+stat_smooth(method = 'lm', se = FALSE)+scale_color_manual(values = yrColors)+theme_black(base_size = 20)+ylab("Growth Sensitivity to Drought (PDSI)")+xlab("DBH (cm)")+theme(legend.title = element_blank())
dev.off()

ggplot(jja.site.df.yr, aes(sand, pr30yr,color = slope.est, shape = age))+geom_point()+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), width = 0.5) + ylim(500, 1000)

summary(lm(slope.est ~ sand + age  ,data = jja.site.df.yr))
summary(lm(slope.est ~ sand + pr30yr + age ,data = jja.site.df.age))
summary(lm(slope.est ~  pr30yr + age +DBH,data = jja.site.df.age))

reformed.df <- dcast(jja.site.df.age[c("site", "age", "coords.x1", "coords.x2", 'slope.est', "DBH" , "pr30yr", "tm30yr",'sand')], coords.x1 + coords.x2+site+DBH+pr30yr+tm30yr+sand ~ age, mean, na.rm=TRUE, value.var = 'slope.est') 
reformed.df$diff <- reformed.df$Past - reformed.df$Modern
sens.dif <- gam(Modern ~  pr30yr + DBH   , data = reformed.df)
summary(sens.dif) #Deviance explained = 41.1%

gam.sens.age <- gam(slope.est ~  pr30yr + DBH   , data = jja.site.df.age)
summary(gam.sens.age)
#sens.post <- gam(slope.est ~ pr30yr + tm30yr +sand  , data = yr.sens.df[yr.sens.df$age=="Post-1950",])
#summary(sens.post) # explains 36.8% of deviance:

sens.df <- jja.site.df.age



#install.packages("plot3D")
library(plot3D)

# created  a funciton that takes the data of interest, fits the gam model:
# gam(sensitivity ~ precip + temperature) and plots a 3d surface of it
plot3dsensitivity <- function(sens.df, age, class, col, add ){
  df <- sens.df[sens.df[,c(age)] == class,]
  df <- df[!is.na(df$slope.est),]
  # x, y, z variables
  x <- df$pr30yr
  y <- df$DBH
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
            xlab = "\n\n\n\n Precip", ylab = "\n\n\n\n DBH (cm)", zlab = "\n\n\n\n drought sensitivity", add= add ,
            surf = list(x = x.pred, y = y.pred, z = z.pred,  
                        facets = NA, fit = fitpoints), main = paste("Drought Sensitivity by climate"),
            zlim=c(0,0.1))
  
}


# plot Past and Modern predictive surfaces on the smae plot
png(height = 5, width = 9, units = 'in', res= 300, 'outputs/sensitivity_surface3d_age.png')
plot3dsensitivity(jja.site.df.age, "age","Past", "#009E73",FALSE)

plot3dsensitivity(jja.site.df.age, "age","Modern", "#D55E00",TRUE)
legend(x = 0.5, y = 0 ,
       legend = c(expression(atop("Modern pre-1950", "(low CO"[2]*")")), expression(atop("Modern post-1950", "(high CO"[2]*")"))), 
       col = c("#009E73", 
               "#D55E00"), 
       pch = c(18, 18), 
       bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))

dev.off()

# plot the pre and post 1950 sensitivity surfaces:
yr.sens.df <- jja.site.df.yr 

png(height = 5, width = 9, units = 'in', res= 300,'outputs/sensitivity_surface3d_pre_post_1950_precip_DBH.png')
#sens.df, age, class, col, add
plot3dsensitivity(sens.df = jja.site.df.yr, age = "age",class = "Pre-1950", col = "#009E73",add = FALSE)
plot3dsensitivity(jja.site.df.yr, "age","Post-1950", "#D55E00",TRUE)
legend(x = 0.5, y = 0 ,
       legend = c(expression(atop("All trees Pre-1950", "(low CO"[2]*")")), expression(atop("All trees Post-1950", "(high CO"[2]*")"))), 
       col = c("#009E73", 
               "#D55E00"), 
       pch = c(18, 18), 
       bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))

dev.off()










#-----------------------------modeling drought sensitivity over space:
gam.sens <- mgcv::gam(slope.est ~ pr30yr  + DBH , data = jja.sens.df)
jja.sens.df$gam_ypred <- predict(gam.sens, data = jja.sens.df)
sand <- lm(slope.est ~ pr30yr + DBH*pi, data = site.df[!site.df$site %in% "UNC",]) # outside of UNCAS dusnes, sesnsitivyt depends on soil type
summary(gam.sens) # explains 27.4% of deviance:

# get pr30yr for the whole region:
prism <- raster(paste0(workingdir,"PRISM_ppt_30yr_normal_4kmM2_all_bil/PRISM_ppt_30yr_normal_4kmM2_annual_bil.bil"))
prism.alb <- projectRaster(prism, crs='+init=epsg:3175')


# get FIA average DBH for each grid cell:

FIA <- read.csv('/Users/kah/Documents/bimodality/data/FIA_species_plot_parameters_paleongrid.csv')
speciesconversion <- read.csv('/Users/kah/Documents/bimodality/data/fia_conversion_v02-sgd.csv')

FIA.pal <- merge(FIA, speciesconversion, by = 'spcd' )
FIA.by.paleon <- dcast(FIA.pal, x + y+ cell+ plt_cn ~ PalEON, mean, na.rm=TRUE, value.var = 'dbh') #sum all species in common taxa in FIA grid cells
fia.melt <- melt(FIA.by.paleon, id.vars = c('x', 'y', 'cell', 'plt_cn', 'Var.5')) # melt the dataframe
#fia.by.cell <- dcast(fia.melt, x + y+ cell ~ variable, mean, na.rm=TRUE, value.var = 'value') # average species densities and total density within each grid cell

Oak.sites <- FIA.by.paleon[,c("x","y","cell", "Oak")]
colnames(Oak.sites) <- c("x", "y","cell", "DBH")
# extract pr30yr for all sites where we have FIA data:
Oak.sites$pr30yr <- raster::extract(prism.alb, Oak.sites[,c("x","y")])


# predict gam for whole region:
July_pdsi_sens_pred <- as.vector(predict(gam.sens, newdata = Oak.sites))
Oak.sites$July_pdsi_sens_pred <- July_pdsi_sens_pred
ggplot(Oak.sites, aes(x,y, fill = July_pdsi_sens_pred))+geom_raster()


# assume all forests have similar drought sensitivity as oaks:

FIA.pal <- merge(FIA, speciesconversion, by = 'spcd' )
FIA.by.paleon <- dcast(FIA.pal, x + y+ cell+ plt_cn ~ PalEON , mean, na.rm=TRUE, value.var = 'dbh') #sum all species in common taxa in FIA grid cells
fia.melt <- melt(FIA.by.paleon, id.vars = c('x', 'y', 'cell', 'plt_cn')) # melt the dataframe
fia.by.cell <- dcast(fia.melt, x + y+ cell ~ variable, sum, na.rm=TRUE, value.var = 'value') # average species densities and total density within each grid cell
fia.by.cell[fia.by.cell == 0] <- NA
fia.by.cell$DBH <- rowMeans(fia.by.cell[,4:length(fia.by.cell)], na.rm=TRUE)
ggplot(fia.by.cell, aes(x,y, fill = DBH))+geom_raster()

DBH_all <- fia.by.cell[,c("x", "y", "cell", "DBH")]
DBH_all$pr30yr <- raster::extract(prism.alb, DBH_all[,c("x","y")])

# now project gam for whole region
July_pdsi_sens_pred <- as.vector(predict.gam(gam.sens, newdata = DBH_all))
DBH_all$July_pdsi_sens_pred <- July_pdsi_sens_pred
ggplot(DBH_all, aes(x,y, fill = July_pdsi_sens_pred))+geom_raster()

write.csv(DBH_all, "outputs/DBH_modern_8km.csv")

# predict the Oak sensitivity landscape if all were Modern trees (future landscape):
Oak.Modern <- Oak.sites
Oak.Modern$age <- "Modern"
July_pdsi_Modern_sens_pred <- as.vector(predict.gam(gam.pr.dbh, newdata = Oak.Modern))
Oak.Modern$July_pdsi_Modern_sens_pred <- July_pdsi_Modern_sens_pred
# if all trees were Past:
Oak.Past <- Oak.Modern
Oak.Past$age <- "Past"
July_pdsi_Past_sens_pred <-as.vector(predict(gam.pr.dbh, newdata = Oak.Past))
Oak.Past$July_pdsi_Past_sens_pred <- July_pdsi_Past_sens_pred


ggplot(Oak.Past, aes(x, y, fill = July_pdsi_Past_sens_pred))+geom_raster()
ggplot(Oak.Past, aes(x, y, fill = July_pdsi_Modern_sens_pred))+geom_raster()
#Oak.Past$diff <- Oak.Past$July_pdsi_Past_sens_pred - Oak.Past$July_pdsi_Modern_sens_pred
#ggplot(Oak.Past, aes(x, y, fill = diff ))+geom_raster()

# predict the full landscape if all trees were Modern

All.Modern <- DBH_all
All.Modern$age <- "Modern"
July_pdsi_Modern_sens_pred <- as.vector(predict.gam(gam.pr.dbh, newdata = All.Modern))
All.Modern$July_pdsi_Modern_sens_pred <- July_pdsi_Modern_sens_pred
# if all trees were Past:
All.Past <- All.Modern
All.Past$age <- "Past"
July_pdsi_Past_sens_pred <-as.vector(predict(gam.pr.dbh, newdata = All.Past))
All.Past$July_pdsi_Past_sens_pred <- July_pdsi_Past_sens_pred

ggplot(DBH_all, aes(x, y, fill = DBH))+geom_raster()

ggplot(All.Past, aes(x, y, fill = July_pdsi_Past_sens_pred))+geom_raster()
ggplot(All.Past, aes(x, y, fill = July_pdsi_Modern_sens_pred))+geom_raster()
ggplot(All.Past, aes(x, y, fill = July_pdsi_Past_sens_pred))+geom_raster()
ggplot(All.Past, aes(x, y, fill = DBH))+geom_raster()

# map out all predictions over the region:
all_states <- map_data("state")
states <- subset(all_states, region %in% c(  "illinois", "minnesota", "wisconsin", "iowa", "south dakota",
                                             "north dakota", 'michigan', 'missouri', 'indiana') )
coordinates(states)<-~long+lat
class(states)
proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
mapdata<-spTransform(states, CRS('+init=epsg:3175'))
mapdata<-data.frame(mapdata)

red.pal <- c('#ffffb2',
  '#fecc5c',
  '#fd8d3c',
  '#f03b20',
  '#bd0026')

# map out sensitivity to drought over all oaks:
sites.map <- ggplot()+ geom_raster(data=Oak.sites, aes(x=x, y=y, fill = July_pdsi_sens_pred))+
  labs(x="easting", y="northing", title="Oak Drought Sensitivity") + 
  scale_fill_gradientn(colours = red.pal, name ="Drought \n Sensitivity", limits = c(-0.03, 0.075))+
  coord_cartesian(xlim = c(-59495.64, 725903.4), ylim=c(68821.43, 1480021))
sites.map.oak <- sites.map +geom_polygon(data=data.frame(mapdata), aes(x=long, y=lat, group=group),
                                         colour = "darkgrey", fill = NA)+theme_bw() + theme_black(base_size = 20)+
  theme(axis.text = element_blank(),
        axis.ticks=element_blank(),
        axis.title = element_blank(),
        legend.key = element_rect(),
        #legend.background = element_rect(fill = "white"),
        
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        title = element_text(margin = margin(t = 0, r = 20, b = 10, l = 0))) 
sites.map.oak

# sensitivity for all forests:
# map out sensitivity to drought over all oaks:
sites.map <- ggplot()+ geom_raster(data=DBH_all, aes(x=x, y=y, fill = July_pdsi_sens_pred))+
  labs(x="easting", y="northing", title="All Trees Drought Sensitivity") + 
  scale_fill_gradientn(colours = red.pal, name ="Drought \n Sensitivity", limits = c(-0.03, 0.075))+
  coord_cartesian(xlim = c(-59495.64, 725903.4), ylim=c(68821.43, 1480021))
sites.map.all <- sites.map +geom_polygon(data=data.frame(mapdata), aes(x=long, y=lat, group=group),
                                         colour = "darkgrey", fill = NA)+theme_bw() + theme_black(base_size = 20)+
  theme(axis.text = element_blank(),
        axis.ticks=element_blank(),
        axis.title = element_blank(),
        legend.key = element_rect(),
        #legend.background = element_rect(fill = "white"),
        
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        title = element_text(margin = margin(t = 0, r = 20, b = 10, l = 0))) 
sites.map.all


png(width = 12, height = 6, units = "in", res = 300, "outputs/all_modern_drought_sens_predmaps.png")
grid.arrange(sites.map.oak, sites.map.all, ncol = 2)
dev.off()


# ----------------------------- Modern + Past comparison -----------------
#oak sensitivity for Past trees map:
sites.map <- ggplot()+ geom_raster(data=Oak.Past, aes(x=x, y=y, fill = July_pdsi_Past_sens_pred))+
  labs(x="easting", y="northing", title="Drought Sensitivity 1895-1950") + 
  scale_fill_gradientn(colours = red.pal, name ="Drought \n Sensitivity", limits = c(-0.03, 0.075))+
  coord_cartesian(xlim = c(-59495.64, 725903.4), ylim=c(68821.43, 1480021))
sites.map.Past <- sites.map +geom_polygon(data=data.frame(mapdata), aes(x=long, y=lat, group=group),
                                     colour = "darkgrey", fill = NA)+theme_bw() + theme_black(base_size = 20)+
  theme(axis.text = element_blank(),
        axis.ticks=element_blank(),
        axis.title = element_blank(),
        legend.key = element_rect(),
        #legend.background = element_rect(fill = "white"),
        
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        title = element_text(margin = margin(t = 0, r = 20, b = 10, l = 0))) 
sites.map.Past


ggplot()+ geom_raster(data=Oak.Past, aes(x=x, y=y, fill = DBH))+
  labs(x="easting", y="northing", title="Drought Sensitivity 1895-1950") + 
  scale_fill_gradientn(colours = red.pal, name ="Drought \n Sensitivity", limits = c(-0.03, 0.075))+
  coord_cartesian(xlim = c(-59495.64, 725903.4), ylim=c(68821.43, 1480021))


# oak sensitivity for Modern trees map:
sites.map <- ggplot()+ geom_raster(data=Oak.Past, aes(x=x, y=y, fill = July_pdsi_Modern_sens_pred))+
  labs(x="easting", y="northing", title="Drought Sensitivity 1950-present") + 
  scale_fill_gradientn(colours = red.pal, name ="Drought \n Sensitivity", limits = c(-0.03, 0.075))+
  coord_cartesian(xlim = c(-59495.64, 725903.4), ylim=c(68821.43, 1480021))
sites.map.Modern <- sites.map +geom_polygon(data=data.frame(mapdata), aes(x=long, y=lat, group=group),
                                         colour = "darkgrey", fill = NA)+theme_bw() + theme_black(base_size = 20)+
  theme(axis.text = element_blank(),
        axis.ticks=element_blank(),
        axis.title = element_blank(),
        legend.key = element_rect(),
        #legend.background = element_rect(fill = "white"),
        
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        title = element_text(margin = margin(t = 0, r = 20, b = 10, l = 0))) 
sites.map.Modern

png(width = 12, height = 6, units = "in", res = 300, "outputs/Oak_modern_past_drought_sens_predmaps.png")
grid.arrange(sites.map.Past, sites.map.Modern, ncol = 2)
dev.off()



# oak sensitivity for Past trees map:
sites.map <- ggplot()+ geom_raster(data=All.Past, aes(x=x, y=y, fill = July_pdsi_Past_sens_pred))+
  labs(x="easting", y="northing", title="Drought Sensitivity 1895-1950") + 
  scale_fill_gradientn(colours = red.pal, name ="Drought \n Sensitivity", limits = c(-0.03, 0.075))+
  coord_cartesian(xlim = c(-59495.64, 725903.4), ylim=c(68821.43, 1480021))
all.map.Past <- sites.map + geom_polygon(data=data.frame(mapdata), aes(x=long, y=lat, group=group),
                                        colour = "darkgrey", fill = NA)+theme_bw() + theme_black(base_size = 20)+
  theme(axis.text = element_blank(),
        axis.ticks=element_blank(),
        axis.title = element_blank(),
        legend.key = element_rect(),
        #legend.background = element_rect(fill = "white"),
        
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        title = element_text(margin = margin(t = 0, r = 20, b = 10, l = 0))) 
all.map.Past

# all tresssensitivity for Modern trees map:
sites.map <- ggplot()+ geom_raster(data=All.Past, aes(x=x, y=y, fill = July_pdsi_Modern_sens_pred))+
  labs(x="easting", y="northing", title="Drought Sensitivity 1950-present") + 
  scale_fill_gradientn(colours = red.pal, name ="Drought \n Sensitivity", limits = c(-0.03, 0.075))+
  coord_cartesian(xlim = c(-59495.64, 725903.4), ylim=c(68821.43, 1480021))
all.map.Modern <- sites.map +geom_polygon(data=data.frame(mapdata), aes(x=long, y=lat, group=group),
                                           colour = "darkgrey", fill = NA)+theme_bw() + theme_black(base_size = 20)+
  theme(axis.text = element_blank(),
        axis.ticks=element_blank(),
        axis.title = element_blank(),
        legend.key = element_rect(),
        #legend.background = element_rect(fill = "white"),
        
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        title = element_text(margin = margin(t = 0, r = 20, b = 10, l = 0))) 
all.map.Modern

png(width = 12, height = 6, units = "in", res = 300, "outputs/all_modern_past_drought_sens_predmaps.png")
grid.arrange(all.map.Past, all.map.Modern, ncol = 2)
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
