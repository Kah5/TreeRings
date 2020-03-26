# script to read in all the rwl files (with the headers cleaned & fixed), and merge with climate data
# Author: Kelly A. Heilman
# Last Updated: March 25, 2020
# note: Need to run clean_separate_data.R before running this script, this loads required packages and required datasets


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
setwd("/Users/kah/Documents/TreeRings2")

#####################################
#read in rwl & add site + year codes#
#####################################

# quick function to read, detrend, and add the year as a column:
# this function will also just calculate BAI instead
read_detrend_year <- function( filename, method , rwiorbai, site){
  if(site %in% c( "UNI")){
    newseries <- read.csv(paste0("cleanrwl/",site,"ww.csv"))
    rwl.stats(newseries)
    file.tuc <- read.tucson( filename )
    rownames(newseries) <- rownames(file.tuc)
    
  }else{
    if(site %in% c("HIC","AVO","GLL4", "GLL1", "GLL2", "GLL3" )){
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
  mean.rwi.stat <- rwi.stats(gp.treeMean2)
  mean.rwi.stat$site <- site
  write.csv(mean.rwi.stat, paste0("outputs/Stats/mean.rwi.stats.", site,".csv"))
  
  ifelse(rwiorbai == "rwi" & method != "none", 
         detrended <- detrend(rwl = newseries, method = method),
          ifelse(rwiorbai == "bai" & method != "none",
          detrended <- bai.out(rwl = newseries),
          detrended <- newseries))
  
  
  
  if(site %in% "HIC"){
    detrended.mean <- treeMean(detrended, read.ids(detrended, stc = c(3,4,1)), na.rm=TRUE)
    colnames(detrended.mean) <- paste0(site,colnames(detrended.mean))
    HIC.table <- read.csv("crns/HIC_translation.csv")
   
    new.cols <- merge(data.frame(RWL= colnames(detrended.mean), order = 1:length(colnames(detrended.mean))), HIC.table, by = "RWL")
    colnames(detrended.mean) <- new.cols[order(new.cols$order),]$ID
   detrended.mean<- detrended.mean[,1:32]
    detrended.mean <- treeMean(detrended.mean, autoread.ids(detrended.mean), na.rm=TRUE)
    colnames(detrended.mean) <- paste0(site,colnames(detrended.mean))
  }else{
    if(site %in% "GLA"){
      detrended.mean <-  treeMean(detrended, autoread.ids(newseries), na.rm=TRUE)
      
      colnames(detrended.mean) <- paste0(site,colnames(detrended.mean))
      GLA.table <- read.csv("crns/GLA_translation_table.csv")
      
      new.cols <- merge(data.frame(RWL= colnames(detrended.mean), order = 1:length(colnames(detrended.mean))), GLA.table, by = "RWL")
      colnames(detrended.mean) <- new.cols[order(new.cols$order),]$ID
      detrended.mean<- detrended.mean[,1:15]
      detrended.mean <- treeMean(detrended.mean, autoread.ids(detrended.mean), na.rm=TRUE)
      colnames(detrended.mean) <- paste0(site,colnames(detrended.mean))
      new.cols2 <- merge(data.frame(RWL= colnames(detrended.mean), order = 1:length(colnames(detrended.mean))), GLA.table, by = "RWL")
      colnames(detrended.mean) <- new.cols2[order(new.cols2$order),]$ID
      
    }else{
    if(site %in% "GLL4"){
      detrended.mean <- treeMean(detrended, read.ids(detrended, stc = c(4,7,1)), na.rm=TRUE)
      colnames(detrended.mean) <- paste0(site, colnames(detrended.mean))
      # quick fix for GLL4:
      
      colnames(detrended.mean) <- c("GLL41", "GLL413", "GLL414", "GLL415", "GLL42", "GLL45", "GLL47", "GLL48", "GLL49")
      
    }else{
      if(site %in% "COR"){
        detrended.mean <- treeMean(detrended, autoread.ids(detrended), na.rm=TRUE)
        colnames(detrended.mean) <- paste0(site,"19",colnames(detrended.mean))
      }else{
        if(site %in% "UNC"){
          detrended.mean <- treeMean(detrended, autoread.ids(detrended), na.rm=TRUE)
          colnames(detrended.mean) <- paste0(site,colnames(detrended.mean), "a11")
          colnames(detrended.mean) <- substring(colnames(detrended.mean), 1, 8)
        }else{
          if(site %in% "AVO"){
            detrended.mean <- treeMean(detrended, autoread.ids(detrended), na.rm=TRUE)
            colnames(detrended.mean) <- ifelse(nchar(colnames(detrended.mean)) <= 4, paste0(site, colnames(detrended.mean), "a"), paste0(site, colnames(detrended.mean)))
             }else{
      detrended.mean <- treeMean(detrended, autoread.ids(detrended), na.rm=TRUE)
      colnames(detrended.mean) <- paste0(site,colnames(detrended.mean))
    }
    }
    }
  }}}
  mean.rwi.stat <- rwl.stats(detrended.mean)
  write.csv(mean.rwi.stat, paste0("outputs/Stats/mean.bai.stats.", site,".csv"))
  mean.rwi.stat <- rwi.stats(gp.treeMean2)
  mean.rwi.stat$site <- site
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

Hickory.rwi <- read_detrend_year(filename = "cleanrwl/HICww.rwl", method = "none", rwiorbai = "rwi", site = "HIC")
StCroix.rwi <- read_detrend_year("cleanrwl/STCww.rwl", method = "none", rwiorbai = "rwi", site = "STC")
Bonanza.rwi <- read_detrend_year("cleanrwl/BONww.rwl", method = "none", rwiorbai = "rwi", site = "BON")
Townsend.rwi <- read_detrend_year("cleanrwl/TOWww.rwl", method = "none", rwiorbai = "rwi", site = "TOW")#townsedn woods
Pleasant.rwi <- read_detrend_year("cleanrwl/PLEww.rwl", method = "none", rwiorbai = "rwi", site = "PLE") #Pleasant valley conservency
Coral.rwi <- read_detrend_year(filename = "cleanrwl/CORww.rwl", method = "none", rwiorbai = "rwi", site = "COR")
Uncas.rwi <- read_detrend_year(filename = "cleanrwl/UNCww.rwl", method = "none", rwiorbai = "rwi", site = "UNC")
Glacial.rwi <- read_detrend_year(filename = "cleanrwl/GLAww.rwl", method = "none", rwiorbai = "rwi", site = "GLA")
Englund.rwi <- read_detrend_year("cleanrwl/ENGww.rwl", method = "none", rwiorbai = "rwi", site = "ENG")
Mound.rwi <- read_detrend_year("cleanrwl/MOUww.rwl", method = "none", rwiorbai = "rwi", site = "MOU")
GLL1.rwi <- read_detrend_year(filename = "cleanrwl/GLL1ww.rwl", method = "none", rwiorbai = "rwi", site = "GLL1")
GLL2.rwi <- read_detrend_year("cleanrwl/GLL2ww.rwl", method = "none", rwiorbai = "rwi", site = "GLL2")
GLL3.rwi <- read_detrend_year("cleanrwl/GLL3ww.rwl", method = "none", rwiorbai = "rwi", site = "GLL3")
GLL4.rwi <- read_detrend_year("cleanrwl/GLL4ww.rwl", method = "none", rwiorbai = "rwi", site = "GLL4")
PVC.rwi <- read_detrend_year("cleanrwl/PVCww.rwl", method = "none", rwiorbai = "rwi", site = "PVC")
AVO.rwi <- read_detrend_year(filename = "cleanrwl/AVOww.rwl", method = "none", rwiorbai = "rwi", site = "AVO")
UNI.rwi <- read_detrend_year("cleanrwl/UNIww.rwl", method = "none", rwiorbai = "rwi", site = "UNI")




detrended.list <- list(Hickory.rwi, StCroix.rwi, Bonanza.rwi,Townsend.rwi,Pleasant.rwi, Coral.rwi,
                       Uncas.rwi, Glacial.rwi, Englund.rwi, Mound.rwi, GLL1.rwi, GLL2.rwi, 
                       GLL3.rwi, GLL4.rwi, PVC.rwi, AVO.rwi)#, UNI.bai) # omitting UNI right now

saveRDS(detrended.list,"data/no_detrend_list_site.rds")
source("R/manuscript_code/tree_age_agg.R")

# apply the tree_age_agg function on all of the detrended tree ring series
# note: writes to a folder data/tree_growth_age
detrended.age <- lapply(detrended.list, FUN = tree_age_agg,   age1950 = 10,  type = "RWI_Spline_detrended" )

# use do.calll to make these a dataframe
detrended.age.df <- do.call(rbind, detrended.age)

ggplot(detrended.age.df[detrended.age.df$site %in% "GLA",], aes(x = year, y = RWI, color = ID))+geom_point()+geom_line()+facet_wrap(~site, ncol = 5, scales = "free_y")+theme_bw()

ggplot(detrended.age.df[detrended.age.df$site %in% "GLA",], aes(x = RWI_3, y = RWI, color = ID))+geom_point()

age.classes <- detrended.age.df %>% group_by(site, ID)  %>% drop_na() %>% dplyr::summarise(pre1800 = min(year) < 1880  , pre1950 = min(year, na.rm = TRUE) <1930 & min(year, na.rm = TRUE) >=1880 , post1950 = min(year, na.rm = TRUE) >1930)

age.classes  %>% group_by(site) %>% dplyr::summarise(pre1800_n=sum(pre1800, na.rm=TRUE), pre1950_n = sum(pre1950, na.rm=TRUE), post1950_n = sum(post1950, na.rm=TRUE))

# get establishment dates
est.dates <- detrended.age.df %>% group_by(site, ID)  %>% drop_na() %>% dplyr::summarise(est = min(year))
est.dates$class <- ifelse(est.dates$est <= 1890, "old/mature", ifelse(est.dates$est <=1940, "mid", "young"))
write.csv(age.classes, "data/n_trees_ageclass_by_site_rwi_v2.csv")

###################################
# add climate data to the age trends
####################################
# note about climate data: GHCN climate data provides PDSI esimtates, while PRISM is more commonly used and can be used to get VPD data.
# both GHCN and PRISM have Precip and temperature estimates, but PRSIM data should be used for this b/c GHCN is over the whole climate zone, PRISM is point estimates
# from GHCN, also get the 1, 2, 3, 6, 9, 12, and 24 month drought indices: SPEI, 
# this function reads in climate data from each site and adds it to the appropriate site
get.clim <- function(site.df, climatedata){
  site.code <- site.df[1,]$site
  
  if(climatedata == "GHCN"){
    if(site.code %in% c("BON", "GLL1", "GLL2", "GLL3", "GLL4")){
      MNcd.clim <- read.csv("data/climate_files/West_central_MN_nclimdiv.csv")
    } else{ if(site.code %in% c("HIC", "COR","GLA", "PVC" )){
      MNcd.clim <- read.csv("data/climate_files/NE_illinois_climdiv.csv")
    }  else{ if(site.code == "W-R" ){
      MNcd.clim <- read.csv("data/climate_files/West_central_MN_nclimdiv.csv")
    } else{ if(site.code == 'SAW'){
      MNcd.clim <- read.csv("data/climate_files/NE_illinois_climdiv.csv")
    }else{ if(site.code == "STC"){
      MNcd.clim <- read.csv("data/climate_files/East_Central_MN_CDODiv5039587215503.csv")
    }else{ if(site.code == "ENG"){
      MNcd.clim <- read.csv("data/climate_files/Central_MN_CDO.csv")
    }else{ if(site.code == "TOW"){
      MNcd.clim <- read.csv("data/climate_files/South_central_MN_CDO.csv")
    }else{ if(site.code == "MOU"){
      MNcd.clim <- read.csv("data/climate_files/South_East_MN_CDO.csv")
    }else{ if(site.code %in% c("UNC", "AVO")){
      MNcd.clim <- read.csv("data/climate_files/East_Central_MN_CDODiv5039587215503.csv")
    }else { if(site.code == 'PLE'){
      MNcd.clim <- read.csv('data/climate_files/south_central_WI_climdiv.csv')
    }else { if(site.code == 'YRF'){
      MNcd.clim <- read.csv('data/climate_files/IA_nclim_div_northeast.csv')
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
    
    # conversions to cm and mm
    MNcd.clim$PCP <- MNcd.clim$PCP*25.54
    MNcd.clim$TMIN <- (MNcd.clim$TMIN - 32)/1.8
    MNcd.clim$TMAX <- (MNcd.clim$TMAX - 32)/1.8
    MNcd.clim$TAVG <- (MNcd.clim$TAVG - 32)/1.8
    
    
    keeps <- c("Year", "Month",  "PCP")
    keepstavg <- c("Year", "Month", "TAVG")
    keepst <- c("Year", "Month",  "TMAX")
    keepstmin <- c("Year", "Month",  "TMIN")
    keepspdsi <- c("Year", "Month",  "PDSI")
    keepssp <- c("Year", "Month",  "SP01" ,  "SP02"  , "SP03",     
                  "SP06", "SP09", "SP12", "SP24")
    
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
    
    MNsp.df <- MNcd.clim[,keepssp]
    MNsp.df[MNsp.df == -99.99]<- NA
    
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
    
    
    spei.m <- data.frame( MNsp.df %>% group_by(Year, Month) )
    SPEI_01 <- data.frame( MNsp.df[,c("Year","Month","SP01")] %>% group_by(Year) %>% spread(Month, SP01))
    colnames(SPEI_01) <- c("Year", paste0("SP01_", 1:12))
    SPEI_02 <- data.frame( MNsp.df[,c("Year","Month","SP02")] %>% group_by(Year) %>% spread(Month, SP02))
    colnames(SPEI_02) <- c("Year", paste0("SP02_", 1:12))
    SPEI_03 <- data.frame( MNsp.df[,c("Year","Month","SP03")] %>% group_by(Year) %>% spread(Month, SP03))
    colnames(SPEI_03) <- c("Year", paste0("SP03_", 1:12))
    SPEI_06 <- data.frame( MNsp.df[,c("Year","Month","SP06")] %>% group_by(Year) %>% spread(Month, SP06))
    colnames(SPEI_06) <- c("Year", paste0("SP06_", 1:12))
    SPEI_09 <- data.frame( MNsp.df[,c("Year","Month","SP09")] %>% group_by(Year) %>% spread(Month, SP09))
    colnames(SPEI_09) <- c("Year", paste0("SP09_", 1:12))
    SPEI_12 <- data.frame( MNsp.df[,c("Year","Month","SP12")] %>% group_by(Year) %>% spread(Month, SP12))
    colnames(SPEI_12) <- c("Year", paste0("SP12_", 1:12))
    SPEI_24 <- data.frame( MNsp.df[,c("Year","Month","SP24")] %>% group_by(Year) %>% spread(Month, SP24))
    colnames(SPEI_24) <- c("Year", paste0("SP24_", 1:12))
    
    
    annual.p <- aggregate(PCP~Year, data = MNp.df[1:1440,], FUN = sum, na.rm=T)
    annual.t <- aggregate(TAVG ~ Year, data = MNtavg.df[1:1440,], FUN = 'mean', na.rm=T)
    annual.mint <- aggregate(TMIN ~Year, data = MNtmin.df[1:1440,], FUN = 'mean', na.rm = T)
    annual.pdsi <- aggregate(PDSI ~ Year, data = MNpdsi.df[1:1440,], FUN = 'mean', na.rm = T)
    annual.pdsi.m <- aggregate(PDSI ~ Year + Month, data = MNpdsi.df[1:1440,], FUN = 'mean', na.rm = T)
    jul.pdsi <- annual.pdsi.m[annual.pdsi.m$Month == 7,] 
    jja.pdsi <- aggregate(PDSI ~ Year, data = MNpdsi.df[MNpdsi.df$Month %in% 6:8 & MNpdsi.df$Year %in% 1895:2014,], FUN = 'mean', na.rm = T)
    jja.pdsi.m <- aggregate(PDSI ~ Year + Month, data = MNpdsi.df[MNpdsi.df$Month %in% 6:8 & MNpdsi.df$Year %in% 1895:2014,], FUN = 'mean', na.rm = T)
    
   
    
    PCPs <- MNcd.clim[,c("Year", "Month", "PCP")] %>% group_by(Year, Month) %>% spread(key = Month, value = PCP, sep = "_pcp_" ) 
    TMAXs <- MNcd.clim[,c("Year", "Month", "TMAX")] %>% group_by(Year, Month) %>% spread(key = Month, value = TMAX, sep = "_tmax_" ) 
    TMINs <- MNcd.clim[,c("Year", "Month", "TMIN")] %>% group_by(Year, Month) %>% spread(key = Month, value = TMIN, sep = "_tmin_" ) 
    TAVGs <- MNcd.clim[,c("Year", "Month", "TAVG")] %>% group_by(Year, Month) %>% spread(key = Month, value = TAVG, sep = "_tavg_" ) 
    PDSIs <- MNcd.clim[,c("Year", "Month", "PDSI")] %>% group_by(Year, Month) %>% spread(key = Month, value = PDSI, sep = "_pdsi_" ) 
    
    one <- merge(PCPs, TMAXs, by = "Year")
    two <- merge(one, TMINs, by = "Year")
    three <- merge(two, TAVGs, by = "Year")
    ghcns <- merge(three, PDSIs, by = "Year")
    ghcns$site <- site.code
    write.csv(ghcns, paste0("climate/",site.code, "_", climatedata, "full_mo.csv"), row.names=FALSE)
    
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
                          Jul.pdsi = jul.pdsi[1:120,]$PDSI, 
                          SPEI_01[1:120,],
                          SPEI_02[1:120,],
                          SPEI_03[1:120,],
                          SPEI_06[1:120,],
                          SPEI_09[1:120,],
                          SPEI_12[1:120,],
                          SPEI_24[1:120,]) 
    
    
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
    MNcd.clim$PRISM <- as.numeric(MNcd.clim$Month)
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
    
    
    PCPs <- MNcd.clim[,c("Year", "PRISM", "PCP")] %>% group_by(Year, PRISM) %>% spread(key = PRISM, value = PCP, sep = "_pcp_" ) 
    TMAXs <- MNcd.clim[,c("Year", "PRISM", "TMAX")] %>% group_by(Year, PRISM) %>% spread(key = PRISM, value = TMAX, sep = "_tmax_" ) 
    TMINs <- MNcd.clim[,c("Year", "PRISM", "TMIN")] %>% group_by(Year, PRISM) %>% spread(key = PRISM, value = TMIN, sep = "_tmin_" ) 
    TAVGs <- MNcd.clim[,c("Year", "PRISM", "TAVG")] %>% group_by(Year, PRISM) %>% spread(key = PRISM, value = TAVG, sep = "_tavg_" ) 
    VPDmins <- MNcd.clim[,c("Year", "PRISM", "VPDmin")] %>% group_by(Year, PRISM) %>% spread(key = PRISM, value = VPDmin, sep = "_vpdmin_" ) 
    VPDmaxs <- MNcd.clim[,c("Year", "PRISM", "VPDmax")] %>% group_by(Year, PRISM) %>% spread(key = PRISM, value = VPDmax, sep = "_vpdmax_" ) 
    PET <- MNcd.clim[,c("Year", "PRISM", "PET")] %>% group_by(Year, PRISM) %>% spread(key = PRISM, value = PET, sep = "_PET_" ) 
    BAL <- MNcd.clim[,c("Year", "PRISM", "BAL")] %>% group_by(Year, PRISM) %>% spread(key = PRISM, value = BAL, sep = "_BAL_" ) 
    
    one <- merge(PCPs, TMAXs, by = "Year")
    two <- merge(one, TMINs, by = "Year")
    three <- merge(two, TAVGs, by = "Year")
    four <- merge(three, VPDmins, by = "Year")
    five <- merge(four, VPDmaxs, by = "Year")
    six <- merge(five, PET, by = "Year")
    prisms <- merge(six, BAL, by = "Year")
    prisms$site <- site.code
    write.csv(prisms, paste0("climate/",site.code, "_", climatedata, "full_mo.csv"), row.names=FALSE)
    
    
    
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


#------------------------------ get the [full] climate datasets and collate: --------------------

# list filenames from the individual climate files that we just created  (now in the climate/ folder)
prisms <- paste0("climate/", list.files("climate", pattern = "PRISMfull"))
ghcns <- paste0("climate/", list.files("climate", pattern = "GHCNfull"))

# read all the prism and ghcn climate data & bind in a dataframe
prism <- lapply( prisms, read.csv )
ghcn <- lapply( ghcns, read.csv )

ghcn.df <- do.call(rbind, ghcn)
prism.df <- do.call(rbind, prism)

# save for later use
write.csv(ghcn.df, "outputs/full_ghcn_all_months.csv", row.names = FALSE)
write.csv(prism.df, "outputs/full_prism_all_months.csv", row.names = FALSE)



#-------------------------------Exploratory data analysis and development of full.ghcn--------------------

# make some exlploratory data plots of RWI over time:


ggplot(det.age.clim.prism.df[det.age.clim.prism.df$site %in% "BON",], aes(x = year, y = RWI, color = ID))+geom_point()+geom_line()+facet_wrap(~site, ncol = 5, scales = "free_y")+theme_bw()

ggplot(det.age.clim.prism.df, aes(x = year, y = RWI, color = ageclass))+geom_point(size = 0.1)+geom_line(size = 0.1)+facet_wrap(~site, ncol = 5, scales = "free_y")+theme_bw()

ggplot(det.age.clim.prism.df[det.age.clim.prism.df$site %in% "COR",], aes(x = year, y = RWI, color = ID))+geom_point()+geom_line()+facet_wrap(~site, ncol = 5, scales = "free_y")+theme_bw()


# plot the RWI vs selected climate variables
ggplot(det.age.clim.prism.df[det.age.clim.prism.df$site %in% "BON",], aes(x = jul.VPDmax, y = RWI, color = ageclass))+geom_point()+stat_smooth(method = 'lm')+facet_wrap(~site, ncol = 5)
ggplot(det.age.clim.ghcn.df, aes(x = Jul.pdsi, y = RWI, color = ageclass))+geom_point()+stat_smooth(method = 'lm')+facet_wrap(~site, ncol = 5)
ggplot(det.age.clim.ghcn.df, aes(x = JJA.pdsi, y = RWI, color = ageclass))+geom_point()+stat_smooth(method = 'lm')+facet_wrap(~site, ncol = 5)
ggplot(det.age.clim.ghcn.df, aes(x = SP01_7, y = RWI, color = ageclass))+geom_point()+stat_smooth(method = 'lm')+facet_wrap(~site, ncol = 5)

#summary(lm(RWI ~ JJA.pdsi + ageclass + site ,data = det.age.clim.ghcn.df))



# calculating resilience, resistance, and recovery from drought:

ggplot(det.age.clim.ghcn.df, aes(year, JJA.pdsi))+geom_line()+geom_vline(xintercept = 1988, color = "red")+geom_vline(xintercept = 1934, color = "blue")+facet_wrap(~site)
ggplot(det.age.clim.ghcn.df, aes(year, SP09_6))+geom_line()+geom_vline(xintercept = 1988, color = "red")+geom_vline(xintercept = 1934, color = "blue")+facet_wrap(~site)

# in Minnesota, 1988 was a severe summer drought year that was both preceded and followed by non-droughty summers, lets look at the resilience and resistance to this drought across sites. 
# Note that the strength of the drought was not as strong in Illinois

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> drought metrics: <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# Resistance Rt =  Dr/PreDr 
# Recovery Rc = PostDr/Dr 
# Resilience Rs = PostDr/PreDr

# where Dr = drought year BAI
      # PreDr = n years before drought
      # PostDr = n years after drought

# evaluate these metrics for each tree using a n = 3 window for PreDr and PostDr

# drought year == 1988

drought.df <- det.age.clim.ghcn.df[,c("year", "site", "ID", "Age", "RWI","JJA.pdsi", "ageclass")]
drought.df$droughtclass <- ifelse(drought.df$year %in% 1988, "Dr", ifelse(drought.df$year %in% 1985:1987, "PreDr", ifelse(drought.df$year %in% 1989:1991, "PostDr", "Non-drought")))

drought.df <- drought.df %>% group_by(ID, site, droughtclass) %>% summarise(meanBAI = mean(RWI, na.rm=TRUE)) %>% spread(droughtclass, meanBAI)

# now calculate resistance, resilience, and recovery
drought.df.1988 <- drought.df %>% mutate(Resistance =  Dr/PreDr, 
                      Recovery = PostDr/Dr,
                      Resilience = PostDr/PreDr)

ggplot(drought.df.1988, aes(site, Resistance))+geom_boxplot()+theme_bw()+geom_hline(yintercept = 1)+ylim(0,2)
ggplot(drought.df.1988, aes(site, Recovery))+geom_boxplot()+theme_bw()+geom_hline(yintercept = 1)+ylim(0,2)
ggplot(drought.df.1988, aes(site, Resilience))+geom_boxplot()+theme_bw()+geom_hline(yintercept = 1)+ylim(0,2)

# do the same but by ageclass:
drought.df.age <- det.age.clim.ghcn.df[,c("year", "site", "ID", "Age", "RWI","JJA.pdsi", "ageclass")]
drought.df.age$droughtclass <- ifelse(drought.df.age$year %in% 1988, "Dr", ifelse(drought.df.age$year %in% 1985:1987, "PreDr", ifelse(drought.df.age$year %in% 1989:1991, "PostDr", "Non-drought")))

drought.df.age <- drought.df.age %>% group_by(ID, site, ageclass,droughtclass) %>% summarise(meanBAI = mean(RWI, na.rm=TRUE)) %>% spread(droughtclass, meanBAI)

drought.df.1988.age <- drought.df.age %>% group_by(ageclass) %>% 
                                  mutate(Resistance =  Dr/PreDr, 
                                         Recovery = PostDr/Dr,
                                         Resilience = PostDr/PreDr)

ggplot(drought.df.1988.age, aes(site, Resistance, fill = ageclass))+geom_boxplot()+theme_bw()+geom_hline(yintercept = 1)+ylim(0,2)
ggplot(drought.df.1988.age, aes(site, Recovery, fill = ageclass))+geom_boxplot()+theme_bw()+geom_hline(yintercept = 1)+ylim(0,2)
ggplot(drought.df.1988.age, aes(site, Resilience, fill = ageclass))+geom_boxplot()+theme_bw()+geom_hline(yintercept = 1)+ylim(0,2)


ggplot(drought.df.1988.age, aes(ageclass, Resistance, fill = ageclass))+geom_boxplot()+theme_bw()+geom_hline(yintercept = 1)+ylim(0,2)
ggplot(drought.df.1988.age, aes(ageclass, Recovery, fill = ageclass))+geom_boxplot()+theme_bw()+geom_hline(yintercept = 1)+ylim(0,2)
ggplot(drought.df.1988.age, aes(ageclass, Resilience, fill = ageclass))+geom_boxplot()+theme_bw()+geom_hline(yintercept = 1)+ylim(0,2)

# now plot resistance and reslience against the site characteristics:

# get site characteristics
locs <- read.csv("outputs/priority_sites_locs.csv")
locs$code <- as.character(locs$code)
locs[9:12,]$code <- c( "GLL1", "GLL2", "GLL3", "GLL4")
sites <- c("COR", "HIC", "STC", "GLA", "TOW", "ENG", "UNC", "BON", "MOU", "GLL4", "GLL3", "GLL2", "GLL1", "PVC", "AVO", "PLE", "UNI")

speciesdf<- data.frame(code = c("BON", "COR", "GLA", "GLL1", "GLL2", "GLL3", "GLL4",
                                "HIC", "MOU", "PLE", "PVC", "STC", "TOW", "UNC", "AVO", "ENG", "PLE", "UNI"),
                       species =  c( "QUMA", "QUAL", "QUAL/QUMA", "QUMA","QUMA", "QUMA","QUMA",
                                     "QUAL/QUMA", "QURA/QUVE", "QUAL/QUMA", "QUMA", "QUMA", "QURA", "QUMA", "QURA", "QURA", "QUAL", "QUAL"))

locs <- merge(locs, speciesdf, by = "code")

# workingdir <- "/Users/kah/Documents/bimodality/data/"

# read in and average prism data (this is modern 30year normals)
# prism <- raster(paste0(workingdir,"PRISM_ppt_30yr_normal_4kmM2_all_bil/PRISM_ppt_30yr_normal_4kmM2_annual_bil.bil"))
# prism.alb <- projectRaster(prism, crs='+init=epsg:3175')
# 
# locs$pr30yr <- raster::extract(prism.alb, locs[,c("coords.x1","coords.x2")])
# 
# workingdir <- "/Users/kah/Documents/bimodality/data/"
# 
# # read in and average prism temperature data (this is modern 30year normals)
# prism.t <- raster(paste0(workingdir,'PRISM_tmean_30yr_normal_4kmM2_annual_bil/PRISM_tmean_30yr_normal_4kmM2_annual_bil.bil'))
# prismt.alb <- projectRaster(prism.t, crs='+init=epsg:3175')
# 
# # extract temp
# locs$tm30yr <- raster::extract(prismt.alb, locs[,c("coords.x1","coords.x2")])

workingdir <- "/Users/kah/Documents/TreeRings2"

# now merge locs data with the drought recovery data:
drought.df.1988.site <- merge(drought.df.1988.age, locs, by.x = "site", by.y = "code")

ggplot(drought.df.1988.site, aes(site, Resistance, fill = species))+geom_boxplot()+theme_bw()+geom_hline(yintercept = 1)+ylim(0,2)
ggplot(drought.df.1988.site, aes(site, Recovery, fill = species))+geom_boxplot()+theme_bw()+geom_hline(yintercept = 1)+ylim(0,2)
ggplot(drought.df.1988.site, aes(site, Resilience, fill = species))+geom_boxplot()+theme_bw()+geom_hline(yintercept = 1)+ylim(0,2)

ggplot(drought.df.1988.site, aes(species, Resistance, fill = species))+geom_boxplot()+theme_bw()+geom_hline(yintercept = 1)+ylim(0,2)
ggplot(drought.df.1988.site, aes(species, Recovery, fill = species))+geom_boxplot()+theme_bw()+geom_hline(yintercept = 1)+ylim(0,2)
ggplot(drought.df.1988.site, aes(species, Resilience, fill = species))+geom_boxplot()+theme_bw()+geom_hline(yintercept = 1)+ylim(0,2)


ggplot(drought.df.1988.site, aes(awc, Resistance))+geom_point()+theme_bw()+geom_hline(yintercept = 1)+ylim(0,2)
ggplot(drought.df.1988.site, aes(awc, Recovery))+geom_point()+theme_bw()+geom_hline(yintercept = 1)+ylim(0,2)
ggplot(drought.df.1988.site, aes(awc, Resilience))+geom_point()+theme_bw()+geom_hline(yintercept = 1)+ylim(0,2)

ggplot(drought.df.1988.site, aes(sand, Resistance))+geom_point()+theme_bw()+geom_hline(yintercept = 1)+ylim(0,2)
ggplot(drought.df.1988.site, aes(sand, Recovery))+geom_point()+theme_bw()+geom_hline(yintercept = 1)+ylim(0,2)
ggplot(drought.df.1988.site, aes(sand, Resilience))+geom_point()+theme_bw()+geom_hline(yintercept = 1)+ylim(0,2)

ggplot(drought.df.1988.site, aes(pr30yr, Resistance))+geom_point()+theme_bw()+geom_hline(yintercept = 1)+ylim(0,2)
ggplot(drought.df.1988.site, aes(pr30yr, Recovery))+geom_point()+theme_bw()+geom_hline(yintercept = 1)+ylim(0,2)
ggplot(drought.df.1988.site, aes(pr30yr, Resilience))+geom_point()+theme_bw()+geom_hline(yintercept = 1)+ylim(0,2)

ggplot(drought.df.1988.site, aes(tm30yr, Resistance))+geom_point()+theme_bw()+geom_hline(yintercept = 1)+ylim(0,2)
ggplot(drought.df.1988.site, aes(tm30yr, Recovery))+geom_point()+theme_bw()+geom_hline(yintercept = 1)+ylim(0,2)
ggplot(drought.df.1988.site, aes(tm30yr, Resilience))+geom_point()+theme_bw()+geom_hline(yintercept = 1)+ylim(0,2)


ggplot(drought.df.1988.site, aes(BA, Resistance, fill = site))+geom_boxplot()+theme_bw()+geom_hline(yintercept = 1)+ylim(0,2)
ggplot(drought.df.1988.site, aes(BA, Recovery))+geom_point()+theme_bw()+geom_hline(yintercept = 1)+ylim(0,2)
ggplot(drought.df.1988.site, aes(BA, Resilience))+geom_point()+theme_bw()+geom_hline(yintercept = 1)+ylim(0,2)



# lets do the same for the 1934 drought:

# drought year == 1934

drought.df <- det.age.clim.ghcn.df[,c("year", "site", "ID", "Age", "RWI","JJA.pdsi", "ageclass")]
drought.df$droughtclass3 <- ifelse(drought.df$year %in% 1934, "Dr", ifelse(drought.df$year %in% 1931:1933, "PreDr", ifelse(drought.df$year %in% 1935:1937, "PostDr", "Non-drought")))
drought.df$droughtclass5 <- ifelse(drought.df$year %in% 1934, "Dr", ifelse(drought.df$year %in% 1929:1933, "PreDr", ifelse(drought.df$year %in% 1935:1939, "PostDr", "Non-drought")))

drought.df.3 <- drought.df %>% group_by(ID, site, droughtclass3) %>% summarise(meanBAI = mean(RWI, na.rm=TRUE)) %>% spread(droughtclass3, meanBAI)
drought.df.5 <- drought.df %>% group_by(ID, site, droughtclass5) %>% summarise(meanBAI = mean(RWI, na.rm=TRUE)) %>% spread(droughtclass5, meanBAI)

# now calculate resistance, resilience, and recovery
drought.df.1934.3 <- drought.df.3 %>% mutate(Resistance =  Dr/PreDr, 
                                         Recovery = PostDr/Dr,
                                         Resilience = PostDr/PreDr)

drought.df.1934.5 <- drought.df.5 %>% mutate(Resistance =  Dr/PreDr, 
                                             Recovery = PostDr/Dr,
                                             Resilience = PostDr/PreDr)

ggplot(drought.df.1934.5, aes(site, Resistance))+geom_boxplot()+theme_bw()+geom_hline(yintercept = 1)+ylim(0,2)
ggplot(drought.df.1934.5, aes(site, Recovery))+geom_boxplot()+theme_bw()+geom_hline(yintercept = 1)+ylim(0,2)
ggplot(drought.df.1934.5, aes(site, Resilience))+geom_boxplot()+theme_bw()+geom_hline(yintercept = 1)+ylim(0,2)

#-------------------------------------------------------------------------------
# add the location data to the BAI data and save as csv:
#-------------------------------------------------------------------------------

det.age.clim.prism.df.locs <- merge(det.age.clim.prism.df, locs, by.x = "site", by.y = "code")
det.age.clim.ghcn.df.locs <- merge(det.age.clim.ghcn.df, locs, by.x = "site", by.y = "code")

write.csv(det.age.clim.prism.df.locs, "outputs/data/RWI_age_prism.df")
write.csv(det.age.clim.ghcn.df.locs, "outputs/data/RWI_age_ghcn.df")

#-------------------------------------------------------------------------------
# get tree sizes from field data:
#-------------------------------------------------------------------------------
read_DBH_year <- function( filename, site){
  
  sitecode <- site
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
  mult.core.sites <- c("TOW", "COR", "HIC", "STC", "MOU", "ENG", "PVC", "HIC","UNI", "BON", "PLE",  "GLL1", "GLL2", "GLL3", "GLL4", "GLA")
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
                  if(site %in% "GLA"){
                    colnames(gp.treeMean2) <- paste0(site, colnames(gp.treeMean2))
                    GLA.table <- read.csv("crns/GLA_translation_table.csv")
                    new.cols <- merge(data.frame(RWL= colnames(gp.treeMean2), order = 1:length(colnames(gp.treeMean2))), GLA.table, by = "RWL")
                    colnames(gp.treeMean2) <- new.cols[order(new.cols$order),]$ID
                    
                    }else{
                  colnames(gp.treeMean2) <- paste0(site, colnames(gp.treeMean2))
                  
                }}}}}}
    
    newseries <- gp.treeMean2
    
    site.data <- read.csv(paste0("data/site_maps/all_metadata/", site, "_full_xy.csv"))
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
      
      
    }
    else{
      if(site %in% "GLA"){
        diams <- site.data[c("short", "DBH",  "SpecCode")]
        diams$DBH <- as.numeric(as.character(diams$DBH))
        diams.agg <- diams
        spec <- site.data[complete.cases(site.data[,c("short", "SpecCode")]),c("short", "SpecCode")]
        spec <- spec[!duplicated(spec),]
        diams.agg <- merge(diams.agg, spec, by =c("short", "SpecCode"))
        diams <- diams.agg[,c("short", "DBH", "SpecCode")]
        diams$DBH <- c(diams$DBH) # may need to subtract ~2cm for barkwidth 
        colnames(diams) <- c("ID", "DBH", "SpecCode") 
        
        
        # only find records where we have both DBH and tellervo entries:
        # writecsv with tree rwl that are missing for each site so we can check these:
        not.in.rwl <- diams [!diams$ID %in% colnames(newseries),]
        if(length(not.in.rwl$ID) > 0){ # if there are any records missing, make a csv output
          write.csv(not.in.rwl, paste0("data/site_stats/", site, "-IDS_not_in_tellervo.csv"))
        }
        
        
        diams <- diams [diams$ID %in% colnames(newseries),]
        diams <- diams[! duplicated(diams),]
        newseries <- newseries[,colnames(newseries) %in% diams$ID]
        newseries <- newseries[,colnames(newseries) %in% diams$ID]
        write.csv(diams, paste0("outputs/DBH/species_codes_", sitecode, ".csv"))
      
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
    }}
    
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
    site.data <- read.csv(paste0("data/site_maps/all_metadata/", site, "_full_xy.csv"))
    
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
    if (!is.null(diam )) {
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
  
  # write out the species and diameter and ID to files for use later:
  diams$site <- site
  write.csv(diams, paste0("outputs/data/site_dbh_spec/", site,".csv"), row.names = FALSE)
  
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
Glacial.DBH <- read_DBH_year(filename= "cleanrwl/GLAww.rwl",  site = "GLA") # messed up and DBH not averaged ring
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


# fixing some ID's with UNI:
test.uni<- det.age.clim.ghcn.df[det.age.clim.ghcn.df$site %in% "UNI",]$ID
det.age.clim.ghcn.df[det.age.clim.ghcn.df$site %in% "UNI",]$ID<- substr(test.uni, 1, 4)
det.age.clim.prism.df[det.age.clim.prism.df$site %in% "UNI",]$ID<- substr(test.uni, 1, 4)




# merge the diameter class df with the climate/growth dataframes:
det.age.clim.ghcn.dbh.df <- merge(det.age.clim.ghcn.df, dbh.class.df, by = c("year", "site", "ID"))
det.age.clim.prism.dbh.df <- merge(det.age.clim.prism.df, dbh.class.df, by = c("year", "site", "ID"))

# read in all of the diameter + site level data (output in the read_DBH_year function above):
files.spec <- paste0("outputs/data/site_dbh_spec/",list.files("outputs/data/site_dbh_spec/"))

data.spec <- lapply(files.spec, read.csv)

spec <- do.call(rbind, data.spec)
det.age.clim.class.ghcn.dbh.df <- merge(det.age.clim.ghcn.dbh.df, spec[,c("ID", "SpecCode")], by = "ID")
det.age.clim.class.prism.dbh.df <- merge(det.age.clim.prism.dbh.df, spec[,c("ID", "SpecCode")], by = "ID")


ggplot(det.age.clim.class.ghcn.dbh.df, aes(year, RWI, color = ageclass))+geom_point(size = 0.2)+stat_smooth()+facet_wrap(~site)

write.csv(det.age.clim.class.ghcn.dbh.df, "outputs/data/rwi_age_dbh_ghcn.v2.csv")
write.csv(det.age.clim.class.prism.dbh.df, "outputs/data/rwi_age_dbh_prism.v2.csv")



# ggplot(det.age.clim.class.ghcn.dbh.df, aes(DBH, RWI, color = site))+geom_point(size = 0.2)+stat_smooth()+facet_wrap(~SpecCode)
# 
# 
# ggplot(det.age.clim.class.ghcn.dbh.df, aes(JUNTmin, log(RWI), color = SpecCode))+geom_point(size = 0.2)+stat_smooth()+facet_wrap(~site)
# ggplot(det.age.clim.class.ghcn.dbh.df, aes(SP01_7, log(RWI), color = SpecCode))+geom_point(size = 0.2)+stat_smooth()+facet_wrap(~site, scales = "free_y")
# 
# ggplot(det.age.clim.class.ghcn.dbh.df, aes(year, RWI, color = dbhclass))+geom_point(size = 0.2)+stat_smooth()+facet_wrap(~site, scales = "free_y")
# 
full.ghcn <- merge(det.age.clim.class.ghcn.dbh.df, locs, by.x = "site", by.y = "code")
full.prism <- merge(det.age.clim.class.prism.dbh.df, locs, by.x = "site", by.y = "code")
# 
# ggplot(full.ghcn, aes(JJA.pdsi, log(RWI), color = pr30yr))+geom_point(size = 0.1 )+facet_wrap(~ BA)
# 
# png(height = 6, width = 7, units = "in", res = 200,"outputs/testing_BAI_DBH_rel_by_site.png")
# ggplot(full.ghcn, aes(DBH.x, log(RWI), color = site))+geom_point(size = 0.1 )+facet_wrap(~site)+geom_smooth(method = "lm")+geom_smooth(method = "lm", formula = y ~ log(x), color = "red")+geom_smooth(method = "lm", formula = y ~ x + I(x^2), color = "black")+ylab("log(BAI)")#+facet_wrap(~ site)
# dev.off()
# 
# 
# ggplot(full.ghcn, aes(DBH.x, log(RWI), color = ageclass))+geom_point(size = 0.1 )+facet_wrap(~ageclass)+geom_smooth(method = "lm")+geom_smooth(method = "lm", formula = y ~ log(x), color = "red")+ylab("log(BAI)")#+facet_wrap(~ site)
# 
# ggplot(full.ghcn, aes(Age, RWI, color = Description))+geom_point(size = 0.1 )+facet_wrap(~SpecCode)+stat_smooth(method = "lm", formula = y ~ x + I(x^2))+facet_wrap(~SpecCode)
# ggplot(full.ghcn, aes(DBH.x, log(RWI), color = JJA.pdsi))+geom_point(size = 0.01 )+facet_wrap(~site)+stat_smooth(method = "lm", formula = y ~ x + I(x^2))+scale_color_continuous(low = "red", high = "blue")
# 
# 
# png(height = 6, width = 7, units = "in", res = 200,"outputs/testing_BAI_AGE_rel_by_species.png")
# ggplot(full.ghcn, aes(Age, RWI, color = Description))+geom_point(size = 0.1 )+stat_smooth(method = "lm", formula = y ~ x + I(x^2))+facet_wrap(~SpecCode)
# dev.off()
# 
# 
# test.m<- merge(full.ghcn, est.dates, by = c("site", "ID"))
# 
# png(height= 5, width = 5, units = "in", res = 300, "outputs/three_age_class_bai_yr.png")
# ggplot(test.m[test.m$RWI <= 6200,], aes(Year, RWI, color = class))+geom_point(size = 0.01)+ stat_smooth()+theme_bw()+ylab("BAI")
# dev.off()
# 
# png(height= 5, width = 5, units = "in", res = 300, "outputs/three_age_class_bai_age.png")
# ggplot(test.m[test.m$RWI <= 6200,], aes(Age, RWI, color = class))+geom_point(size = 0.001)+ stat_smooth()+theme_bw()+ylab("BAI")
# dev.off()
# 
# png(height= 5, width = 7, units = "in", res = 300, "outputs/two_age_class_logbai_past_mod.png")
# ggplot(full.ghcn[full.ghcn$RWI <= 6200, ], aes(DBH.x, log(RWI), color = ageclass))+geom_point(size = 0.02)+geom_smooth(method = "lm")+facet_wrap(~fortype)
# dev.off()
# 
# png(height= 5, width = 7, units = "in", res = 300, "outputs/two_age_bai_dbh.png")
# ggplot(full.ghcn[full.ghcn$RWI <= 6200, ], aes(DBH.x, RWI, color = ageclass))+geom_point(size = 0.02)+geom_smooth()+facet_wrap(~fortype)+ylab("Log(BAI)")
# dev.off()
# 
# ggplot(test.m, aes(Age, RWI, color = class))+geom_point(size = 0.01)+ stat_smooth()+facet_wrap(~SpecCode)
# 
# ggplot(test.m, aes(DBH.x, RWI, color = class))+geom_point(size = 0.01)+ stat_smooth()
# ggplot(test.m, aes(DBH.x, RWI, color = class))+geom_point(size = 0.01)+ stat_smooth()+facet_wrap(~site)
# 
# 
# ggplot(test.m, aes(DBH.x, RWI, color = class))+geom_point(size = 0.1 )+stat_smooth(method = "lm", formula = y ~ x + I(x^2))+facet_wrap(~SpecCode)
# 
# ggplot(full.ghcn, aes( log(RWI), SP24_7, color = Description))+geom_point(size = 0.1 )+stat_smooth(method = "lm", formula = y ~ x + I(x^2))+facet_wrap(~SpecCode)
# 

write.csv(full.ghcn, "outputs/full.ghcn.csv", row.names = FALSE)

# merge with full climate dataset:
# ghcn.rwi <- merge( full.ghcn[,c("ID", "year", "site", "DBH", "dbhclass", "ageclass", "SpecCode", "RWI", "RWI_1", "RWI_2", "RWI_3")], ghcn.df, by.x = c("year", "site"), by.y = c("Year", "site"))
# prism.rwi <- merge( full.ghcn[,c("ID", "year", "site", "DBH", "dbhclass", "ageclass", "SpecCode", "RWI", "RWI_1", "RWI_2", "RWI_3")], prism.df, by.x = c("year", "site"), by.y = c("Year", "site"))
full.ghcn.red <- full.ghcn[,c("ID", "year", "site", "DBH.x","dbhclass",  "ageclass", "SpecCode", "RWI", "RWI_1", "RWI_2", "RWI_3")]
colnames( full.ghcn.red )[4] <- "DBH"
ghcn.rwi <- merge( full.ghcn.red , ghcn.df, by.x = c("year", "site"), by.y = c("Year", "site"))
prism.rwi <- merge( full.ghcn.red , prism.df, by.x = c("year", "site"), by.y = c("Year", "site"))


write.csv(ghcn.rwi, "outputs/full_ghcn_all_months_rwi_v2.csv", row.names = FALSE)
write.csv(prism.rwi, "outputs/full_prism_all_months_rwi_v2.csv", row.names = FALSE)

