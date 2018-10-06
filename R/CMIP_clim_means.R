library(raster)
library(sp)
library(rgeos)
library(ggplot2)
library(reshape2)
library(ncdf4)
library(lubridate)

setwd("/Users/kah/Documents/TreeRings/")
# get average future climate projections for RCP 8.5:

rcp <- "85"
climate <- "pr"

# read in the site level data we are using to model:
full.ghcn <- read.csv("outputs/data/rwi_age_dbh_ghcn.df")
sites <- unique(full.ghcn$site)
  
# define a function to extract rcp data for each of these sites:
extract.site.rcps <- function(climate, rcp, sites, time){
  
  setwd(paste0('/Users/kah/Documents/bimodality/data/cc',rcp,climate,time,'/'))
  tr_sites <- read.csv("/Users/kah/Documents/TreeRings/outputs/priority_sites_locs.csv")
  tr_sites$site <- c("AVO", "BAC", "BON","BOO", "CAC", "COR", "DUF", "ENG", "GLL1", "GLL2", "GLL3", "GLL4", 
                     "GLA", "HIC", "LED", "MOU", "PAM", "PLE", "PVC","STC", "TOW", "UNC", "UNI")
  # select sites of interest:
  tr_sites <- tr_sites[tr_sites$site %in% sites,]
  
  coordinates(tr_sites) <- ~coords.x1 +coords.x2
  proj4string(tr_sites) <- '+init=epsg:3175' # tr_sites is currently in great lakes albers + we need to re-project
  tr_sites.ll <- spTransform(tr_sites, crs('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0 '))
  
  month <- sprintf("%02d", 1:12)
  month.abb <- c('Jan', 'Oct', 'Nov', "Dec","Feb","Mar","Apr", "May", 
                 'Jun', "Jul", "Aug", "Sep")
  filenames <- list.files(pattern=paste0("cc",rcp,climate,time,".*\\.tif$", sep = ""))
  s <- stack(filenames)
  #t <- crop(s, extent(tr_sites.ll))#make all into a raster
  t <- crop(s, extent((matrix(c(-97,  40, -87,48), nrow=2))))#make all into a raster
  s <- projectRaster(t, crs='+init=epsg:3175') # project in great lakes albers
  #crop to the extent of indiana & illinois 
  y <- data.frame(rasterToPoints(s)) #covert to dataframe
  
  colnames(y) <- c("x", "y", month.abb)
  y$gridNumber <- cellFromXY(s, y[, 1:2])
  #write.csv(y ,paste0('C:/Users/JMac/Documents/Kelly/biomodality/outputs/ccsm4_2.6_precip.csv' ))
  
  full <- y
  
  if(climate == 'pr'){
    full$total<- rowSums(full[,3:14], na.rm=TRUE)
    full$SI <- rowSums(abs(full[,3:14]-(full[,16]/12)))/full[,16]
    
  }else{
    full[,3:14] <- full[,3:14]/10
    full$mean <- rowMeans((full[,3:14]), na.rm = TRUE)
    full$meanGS <- rowMeans((full[,c( "Jun", "Jul")]), na.rm = TRUE)
    mean.corr <- full$mean
    mean.corr[abs(mean.corr) < 0.8 ] <- 0.8 # assign all mean values near 0 to 0.8 to avoid the cv blowing up
    full$SI <- (abs(apply((full[,3:14]),1, sd, na.rm = TRUE))/abs((mean.corr)))
    full$cv <- (apply(full[,3:14],1, sd, na.rm = TRUE)/full[,15])*100
  }
  
  coordinates(full) <- ~x + y
  gridded(full) <- TRUE
  avgs <- stack(full) 
  
  #plot(avgs) #plots averages
  
  tr_sites <- data.frame(tr_sites)
  avgs.df <- data.frame(x = tr_sites$coords.x1, y =tr_sites$coords.x2)
  if(climate == "pr"){
    avgs.df$total <- raster::extract(avgs$total, avgs.df[,c("x","y")])
    avgs.df$SI <- raster::extract(avgs$SI, avgs.df[,c("x","y")])
    colnames(avgs.df) <- c('x', "y", paste0(climate,"-", rcp), paste0(climate,'-',rcp,'SI')) 
  }else{
    avgs.df$mean <- raster::extract(avgs$mean, avgs.df[,c("x","y")])
    
    avgs.df$SI <- raster::extract(avgs$SI, avgs.df[,c("x","y")])
    avgs.df$meanGS <- raster::extract(avgs$meanGS, avgs.df[,c("x","y")])
    colnames(avgs.df) <- c('x', "y", paste0(climate,"-", rcp), paste0(climate,'-',rcp,'cv'), paste0(climate,'-',rcp,'GS')) 
    
  }
  avgs.df$site <- tr_sites$site
  avgs.df
}

# predictions for the 2050s:
pr85.50 <- extract.site.rcps ("pr", "85", sites = sites, time = "50")
Tmax.50 <- extract.site.rcps ("tx", "85", sites = sites, time = "50")
Tmin.50 <- extract.site.rcps ("tn", "85", sites = sites, time = "50")

# predictions for the 2070s
pr85.70 <- extract.site.rcps ("pr", "85", sites = sites, time = "70")
Tmax.70 <- extract.site.rcps ("tx", "85", sites = sites, time = "70")
Tmin.70 <- extract.site.rcps ("tn", "85", sites = sites, time = "70")

# need to convert to C and then F
toFahrenheit = function(celsius) {
  f = (9/5) * celsius + 32; 
}

Tmax.50$`tx-85` <- toFahrenheit(Tmax.50$`tx-85`)
Tmax.50$`tx-85cv` <- toFahrenheit(Tmax.50$`tx-85cv`)
Tmax.50$`tx-85GS` <- toFahrenheit(Tmax.50$`tx-85GS`)

Tmax.70$`tx-85` <- toFahrenheit(Tmax.70$`tx-85`)
Tmax.70$`tx-85cv` <- toFahrenheit(Tmax.70$`tx-85cv`)
Tmax.70$`tx-85GS` <- toFahrenheit(Tmax.70$`tx-85GS`)

Tmin.50$`tn-85` <- toFahrenheit(Tmin.50$`tn-85`)
Tmin.50$`tn-85cv` <- toFahrenheit(Tmin.50$`tn-85cv`)
Tmin.50$`tn-85GS` <- toFahrenheit(Tmin.50$`tn-85GS`)

Tmin.70$`tn-85` <- toFahrenheit(Tmin.70$`tn-85`)
Tmin.70$`tn-85cv` <- toFahrenheit(Tmin.70$`tn-85cv`)
Tmin.70$`tn-85GS` <- toFahrenheit(Tmin.70$`tn-85GS`)


# compile Tmaxes and precips into one data fram with a site column:
rcp8.5_2070 <- merge(Tmax.70, pr85.70, by = c("site", "x","y"))
rcp8.5_2050 <- merge(Tmax.50, pr85.50, by = c("site", "x","y"))
colnames(rcp8.5_2050)[4:8] <- c("Tx_85_50", "Tx_85_50cv", "Tx_85_50GS",
                                "Pr_85_50", "Pr_85_50si")

colnames(rcp8.5_2070)[4:8] <- c("Tx_85_70", "Tx_85_70cv", "Tx_85_70GS",
                                "Pr_85_70", "Pr_85_70si")

setwd("/Users/kah/Documents/TreeRings/")
rcp85 <- merge(rcp8.5_2050, rcp8.5_2070, by = c("site", "x","y"))

ggplot(rcp85, aes(Tx_85_70GS, Pr_85_70, color = site))+geom_point()

write.csv(rcp85, "outputs/rcp8.5_mean_Pr_TMAX_proj_sites.csv", row.names = FALSE)
