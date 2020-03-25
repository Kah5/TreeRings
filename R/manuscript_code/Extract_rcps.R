library(raster)
library(sp)
library(rgeos)
library(ggplot2)
library(reshape2)
library(ncdf4)
library(lubridate)

# get average future climate projections for RCP 8.5:

rcp <- "85"
climate <- "pr"


extract.site.rcps <- function(climate, rcp, sites){
  
  setwd(paste0('/Users/kah/Documents/bimodality/data/cc',rcp,climate,'70/'))
  spec.table <- read.csv('/Users/kah/Documents/bimodality/data/midwest_pls_full_density_pr_alb1.7-5.csv')
  coordinates(spec.table) <- ~x +y
  proj4string(spec.table) <- '+init=epsg:3175'
  spec.table.ll <- spTransform(spec.table, crs('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0 '))
  
  month <- sprintf("%02d", 1:12)
  month.abb <- c('Jan', 'Oct', 'Nov', "Dec","Feb","Mar","Apr", "May", 
                 'Jun', "Jul", "Aug", "Sep")
  filenames <- list.files(pattern=paste0("cc",rcp,climate,"70",".*\\.tif$", sep = ""))
  s <- stack(filenames)
  t <- crop(s, extent(spec.table.ll))#make all into a raster
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
    mean.corr <- full$mean
    mean.corr[abs(mean.corr) < 0.8 ] <- 0.8 # assign all mean values near 0 to 0.8 to avoid the cv blowing up
    full$SI <- (abs(apply((full[,3:14]),1, sd, na.rm = TRUE))/abs((mean.corr)))
    full$cv <- (apply(full[,3:14],1, sd, na.rm = TRUE)/full[,15])*100
  }
  
  coordinates(full) <- ~x + y
  gridded(full) <- TRUE
  avgs <- stack(full) 
  
  
  #plot(avgs) #plots averages
  
  spec.table <- data.frame(spec.table)
  avgs.df <- data.frame(x = spec.table$x, y =spec.table$y)
  if(climate == "pr"){
    avgs.df$total <- raster::extract(avgs$total, spec.table[,c("x","y")])
    avgs.df$SI <- raster::extract(avgs$SI, spec.table[,c("x","y")])
    colnames(avgs.df) <- c('x', "y", paste0(climate,"-", rcp), paste0(climate,'-',rcp,'SI')) 
  }else{
    avgs.df$mean <- raster::extract(avgs$mean, spec.table[,c("x","y")])
    avgs.df$SI <- raster::extract(avgs$SI, spec.table[,c("x","y")])
    colnames(avgs.df) <- c('x', "y", paste0(climate,"-", rcp), paste0(climate,'-',rcp,'cv')) 
    
  }
  avgs.df
}

pr85 <- extract.rcps ("pr", "85")
