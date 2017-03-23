# This script will read .gpx files with coordinates of plot centers and or mapped trees, and create stand maps
library(maptools)
library(sp)  # classes for spatial data
library(raster)  # grids, rasters
library(rasterVis)  # raster visualisation
library(maptools)
library(rgeos)
library(ggplot2)
library(rgdal)
library(ggrepel)
library(grid)
#library(plotKML)
library(plotKML)
library(dplyr)
# read in a gpx file
test <- readGPX("C:/Users/JMac/Box Sync/GPX/Waypoints_06-AUG-16.gpx")


# put all the files into the same directory, then

myfiles <- list.files("C:/Users/JMac/Box Sync/GPX/2016/")
mydir <- 'C:/Users/JMac/Box Sync/GPX/2016/'


wpfull <- NULL

for (i in 1:length(myfiles)) {
  # - Select first file from the list and import data into R object
  wplist <- readGPX( paste0(mydir,myfiles[i]), way=T)
  # extract latitude, longituDe, elevation, time, name and comments and apppend to R dataframe
  wpdf<- wplist$waypoints
  # append dataframe from last index to a full waypoint object
  wpfull <- bind_rows(wpfull, wpdf)
}

head(wpfull)
wpfull$code <- substr(wpfull$name, 1, 3)


# plot out 
ggplot(data = wpfull, aes(x = lon, y = lat))+geom_point()

# get non-exact lat longs
mound <- readOGR("data/Treecores.kml", layer= "2015sites")
#mound.lat <- mound
#mound <- spTransform(mound, CRSobj = CRS('+init=epsg:3175'))
mound <- data.frame(mound)
mound.lat <- data.frame(mound)
mound$code <- c("LED", "MOU", "BON", "UNI", "PAM", "ENG", "BAC", "CAC", "GLA", "BOO","DUF", "PLE")
mound.lat$code <- c("LED", "MOU", "BON", "UNI", "PAM", "ENG", "BAC", "CAC", "GLA", "BOO","DUF", "PLE")
mound <- rbind(mound, priority[priority$code %in% c("TOW", "HIC", "STC"),]) # add townsend woods




MN.priority <- readOGR("data/priority.kml", layer = "Priority")
#read in layer of sites cored in 2016
sites16 <- readOGR("data/Treecores.kml", layer= "2016sites")
sites16.lat <- sites16
#sites16 <- spTransform(sites16, CRSobj = CRS('+init=epsg:3175'))
sites16 <- data.frame(sites16)
sites16.lat <- data.frame(sites16.lat)
sites16$code <- c("COR", "PVC", "UNC", "ITA", "AVO", "GLE", "MAP", "GLL")
sites16.lat$code <- c("COR", "PVC", "UNC", "ITA", "AVO", "GLE", "MAP", "GLL")
sites16$Description <- c("Forest", "Savanna", "Savanna", 
                         "Forest", "Savanna & Forest", "Savanna & Forest", "Savanna & Forest", "Savanna & Forest")
# merge the two data sets using rbind
priority <- rbind(mound, sites16)
colnames(priority) <- c("Name", "Description", "lon.coarse", "lat.coarse", 'elev', "optional", "code")


coarse <- priority[,c("lon.coarse", 'lat.coarse', 'code', "Description")]
waypt <- wpfull[,c('lon', 'lat', 'code', 'name')]


# read in lat long from 2015 sites
sites15 <- readOGR("data/Cored trees.kml", layer = "Trees_sites")

sites15.lat <- sites15
#sites16 <- spTransform(sites16, CRSobj = CRS('+init=epsg:3175'))
sites15 <- as.data.frame(sites15)
colnames(sites15) <- c('name', "Description", "lon", "lat", "ele")
sites15$name <- as.character(sites15$name)
sites15$code <- substr(sites15$name, 1, 3)
sites15$time <- NA
sites15$sym <- NA

# reorder
sites15<- sites15[,c("lon", "lat", "ele", "time", "name", "sym", "code")]

# add onto wpfull:
wpfull <- rbind(wpfull, sites15)

# merge the datasets together
full <- merge(coarse, waypt, by = "code", all = TRUE)


ggplot(full, aes(lon.coarse, lat.coarse))+geom_point()

#map out 
# need to fix the projections so that they match up
all_states <- map_data("state")
states <- subset(all_states, region %in% c(  "illinois", "minnesota", "wisconsin", "iowa", 
                                           'michigan', 'missouri', 'indiana') )
coordinates(states)<-~long+lat
class(states)
proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
#mapdata<-spTransform(states, CRS('+init=epsg:3175'))
mapdata<-data.frame(states)


# map out the sites
sites.map <- ggplot()+ geom_point(data = full, aes(x = lon.coarse, y = lat.coarse), cex = 2.5)+
  geom_polygon(data=data.frame(mapdata), aes(x=long, y=lat, group=group),
                                     colour = "darkgrey", fill = NA)+theme_bw() +
  theme(axis.text = element_text(size = 14),
        axis.text.y = element_text(angle = 90, size = rel(0.7), hjust = 0.75),
        legend.key = element_rect(),
        legend.background = element_rect(fill = "white"),
        
        panel.grid.major = element_line(colour = "grey40"),
        panel.grid.minor = element_blank()) + theme_bw()+ coord_map()
sites.map

# write out the full data to csv
write.csv(full, "C:/Users/JMac/Documents/Kelly/TreeRings/outputs/lat_long_sites.csv")

#####################################
# read in metadata from a stand#
#####################################

map.plot <- function(sitecode){
  
  site <- read.csv(paste0("data/site_maps/stand_metadata/", sitecode,"_metadata.csv"))
  site$name <- sitecode
  site <- merge(site, wpfull[,c('lon', 'lat', 'ele','name')], by = 'name')


  # convert lat long to albers projection:
  coordinates(site) <- ~lon +lat
  proj4string(site) <- '+init=epsg:4326' # define native proj
  site.alb <- spTransform(site, CRS('+init=epsg:3175')) # converte to albers
  site.alb <- data.frame(site.alb)

  # function converts degrees to radians, since R deals with radians
  as_radians <- function(deg) {(deg * pi) / (180)}

  # find X-y coordinates of the trees within the plots:
  site.alb$x_tree <- site.alb$lon + cos(as_radians(site.alb$direction))*(site.alb$dist2center + (0.5*(site.alb$DBH..cm./100)))
  site.alb$y_tree <- site.alb$lat + sin(as_radians(site.alb$direction))*(site.alb$dist2center + (0.5*(site.alb$DBH..cm./100)))


  ggplot(site.alb, aes(x = x_tree, y = y_tree, color = Species, size = DBH..cm.))+geom_point() + theme_bw()


  circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
  }

  dat <- circleFun(c(site.alb[1,]$lon,site.alb[1,]$lat),30,npoints = 100)
  #geom_path will do open circles, geom_polygon will do filled circles
  specColors
  species <-c("Bur Oak", "White Oak", "Red Oak", "Chinkapin Oak", "Shagbark Hickory","Sugar Maple",
              "Red Maple",
              "Basswood", "Quaking Aspen", "Aspen","Red Pine", "White Pine", "White Spruce", 
              "Green Ash", "Black Cherry", "Hophornbeam","Ironwood","Standing Dead")
    
  ggplot()+ geom_point(data = site.alb, aes(x = x_tree, y = y_tree, color = Species, size = DBH..cm.)) + 
    #scale_color_manual(values = specColors)
    theme_bw() + geom_path(data = dat, aes(x=x,y=y)) + ggtitle(paste(sitecode, "plot map"))
}

map.plot("ITA1")
map.plot("ITA2")
map.plot("GLL1")
map.plot("GLL2")
map.plot("GLL3")
map.plot("GLL4")
map.plot("UNC1")
map.plot("AVO")
map.plot("PVC")
map.plot("GLE1")
map.plot("COR1")
map.plot("DUF-1")
map.plot("DUF-2")
map.plot("GLA-2")


########################################
# we have some dispersed sampling sites#
########################################

# for 2016: MAP3, MAP4, GLE2
# for 2015: 

map.dispersed <- function(sitecode){
  
  site <- read.csv(paste0("data/site_maps/dispersed_metadata/",sitecode,"_metadata.csv"))
  
  site$code <- sitecode
  site$name <- paste0(sitecode, "-", as.character(site$TagID))
  site <- merge(site, wpfull[,c('lon', 'lat', 'ele','name')], by = 'name')

  # convert lat long to albers projection:
  coordinates(site) <- ~lon +lat
  proj4string(site) <- '+init=epsg:4326' # define native proj
  site.alb <- spTransform(site, CRS('+init=epsg:3175')) # converte to albers
  site.alb <- data.frame(site.alb)

  ggplot()+ geom_point(data = site.alb, aes(x = lon, y = lat, color = Species, size = DBH..cm.)) +
    theme_bw()
}
map.dispersed("MAP3")
map.dispersed("TOW")
# map4