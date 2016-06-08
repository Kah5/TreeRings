#make a map of sites across midwest

library(sp)  # classes for spatial data
library(raster)  # grids, rasters
library(rasterVis)  # raster visualisation
library(maptools)
library(rgeos)
library(ggplot2)
library(rgdal)

MN.priority <- readOGR("data/Fieldwork 2016.kml", layer = "Priority")
IL.MCCD <- readOGR('data/Fieldwork 2016.kml', layer = "McHenry")

MN.priority <- spTransform(MN.priority, CRSobj = CRS('+init=epsg:3175'))
priority <- data.frame(MN.priority)
IL.MCCD <- spTransform(IL.MCCD, CRSobj = CRS('+init=epsg:3175'))
IL.MCCD <- data.frame(IL.MCCD)

priority <- rbind(priority, IL.MCCD)

#use avg.alb from extract_PT.R

#plot(avg.alb)


#map out 
all_states <- map_data("state")
states <- subset(all_states, region %in% c(  "illinois", "minnesota", "wisconsin" ) )
coordinates(states)<-~long+lat
class(states)
proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
mapdata<-spTransform(states, CRS('+init=epsg:3175'))

test<- crop(avg.alb, extent(mapdata))
test.df <- as.data.frame(test, xy = TRUE)
#avg.alb <- as.data.frame(avg.alb, xy = TRUE)

mapdata<-data.frame(mapdata)

#make the map in GGPLOT

sites.map <- ggplot()+ geom_raster(data=test.df, aes(x=x, y=y, fill = avg))+
  labs(x="easting", y="northing", title="Tree Core Sites") + 
  scale_fill_gradientn(colours = rainbow(4), name ="MAP 1900-1910 (cm) ")
sites.map <- sites.map + geom_point(data = priority, aes(x = coords.x1, y = coords.x2, shape = Description))#+ geom_text(hjust=0, vjust=0)
sites.map <- sites.map +geom_polygon(data=data.frame(mapdata), aes(x=long, y=lat, group=group),colour = "Black", fill = NA)

pdf("core_sites_2016.pdf")              
sites.map

dev.off()
