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
#getting gridded climate data
#need to clean this up

precip.1900<- read.table("./data/precip_2014/precip.1900")
precip.1901<- read.table("./data/precip_2014/precip.1901")
precip.1902<- read.table("./data/precip_2014/precip.1902")
precip.1903<- read.table("./data/precip_2014/precip.1903")
precip.1904<- read.table("./data/precip_2014/precip.1904")
precip.1905<- read.table("./data/precip_2014/precip.1905")
precip.1906<- read.table("./data/precip_2014/precip.1906")
precip.1907<- read.table("./data/precip_2014/precip.1907")
precip.1908<- read.table("./data/precip_2014/precip.1908")
precip.1909<- read.table("./data/precip_2014/precip.1909")
precip.1910<- read.table("./data/precip_2014/precip.1910")
precip.2010<- read.table("./data/precip_2014/precip.2010")

Lat <- precip.1900[,2]
Long <- precip.1900[,1]

p.1900<- rowSums(precip.1900[,3:14], na.rm = TRUE)
p.1901<- rowSums(precip.1901[,3:14], na.rm = TRUE)
p.1902<- rowSums(precip.1902[,3:14], na.rm = TRUE)
p.1903<- rowSums(precip.1903[,3:14], na.rm = TRUE)
p.1904<- rowSums(precip.1904[,3:14], na.rm = TRUE)
p.1905<- rowSums(precip.1905[,3:14], na.rm = TRUE)
p.1906<- rowSums(precip.1906[,3:14], na.rm = TRUE)
p.1907<- rowSums(precip.1907[,3:14], na.rm = TRUE)
p.1908<- rowSums(precip.1908[,3:14], na.rm = TRUE)
p.1909<- rowSums(precip.1909[,3:14], na.rm = TRUE)
p.1910<- rowSums(precip.1910[,3:14], na.rm = TRUE)

avg.p<- rowMeans(data.frame(p.1900,
                            p.1901, 
                            p.1902,
                            p.1903, 
                            p.1904, 
                            p.1905, 
                            p.1906, 
                            p.1907,
                            p.1908,
                            p.1909, 
                            p.1910))/11

averages <- data.frame(Lat = Lat, 
                       Long = Long, 
                       avg = avg.p)

coordinates(averages) <- ~Long + Lat
gridded(averages) <- TRUE
avg.rast <- raster(averages)
projection(avg.rast) <- CRS("+init=epsg:4326")

avg.alb <- projectRaster(avg.rast, crs='+init=epsg:3175')
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
