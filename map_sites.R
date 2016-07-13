#make a map of sites across midwest

library(sp)  # classes for spatial data
library(raster)  # grids, rasters
library(rasterVis)  # raster visualisation
library(maptools)
library(rgeos)
library(ggplot2)
library(rgdal)
library(ggrepel)

MN.priority <- readOGR("data/Fieldwork 2016.kml", layer = "Priority")
IL.MCCD <- readOGR('data/Fieldwork 2016.kml', layer = "McHenry")

MN.priority <- spTransform(MN.priority, CRSobj = CRS('+init=epsg:3175'))
priority <- data.frame(MN.priority)

priority$code <- c('', "ITA", "GLN", "MAP", "LAW", "MSC", "UNC", "BJP", "AVH", "GLK")
IL.MCCD <- spTransform(IL.MCCD, CRSobj = CRS('+init=epsg:3175'))
IL.MCCD <- data.frame(IL.MCCD)
IL.MCCD$code <- c("GLA", "PLV", " ", "HAR", "BEC", " ", "ELN", "COR")
priority <- rbind(priority, IL.MCCD)


#for NAPC, create a map with just these tree cores:
priority <- readOGR('data/Treecores.kml', layer = "NAPCsites")
priority <- spTransform(priority, CRSobj = CRS('+init=epsg:3175'))
priority <- data.frame(priority)
priority$Names <- c('Pleasant Valley', "Townsend Woods", "Hickory Grove", "Bonanza Prairie")

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

coordinates(precip.1900) <- ~V1 + V2
gridded(precip.1900) <- TRUE
te <- stack(precip.1900)

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
                       avg = avg.p*10)

coordinates(averages) <- ~Long + Lat
gridded(averages) <- TRUE
avg.rast <- raster(averages)
projection(avg.rast) <- CRS("+init=epsg:4326")

avg.alb <- projectRaster(avg.rast, crs='+init=epsg:3175')
#plot(avg.alb)


#map out 
all_states <- map_data("state")
states <- subset(all_states, region %in% c(  "illinois", "minnesota", "wisconsin") )
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
  scale_fill_gradientn(colours = rainbow(4), name ="MAP 1900-1910 (mm) ")
sites.map <- sites.map +geom_polygon(data=data.frame(mapdata), aes(x=long, y=lat, group=group),colour = "darkgrey", fill = NA)

sites.map <- sites.map + geom_point(data = priority, aes(x = coords.x1, y = coords.x2, shape = Description), cex = 2.5)+
  geom_text_repel(data = priority,aes(x = coords.x1, y = coords.x2,label=Names),
                  fontface = 'bold', color = 'white',
                  box.padding = unit(0.25, "lines"),
                  point.padding = unit(1.0, "lines"))

pdf("outputs/NAPC_sites_2015.pdf")              
sites.map

dev.off()




#now map for temperature from GHCN data
air_temp.1900<- read.table("./data/air_temp_2014/air_temp.1900")
air_temp.1901<- read.table("./data/air_temp_2014/air_temp.1901")
air_temp.1902<- read.table("./data/air_temp_2014/air_temp.1902")
air_temp.1903<- read.table("./data/air_temp_2014/air_temp.1903")
air_temp.1904<- read.table("./data/air_temp_2014/air_temp.1904")
air_temp.1905<- read.table("./data/air_temp_2014/air_temp.1905")
air_temp.1906<- read.table("./data/air_temp_2014/air_temp.1906")
air_temp.1907<- read.table("./data/air_temp_2014/air_temp.1907")
air_temp.1908<- read.table("./data/air_temp_2014/air_temp.1908")
air_temp.1909<- read.table("./data/air_temp_2014/air_temp.1909")
air_temp.1910<- read.table("./data/air_temp_2014/air_temp.1910")
air_temp.2010<- read.table("./data/air_temp_2014/air_temp.2010")

Lat <- air_temp.1900[,2]
Long <- air_temp.1900[,1]

t.1900<- rowSums(air_temp.1900[,3:14], na.rm = TRUE)
t.1901<- rowSums(air_temp.1901[,3:14], na.rm = TRUE)
t.1902<- rowSums(air_temp.1902[,3:14], na.rm = TRUE)
t.1903<- rowSums(air_temp.1903[,3:14], na.rm = TRUE)
t.1904<- rowSums(air_temp.1904[,3:14], na.rm = TRUE)
t.1905<- rowSums(air_temp.1905[,3:14], na.rm = TRUE)
t.1906<- rowSums(air_temp.1906[,3:14], na.rm = TRUE)
t.1907<- rowSums(air_temp.1907[,3:14], na.rm = TRUE)
t.1908<- rowSums(air_temp.1908[,3:14], na.rm = TRUE)
t.1909<- rowSums(air_temp.1909[,3:14], na.rm = TRUE)
t.1910<- rowSums(air_temp.1910[,3:14], na.rm = TRUE)

avg.t<- rowMeans(data.frame(t.1900,
                            t.1901, 
                            t.1902,
                            t.1903, 
                            t.1904, 
                            t.1905, 
                            t.1906, 
                            t.1907,
                            t.1908,
                            t.1909, 
                            t.1910))/11

#make this into a datframe with correct lat long
averages.t <- data.frame(Lat = Lat, 
                       Long = Long, 
                       avg = avg.t)

coordinates(averages.t) <- ~Long + Lat
gridded(averages.t) <- TRUE
avg.t.rast <- raster(averages.t) #convert dataframe to a raster
projection(avg.t.rast) <- CRS("+init=epsg:4326") # native projection for GHCN data
avg.t.alb <- projectRaster(avg.t.rast, crs='+init=epsg:3175') # change to great lakes albers

#create states again
all_states <- map_data("state")
states <- subset(all_states, region %in% c(  "illinois", "minnesota", "wisconsin" ) )
coordinates(states)<-~long+lat
class(states)
proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
mapdata<-spTransform(states, CRS('+init=epsg:3175'))

test.t<- crop(avg.t.alb, mapdata)
test.t.df <- as.data.frame(test.t, xy = TRUE)


#make the map in GGPLOT

sites.t.map <- ggplot()+ geom_raster(data=test.t.df, aes(x=x, y=y, fill = avg))+
  labs(x="easting", y="northing", title="Tree Core Sites") + 
  scale_fill_gradientn(colours = rainbow(4), name ="Mean Temp. (DegC) ")
sites.t.map <- sites.t.map +geom_polygon(data=data.frame(mapdata), aes(x=long, y=lat, group=group),colour = "darkgrey", fill = NA)
sites.t.map <- sites.t.map + geom_point(data = priority, aes(x = coords.x1, y = coords.x2, shape = Description), cex = 2.5)+
  geom_text_repel(data = priority,aes(x = coords.x1, y = coords.x2,label=Names),
                  fontface = 'bold', color = 'white',
                  box.padding = unit(0.25, "lines"),
                  point.padding = unit(1.0, "lines"))


#################################################
#now map for potential evaporation from GHCN data
#################################################

Eo150.1900<- read.table("./data/Eo150_2014/Eo150.1900")
Eo150.1901<- read.table("./data/Eo150_2014/Eo150.1901")
Eo150.1902<- read.table("./data/Eo150_2014/Eo150.1902")
Eo150.1903<- read.table("./data/Eo150_2014/Eo150.1903")
Eo150.1904<- read.table("./data/Eo150_2014/Eo150.1904")
Eo150.1905<- read.table("./data/Eo150_2014/Eo150.1905")
Eo150.1906<- read.table("./data/Eo150_2014/Eo150.1906")
Eo150.1907<- read.table("./data/Eo150_2014/Eo150.1907")
Eo150.1908<- read.table("./data/Eo150_2014/Eo150.1908")
Eo150.1909<- read.table("./data/Eo150_2014/Eo150.1909")
Eo150.1910<- read.table("./data/Eo150_2014/Eo150.1910")
Eo150.2010<- read.table("./data/Eo150_2014/Eo150.2010")

Lat <- Eo150.1900[,2]
Long <- Eo150.1900[,1]

e.1900<- rowSums(Eo150.1900[,6:11], na.rm = TRUE)
e.1901<- rowSums(Eo150.1901[,6:11], na.rm = TRUE)
e.1902<- rowSums(Eo150.1902[,6:11], na.rm = TRUE)
e.1903<- rowSums(Eo150.1903[,6:11], na.rm = TRUE)
e.1904<- rowSums(Eo150.1904[,6:11], na.rm = TRUE)
e.1905<- rowSums(Eo150.1905[,6:11], na.rm = TRUE)
e.1906<- rowSums(Eo150.1906[,6:11], na.rm = TRUE)
e.1907<- rowSums(Eo150.1907[,6:11], na.rm = TRUE)
e.1908<- rowSums(Eo150.1908[,6:11], na.rm = TRUE)
e.1909<- rowSums(Eo150.1909[,6:11], na.rm = TRUE)
e.1910<- rowSums(Eo150.1910[,6:11], na.rm = TRUE)

avg.e<- rowMeans(data.frame(e.1900,
                            e.1901, 
                            e.1902,
                            e.1903, 
                            e.1904, 
                            e.1905, 
                            e.1906, 
                            e.1907,
                            e.1908,
                            e.1909, 
                            e.1910))/11

#make this into a datframe with correct lat long
averages.e <- data.frame(Lat = Lat, 
                         Long = Long, 
                         avg = avg.e)

coordinates(averages.e) <- ~Long + Lat
gridded(averages.e) <- TRUE
avg.e.rast <- raster(averages.e) #convert dataframe to a raster
projection(avg.e.rast) <- CRS("+init=epsg:4326") # native projection for GHCN data
avg.e.alb <- projectRaster(avg.e.rast, crs='+init=epsg:3175') # change to great lakes albers

#create states again
all_states <- map_data("state")
states <- subset(all_states, region %in% c(  "illinois", "minnesota", "wisconsin" ) )
coordinates(states)<-~long+lat
class(states)
proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
mapdata<-spTransform(states, CRS('+init=epsg:3175'))

test.e<- crop(avg.e.alb, extent(mapdata))
test.e.df <- as.data.frame(test.e, xy = TRUE)


#make the map in GGPLOT

sites.e.map <- ggplot()+ geom_raster(data=test.e.df, aes(x=x, y=y, fill = avg))+
  labs(x="easting", y="northing", title="Tree Core Sites") + 
  scale_fill_gradientn(colours = rainbow(4), name ="Mean Apr-Sept \n Potential Evaporation ")
sites.e.map <- sites.e.map +geom_polygon(data=data.frame(mapdata), aes(x=long, y=lat, group=group),colour = "darkgrey", fill = NA)
sites.e.map <- sites.e.map + geom_point(data = priority, aes(x = coords.x1, y = coords.x2, shape = Description), cex = 2.5)+
  geom_text_repel(data = priority,aes(x = coords.x1, y = coords.x2,label=Names),
                  fontface = 'bold', color = 'white',
                  box.padding = unit(0.25, "lines"),
                  point.padding = unit(1.0, "lines"))

#################################################
#now map for actual evaporation from GHCN data
#################################################

E150.1900<- read.table("./data/E150_2014/E150.1900")
E150.1901<- read.table("./data/E150_2014/E150.1901")
E150.1902<- read.table("./data/E150_2014/E150.1902")
E150.1903<- read.table("./data/E150_2014/E150.1903")
E150.1904<- read.table("./data/E150_2014/E150.1904")
E150.1905<- read.table("./data/E150_2014/E150.1905")
E150.1906<- read.table("./data/E150_2014/E150.1906")
E150.1907<- read.table("./data/E150_2014/E150.1907")
E150.1908<- read.table("./data/E150_2014/E150.1908")
E150.1909<- read.table("./data/E150_2014/E150.1909")
E150.1910<- read.table("./data/E150_2014/E150.1910")
E150.2010<- read.table("./data/E150_2014/E150.2010")

Lat <- E150.1900[,2]
Long <- E150.1900[,1]

et.1900<- rowSums(E150.1900[,6:11], na.rm = TRUE)
et.1901<- rowSums(E150.1901[,6:11], na.rm = TRUE)
et.1902<- rowSums(E150.1902[,6:11], na.rm = TRUE)
et.1903<- rowSums(E150.1903[,6:11], na.rm = TRUE)
et.1904<- rowSums(E150.1904[,6:11], na.rm = TRUE)
et.1905<- rowSums(E150.1905[,6:11], na.rm = TRUE)
et.1906<- rowSums(E150.1906[,6:11], na.rm = TRUE)
et.1907<- rowSums(E150.1907[,6:11], na.rm = TRUE)
et.1908<- rowSums(E150.1908[,6:11], na.rm = TRUE)
et.1909<- rowSums(E150.1909[,6:11], na.rm = TRUE)
et.1910<- rowSums(E150.1910[,6:11], na.rm = TRUE)

avg.et<- rowMeans(data.frame(et.1900,
                            et.1901, 
                            et.1902,
                            et.1903, 
                            et.1904, 
                            et.1905, 
                            et.1906, 
                            et.1907,
                            et.1908,
                            et.1909, 
                            et.1910))/11

#make this into a datframe with correct lat long
averages.et <- data.frame(Lat = Lat, 
                         Long = Long, 
                         avg = avg.et)

coordinates(averages.et) <- ~Long + Lat
gridded(averages.et) <- TRUE
avg.et.rast <- raster(averages.et) #convert dataframe to a raster
projection(avg.et.rast) <- CRS("+init=epsg:4326") # native projection for GHCN data
avg.et.alb <- projectRaster(avg.et.rast, crs='+init=epsg:3175') # change to great lakes albers

#create states again
all_states <- map_data("state")
states <- subset(all_states, region %in% c(  "illinois", "minnesota", "wisconsin" ) )
coordinates(states)<-~long+lat
class(states)
proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
mapdata<-spTransform(states, CRS('+init=epsg:3175'))

test.et<- crop(avg.et.alb, extent(mapdata))
test.et.df <- as.data.frame(test.et, xy = TRUE)


#make the map in GGPLOT

sites.et.map <- ggplot()+ geom_raster(data=test.et.df, aes(x=x, y=y, fill = avg))+
  labs(x="easting", y="northing", title="Tree Core Sites") + 
  scale_fill_gradientn(colours = rainbow(4), name ="Mean Apr.-Sept.\n actual et ")
sites.et.map <- sites.et.map +geom_polygon(data=data.frame(mapdata), aes(x=long, y=lat, group=group),colour = "darkgrey", fill = NA)
sites.et.map <- sites.et.map + geom_point(data = priority, aes(x = coords.x1, y = coords.x2, shape = Description), cex = 2.5)+
  geom_text_repel(data = priority,aes(x = coords.x1, y = coords.x2,label=Names),
                  fontface = 'bold', color = 'white',
                  box.padding = unit(0.25, "lines"),
                  point.padding = unit(1.0, "lines"))

#now calculet precip. - potential evaporation
PET.1900<- precip.1900[,3:14]-Eo150.1900[,3:14]
PET.1901<- precip.1901[,3:14]-Eo150.1901[,3:14]
PET.1902<- precip.1902[,3:14]-Eo150.1902[,3:14]
PET.1903<- precip.1903[,3:14]-Eo150.1903[,3:14]
PET.1904<- precip.1904[,3:14]-Eo150.1904[,3:14]
PET.1905<- precip.1905[,3:14]-Eo150.1905[,3:14]
PET.1906<- precip.1906[,3:14]-Eo150.1906[,3:14]
PET.1907<- precip.1907[,3:14]-Eo150.1907[,3:14]
PET.1908<- precip.1908[,3:14]-Eo150.1908[,3:14]
PET.1909<- precip.1909[,3:14]-Eo150.1909[,3:14]
PET.1910<- precip.1910[,3:14]-Eo150.1910[,3:14]

pet.1900<- rowSums(PET.1900[,6:11], na.rm = TRUE)
pet.1901<- rowSums(PET.1901[,6:11], na.rm = TRUE)
pet.1902<- rowSums(PET.1902[,6:11], na.rm = TRUE)
pet.1903<- rowSums(PET.1903[,6:11], na.rm = TRUE)
pet.1904<- rowSums(PET.1904[,6:11], na.rm = TRUE)
pet.1905<- rowSums(PET.1905[,6:11], na.rm = TRUE)
pet.1906<- rowSums(PET.1906[,6:11], na.rm = TRUE)
pet.1907<- rowSums(PET.1907[,6:11], na.rm = TRUE)
pet.1908<- rowSums(PET.1908[,6:11], na.rm = TRUE)
pet.1909<- rowSums(PET.1909[,6:11], na.rm = TRUE)
pet.1910<- rowSums(PET.1910[,6:11], na.rm = TRUE)

avg.pet<- rowMeans(data.frame(pet.1900,
                             pet.1901, 
                             pet.1902,
                             pet.1903, 
                             pet.1904, 
                             pet.1905, 
                             pet.1906, 
                             pet.1907,
                             pet.1908,
                             pet.1909, 
                             pet.1910))/11

#make this into a datframe with correct lat long
averages.pet <- data.frame(Lat = Lat, 
                          Long = Long, 
                          avg = avg.pet)

coordinates(averages.pet) <- ~Long + Lat
gridded(averages.pet) <- TRUE
avg.pet.rast <- raster(averages.pet) #convert dataframe to a raster
projection(avg.pet.rast) <- CRS("+init=epsg:4326") # native projection for GHCN data
avg.pet.alb <- projectRaster(avg.pet.rast, crs='+init=epsg:3175') # change to great lakes albers

#create states again
all_states <- map_data("state")
states <- subset(all_states, region %in% c(  "illinois", "minnesota", "wisconsin" ) )
coordinates(states)<-~long+lat
class(states)
proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
mapdata<-spTransform(states, CRS('+init=epsg:3175'))

test.pet<- crop(avg.pet.alb, extent(mapdata))
test.pet.df <- as.data.frame(test.pet, xy = TRUE)


#make the map in GGPLOT

sites.pet.map <- ggplot()+ geom_raster(data=test.pet.df, aes(x=x, y=y, fill = avg))+
  labs(x="easting", y="northing", title="Tree Core Sites") + 
  scale_fill_gradientn(colours = rainbow(4), name ="Mean Apr.-Sept.\n Precip- PET ")
sites.pet.map <- sites.pet.map +geom_polygon(data=data.frame(mapdata), aes(x=long, y=lat, group=group),colour = "darkgrey", fill = NA)
sites.pet.map <- sites.pet.map + geom_point(data = priority, aes(x = coords.x1, y = coords.x2, shape = Description), cex = 2.5)+
  geom_text_repel(data = priority,aes(x = coords.x1, y = coords.x2,label=Names),
                  fontface = 'bold', color = 'white',
                  box.padding = unit(0.25, "lines"),
                  point.padding = unit(1.0, "lines"))



pdf("outputs/NAPC_sites_2016.pdf")              
sites.map
sites.t.map
sites.e.map
sites.et.map
sites.pet.map
dev.off()

#write.csv(priority, "outputs/priority_sites.csv")
