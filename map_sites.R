#make a map of sites across midwest

library(sp)  # classes for spatial data
library(raster)  # grids, rasters
library(rasterVis)  # raster visualisation
library(maptools)
library(rgeos)
library(ggplot2)
library(rgdal)
library(ggrepel)
library(grid)

MN.priority <- readOGR("data/priority.kml", layer = "Priority")
#IL.MCCD <- readOGR('data/Fieldwork 2016.kml', layer = "McHenry")

MN.priority.alb <- spTransform(MN.priority, CRSobj = CRS('+init=epsg:3175'))
priority <- data.frame(MN.priority)
priority.lat <- data.frame(priority)

priority$code <- c("ITA", "GLE", "MAP", "UNC", "AVO", "STC", "GLL", "GLA", "PVC", 'BON', 'COR', "HIC", "ENG", "TOW")
priority.lat$code <- c("ITA", "GLE", "MAP", "UNC", "AVO", "STC", "GLL", "GLA", "PVC", 'BON', 'COR', "HIC", "ENG", "TOW")

#IL.MCCD <- spTransform(IL.MCCD, CRSobj = CRS('+init=epsg:3175'))
#IL.MCCD <- data.frame(IL.MCCD)
#IL.MCCD$code <- c("GLA", "PLV", " ", "HAR", "BEC", " ", "ELN", "COR")
#priority <- rbind(priority, IL.MCCD)
mound <- readOGR("data/Treecores.kml", layer= "2015sites")
mound.lat <- mound
mound <- spTransform(mound, CRSobj = CRS('+init=epsg:3175'))
mound <- data.frame(mound)
mound.lat <- data.frame(mound.lat)
mound$code <- c("LED", "MOU", "BON", "UNI", "PAM", "ENG", "BAC", "CAC", "GLA", "BOO","DUF", "PLE")
mound.lat$code <- c("LED", "MOU", "BON", "UNI", "PAM", "ENG", "BAC", "CAC", "GLA", "BOO","DUF", "PLE")

#read in layer of sites cored in 2016
sites16 <- readOGR("data/Treecores.kml", layer= "2016sites")
sites16.lat <- sites16
sites16 <- spTransform(sites16, CRSobj = CRS('+init=epsg:3175'))
sites16 <- data.frame(sites16)
sites16.lat <- data.frame(sites16.lat)
sites16$code <- c("COR", "PVC", "UNC", "ITA", "AVO", "GLE", "MAP")
sites16.lat$code <- c("COR", "PVC", "UNC", "ITA", "AVO", "GLE", "MAP")
sites16$Description <- c("Forest", "Savanna", "Savanna", 
                         "Forest", "Savanna & Forest", "Savanna & Forest", "Savanna & Forest")
# merge the two data sets using rbind
priority <- rbind(mound, sites16)

#priority<- rbind(priority,mound[2,]) # just add mound prairie to priority
#priority.lat <- rbind(priority.lat[,c('Name', "Description", "coords.x1", "coords.x2", "code")], mound.lat[,c('Name', "Description", "coords.x1", "coords.x2", "code")])
#for NAPC, create a map with just these tree cores:
#priority <- readOGR('data/Treecores.kml', layer = "NAPCsites")
#priority <- spTransform(priority, CRSobj = CRS('+init=epsg:3175'))
#priority <- data.frame(priority)
#priority$code <- c("ITA", "GLE", "MAP", "UNC", "AVH", "STC", "GLL", "GLA", "PVC", 'BON', 'COR', "HIC", "ENG", "TOW")
#priority$Names <- c('Pleasant Valley', 'St. Croix Savanna',"Townsend Woods", "Hickory Grove", "Bonanza Prairie")
#places <- c('St. Croix Savanna',"Townsend Woods", "Hickory Grove", "Bonanza Prairie")

#map against soils data
library(raster)
ksat <- raster('C:/Users/JMac/Box Sync/GSSURGOtifs/8km_UMW_ksat1.tif')
ksat.alb <- projectRaster(ksat, crs='+init=epsg:3175')

awc <- raster('C:/Users/JMac/Box Sync/GSSURGOtifs/8km_UMW_awc1.tif')
awc.alb <- projectRaster(awc, crs = '+init=epsg:3175')

sand <- raster("C:/Users/JMac/Box Sync/GSSURGOtifs/1km_UMW_sand1/8km_UMW_sand1.tif")
sand.alb <- projectRaster(sand, crs = '+init=epsg:3175')

priority$sand <- extract(sand.alb, priority[,c("coords.x1", "coords.x2")])
priority$ksat <- extract(ksat.alb, priority[,c("coords.x1","coords.x2")])
priority$awc <- extract(awc.alb, priority[,c("coords.x1","coords.x2")])
write.csv(priority, "outputs/priority_sites_locs.csv")

#map out 
all_states <- map_data("state")
states <- subset(all_states, region %in% c(  "illinois", "minnesota", "wisconsin", "iowa", "south dakota",
                                             "north dakota", 'michigan', 'missouri', 'indiana') )
coordinates(states)<-~long+lat
class(states)
proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
mapdata<-spTransform(states, CRS('+init=epsg:3175'))
mapdata<-data.frame(mapdata)

#make the map in GGPLOT

cbPalette2 <- c('#d7191c',
                '#fdae61',
                '#ffffbf',
                '#abd9e9',
                '#2c7bb6')

cbPalette <- c('#a6611a',
               '#dfc27d',
               '#80cdc1',
               '#018571')

#plot the points over soils data

plot(awc.alb)
plot(priority$coords.x1, priority$coords.x2, add = TRUE)
plot(ksat.alb)
plot(priority$coords.x1, prirority$coords.x2, add = TRUE)

#priority <- priority[priority$Names %in% places, ]
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

precip.1900<- read.table("./data/precip_2014/precip.1900")
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
plot(avg.alb)


#map out 
all_states <- map_data("state")
states <- subset(all_states, region %in% c(  "illinois", "minnesota", "wisconsin", "iowa", "south dakota",
                                             "north dakota", 'michigan', 'missouri', 'indiana') )
coordinates(states)<-~long+lat
class(states)
proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
mapdata<-spTransform(states, CRS('+init=epsg:3175'))

library(raster)
# use state bounds from gadm website:
us = readRDS('data/USA_adm1.rds')


states <- c("Illinois", "Minnesota", "Wisconsin")
us <- spTransform(us, CRS('+init=epsg:3175 +proj=aea +lat_1=42.122774 +lat_2=49.01518 +lat_0=45.568977 +lon_0=-83.248627 +x_0=1000000 +y_0=1000000 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0 '))
us = us[match(toupper(states),toupper(us$NAME_1)),]

test<- crop(avg.alb, extent(mapdata))
test.df <- as.data.frame(test, xy = TRUE)
#avg.alb <- as.data.frame(avg.alb, xy = TRUE)

mapdata<-data.frame(mapdata)

#make the map in GGPLOT

cbPalette2 <- c('#d7191c',
  '#fdae61',
  '#ffffbf',
  '#abd9e9',
  '#2c7bb6')

cbPalette <- c('#a6611a',
  '#dfc27d',
  '#80cdc1',
  '#018571')


sites.map <- ggplot()+ geom_raster(data=test.df, aes(x=x, y=y, fill = avg))+
  labs(x="easting", y="northing", title="A). Tree Core Sites 2015 & 2016") + 
  scale_fill_gradientn(colours = cbPalette2, name ="MAP (mm/yr) ")+
  coord_cartesian(xlim = c(-59495.64, 725903.4), ylim=c(68821.43, 1480021))
sites.map <- sites.map +geom_polygon(data=data.frame(mapdata), aes(x=long, y=lat, group=group),
                                     colour = "darkgrey", fill = NA)+theme_bw() +
  theme(axis.text = element_text(size = 14),
        axis.text.y = element_text(angle = 90, size = rel(0.7), hjust = 0.75),
        legend.key = element_rect(),
        legend.background = element_rect(fill = "white"),
        
        panel.grid.major = element_line(colour = "grey40"),
        panel.grid.minor = element_blank())
sites.map

sites.map2 <- sites.map + geom_point(data = priority, aes(x = coords.x1, y = coords.x2, shape = Description), cex = 2.5)+
  scale_shape_manual(values=1:4)+
  geom_text_repel(data = priority, aes(x = coords.x1, y = coords.x2,label=code),
                  fontface = 'bold', color = 'black',
                  box.padding = unit(1.5, "lines"),
                  point.padding = unit(1.5, "lines"))
sites.map2





png("outputs/precip_only.png")              
sites.map
dev.off()

png("outputs/precip_sites_full.png")
sites.map2
dev.off()

#now make a map for sites cored in 2015 only:

map2015 <- sites.map + geom_point(data = mound, aes(x = coords.x1, y = coords.x2, shape = Description), cex = 2.5)+
  scale_shape_manual(values=c(1,2,3))+
  geom_text_repel(data = mound, aes(x = coords.x1, y = coords.x2,label=code),
                  fontface = 'bold', color = 'black',
                  box.padding = unit(1.5, "lines"),
                  point.padding = unit(1.5, "lines"))+
  labs(x="easting", y="northing", title="B). Tree Core Sites 2015") 

png("outputs/precip_sites_2015.png")
map2015
dev.off()

# map of sites cored in 2016 only:
map2016 <- sites.map + geom_point(data = sites16, aes(x = coords.x1, y = coords.x2, shape = Description), cex = 2.5)+
  scale_shape_manual(values=c(1,2,4))+
  geom_text_repel(data = sites16, aes(x = coords.x1, y = coords.x2,label=code),
                  fontface = 'bold', color = 'black',
                  box.padding = unit(1.5, "lines"),
                  point.padding = unit(1.5, "lines"))+
  labs(x="easting", y="northing", title="C). Tree Core Sites 2016")

png("outputs/precip_sites_2016.png")
map2016
dev.off()


png(width = 800, height = 800, 'outputs/precip_sites_map_fig1.png')
pushViewport(viewport(layout = grid.layout(2, 2)))
print(sites.map2, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(map2015, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(map2016, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
dev.off()


# now map for temperature from GHCN data
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
states <- subset(all_states, region %in% c(  "illinois", "minnesota", "wisconsin", "iowa", "south dakota",
                                             "north dakota", 'michigan', 'missouri', 'indiana', 'kentucky', 'nebraska', 'arkansas','kansas' ) )
coordinates(states)<-~long+lat
class(states)
proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
mapdata<-spTransform(states, CRS('+init=epsg:3175'))

test.t<- crop(avg.t.alb, mapdata)
test.t.df <- as.data.frame(test.t, xy = TRUE)

mapdata <- data.frame(mapdata)
#make the map in GGPLOT

sites.t.map <- ggplot()+ geom_raster(data=test.t.df, aes(x=x, y=y, fill = avg))+
  labs(x="easting", y="northing", title="Tree Core Sites") + 
  scale_fill_gradientn(colours = rev(rainbow(5)), name ="Mean Temp. (DegC) ")+
  coord_cartesian(xlim = c(-59495.64, 725903.4), ylim=c(68821.43, 1480021))

sites.t.map <- sites.t.map +geom_polygon(data=data.frame(mapdata), aes(x=long, y=lat, group=group),colour = "darkgrey", fill = NA)
sites.t.map2 <- sites.t.map + geom_point(data = priority, aes(x = coords.x1, y = coords.x2, shape = Description), cex = 2.5)+
  geom_text_repel(data = priority,aes(x = coords.x1, y = coords.x2,label=code),
                  fontface = 'bold', color = 'black',
                  box.padding = unit(1.5, "lines"),
                  point.padding = unit(1.5, "lines"))
png("outputs/temp_map.png")
sites.t.map
dev.off()

png("outputs/temp_map_full.png")
sites.t.map2
dev.off()

#making natural earth maps (from simon gorings blog)

library(raster)
library(rgdal)
library(ggplot2)
library(reshape2)
library(plyr)
library(rgeos)

#  Assuming you have a path 'Maps' that you store your spatial files in.  This
#  is all downloaded from <a href="http://www.naturalearthdata.com/downloads/">http://www.naturalearthdata.com/downloads/</a> using the
#  1:50m "Medium" scale data.

nat.earth <- stack('data/NE2_50m_SR_W/NE2_50M_SR_W/NE2_50M_SR_W.tif')

ne_lakes <- readOGR('data/ne_50m_lakes/ne_50m_lakes.shp',
                    'ne_50m_lakes')

ne_rivers <- readOGR('data/ne_50m_rivers_lake_centerlines/ne_50m_rivers_lake_centerlines.shp',
                     'ne_50m_rivers_lake_centerlines')

ne_coast <- readOGR('data/ne_50m_coastline/ne_50m_coastline.shp',
                    'ne_50m_coastline')
ne_states <- readOGR('data/ne_50m_admin_1_states_provinces_lakes/ne_50m_admin_1_states_provinces_lakes.shp',
                     'ne_50m_admin_1_states_provinces_lakes')

#  I have a domain I'm interested in, but there's no reason you can't define something else:
quick.subset <- function(x, longlat){
  
  # longlat should be a vector of four values: c(xmin, xmax, ymin, ymax)
  x@data$id <- rownames(x@data)
  
  x.f = fortify(x, region="id")
  x.join = join(x.f, x@data, by="id")
  
  x.subset <- subset(x.join, x.join$long > longlat[1] & x.join$long < longlat[2] &
                       x.join$lat > longlat[3] & x.join$lat < longlat[4])
  
  x.subset
}

domain <- c(-100, -85, 36.5, 49.7)
#lakes.subset <- quick.subset(ne_lakes, domain)
river.subset <- quick.subset(ne_rivers, domain)
coast.subset <- quick.subset(ne_coast, domain)
#states.subset <- quick.subset(ne_states, domain)

nat.crop <- crop(nat.earth, y=extent(domain))

rast.table <- data.frame(xyFromCell(nat.crop, 1:ncell(nat.crop)),
                         getValues(nat.crop/255))

rast.table$rgb <- with(rast.table, rgb(NE2_50M_SR_W.1,
                                       NE2_50M_SR_W.2,
                                       NE2_50M_SR_W.3,
                                       1))
# et voila!

map<- ggplot(data = rast.table, aes(x = x, y = y)) +
  geom_tile(fill = rast.table$rgb) +
  geom_polygon(data=ne_lakes, aes(x = long, y = lat, group = group), fill = '#ADD8E6') +
  #geom_polygon(data = ne_states, aes(x = long, y = lat, group = group), color = 'black')+
  scale_alpha_discrete(range=c(1,0)) +
  
  geom_path(data=ne_lakes, aes(x = long, y = lat, group = group), color = 'blue') +
  geom_path(data=river.subset, aes(x = long, y = lat, group = group), color = 'blue') +
  geom_path(data = ne_states, aes(x = long, y = lat, group = group), color = 'black')+
  #geom_path(data=coast.subset, aes(x = long, y = lat, group = group), color = 'blue') +
  xlim(-97, -87) +
  ylim(36.5, 49.7) +
  xlab('') + ylab('')
map <-map + geom_point(data = priority.lat, aes(x = coords.x1, y = coords.x2, shape = Description, label=code), cex = 2.5)+
  geom_text(data = priority.lat, aes(x = coords.x1, y = coords.x2, label= code ),hjust = -0.5, vjust=0.25 )

map
#coord_map(xlim= c(-100, -85),ylim= c(36.5, 49.7))

#pdf('outputs/NAPC_sites_2015_temp.pdf')
sites.t.map
sites.t.map2
#dev.off()

png('outputs/temp_map.png')
sites.t.map
dev.off()

png('outputs/temp_map_sites.png')
sites.t.map2
dev.off()

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
  geom_text_repel(data = priority,aes(x = coords.x1, y = coords.x2,label=code),
                  fontface = 'bold', color = 'white',
                  box.padding = unit(0.25, "lines"),
                  point.padding = unit(1.0, "lines"))
sites.e.map
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
  geom_text_repel(data = priority,aes(x = coords.x1, y = coords.x2,label=code),
                  fontface = 'bold', color = 'white',
                  box.padding = unit(0.25, "lines"),
                  point.padding = unit(1.0, "lines"))
sites.et.map

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
  geom_text_repel(data = priority,aes(x = coords.x1, y = coords.x2,label=code),
                  fontface = 'bold', color = 'white',
                  box.padding = unit(0.25, "lines"),
                  point.padding = unit(1.0, "lines"))

sites.pet.map

pdf("outputs/DDIG_sites_2016.pdf")              
sites.map
sites.t.map
sites.e.map
sites.et.map
sites.pet.map
dev.off()

#write.csv(priority, "outputs/priority_sites.csv")
