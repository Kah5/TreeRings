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


# reading in data from google maps (not all are exact site points)

MN.priority <- readOGR("data/priority.kml", layer = "Priority")
#IL.MCCD <- readOGR('data/Fieldwork 2016.kml', layer = "McHenry")

MN.priority.alb <- spTransform(MN.priority, CRSobj = CRS('+init=epsg:3175'))
priority <- data.frame(MN.priority.alb)
priority.lat <- data.frame(priority)

priority$code <- c("ITA", "GLE", "MAP", "UNC", "AVO", "STC", "GLL", "GLA", "PVC", 'BON', 'COR', "HIC", "ENG", "TOW")
priority.lat$code <- c("ITA", "GLE", "MAP", "UNC", "AVO", "STC", "GLL", "GLA", "PVC", 'BON', 'COR', "HIC", "ENG", "TOW")

#IL.MCCD <- spTransform(IL.MCCD, CRSobj = CRS('+init=epsg:3175'))
#IL.MCCD <- data.frame(IL.MCCD)
#IL.MCCD$code <- c("GLA", "PLV", " ", "HAR", "BEC", " ", "ELN", "COR")
#priority <- rbind(priority, IL.MCCD)
mound <- readOGR("data/Treecores.kml", layer= "2015sites")
#mound.lat <- mound
mound <- spTransform(mound, CRSobj = CRS('+init=epsg:3175'))
mound <- data.frame(mound)
mound.lat <- data.frame(mound)
mound$code <- c("LED", "MOU", "BON", "UNI", "PAM", "ENG", "BAC", "CAC", "GLA", "BOO","DUF", "PLE")
mound.lat$code <- c("LED", "MOU", "BON", "UNI", "PAM", "ENG", "BAC", "CAC", "GLA", "BOO","DUF", "PLE")
mound <- rbind(mound, priority[priority$code %in% c("TOW", "HIC", "STC"),]) # add townsend woods

#read in layer of sites cored in 2016
sites16 <- readOGR("data/Treecores.kml", layer= "2016sites")
sites16.lat <- sites16
sites16 <- spTransform(sites16, CRSobj = CRS('+init=epsg:3175'))
sites16 <- data.frame(sites16)
sites16.lat <- data.frame(sites16.lat)
sites16$code <- c("COR", "PVC", "UNC", "ITA", "AVO", "GLE", "MAP", "GLL")
sites16.lat$code <- c("COR", "PVC", "UNC", "ITA", "AVO", "GLE", "MAP", "GLL")
sites16$Description <- c("Forest", "Savanna", "Savanna", 
                         "Forest", "Savanna & Forest", "Savanna & Forest", "Savanna & Forest", "Savanna & Forest")

# read in the points from GLL, AVO, PVC
GLL_PVC <- readOGR("data/GLL_PVC.kml", layer= "GLL_PVC")
GLL_PVC.lat <- GLL_PVC
GLL_PVC <- spTransform(GLL_PVC, CRSobj = CRS('+init=epsg:3175'))
GLL_PVC <- data.frame(GLL_PVC)
GLL_PVC.lat <- data.frame(GLL_PVC.lat)
GLL_PVC$code <- c("GL4", "GL3", "GL2", "GL1", "PVC")
GLL_PVC.lat$code <- c("GL4", "GL3", "GL2", "GL1", "PVC")
GLL_PVC$Description <- c("Forest", "Savanna", "Savanna", 
                         "Forest", "Savanna")
GLL_PVC<- GLL_PVC[GLL_PVC$code %in% c("GL4", "GL3", "GL2", "GL1"),]

# merge the two data sets using rbind
priority <- rbind(mound, sites16, GLL_PVC)
coordinates(priority) <- ~coords.x1 + coords.x2
proj4string(priority) <- '+init=epsg:3175'
priority.lat <- spTransform(priority, CRSobj = CRS("+init=epsg:4326"))
priority.lat <- data.frame(priority.lat)
priority$PDSI_time <- c("Not measured", "No Change", "Growth Change", "Not measured", 
                        "Not measured", "No Change", "Not measured", "Not measured", 
                        "Growth Change", "Not measured", "Not measured","Growth Change", "Slope Change","Slope Change","No Change",
                        "Growth Change", "Not measured", "No Change", "Not measured", 
                        "Not measured", "Not measured", "Not measured", "Not measured",
                        "UNK", "UNK", "UNK", "UNK", "UNK")
write.csv(priority, "outputs/priority_sites.csv")

#--------------- What was the tree density in the PLS data? -------------------------
# read in the version 1.7-5 data (pls) merged with the upper midwest data:
#dens <- read.csv("/Users/kah/Documents/bimodality/data/midwest_pls_full_density_alb1.7-5.csv")
dens <- read.csv("/Users/kah/Documents/bimodality/data/PLS_FIA_density_climate_full.csv")
dens <- dens[names(dens) %in% c("x", "y", "PLSdensity")]
ggplot(dens, aes(x, y, fill = PLSdensity) ) + geom_raster()

# need to convert density data to a raster:
coordinates(dens)<- ~x+y
gridded(dens) <- TRUE
dens.rast <- raster(dens)
plot(dens.rast)
proj4string(dens.rast) <- '+init=epsg:3175'
priority$PLSdensity <- raster::extract(dens.rast, priority[,c("coords.x1", "coords.x2")])
priority$PLSclass <- ifelse(priority$PLSdensity < 0.5, "Prairie", ifelse(priority$PLSdensity <= 47, "Savanna", "Forest"))
dens <- as.data.frame(dens)


#now extract the priority values

#priority<- rbind(priority,mound[2,]) # just add mound prairie to priority
#priority.lat <- rbind(priority.lat[,c('Name', "Description", "coords.x1", "coords.x2", "code")], mound.lat[,c('Name', "Description", "coords.x1", "coords.x2", "code")])
#for NAPC, create a map with just these tree cores:
#priority <- readOGR('data/Treecores.kml', layer = "NAPCsites")
#priority <- spTransform(priority, CRSobj = CRS('+init=epsg:3175'))
#priority <- data.frame(priority)
#priority$code <- c("ITA", "GLE", "MAP", "UNC", "AVH", "STC", "GLL", "GLA", "PVC", 'BON', 'COR', "HIC", "ENG", "TOW")
#priority$Names <- c('Pleasant Valley', 'St. Croix Savanna',"Townsend Woods", "Hickory Grove", "Bonanza Prairie")
#places <- c('St. Croix Savanna',"Townsend Woods", "Hickory Grove", "Bonanza Prairie")

# read in the data on cores sampled at each site:
full.cores <- read.csv("data/tree_cores_data_sheet_full_2016_2015.csv")
full.cores$CW1 <- as.numeric(as.character(full.cores$CW1))
full.cores$CW2 <- as.numeric(as.character(full.cores$CW2))
full.cores$area <- 0.00007854*((full.cores$DBH..cm.*pi)/100)^2
DBH.means <- full.cores %>% 
  group_by(Site.Code) %>%
  summarise(DBH = mean(DBH..cm., na.rm= TRUE), DBH.sd = mean(DBH..cm., na.rm=TRUE), 
            n = n(), BA = sum(area, na.rm=TRUE)*10000,nspecies = length(unique(Species)), CW_avg = mean((CW1 + CW2)/2))

DBH.means <- data.frame(DBH.means)

ggplot(DBH.means, aes(Site.Code, y = DBH))+geom_bar(stat = 'identity')
ggplot(DBH.means, aes(BA, y = DBH))+geom_point()

priority <- merge(priority, DBH.means, by.x = "code", by.y = "Site.Code")

#map against soils data
library(raster)
ksat <- raster('/Users/kah/Documents/bimodality/data/8km_UMW_ksatalb.tif')
ksat.alb <- projectRaster(ksat, crs='+init=epsg:3175')

awc <- raster('/Users/kah/Documents/bimodality/data/8km_UMW_awc1.tif')
awc.alb <- projectRaster(awc, crs = '+init=epsg:3175')

sand <- raster("/Users/kah/Documents/bimodality/data/8km_UMW_sandalb.tif")
sand.alb <- projectRaster(sand, crs = '+init=epsg:3175')

priority$sand <- raster::extract(sand.alb, priority[,c("coords.x1", "coords.x2")])
priority$ksat <- raster::extract(ksat.alb, priority[,c("coords.x1","coords.x2")])
priority$awc <- raster::extract(awc.alb, priority[,c("coords.x1","coords.x2")])
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
priority$pr.av <- extract(test, priority[,c("coords.x1", 'coords.x2')])

mapdata <- data.frame(mapdata)

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
  scale_fill_gradientn(colours = cbPalette2, name =" Mean \n Annual \n Precipitation \n (mm/yr) ")+
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

priority <- data.frame(priority)
sites.map2 <- sites.map + geom_point(data = priority, aes(x = coords.x1, y = coords.x2, shape = Description), cex = 2.5)+
  scale_shape_manual(values=1:4)+
  geom_text_repel(data = priority, aes(x = coords.x1, y = coords.x2,label=code),
                  fontface = 'bold', color = 'black',
                  box.padding = unit(1.5, "lines"),
                  point.padding = unit(1.5, "lines"))
sites.map2

sites.map3 <- sites.map + geom_point(data = priority, aes(x = coords.x1, y = coords.x2, shape = Description), cex = 2.5)+
  scale_shape_manual(values=1:4)+theme(axis.text = element_blank(), axis.ticks=element_blank(),
                                      legend.key.size = unit(2,'lines')
                                      ,legend.background = element_rect(fill=alpha('transparent', 0.4)),panel.grid.major = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1))+
  ggtitle("")

sites.map3

sites.mapblack <- sites.map + geom_point(data = priority, aes(x = coords.x1, y = coords.x2, shape = Description ), cex = 2.5, fill = "white", colour = "black")+
  scale_shape_manual(values=1:4)+theme_black()+theme(axis.text = element_blank(),
                                                     axis.title = element_blank(),
                                                     legend.key = element_rect(),
                                                
                                                     axis.ticks = element_blank(),
                                                     panel.grid.major = element_blank(),
                                                     panel.grid.minor = element_blank())+ggtitle(" ")+theme(legend.key = element_rect(fill = "white"))
 # geom_text_repel(data = priority, aes(x = coords.x1, y = coords.x2,label=code),
               #   fontface = 'bold', color = 'black',
                #  box.padding = unit(1.5, "lines"),
                 # point.padding = unit(1.5, "lines"))
sites.mapblack

measured <- priority[!priority$PDSI_time %in% "Not measured",]
sites.cor <- sites.map + geom_point(data = measured, aes(x = coords.x1, y = coords.x2, shape = Description, color = PDSI_time), cex = 2.5)+
  scale_shape_manual(values=c(15,16,17,3))+
  geom_text_repel(data = measured, aes(x = coords.x1, y = coords.x2,label=code),
                  fontface = 'bold', color = 'black',
                  box.padding = unit(1.5, "lines"),
                  point.padding = unit(1.5, "lines"))
sites.cor
png(height = 6, width = 6, units = 'in', res = 300, "outputs/PDSI_time_cor_map.png")              
sites.cor
dev.off()

png(height = 6, width = 6, units = 'in', res = 300, "outputs/Tree_core_map_nolabels.png")              
sites.map3
dev.off()

png(height = 6, width = 6, units = 'in', res = 300, "outputs/Tree_core_sites_map_black_background.png")              
sites.mapblack
dev.off()

png("outputs/precip_only.png")              
sites.map
dev.off()

png("outputs/precip_sites_full.png")
sites.map2+theme_black()
dev.off()
#------------------------------ natural earth maps------------------------------
library(raster)
library(rgdal)
library(ggplot2)
library(reshape2)
library(plyr)
library(rnaturalearth)
#  Assuming you have a path 'Maps' that you store your spatial files in.  This
#  is all downloaded from <a href=>http://www.naturalearthdata.com/downloads/</a> using the
#  1:50m "Medium" scale data.

# lakes
ne_lakes <- ne_download(scale = 50, type = 'lakes', category = 'physical')
sp::plot(ne_lakes, col = 'blue')

# rivers
ne_rivers <- ne_download(scale = 110, type = 'rivers_lake_centerlines', category = 'physical')
sp::plot(ne_rivers, col = 'blue')

# coast:
ne_coast <- ne_download(scale = 110, type = 'coastline', category = 'physical')
sp::plot(ne_coast, col = 'blue')

# states:
ne_state <- ne_download(scale = 110, type = 'states', category = 'cultural')
#ne_NE2 <- ne_download(scale = 50, type = 'NE2_50M', category = 'raster')
#sp::plot(ne_HYP)

#nat.earth<- stack(ne_MSR)
nat.earth <- stack('data/NE2_50M_SR_W/NE2_50M_SR_W/NE2_50M_SR_W.tif')

#ne_lakes <- readOGR('./Maps/NaturalEarth/ne_50m_lakes.shp',
 #                   'ne_50m_lakes')

#ne_rivers <- readOGR('./Maps/NaturalEarth/ne_50m_rivers_lake_centerlines.shp',
 #                    'ne_50m_rivers_lake_centerlines')

#ne_coast <- readOGR('./Maps/NaturalEarth/ne_50m_coastline.shp',
 #                   'ne_50m_coastline')

#  I have a domain I'm interested in, but there's no reason you can't define something else:
quick.subset <- function(x, longlat){
  
  # longlat should be a vector of four values: c(xmin, xmax, ymin, ymax)
  x@data$id <- rownames(x@data)
  
  x.f = fortify(x, region="id")
  x.join = plyr::join(x.f, x@data, by="id")
  
  x.subset <- subset(x.join, x.join$long > longlat[1] & x.join$long < longlat[2] &
                       x.join$lat > longlat[3] & x.join$lat < longlat[4])
  
  x.subset
}

domain <- c(-105, -80, 30.5, 55)
lakes.subset <- quick.subset(ne_lakes, domain)
river.subset <- quick.subset(ne_rivers, domain)
coast.subset <- quick.subset(ne_coast, domain)
state.subset <- quick.subset(ne_state, c(-105, -70, 30.5, 55))
nat.crop <- crop(nat.earth, y=extent(domain))

rast.table <- data.frame(xyFromCell(nat.crop, 1:ncell(nat.crop)),
                         getValues(nat.crop/255))

rast.table$rgb <- with(rast.table, rgb(NE2_50M_SR_W.1,
                                       NE2_50M_SR_W.2,
                                     NE2_50M_SR_W.3,
                                       1))
# et voila!

NEmap <- ggplot()+
  geom_raster(data = rast.table, aes(x = x, y = y, fill = NE2_50M_SR_W.1)) +scale_fill_gradientn(colours = rev(cbPalette), guide = FALSE)+
  
  geom_path(data=state.subset, aes(x = long, y = lat, group = group), color = 'grey40')+
  geom_path(data=lakes.subset, aes(x = long, y = lat, group = group), color = 'blue') +
  geom_polygon(data=lakes.subset, aes(x = long, y = lat, group = group), fill = '#ADD8E6') +
  scale_alpha_discrete(range=c(1,0)) +
  #geom_path(data=river.subset, aes(x = long, y = lat, group = group), color = 'blue') +
  #geom_path(data=coast.subset, aes(x = long, y = lat, group = group), color = 'blue') + 
  #coord_equal()+#geom_raster(fill = rast.table$MSR_50M)+
  #scale_x_continuous(expand=c(0,0)) +
  #scale_y_continuous(expand=c(0,0)) +
  xlab('') + ylab('')+ coord_cartesian(xlim = c(-100, -83), ylim=c(36.5, 50)) 

NEmapfull <- NEmap + geom_point(data = priority.lat, aes(x = coords.x1, y = coords.x2), cex = 2.5)+
  geom_text_repel(data = priority.lat, aes(x = coords.x1, y = coords.x2,label=code),
                  fontface = 'bold', color = 'black',
                  box.padding = unit(0.5, "lines"),
                  point.padding = unit(0.5, "lines"))+theme_bw()+ #geom_path(data=river.subset, aes(x = long, y = lat, group = group), color = 'blue')+
                   theme(legend.position = 'none')

png(height= 8, width = 9, units = "in", res = 300, "outputs/NE_map_sites.png")
NEmapfull
dev.off()

NEmap + geom_text(data = priority.lat, aes(x = coords.x1, y = coords.x2, label = code))+
  geom_text_repel(data = priority.lat, aes(x = coords.x1, y = coords.x2,label=code),
                  fontface = 'bold', color = 'black',
                  box.padding = unit(0.5, "lines"),
                  point.padding = unit(0.5, "lines"))



# plot with the sites that have:
site.bays <- data.frame(code = c("MOU", "BON", "ENG", "GLA", "UNC", "GL1", "GL2", "GL3", "AVO","COR"),
                        site = c("MOU", "BON", "ENG", "GLA", "UNC", "GLL1", "GLL2", "GLL3", "AVO", "COR"))
# need to reclassify the woodlands to forests for mapping:


# map out one the google earth pretty map:
NEmap + geom_point(data = priority.lat[priority.lat$code %in% site.bays$code,], aes(x = coords.x1, y = coords.x2,shape=Description, color = Description))+ 
  geom_text_repel(data = priority.lat[priority.lat$code %in% site.bays$code,], aes(x = coords.x1, y = coords.x2,label=code),
                        fontface = 'bold', color = 'black',
                        box.padding = unit(0.5, "lines"),
                        point.padding = unit(0.5, "lines"))

# merge the sites with climate data pulled for tree ring analysis:
full.ghcn.sites <- read.csv("outputs/full.ghcn.sites.struct.before.splitting.csv")

full.ghcn.sites <- merge(full.ghcn.sites, site.bays, by = "site")

full.ghcn.unique <- unique(full.ghcn.sites[, c("site", "JUNTmax", "MAP.prism", "prism_Month_tmax_6", "prism_tmax_jja", "jja.VPDmax","year")])

site.means <- full.ghcn.sites %>% group_by(site, code, structure ) %>% dplyr::summarise(site.tmax = mean(JUNTmax, na.rm = TRUE),
                                                                site.sd.tmax = sd(JUNTmax, na.rm = TRUE),
                                                                site.ci.lo.tmax = quantile(JUNTmax, 0.025, na.rm = TRUE),
                                                                site.ci.high.tmax = quantile(JUNTmax, 0.975, na.rm = TRUE),
                                                                site.MAP = mean(MAP.prism, na.rm = TRUE), 
                                                                site.sd.MAP = sd (MAP.prism, na.rm = TRUE),
                                                                site.ci.lo.MAP = quantile(MAP.prism, 0.025, na.rm = TRUE),
                                                                site.ci.high.MAP = quantile(MAP.prism, 0.975, na.rm = TRUE),
                                                                site.tmax.prism = mean(prism_Month_tmax_6), 
                                                                site.tmax.prism.jja = mean(prism_tmax_jja), 
                                                                site.VPDmax = mean(jja.VPDmax))


full.ghcn.priority <- left_join(priority.lat, site.means, by = "code")



# ---------plot Precipitation vs. summer tmax of each site & projections
climate.space.ci <- ggplot(na.omit(full.ghcn.priority), aes(site.MAP, site.tmax, shape = structure, color = structure))+geom_point(size = 3)+scale_color_manual(values = c("Savanna"='sienna4',
                                                                                                                                                       "Forest"='forestgreen'))+
  geom_errorbar(aes(x = site.MAP, ymin = site.ci.lo.tmax ,ymax = site.ci.high.tmax), alpha = 0.5)+
  geom_errorbarh(aes(y = site.tmax, xmin = site.ci.lo.MAP, xmax = site.ci.high.MAP), alpha = 0.5)+xlim(400, 1200)+ylim(20,31)+theme_bw(base_size = 12)+ylab(expression("June Mean Maximum Temperature (" *
                                                                                                                                              degree * "C)"))+xlab("Total Annual Precipitation (mm)")+theme(panel.grid = element_blank(), legend.position = "none")

climate.space.sd <- ggplot(na.omit(full.ghcn.priority), aes(site.MAP, site.tmax, shape = structure, color = structure))+geom_point(size = 3)+scale_color_manual(values = c("Savanna"='sienna4',
                                                                                                                                                       "Forest"='forestgreen'))+
  geom_errorbar(aes(x = site.MAP, ymin = site.tmax -site.sd.tmax ,ymax = site.tmax+site.sd.tmax))+
  geom_errorbarh(aes(y = site.tmax, xmin = site.MAP-site.sd.MAP, xmax = site.MAP+site.sd.MAP))+xlim(400, 1200)+ylim(20,31)+theme_bw(base_size = 12)+ylab(expression("June Mean Maximum Temperature (" *
                                                                                                                                                        degree * "C)"))+xlab("Total Annual Precipitation (mm)")+theme(panel.grid = element_blank(), legend.position = "none")

# plot out the sites along the map
full.ghcn.priority$coords.x1.jitter <- full.ghcn.priority$coords.x1
full.ghcn.priority$coords.x2.jitter <- full.ghcn.priority$coords.x2

full.ghcn.priority[full.ghcn.priority$code %in% "GL1",]$coords.x1.jitter <- full.ghcn.priority[full.ghcn.priority$code %in% "GL1",]$coords.x1 + 0.05
full.ghcn.priority[full.ghcn.priority$code %in% "GL2",]$coords.x1.jitter <- full.ghcn.priority[full.ghcn.priority$code %in% "GL2",]$coords.x1 - 0.05
full.ghcn.priority[full.ghcn.priority$code %in% "GL3",]$coords.x2.jitter <- full.ghcn.priority[full.ghcn.priority$code %in% "GL3",]$coords.x2 - 0.05

full.ghcn.priority$coords.x2

sites.bays.map <- NEmap + geom_point(data = full.ghcn.priority[full.ghcn.priority$code %in% site.bays$code,], aes(x = coords.x1.jitter, y = coords.x2.jitter, shape=structure, color =structure), size = 2)+
  scale_color_manual(values = c("Savanna"='sienna4', "Forest"='forestgreen'))+
  geom_text_repel(data = full.ghcn.priority[full.ghcn.priority$code %in% site.bays$code,], aes(x = coords.x1, y = coords.x2,label=code),
                  fontface = 'bold', color = 'black',
                  box.padding = unit(0.5, "lines"),
                  point.padding = unit(0.5, "lines")) + coord_cartesian(xlim = c(-100, -85), ylim=c(38, 49)) +theme_bw()+theme(legend.title = element_blank(), legend.position= c(0.2, 0.2),  
                                                                                                                               legend.background = element_rect(color = "black", fill = "white", size = 1, linetype = "solid"),
                                                                                                                               legend.text=element_text(size=12))

png(height = 4, width = 8, units = "in", res = 300, "outputs/site_map_and_climate_space.png")
plot_grid(sites.bays.map, climate.space.ci, ncol = 2, align = "hv", labels = "AUTO")
dev.off()

png(height = 4, width = 8, units = "in", res = 300, "outputs/site_map_and_climate_space_sd.png")
plot_grid(sites.bays.map, climate.space.sd, ncol = 2, align = "hv", labels = "AUTO")
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

# now lets map out in PLS density space:

sc <- scale_colour_gradientn(colours = rev(terrain.colors(8)), limits=c(0, 16))
cbpalette <- c("#ffffcc", "#c2e699", "#78c679", "#31a354", "#006837")

dens$ecoclass <- ifelse(dens$PLSdensity >= 48, "Forest", ifelse(dens$PLSdensity >= 0.5, "Savanna", "Prairie"))

sites.map.pls <- ggplot()+ geom_raster(data=dens, aes(x=x, y=y, fill = PLSdensity))+
  #labs( title="A). Tree Core Sites 2015 & 2016") + 
  scale_fill_gradientn(colours = cbpalette, name ="PLS Tree density (trees/ha)")+
  coord_cartesian(xlim = c(-59495.64, 725903.4), ylim=c(68821.43, 1480021))
sites.map.pls <- sites.map.pls +geom_polygon(data=data.frame(mapdata), aes(x=long, y=lat, group=group),
                                     colour = "darkgrey", fill = NA, size  = 1.5)+theme_black()+
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        legend.key = element_rect(),
       
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
sites.map.pls + theme_black()

png("outputs/bw_tree_density_map.png")
sites.map.pls
dev.off()

 df.dens <- dens
sites.map.eco <- ggplot()+geom_raster(data = df.dens, aes(x=x, y=y, fill = ecoclass))+
   
  scale_fill_manual(values = c('#f7fcb9',
                      '#addd8e',
                      '#31a354'),limits = c("Prairie", "Savanna", "Forest"), name ="Biome")#+
  #coord_cartesian(xlim = c(-59495.64, 725903.4), ylim=c(68821.43, 1480021))
sites.map.eco <- sites.map.eco +geom_polygon(data=data.frame(mapdata), aes(x=long, y=lat, group=group),
                                             colour = "darkgrey", fill = NA, size  = 1.5)+theme_black()+
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        legend.key = element_rect(),
        
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
sites.map.eco<- sites.map.eco  + coord_cartesian(xlim = c(-59495.64, 845903.4), ylim=c(68821.43, 1480021))

png("outputs/bw_ecotype_map.png")
sites.map.eco
dev.off()

sites.map.pls <- sites.map.pls + geom_point(data = priority[complete.cases(priority),], aes(x = coords.x1, y = coords.x2, shape = Description), cex = 2.5)+
  scale_shape_manual(values=1:4)+theme(axis.text = element_blank(), axis.ticks=element_blank(),
                                       legend.key.size = unit(2,'lines')
                                       ,legend.background = element_rect(fill=alpha('transparent', 0.4)),panel.grid.major = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1))+
  ggtitle("")+
  geom_text_repel(data = priority[complete.cases(priority),], aes(x = coords.x1, y = coords.x2,label=code),
                  fontface = 'bold', color = 'black')
sites.map.pls
png("outputs/PLS_density_TRsites.png")
sites.map.pls
dev.off()


sc <- scale_colour_gradientn(colours = rev(terrain.colors(8)), limits=c(0, 16))
cbpalette <- c("#ffffcc", "#c2e699", "#78c679", "#31a354", "#006837")
cbPalette <- c("#999999","#009E73", "#E69F00", "#56B4E9",  "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

dens.pr <- read.csv("/Users/kah/Documents/bimodality/data/PLS_FIA_density_climate_full.csv")

dens.pr$density_discrete <- ifelse(dens.pr$PLSdensity <= 0.5, "Prairie", 
                                   ifelse(dens.pr$PLSdensity <= 47, "Savanna",
                                          ifelse(dens.pr$PLSdensity > 47 & dens.pr$PLSdensity <= 100, "47-100",
                                                 ifelse(dens.pr$PLSdensity > 100 & dens.pr$PLSdensity <= 200, "100-200", 
                                                        ifelse(dens.pr$PLSdensity > 200 & dens.pr$PLSdensity <= 300, "200-300", 
                                                               ifelse(dens.pr$PLSdensity > 300 & dens.pr$PLSdensity <= 400, "300-400",
                                                                      ifelse(dens.pr$PLSdensity > 400 & dens.pr$PLSdensity <= 500, "400-500",
                                                                             ifelse(dens.pr$PLSdensity > 500 & dens.pr$PLSdensity <= 600, "500 - 600",
                                                                                    ifelse(dens.pr$PLSdensity > 600,  "600 +", "NA")))))))))

dens.pr$density_discrete<- factor(dens.pr$density_discrete, c("Prairie", "Savanna","47-100", "100-200", "200-300", "300-400", "400-500", "500-600", "600+", "NA"))


pls.map.alt.color <- ggplot()+ geom_polygon(data = mapdata[mapdata$region %in% c("illinois", "indiana", "minnesota", "wisconsin", "michigan"),], aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=dens.pr, aes(x=x, y=y, fill = density_discrete))+
  geom_polygon(data = mapdata[mapdata$region %in% c("illinois", "indiana", "minnesota", "wisconsin", "michigan"),], aes(group = group,x=long, y =lat),color = "grey", fill = 'NA', size = 1)+
  labs(x="easting", y="northing")+ #+ 
  scale_fill_manual(values = c('#dfc27d',
                               '#8c510a',
                               '#d9f0a3',
                               '#addd8e',
                               '#78c679',
                               '#41ab5d',
                               '#238443',
                               '#005a32'), name ="Tree \n Density", na.value = 'darkgrey') +
  
  theme_black(base_size = 8)+ theme(legend.background = element_rect(fill=alpha('transparent', 0)) ,axis.line=element_blank(),axis.text=element_blank(),
                                 legend.key.size = unit(0.5, "lines"),axis.text.y=element_blank(),axis.ticks=element_blank(),
                                 
                                 axis.title=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ggtitle("")+coord_equal() 


png(height = 4, width = 3, units = "in", res = 300,"outputs/PLS_density_map_full_alt_colors_bw.png")
pls.map.alt.color
dev.off()

# make an FIA map of the same thing:

dens.pr$fiadensity_discrete <- ifelse(dens.pr$FIAdensity <= 0.5, "Prairie", 
                                   ifelse(dens.pr$FIAdensity <= 47, "Savanna",
                                          ifelse(dens.pr$FIAdensity > 47 & dens.pr$FIAdensity <= 100, "47-100",
                                                 ifelse(dens.pr$FIAdensity > 100 & dens.pr$FIAdensity <= 200, "100-200", 
                                                        ifelse(dens.pr$FIAdensity > 200 & dens.pr$FIAdensity <= 300, "200-300", 
                                                               ifelse(dens.pr$FIAdensity > 300 & dens.pr$FIAdensity <= 400, "300-400",
                                                                      ifelse(dens.pr$FIAdensity > 400 & dens.pr$FIAdensity <= 500, "400-500",
                                                                             ifelse(dens.pr$FIAdensity > 500 & dens.pr$FIAdensity <= 600, "500 - 600",
                                                                                    ifelse(dens.pr$FIAdensity > 600,  "600 +", "NA")))))))))

dens.pr$fiadensity_discrete<- factor(dens.pr$fiadensity_discrete, c( "Savanna","47-100", "100-200", "200-300", "300-400", "400-500", "500-600", "600+", "NA"))


FIA.map.alt.color <- ggplot()+ geom_polygon(data = mapdata[mapdata$region %in% c("illinois", "indiana", "minnesota", "wisconsin", "michigan"),], aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=dens.pr, aes(x=x, y=y, fill = fiadensity_discrete))+
  geom_polygon(data = mapdata[mapdata$region %in% c("illinois", "indiana", "minnesota", "wisconsin", "michigan"),], aes(group = group,x=long, y =lat),color = "grey", fill = 'NA', size = 1)+
  labs(x="easting", y="northing")+ #+ 
  scale_fill_manual(values = c(
                               '#8c510a',
                               '#d9f0a3',
                               '#addd8e',
                               '#78c679',
                               '#41ab5d',
                               '#238443',
                               '#005a32'), name ="Tree \n Density", na.value = 'darkgrey') +
  
  theme_black(base_size = 8)+ theme(legend.background = element_rect(fill=alpha('transparent', 0)) ,axis.line=element_blank(),axis.text=element_blank(),
                                    legend.key.size = unit(0.5, "lines"),axis.text.y=element_blank(),axis.ticks=element_blank(),
                                    
                                    axis.title=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ggtitle("")+coord_equal() 


png(height = 4, width = 3, units = "in", res = 300,"outputs/FIA_density_map_full_alt_colors_bw.png")
FIA.map.alt.color
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

priority$avg.t.alb <- extract(avg.t.alb, priority[ ,c("coords.x1", "coords.x2")])
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

priority$evap.av <- extract(avg.e.alb, priority[,c("coords.x1", "coords.x2")])

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

priority$et.av <- extract(avg.et.alb, priority[,c("coords.x1", "coords.x2")])

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

priority$PET.av <- extract(avg.pet.alb, priority[,c('coords.x1', 'coords.x2')])

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

write.csv(priority, "outputs/priority_sites_full_envt.csv")
