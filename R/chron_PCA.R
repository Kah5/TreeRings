# This script conducts a PCA analysis on the Tree ring data across the midwest
# Author: Kelly Heilman

library(ggplot2)
library(sp)  # classes for spatial data
library(raster)  # grids, rasters
library(rasterVis)  # raster visualisation
library(maptools)
library(rgeos)
library(rgdal)


# read in the detrended chrons:
crn.full <- read.csv('outputs/Cronology_full_by_yr.csv')
summary(crn.full[crn.full$Year %in% 1901:2015,])
 
common <- crn.full[crn.full$Year %in% 1901:2015,]

TR <- princomp(scale(common[,!names(common) %in% c("Year", "Englund")]))
plot(TR)
biplot(TR)

loadings <- as.data.frame(TR$loadings[1:14,1:14])
loadings$site <- rownames(TR$loadings[1:14,1:14])

loadings$code <- c("BON", "COR", "GLA", "GL1", "GL2", "GL3", "GL4",
                   "HIC", "MOU", "PLE", "PVC", "STC", "TOW", "UNC")
loadings$species <- c("QUMA", "QUAL", "QUAL/QUMA", "QUMA","QUMA", "QUMA","QUMA",
                      "QUAL/QUMA", "QURA", "QUAL/QUMA", "QUMA", "QUMA", "QURA", "QUMA")

# read the site level map data
locs <- read.csv("outputs/priority_sites.csv")
#sites <- c("COR", "HIC", "STC", "GLA", "TOW", "ENG", "UNC", "BON", "MOU", "GL1", "GL2", "GL3", "GL4", "PVC")
load.loc <- merge(loadings, locs, by = "code")

# get polygons for the states in question
all_states <- map_data("state")
states <- subset(all_states, region %in% c(  "illinois", "minnesota", "wisconsin", "iowa", "south dakota",
                                             "north dakota", 'michigan', 'missouri', 'indiana') )
coordinates(states)<-~long+lat
class(states)
proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
mapdata<-spTransform(states, CRS('+init=epsg:3175'))
mapdata<-data.frame(mapdata)

ggplot(load.loc, aes(x = Comp.1, y = Comp.2, color = species))+geom_point()

a <- ggplot(load.loc, aes(x =coords.x1, y =coords.x2, color = Comp.1))+geom_point()
a <- a + geom_polygon(data=data.frame(mapdata), aes(x=long, y=lat, group=group),
                       colour = "darkgrey", fill = NA)+theme_bw() + coord_cartesian(xlim = c(-59495.64, 724000), ylim=c(68821.43, 1480021))

b<- ggplot(load.loc, aes(coords.x1, coords.x2, color=Comp.2, shape=factor(Description) ))+geom_point()
c<- ggplot(load.loc, aes(coords.x1, coords.x2, color=Comp.3))+geom_point()
d<- ggplot(load.loc, aes(coords.x1, coords.x2, color=Comp.4))+geom_point()

ggplot(load.loc,aes(sand, Comp.1))+geom_point()
ggplot(load.loc,aes(sand, Comp.2))+geom_point()
ggplot(load.loc,aes(sand, Comp.3))+geom_point()

summary(load.loc[load.loc$Description == "Savanna",])
summary(load.loc[load.loc$Description == "Forest",])
