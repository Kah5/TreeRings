library(ggplot2)
library(sp)
library(rgdal)
library(maps)
library(sp)
library(plotKML)

# points <- list.files("/Users/kah/Documents/TreeRings/GPX/", pattern = "Waypoints_")
# points.f <- paste0("/Users/kah/Documents/TreeRings/GPX/", points)
# GPX.files <- lapply(points.f, readGPX)



sites <- read.csv("data/tree_cores_data_sheet_full_2016_2015.csv - tree_cores_data_sheet.csv-3.csv")

sites$cores_per_tree <- as.factor(sites$cores)
#map out 
all_states <- map_data("state")
states <- subset(all_states, region %in% c(  "illinois", "minnesota", "wisconsin", "iowa", "south dakota",
                                             "north dakota", 'nebraska','oklahoma','michigan', 'missouri', 'indiana') )
coordinates(states)<- ~long+lat
class(states)
proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
#mapdata<-spTransform(states, CRS('+init=epsg:3175'))
mapdata<-data.frame(states)


ggplot(sites[sites$TRstatus %in% c('done', 'in progress') & sites$Sampling.method %in% c('all trees in plot'),], aes( longitude, latitude, color = as.factor(Ncores_tree)))+geom_point()+geom_polygon(data=data.frame(mapdata), aes(x=long, y=lat, group=group),
                                                                                     colour = "darkgrey", fill = NA)+theme_bw()+coord_cartesian(xlim = c(-97,-87), ylim = c(37, 49)) 

png('outputs/map_sites_n_cores.png')
ggplot(sites[sites$TRstatus %in% c('done', 'in progress'),], aes( longitude, latitude, color = cores_per_tree))+geom_point()+geom_polygon(data=data.frame(mapdata), aes(x=long, y=lat, group=group),
                                                                                                                                  colour = "darkgrey", fill = NA)+theme_bw()+coord_cartesian(xlim = c(-97,-87), ylim = c(37, 49))  + ggtitle("KH tree core sites")

dev.off()
