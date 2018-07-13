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
library(Hmisc)

# KH note: if the library says it didnt load, you need to install the package library using:
# install.packages("package name") i.e.
install.packages("sp")

# set the directory (change this to your tree ring directory)
setwd("/Users/kah/Documents/TreeRings")


# read in your gps data (it should be saved in the directory you listed above)
LATLONG <- read.csv("GPS coordinates.csv")

# read in the plot data: 
Hem.plot <- read.csv("Hemlock_Plots.csv")
Hem.plot <- Hem.plot[,1:8] # only look at the first 8 columns

# merge the lat longs with the Hem.plot data set
plot.data <- merge(LATLONG, Hem.plot, by.x = "Site", by.y = "PlotID")
# change the column names to more R friendly names:
colnames(plot.data) <- c("Site", "lat", "lon", "ID", "direction", "GPS.Coordinates", 
                         "dist2center", "DBH.cm", "Species", "Comments")

# make the lon negative b/c we are in western hemisphere
plot.data$lon <- plot.data$lon * (-1)

# now we make the data spatial and convert to a spatial projection that is in meters, not degrees
coordinates(plot.data) <- ~lon +lat
proj4string(plot.data) <- '+init=epsg:4326' # define native proj
plot.data.alb <- spTransform(plot.data, CRS('+init=epsg:3175')) # converte to albers
plot.data.alb <- data.frame(plot.data.alb)

# function converts degrees to radians, since R deals with radians
as_radians <- function(deg) {(deg * pi) / (180)}

# find X-y coordinates of the trees within the plots:
plot.data.alb$x_tree <- plot.data.alb$lon + cos(as_radians(plot.data.alb$direction))*(plot.data.alb$dist2center + (0.5*(plot.data.alb$DBH.cm/100)))
plot.data.alb$y_tree <- plot.data.alb$lat + sin(as_radians(plot.data.alb$direction))*(plot.data.alb$dist2center + (0.5*(plot.data.alb$DBH.cm/100)))


# make a basic plots of the whole site + save as a png :

crampton <- ggplot(plot.data.alb, aes(x = x_tree, y = y_tree, color = Species, size = DBH.cm))+geom_point() + theme_bw()

png(height = 8, width = 8, units = "in", res = 300, "Crampton_all_trees.png")
crampton
dev.off()

circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

# this create individual maps of the stands using this for loop
sites <- unique(plot.data$Site)


# it loops through each unique site and outputs a stand map picture
for (i in 1:length(unique(plot.data$Site))){

  # make a data frame that has information to plot a circle around each plot
  #geom_path will do open circles, geom_polygon will do filled circles
if(sites[i] == "INT"){ 
            dat <-  circleFun(c(unique(plot.data.alb[plot.data.alb$Site %in% sites[i],]$lon), unique(plot.data.alb[plot.data.alb$Site %in% sites[i],]$lat)), 30, npoints = 100)
}else{
  dat <- circleFun(c(unique(plot.data.alb[plot.data.alb$Site %in% sites[i],]$lon), unique(plot.data.alb[plot.data.alb$Site %in% sites[i],]$lat)), 20, npoints = 100)} 
          
 
  

#names(colorsforspec) <- species

 plot.map <- ggplot()+ geom_point(data = plot.data.alb[plot.data.alb$Site %in% sites[i],], aes(x = x_tree, y = y_tree, color = Species, size = DBH.cm)) + theme_bw() + geom_path(data = dat, aes(x=x, y=y)) + ggtitle(paste(sites[i], "plot map")) #+ scale_color_manual(name = species,values=colorsforspec) +ylab("y coord")+xlab("x coord")+theme(legend.title = element_blank())

ggsave(plot.map, filename = paste0(sites[i], "plot_map.png"))


}

# now write out the xy values of each tree as lat long and output it as a csv file:
coordinates(plot.data.alb) <- ~x_tree + y_tree
proj4string(plot.data.alb) <- '+init=epsg:3175' # define native proj
plot.data.alb <- spTransform(plot.data.alb, CRS('+init=epsg:4326')) # converte to albers
plot.data.alb <- data.frame(plot.data.alb)

write.csv(plot.data.alb, "plot_data_with_x_y_tree_coords.csv", row.names = FALSE)


