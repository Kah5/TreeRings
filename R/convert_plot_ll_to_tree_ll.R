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

# read all gpx files in and get lat longs:
cores <- read.csv("/Users/kah/Downloads/tree_cores_data_sheet_full_2016_2015.csv - tree_cores_data_sheet.csv-3.csv")
class(cores$dist2center)

cores$lon <- cores$Plot_Long
cores$lat <- cores$Plot_Lat

plot.data.full <- cores#[!is.na(cores$Plot_Long),]

for(i in 1: length(plot.data.full$Plot_Long)){
  
  plot.data  <- plot.data.full[i,]
  
if(is.na(plot.data$Tree_Long) & !is.na(plot.data$Plot_Long) & !is.na(plot.data$DBH_calc)){


# now we make the data spatial and convert to a spatial projection that is in meters, not degrees

coordinates(plot.data) <- ~lon +lat
proj4string(plot.data) <- '+init=epsg:4326' # define native proj
plot.data.alb <- spTransform(plot.data, CRS('+init=epsg:3175')) # converte to albers
plot.data.alb <- data.frame(plot.data.alb)

# function converts degrees to radians, since R deals with radians
as_radians <- function(deg) {(deg * pi) / (180)}

# find X-y coordinates of the trees within the plots:
plot.data.alb$x_tree <- plot.data.alb$lon + cos(as_radians(plot.data.alb$direction))*(plot.data.alb$dist2center + (0.5*(as.numeric(as.character(plot.data.alb$DBH_calc))/100)))
plot.data.alb$y_tree <- plot.data.alb$lat + sin(as_radians(plot.data.alb$direction))*(plot.data.alb$dist2center + (0.5*(as.numeric(as.character(plot.data.alb$DBH_calc))/100)))

if(is.na(plot.data.alb$y_tree)){ # if there were no distances or DBH values, just assign na
  plot.data.full[i,]$Tree_Long <- NA
  plot.data.full[i,]$Tree_Lat <- NA
}else{
plot.data.alb.nona <- plot.data.alb[!is.na(plot.data.alb$x_tree),]
# now convert albers back to ll:
coordinates(plot.data.alb.nona) <- ~x_tree + y_tree
proj4string(plot.data.alb.nona) <- '+init=epsg:3175'  # define native proj
plot.data.alb.nona <- spTransform(plot.data.alb.nona, CRS('+init=epsg:4326')) # converte to 

plot.data.alb.nona.df <- data.frame(plot.data.alb.nona)
plot.data.full[i,]$Tree_Long <- plot.data.alb.nona.df$x_tree
plot.data.full[i,]$Tree_Lat <- plot.data.alb.nona.df$y_tree

}
}
}





head(plot.data.full)
plot.data.full %>% group_by(Site.Code) %>% dplyr::summarise(
                                                     nspec = length(unique(Species)))




plot.data.concat <- plot.data.full %>% select(Plot_Long, Plot_Lat, Tree_Long, Tree_Lat, Site.Code, Year.Cored, Tag.ID, Species, DBH_calc)

colnames(plot.data.concat)[9] <- "DBH_cm"

plot.data.concat <- plot.data.concat[!is.na(plot.data.concat$Tree_Long),]
write.csv(plot.data.full, "outputs/data/full_tree_core_lat_long_v2.csv")

plot.data.full$non.oak <- ifelse(as.character(plot.data.full$Species) %in% c("White Oak", "Bur Oak", "Northern Red Oak","Swamp Red Oak", "Bur oak", "Black Oak"), "Oak", as.character(plot.data.full$Species))

View(plot.data.full %>% group_by(Site.Code) %>% dplyr::summarise(
  nspec = length(unique(non.oak))))


write.csv(plot.data.concat, "outputs/data/Heilman_tree_core_lat_long_concise_v2.csv")
