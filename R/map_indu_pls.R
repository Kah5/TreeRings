# playing with PLS data at indiana national lakeshore

library(sp)
library(rgeos)
library(raster)
library(rgdal)
library(ggplot2)
library(plyr)

#read in shapefile for indiana national lakeshore:
indu <- readOGR("C:/Users/JMac/Documents/Kelly/Biomodality/data/indu_tracts/indu_boundary.shp", layer = "indu_boundary")
plot(indu)

#shapefile is already projected into a lat long coordinate system, so lets convert to pls

indu.alb <- spTransform(indu, CRSobj = '+init=epsg:3175')

plot(indu.alb)

stem.dens <- read.csv("C:/Users/JMac/Documents/Kelly/Biomodality/outputs/IN_ILdensestimates_v1.6-5.csv")
coordinates(stem.dens) <- ~PointX + PointY
proj4string(stem.dens) <-  '+init=epsg:3175'


overlay=stem.dens[complete.cases(over(stem.dens, indu.alb)),]


# fortify so we can plot with ggplot2
indu.alb@data$id = rownames(indu.alb@data)
indu.points = fortify(indu.alb, region="id")
#indu.df = join(indu.points, indu@data, by="id")


ggplot(data.frame(overlay), aes(x = PointX, y = PointY, color = species1))+geom_point()+
  geom_polygon(data = indu.points, aes(group = group,x=long, y =lat),colour="black", fill = NA)+theme_bw()
