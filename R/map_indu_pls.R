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

# data has stem density so it is at the pls survey point level
stem.dens <- read.csv("C:/Users/JMac/Documents/Kelly/Biomodality/outputs/IN_ILdensestimates_v1.6-5.csv")
coordinates(stem.dens) <- ~PointX + PointY
proj4string(stem.dens) <-  '+init=epsg:3175'

# overlay extracts only pls points that fall within the IDNL polygon
overlay = stem.dens[complete.cases(over(stem.dens, indu.alb)),]

# data without stem density but georeferenced trees

pls <- read.csv("C:/Users/JMac/Documents/Kelly/Biomodality/outputs/ndilinpls_for_density_v1.6-5.csv", stringsAsFactors = FALSE)

as_radians <- function(deg) {(deg * pi) / (180)}
pls$TreeX1 <- pls$PointX + cos(as_radians(pls$az1))*pls$dist1
pls$TreeY1 <- pls$PointY + sin(as_radians(pls$az1))*pls$dist1
pls$TreeX2 <- pls$PointX + cos(as_radians(pls$az2))*pls$dist2
pls$TreeY2 <- pls$PointY + sin(as_radians(pls$az2))*pls$dist2

#this doesnt work yet because I need to reformat the data
first <- data.frame(x = pls$TreeX1, 
                    y = pls$TreeY1, 
                    diam = pls$diam1, 
                    spec = pls$species1 
                    )

second <- data.frame(x = pls$TreeX2, 
                     y = pls$TreeY2, 
                     diam = pls$diam2, 
                     spec = pls$species2 
)

pls <- rbind(first, second)
pls<- pls[complete.cases(pls),]
coordinates(pls) <- ~x+y
proj4string(pls) <-  '+init=epsg:3175'

# extract only pls trees that fall within the IDNL polygon
treeoverlay = pls[complete.cases(over(pls, indu.alb)),]


# fortify so we can plot with ggplot2
indu.alb@data$id = rownames(indu.alb@data)
indu.points = fortify(indu.alb, region="id")
#indu.df = join(indu.points, indu@data, by="id")

# make several plots with PLS data overlaid
ggplot(data.frame(overlay), aes(x = PointX, y = PointY, color = species1))+geom_point()+
  geom_polygon(data = indu.points, aes(group = group,x=long, y =lat),colour="black", fill = NA)+theme_bw()

ggplot(data.frame(overlay), aes(x = PointX, y = PointY, color = species2))+geom_point()+
  geom_polygon(data = indu.points, aes(group = group,x=long, y =lat),colour="black", fill = NA)+theme_bw()

ggplot(data.frame(overlay), aes(x = PointX, y = PointY, color = stem.density))+geom_point()+
  geom_polygon(data = indu.points, aes(group = group,x=long, y =lat),colour="black", fill = NA)+theme_bw()


# plots for the trees
ggplot(data.frame(treeoverlay), aes(x = x, y = y, color = spec))+geom_point()+
  geom_polygon(data = indu.points, aes(group = group,x=long, y =lat),colour="black", fill = NA)+theme_bw()


ggplot(data.frame(treeoverlay), aes(x = x, y = y, color =diam))+geom_point()+
  scale_colour_gradientn(colours = rainbow(3))+
  geom_polygon(data = indu.points, aes(group = group,x=long, y =lat),colour="black", fill = NA)+theme_bw()
