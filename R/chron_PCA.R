# This script conducts a PCA analysis on the Tree ring data across the midwest
# Author: Kelly Heilman

library(ggplot2)
library(sp)  # classes for spatial data
library(raster)  # grids, rasters
library(maptools)
library(rgeos)
library(rgdal)
source("R/theme_black_fn.R")


# read in the detrended chrons:
crn.full <- read.csv('/Users/kah/Documents/TreeRings/outputs/Cronology_full_by_yr.csv')
crn.m <- melt(crn.full, id.vars = c("Year"))
summary(crn.full[crn.full$Year %in% 1901:2015,])


quartz()
ggplot(crn.m, aes(Year, value, color = variable))+geom_line()+facet_wrap(~variable)

common <- crn.full[crn.full$Year %in% 1901:2015,]

TR <- princomp(common[,!names(common) %in% c("Year", "Englund")])
covmat.chrons <- cov.wt(common[,!names(common) %in% c("Year", "Englund")])

TR <- princomp(covmat = covmat.chrons)
plot(TR)
biplot(TR)

loadings <- as.data.frame(TR$loadings[1:14,1:14])
loadings$site <- rownames(TR$loadings[1:14,1:14])

loadings$code <- c("BON", "COR", "GLA", "GL1", "GL2", "GL3", "GL4",
                   "HIC", "MOU", "PLE", "PVC", "STC", "TOW", "UNC")
loadings$species <- c("QUMA", "QUAL", "QUAL/QUMA", "QUMA","QUMA", "QUMA","QUMA",
                      "QUAL/QUMA", "QURA/QUVE", "QUAL/QUMA", "QUMA", "QUMA", "QURA", "QUMA")

# read the site level map data
locs <- read.csv("/Users/kah/Documents/TreeRings/outputs/priority_sites.csv")
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

# ------------Do the interannual growth varaitions goup by space, species, or climate?-------------
# plot pc 1 and 2 loadings and color by average climate
ggplot(load.loc, aes(x = Comp.1, y = PLSdensity, color = PLSdensity))+geom_point(size = 5)

ggplot(load.loc, aes(x = Comp.1, y = Comp.2, color = pr.av))+geom_point()

ggplot(load.loc, aes(x = Comp.1, y = Comp.2, color = avg.t.alb))+geom_point()

ggplot(load.loc, aes(x = Comp.1, y = Comp.2, color = evap.av))+geom_point()

ggplot(load.loc, aes(x = Comp.1, y = Comp.2, color = et.av))+geom_point()




a <- ggplot(load.loc, aes(x =coords.x1, y =coords.x2, color = Comp.1))+geom_point()+scale_color_gradientn(colors= rainbow(4))
a <- a + geom_polygon(data=data.frame(mapdata), aes(x=long, y=lat, group=group),
                       colour = "darkgrey", fill = NA)+theme_bw() + coord_cartesian(xlim = c(-59495.64, 724000), ylim=c(68821.43, 1480021))

a


# component 3 occurs along a gradient of PR/soil type:
png("outputs/Chron_PCA/comp3_map.png")

ggplot()+geom_polygon(data=data.frame(mapdata), aes(x=long, y=lat, group=group),colour = "darkgrey", fill = NA)+ geom_jitter(data = load.loc, aes(coords.x1, coords.x2, fill= Comp.3),width = 15000, height = 20000, size = 4,shape =21)+
  #scale_fill_gradientn(colors =rev(brewer.pal(5,"Spectral")), name = "PC 3")+
  scale_fill_gradient2(low = "red", high = "blue", name = "PC 3")+
  theme_black()+ coord_cartesian(xlim = c(-59495.64, 724000), ylim=c(500000, 1500000))+theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank())

dev.off()


#ggplot(data = load.loc, aes(coords.x1, coords.x2, color=Comp.3))+
ggplot(load.loc, aes(pr.av, sand, color=Comp.3))+geom_point(size = 4)+scale_color_gradient2(low = "red", high = "blue", name = "PC 3")+theme_black()
ggplot(load.loc, aes(pr.av, sand, color=Comp.1))+geom_point(size = 4)+scale_color_gradientn(colors =rev(brewer.pal(5,"Spectral")))

d <- ggplot(load.loc, aes(coords.x1, coords.x2, color=Comp.4))+geom_point()

# basic plots of PC3 and 2 vs. climate

png("outputs/Chron_PCA/Comp3_vs_MAP.png")
ggplot(load.loc, aes(pr.av, Comp.3, color = Comp.3))+geom_point(size = 4)+scale_color_gradient2(low = "red", high = "blue", name = "PC 3")+geom_smooth(method = "lm", color = "white")+theme_black()+
  ylab("Principal Component 3 (10.6% variance)")+xlab("Mean annual precipitation (mm/year)")
dev.off()



ggplot(load.loc,aes(avg.t.alb, Comp.3))+geom_point()+geom_smooth(method = "lm")
ggplot(load.loc,aes(evap.av, Comp.3))+geom_point()+geom_smooth(method = "lm")
ggplot(load.loc,aes(et.av, Comp.3))+geom_point()+geom_smooth(method = "lm")
ggplot(load.loc,aes(PET.av, Comp.3))+geom_point()+geom_smooth(method = "lm")
ggplot(load.loc,aes(sand, Comp.3))+geom_point()+geom_smooth(method = "lm")

ggplot(load.loc,aes(pr.av, Comp.2))+geom_point()
ggplot(load.loc,aes(avg.t.alb, Comp.2))+geom_point()
ggplot(load.loc,aes(evap.av, Comp.2))+geom_point()
ggplot(load.loc,aes(et.av, Comp.2))+geom_point()
ggplot(load.loc,aes(PET.av, Comp.2))+geom_point()

# basic plots of PC1 and PC2 vs soil types:
ggplot(load.loc,aes(sand, Comp.1))+geom_point()
ggplot(load.loc,aes(sand, Comp.2))+geom_point()
ggplot(load.loc,aes(ksat, Comp.1))+geom_point()
ggplot(load.loc,aes(ksat, Comp.2))+geom_point()
ggplot(load.loc,aes(awc, Comp.1))+geom_point()
ggplot(load.loc,aes(awc, Comp.2))+geom_point()
ggplot(load.loc,aes(sand, pr.av, color = Comp.1))+geom_point()
ggplot(load.loc,aes(sand, pr.av, color = Comp.2))+geom_point()

summary(load.loc[load.loc$Description == "Savanna",])
summary(load.loc[load.loc$Description == "Forest",])

write.csv(load.loc, "outputs/Chron_PCA/pca_loadings_sites.csv", row.names = FALSE)
