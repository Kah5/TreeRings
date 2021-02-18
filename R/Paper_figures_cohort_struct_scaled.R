# script to produce the figures for the manuscript:
# Author: Kelly A. Heilman
# Last Checked: March 25, 2020
# note: Need to run clean_separate_data.R, RWI_bayes_model.R, and WUE_bayes_model.R before running this script


# script for making figures for the tree ring MS:
library(rjags)
library(ggplot2)
library(caTools)
library(ggridges)
library(tidyr)
library(reshape2)
library(dplyr)
library(cowplot)
library(raster)
library(rgdal)
library(ggplot2)
library(reshape2)
library(plyr)
library(rnaturalearth)
library(ggrepel)

#-------------------------------------------------------------------------------------------------
#                                     Figure 1: Tree ring map and climate
#-------------------------------------------------------------------------------------------------
# read in sites:
priority.lat <- read.csv("outputs/priority_sites.csv")



# read in natural earth + make ggplot maps: code adapted from Simon Gorings blog:
#  
#   downloaded from <a href=>http://www.naturalearthdata.com/downloads/</a> using the
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

#ne.earth <- ne_download(category = "raster")
#rst <- ne_download(scale = 50, type = 'NE2_50M_SR_W', category = 'raster', destdir = paste0(getwd(), "/data"))

# this stopped working for some reason:
nat.earth <- stack('data/NE2_50M_SR_W/NE2_50M_SR_W/NE2_50M_SR_W.tif')
#nat.earth <- stack("data/")

#  Also from simon goring: create a quick subset function
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
# plot out map

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


# add inset  map of the full of full US:

domain <- c(-125, -65, 24, 55)
lakes.full <- quick.subset(ne_lakes, domain)
river.full <- quick.subset(ne_rivers, domain)
coast.full <- quick.subset(ne_coast, domain)
state.full <- quick.subset(ne_state, domain)
nat.full <- crop(nat.earth, y=extent(domain))

rast.table <- data.frame(xyFromCell(nat.full, 1:ncell(nat.full)),
                         getValues(nat.full/255))

rast.table$rgb <- with(rast.table, rgb(NE2_50M_SR_W.1,
                                       NE2_50M_SR_W.2,
                                       NE2_50M_SR_W.3,
                                       1))
# get inset bounding box:
bbox <- data.frame(long = c(-100, -83,  -83, -100, -100),
                   lat = c(36.5, 36.5, 50, 50, 36.5))

# full map of US for creating an inset

NEmap.full.us <- ggplotGrob( ggplot()+
                               geom_raster(data = rast.table, aes(x = x, y = y, fill = NE2_50M_SR_W.1)) +scale_fill_gradientn(colours = rev(cbPalette), guide = FALSE)+
                               
                               geom_path(data=state.full, aes(x = long, y = lat, group = group), color = 'grey40')+
                               geom_path(data=lakes.full, aes(x = long, y = lat, group = group), color = 'blue') +
                               geom_polygon(data=lakes.full, aes(x = long, y = lat, group = group), fill = '#ADD8E6') +
                               scale_alpha_discrete(range=c(1,0)) +
                               geom_path(data = bbox, aes(x = long, y = lat), size = 0.75, color = "black")+
                               #geom_path(data=river.full, aes(x = long, y = lat, group = group), color = 'blue') +
                               #geom_path(data=coast.full, aes(x = long, y = lat, group = group), color = 'blue') + 
                               #coord_equal()+#geom_raster(fill = rast.table$MSR_50M)+
                               #scale_x_continuous(expand=c(0,0)) +
                               #scale_y_continuous(expand=c(0,0)) +
                               xlab('') + ylab('')+ coord_cartesian(xlim = c(-122, -69), ylim=c(26, 50))+theme_bw() +
                               theme(panel.background = element_rect(fill = "transparent",colour = NA),
                                     plot.background = element_rect(fill = "transparent",colour = NA), 
                                     axis.text = element_blank(), axis.ticks = element_blank())
)


bbox2 <- data.frame(long = c(-98, -82,  -82, -98, -98),
                    lat = c(36.5, 36.5, 50, 50, 36.5))


US.map <- ggplot()+
  geom_raster(data = rast.table, aes(x = x, y = y, fill = NE2_50M_SR_W.1)) +scale_fill_gradientn(colours = rev(cbPalette), guide = FALSE)+
  
  geom_path(data=state.full, aes(x = long, y = lat, group = group), color = 'grey40')+
  geom_path(data=lakes.full, aes(x = long, y = lat, group = group), color = 'blue') +
  geom_polygon(data=lakes.full, aes(x = long, y = lat, group = group), fill = '#ADD8E6') +
  scale_alpha_discrete(range=c(1,0)) +
  geom_path(data = bbox2, aes(x = long, y = lat), size = 0.75, color = "black")+
  #geom_path(data=river.full, aes(x = long, y = lat, group = group), color = 'blue') +
  #geom_path(data=coast.full, aes(x = long, y = lat, group = group), color = 'blue') + 
  #coord_equal()+#geom_raster(fill = rast.table$MSR_50M)+
  #scale_x_continuous(expand=c(0,0)) +
  #scale_y_continuous(expand=c(0,0)) +
  xlab('') + ylab('')+ coord_cartesian(xlim = c(-122, -69), ylim=c(26, 50))+theme_bw() +
  theme(panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA))#, 
#axis.text = element_blank(), axis.ticks = element_blank())

# png(height = 4, width = 8, units ="in", res = 300, "outputs/Full_US_map_region.png")
# US.map
# dev.off()

# NEmap + geom_text(data = priority.lat, aes(x = coords.x1, y = coords.x2, label = code))+
#   geom_text_repel(data = priority.lat, aes(x = coords.x1, y = coords.x2,label=code),
#                   fontface = 'bold', color = 'black',
#                   box.padding = unit(0.5, "lines"),
#                   point.padding = unit(0.5, "lines"))
# 


# plot with the sites that have:
site.bays <- data.frame(code = c("MOU", "BON", "ENG", "GLA", "UNC", "GL1", "GL2", "GL3", "AVO","COR"),
                        site = c("MOU", "BON", "ENG", "GLA", "UNC", "GLL1", "GLL2", "GLL3", "AVO", "COR"))
# need to reclassify the woodlands to forests for mapping:


# map out one the google earth pretty map:
# NEmap + geom_point(data = priority.lat[priority.lat$code %in% site.bays$code,], aes(x = coords.x1, y = coords.x2,shape=Description, color = Description))+ 
#   geom_text_repel(data = priority.lat[priority.lat$code %in% site.bays$code,], aes(x = coords.x1, y = coords.x2,label=code),
#                   fontface = 'bold', color = 'black',
#                   box.padding = unit(0.5, "lines"),
#                   point.padding = unit(0.5, "lines"))
# 


# merge the sites with climate data pulled for tree ring analysis:
full.ghcn.sites <- read.csv("outputs/full.ghcn.sites.struct.before.splitting.csv")

full.ghcn.sites <- merge(full.ghcn.sites, site.bays, by = "site")

full.ghcn.unique <- unique(full.ghcn.sites[, c("site", "JUNTmax", "MAP.prism", "prism_PRISM_tmax_6", "prism_tmax_jja", "jja.VPDmax","year")])

site.means <- full.ghcn.sites %>% group_by(site, code, structure ) %>% dplyr::summarise(site.tmax = mean(JUNTmax, na.rm = TRUE),
                                                                                        site.sd.tmax = sd(JUNTmax, na.rm = TRUE),
                                                                                        site.ci.lo.tmax = quantile(JUNTmax, 0.025, na.rm = TRUE),
                                                                                        site.ci.high.tmax = quantile(JUNTmax, 0.975, na.rm = TRUE),
                                                                                        site.MAP = mean(MAP.prism, na.rm = TRUE), 
                                                                                        site.sd.MAP = sd (MAP.prism, na.rm = TRUE),
                                                                                        site.ci.lo.MAP = quantile(MAP.prism, 0.025, na.rm = TRUE),
                                                                                        site.ci.high.MAP = quantile(MAP.prism, 0.975, na.rm = TRUE),
                                                                                        site.tmax.prism = mean(prism_PRISM_tmax_6), 
                                                                                        site.tmax.prism.jja = mean(prism_tmax_jja), 
                                                                                        site.VPDmax = mean(jja.VPDmax))


full.ghcn.priority <- dplyr::left_join(priority.lat, site.means, by = "code")



# ---------plot Precipitation vs. summer tmax of each site & projections
climate.space.ci <- ggplot(na.omit(full.ghcn.priority), aes(site.MAP, site.tmax, shape = structure, color = structure))+geom_point(size = 3)+scale_color_manual(values = c("Savanna"='sienna4',
                                                                                                                                                                           "Forest"='forestgreen'))+
  geom_errorbar(aes(x = site.MAP, ymin = site.ci.lo.tmax ,ymax = site.ci.high.tmax), alpha = 0.5)+
  geom_errorbarh(aes(y = site.tmax, xmin = site.ci.lo.MAP, xmax = site.ci.high.MAP), alpha = 0.5)+xlim(400, 1200)+ylim(20,31)+theme_bw(base_size = 12)+ylab(expression("June Maximum Temperature (" *
                                                                                                                                                                         degree * "C)"))+xlab("Total Annual Precipitation (mm)")+
  theme(panel.grid = element_blank(), legend.position = c(0.8, 0.15), legend.title = element_blank(), plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), axis.line = element_line(color = 'black'))

  #draws x and y axis line
  theme(axis.line = element_line(color = 'black'))
climate.space.sd <- ggplot(na.omit(full.ghcn.priority), aes(site.MAP, site.tmax, shape = structure, color = structure))+geom_point(size = 3)+scale_color_manual(values = c("Savanna"='sienna4',
                                                                                                                                                                           "Forest"='forestgreen'))+
  geom_errorbar(aes(x = site.MAP, ymin = site.tmax -site.sd.tmax ,ymax = site.tmax+site.sd.tmax))+
  geom_errorbarh(aes(y = site.tmax, xmin = site.MAP-site.sd.MAP, xmax = site.MAP+site.sd.MAP))+xlim(400, 1200)+ylim(20,31)+theme_bw(base_size = 12)+ylab(expression("June Maximum Temperature (" *
                                                                                                                                                                      degree * "C)"))+xlab("Total Annual Precipitation (mm)")+theme(panel.grid = element_blank(), legend.position = "none")

# plot out the sites along the map
full.ghcn.priority$coords.x1.jitter <- full.ghcn.priority$coords.x1
full.ghcn.priority$coords.x2.jitter <- full.ghcn.priority$coords.x2

full.ghcn.priority[full.ghcn.priority$code %in% "GL1",]$coords.x1.jitter <- full.ghcn.priority[full.ghcn.priority$code %in% "GL1",]$coords.x1 + 0.05
full.ghcn.priority[full.ghcn.priority$code %in% "GL2",]$coords.x1.jitter <- full.ghcn.priority[full.ghcn.priority$code %in% "GL2",]$coords.x1 - 0.05
full.ghcn.priority[full.ghcn.priority$code %in% "GL3",]$coords.x2.jitter <- full.ghcn.priority[full.ghcn.priority$code %in% "GL3",]$coords.x2 - 0.05

full.ghcn.priority$coords.x2

# sites.bays.map <- NEmap + geom_point(data = full.ghcn.priority[full.ghcn.priority$code %in% site.bays$code,], aes(x = coords.x1.jitter, y = coords.x2.jitter, shape=structure, color =structure), size = 2)+
#   scale_color_manual(values = c("Savanna"='sienna4', "Forest"='forestgreen'))+
#   geom_text_repel(data = full.ghcn.priority[full.ghcn.priority$code %in% site.bays$code,], aes(x = coords.x1, y = coords.x2,label=code),
#                   fontface = 'bold', color = 'black',
#                   box.padding = unit(0.5, "lines"),
#                   point.padding = unit(0.5, "lines")) + coord_cartesian(xlim = c(-100, -85), ylim=c(38, 49)) +theme_bw()+
#   theme(legend.title = element_blank(), legend.position= c(0.8, 0.15),  
#         legend.background = element_rect(color = "black", fill = "white", size = 1, linetype = "solid"),
#         legend.text=element_text(size=12))

# now add an inset map within our regional map:
# site.bayes.map.inset <- sites.bays.map + annotation_custom(grob = NEmap.full.us, xmin = -102, xmax = -92.5,
#                                                            ymin = 36.5, ymax = 41.5)


# make the same map but with numbers instead of site names:
numbered.sites <- full.ghcn.priority[full.ghcn.priority$code %in% site.bays$code,]
numbered.sites <- numbered.sites[order(numbered.sites$code),]
numbered.sites$number <- as.character(1:length(numbered.sites$code))

# sites.bays.map.num <- NEmap + geom_point(data = numbered.sites, aes(x = coords.x1.jitter, y = coords.x2.jitter, shape=structure, color =structure), size = 2)+
#   scale_color_manual(values = c("Savanna"='sienna4', "Forest"='forestgreen'))+
#   geom_text_repel(data = numbered.sites, aes(x = coords.x1, y = coords.x2, label=number),
#                   fontface = 'bold', color = 'black',
#                   box.padding = unit(0.5, "lines"),
#                   point.padding = unit(0.5, "lines")) + coord_cartesian(xlim = c(-100, -85), ylim=c(38, 49)) +theme_bw()+
#   theme(legend.title = element_blank(), legend.position= c(0.8, 0.15),
#         legend.background = element_rect(color = "black", fill = "white", size = 1, linetype = "solid"),
#         legend.text=element_text(size=12))
#
# # now add an inset map within our regional map:
# site.bayes.map.inset.num <- sites.bays.map.num + annotation_custom(grob = NEmap.full.us, xmin = -102, xmax = -92.5,
#                                                            ymin = 36.5, ymax = 41.5)
#

# png(height = 4, width = 8, units = "in", res = 300, "outputs/cpaper_figures_struct_cohort_scaling/site_map_and_climate_space.png")
# plot_grid(sites.bays.map, climate.space.ci, ncol = 2, align = "hv", labels = "AUTO")
# dev.off()
#
# png(height = 4, width = 8, units = "in", res = 300, "outputs/cpaper_figures_struct_cohort_scaling/site_map_and_climate_space_sd.png")
# plot_grid(sites.bays.map, climate.space.sd, ncol = 2, align = "hv", labels = "AUTO")
# dev.off()

# create the same maps but with inset of US
# png(height = 4, width = 8, units = "in", res = 300, "outputs/cpaper_figures_struct_cohort_scaling/site_map_and_climate_space_inset.png")
# plot_grid(site.bayes.map.inset, climate.space.ci, ncol = 2, align = "hv", labels = "AUTO")
# dev.off()
#
# png(height = 4, width = 8, units = "in", res = 300, "outputs/cpaper_figures_struct_cohort_scaling/site_map_and_climate_space_sd_inset.png")
# plot_grid(site.bayes.map.inset, climate.space.sd, ncol = 2, align = "hv", labels = "AUTO")
# dev.off()
#
# # map with sites as numbers instead of site names
# png(height = 4, width = 8, units = "in", res = 300, "outputs/cpaper_figures_struct_cohort_scaling/site_map_and_climate_space_inset.png")
# plot_grid(site.bayes.map.inset.num, climate.space.ci, ncol = 2, align = "hv", labels = "AUTO")
# dev.off()

# --------make the same map, but with ecoregions as the backdrop:
library(sf)
wwf <- readOGR(dsn = "data/official/wwf_terr_ecos.shp")
wwf





# shp <- read_sf('data/na_cec_eco_l1/NA_CEC_Eco_Level1.shp')
# ggplot(shp) + geom_sf(aes(colour = NA_L1NAME))
# read in ecoregion shapefile
#eco <- readOGR(dsn = "data/na_cec_eco_l1/NA_CEC_Eco_Level1.shp", layer = "NA_CEC_Eco_Level1")
#eco

# recieving topology problem errors
#rgeos::gIsValid(eco)
# set 0 width buffer to clean up these errors
#peco_states <- gBuffer(eco, byid=TRUE, width=0)

#rgeos::gIsValid(peco_states)

#eco.2<- gSimplify(eco, tol=0.01, topologyPreserve=TRUE)

# Simplify the shapefile with 'ms_simplify', keeping 1%
# eco01_shp <- ms_simplify(peco_states, keep = 0.01, keep_shapes = T)
# us01_dt <- broom::tidy(eco01_shp, region = "id") %>% data.table()

spydf_states<- wwf
spydf_states <- rgeos::gSimplify(spydf_states, tol = 0.00001)

# this is a well known R / GEOS hack (usually combined with the above) to 
# deal with "bad" polygons
spydf_states <- rgeos::gBuffer(spydf_states, byid=TRUE, width=0)

# any bad polys?
sum(rgeos::gIsValid(spydf_states, byid=TRUE)==FALSE)

## [1] 0

system.time(plot(spydf_states))

#plot(eco)
#plot(ne_state)

# Remove Alaska, Hawaii, and Puerto Rico
ne_state_sub<- ne_state %>% subset(!(gn_name %in% c("Hawaii", "Alaska")))
plot(ne_state_sub)

ne_state_sub <- rgeos::gSimplify(ne_state_sub, tol = 0.00001)

# this is a well known R / GEOS hack (usually combined with the above) to 
# deal with "bad" polygons
ne_state_sub<- rgeos::gBuffer(ne_state_sub, byid=TRUE, width=0)

library(rgeos)
library(sp)
clip_shp = function(small_shp, large_shp){
  # make sure both have the same proj
  large_shp = spTransform(large_shp, CRSobj = CRS(proj4string(small_shp)))
  cat("About to get the intersections, will take a while...", "\n")
  clipped_shp = rgeos::gIntersection(small_shp, large_shp, byid = T, drop_lower_td = T)
  cat("Intersection done", "\n")
  x = as.character(row.names(clipped_shp))
  # these are the data to keep, can be duplicated
  keep = gsub(pattern = "^[0-9]{1,2} (.*)$", replacement = "\\1", x)
  large_shp_data = as.data.frame(large_shp@data[keep,])
  row.names(clipped_shp) = row.names(large_shp_data)
  clipped_shp = spChFIDs(clipped_shp, row.names(large_shp_data))
  # combine and make SpatialPolygonsDataFrame back
  clipped_shp = SpatialPolygonsDataFrame(clipped_shp, large_shp_data)
  clipped_shp
}

clipped.eco <- clip_shp(ne_state_sub, wwf)

# the larger the tol is, the less rows the result will have
thin = function(x, tol = 0.01){
  id = unique(x$id)[1]
  x1 = x[, 1:2]
  names(x1) = c("x", "y")
  x2 <-shapefiles::dp(x1, tol)
  data.frame(long = x2$x, lat = x2$y, id = id)
}

library(ggplot2)
library(dplyr)
# convert shapefile to data frame
shp_df = fortify(clipped.eco, region = "BIOME") # change the region accordingly
# for each group, thin it
shp_df_thin = dplyr::select(shp_df, long, lat, id, group) %>%
  group_by(group) %>%
  do(thin(., tol = 0.02))


biome.names <- unique (clipped.eco@data[c("BIOME", "G200_REGIO")])
biome.names <- biome.names[!is.na(biome.names$G200_REGIO),]

#shp_df_thin<- merge(shp_df_thin, biome.names, by.x = "id", by.y = "BIOME")
biome.names.test <- data.frame(
  id = c(1, 12, 13, 3, 
         4, 5, 6, 7, 
         8, 9, 98),
  BIOME_NAME = c(NA, "Chaparral and Woodlands", "Desert/Aridlands", "Pine-Oak Forests",
                 "Eastern Forests", "Mixed Forests", NA, "Southeastern forests", 
                 "Grasslands & Prairies", "Everglades Flooded Grasslands", NA)
)

shp_df_thin.m <- merge(biome.names.test, shp_df_thin, by = "id")

bbox3 <- data.frame(long = c(-102, -84.25, -84.25, -102, -102),
                    lat = c(36.5, 36.5, 50.0, 50.0, 36.5))

full.us.wwf.eco <- ggplot() + 
  geom_polygon(data = na.omit(shp_df_thin.m), aes(x = long, y = lat, group = group,  fill = BIOME_NAME)) +
  coord_map() +theme_bw()+scale_fill_manual(values = c("Chaparral and Woodlands"= "#a6cee3",
                                                       "Pine-Oak Forests"="#1f78b4",
                                                       "Eastern Forests"="#b2df8a",
                                                       "Mixed Forests"="#33a02c",
                                                       "Desert/Aridlands"="#fb9a99",
                                                       "Southeastern forests"="#e31a1c",
                                                       "Grasslands & Prairies"="#fdbf6f",
                                                       "Everglades Flooded Grasslands"="#ff7f00"))+theme(panel.grid.major = element_blank(), axis.title = element_blank(), legend.title = element_blank())+
  geom_path(data = bbox3, aes(x = long, y = lat), size = 0.75, color = "black")+ theme_bw() +
  theme(axis.title = element_blank(), panel.grid = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = NA), 
        axis.text = element_blank(), axis.ticks = element_blank())



sites.bays.map.num.eco <- full.us.wwf.eco + geom_point(data = numbered.sites, aes(x = coords.x1.jitter, y = coords.x2.jitter, shape=structure, color =structure), size = 2)+
  scale_color_manual(values = c("Savanna"='sienna4', "Forest"='forestgreen'))+
  geom_text_repel(data = numbered.sites, aes(x = coords.x1, y = coords.x2, label=number),
                  fontface = 'bold', color = 'black',
                  box.padding = unit(0.5, "lines"),
                  point.padding = unit(0.5, "lines")) + coord_cartesian(xlim = c(-100, -85), ylim=c(38, 49)) +theme_bw()+
  theme(legend.title = element_blank(), legend.position= c(0.284, 0.155), # legend.position= "none",legend.position = "bottom", legend.direction = "vertical", 
        legend.background = element_rect(color = "black", fill = "white", size = 0.1, linetype = "solid"),
        legend.text=element_text(size=8), legend.spacing = unit(0.1, "lines"), 
        legend.key.size = unit(0.2, "lines"), axis.title = element_blank())
legend.map <- get_legend(sites.bays.map.num.eco)


site.bayes.map.inset.num.eco <- sites.bays.map.num.eco+guides(color = FALSE, shape = FALSE) +
  annotation_custom(grob =  ggplotGrob(full.us.wwf.eco+theme(legend.position = "none", axis.text = element_blank(), axis.ticks = element_blank())), 
                    xmin = -92, xmax = -83.9,
                    ymin = 36, ymax = 41.5)



# now plot the tree ring sites on this map and make an inset
pts.map <- ggplot()+ geom_point(data = numbered.sites, aes(x = coords.x1.jitter, y = coords.x2.jitter, shape=structure, color =structure), size = 2)+
  scale_color_manual(values = c("Savanna"='sienna4', "Forest"='forestgreen'))+theme_bw()

legend.sf <- get_legend(pts.map + theme(legend.position = "bottom", legend.title = element_blank()))
legend.colors <- get_legend(full.us.wwf.eco)


# create the same maps but with inset of US
png(height = 4, width = 8, units = "in", res = 300, "outputs/paper_figures_struct_cohort_scaling/site_map_and_climate_space_inset_eco.png")
plot_grid(site.bayes.map.inset.num.eco, climate.space.ci, ncol = 2, align = "hv", labels = c("a)", "b)"), label_fontface = "plain", label_x = 0.15, label_y = 0.95)
dev.off()

png(height = 4.1, width = 8, units = "in", res = 300, "outputs/paper_figures_struct_cohort_scaling/site_map_and_climate_space_inset_eco2.png")
plot_grid(
  plot_grid(site.bayes.map.inset.num.eco, climate.space.ci, ncol = 2, align = "hv", labels = c("a)", "b)"),label_fontface = "plain",label_x = 0.15, label_y = 0.95),
  plot_grid( legend.sf), ncol = 1, rel_heights = c(1,0.12))
dev.off()

# # make the same figure, but with conus forest biomass underlaying the map:
# 
# biomass <- raster("data/conus_forest_biomass/conus_forest_biomass_mg_per_ha.img")
# biomass
# 
# # may take a bit:
# 
# biomass.small <- raster::crop(biomass, y = c(xmin = -10000, xmax = 2000000, ymin = 1500000, ymax = 3077625))
# 
# plot(biomass.small)
# system.time(biomass.ll <- projectRaster(biomass.small, crs = '+init=epsg:4269'))
# plot(biomass.ll)
# 
# # convert raster to dataframe to plot in ggplot:
# biomass.table <- data.frame(xyFromCell(biomass.ll, 1:ncell(biomass.ll)),
#                          getValues(biomass.ll))
# biomass.table2<- biomass.table[!is.na(biomass.table)]
# ggplot(biomass.table, aes(x,y, fill = getValues.biomass.ll.))+geom_raster()
# #-------------------------------------------------------------------------------------------------
#                                     Figure 3: Dotplots
#-------------------------------------------------------------------------------------------------
# pull parameters from the cohort only model and cohortXtime model:
# cohort only model: lag2_reg_cohort_only_re_t_pr_dry_yrs_site_rs_inter

train.dry.pair <- readRDS("data/train_dry_paired_struct_cohort_scaled_dataset_v5.rds")
test.dry.pair <- readRDS("data/test_dry_paired_struct_cohort_scaled_dataset_v5.rds")
full.dry.pair <- readRDS("data/full_dry_paired_struct_cohort_scaled_dataset_v5.rds")

# assign site numbers to each site:

site.num.df <- data.frame(site = as.character(unique(train.dry.pair$site)), 
                          site.num = 1:length(as.character(unique(train.dry.pair$site))))

# if site num is not in the df, add it
if(! "site.num" %in% colnames(train.dry.pair)){
  train.dry.pair <- merge(train.dry.pair, site.num.df, by = "site" )
}
if(! "site.num" %in% colnames(test.dry.pair)){
  test.dry.pair <- merge(test.dry.pair, site.num.df, by = "site" )
}
if(! "site.num" %in% colnames(full.dry.pair)){
  full.dry.pair <- merge(full.dry.pair, site.num.df, by = "site" )
}



cohort.params <- readRDS("outputs/growth_model/cohort_struct_scaled_lag2_reg_cohort_only_re_t_pr_dry_yrs_site_rs_inter/samps_v5.rds")
pred.pair <- readRDS("outputs/growth_model/cohort_struct_scaled_lag2_reg_cohort_only_re_t_pr_dry_yrs_site_rs_inter/predicted_growth_YP_v5.rds")

pred.cohort.summary <- pred.pair %>% group_by(variable) %>% dplyr::summarise(predicted = mean(exp(value), na.rm =TRUE),
                                                                             Ci.lo = quantile(exp(value), 0.025, na.rm = TRUE),
                                                                             Ci.hi = quantile(exp(value), 0.975, na.rm =TRUE),
                                                                             observed = mean(RWI, na.rm =TRUE))

pred.obs <- summary(lm(pred.cohort.summary$predicted ~ pred.cohort.summary$observed))

mod1 <- read.csv("/Users/kah/Documents/TreeRings2/outputs/growth_model/model_summary/cohort_struct_scaled_lag2_reg_cohort_only_re_t_pr_dry_yrs_site_rs_inter_summary_v5.csv")


# this does a poor job representing iWUE values by itself, but explains som of the variation
p.o.plot.growth.ageclass <- ggplot(pred.cohort.summary, aes(observed, predicted))+geom_point(color = "black", size = 0.5)+
  geom_errorbar(data = pred.cohort.summary,aes(ymin=Ci.lo, ymax=Ci.hi), color = "grey", alpha = 0.5)+geom_point(data = pred.cohort.summary, aes(observed, predicted), color = "black", size = 0.5)+
  geom_abline(aes(slope = 1, intercept = 0), color = "red", linetype = "dashed")+geom_text(data=data.frame(pred.obs$r.squared), aes( label = paste("R^2: ", round(mod1$Rsq, digits = 3), sep="")),parse=T,x=1.5, y=15)+
  ylab("Predicted Tree Growth (mm)")+xlab("Observed Tree Growth (mm)")+theme_bw(base_size = 16)+theme(panel.grid = element_blank())+ylim(0, 18)+xlim(0, 9)




png(height = 4, width = 4, units = "in", res = 200, "outputs/paper_figures_struct_cohort_scaling/RWI_pred_vs_obs_cohort_only_model.png")
p.o.plot.growth.ageclass
dev.off()


samps <- data.frame(as.matrix(cohort.params))


alpha.samps  <- samps[,1:9]# one alpha for each of 4 cohort-strcuture groups
beta2.samps <- samps[,10:11]
beta3.samps <- samps[,12:13]
beta4.samps <- samps[,14:15]
beta5.samps <- samps[,16:17]
beta6.samps <- samps[,18:19]
beta7.samps <- samps[,20:21]
mu_beta.samps <- samps[,22:28]
sigma.samps <- samps[,29]


# plot marginal distributions of cohort + structure specific parameters:
a <- data.frame(alpha.samps)
colnames(a) <- unique(train.dry.pair$site)#[order(unique(train.dry.pair[,c("site", "site.num")])[,2])])
a$num <- rownames(a)
a.m <- melt(a, id.vars=c("num"))
alpha.mplots <- ggplot(a.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Random intercepts")


b2 <- data.frame(beta2.samps)
colnames(b2) <- c("Modern" ,"Past")
#colnames(b2) <- c(paste0(c(unique(train.dry.pair$struct.cohort))))
b2$num <- rownames(b2)
b2.m <- melt(b2, id.vars=c("num"))
b2.mplots <- ggplot(b2.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Precipitation Index slope")

b3 <- data.frame(beta3.samps)
colnames(b3) <- c("Modern" ,"Past")
b3$num <- rownames(b3)
b3.m <- melt(b3, id.vars=c("num"))
b3.mplots <- ggplot(b3.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("DBH Index slope")


b4 <- data.frame(beta4.samps)
colnames(b4) <- c("Modern" ,"Past")
b4$num <- rownames(b4)
b4.m <- melt(b4, id.vars=c("num"))
b4.mplots <- ggplot(b4.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Log_RWI-1 Index slope")

b5 <- data.frame(beta5.samps)
colnames(b5) <- c("Modern" ,"Past")
b5$num <- rownames(b5)
b5.m <- melt(b5, id.vars=c("num"))
b5.mplots <- ggplot(b5.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Log_RWI-2 Index slope")

b6 <- data.frame(beta6.samps)
colnames(b6) <- c("Modern" ,"Past")
b6$num <- rownames(b6)
b6.m <- melt(b6, id.vars=c("num"))
b6.mplots <- ggplot(b6.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Summer Temperature slope")

b7 <- data.frame(beta7.samps)
colnames(b7) <- c("Modern" ,"Past")
b7$num <- rownames(b7)
b7.m <- melt(b7, id.vars=c("num"))
b7.mplots <- ggplot(b7.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Precip*Temp slope")



# -------------------- DOTPLOTS by modern vs. past only ---------------------
# make dotplots for all the factors in the model--
# get summaries by age class from the melted samples:

a1.sum <- a.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                          Ci.low = quantile(value, 0.025), 
                                                          Ci.high = quantile(value, 0.975))
a1.sum$variable <- unique(train.dry.pair$site)

df.site.struct <- unique(train.dry.pair[,c("site", "structure")])
a1.sum <- merge(a1.sum, df.site.struct, by.x = "variable", by.y = "site")
a1.sum$structure <- factor(a1.sum$structure, levels = c( "Forest", "Savanna"))
a1.sum$site.num <- as.character(1:length(a1.sum$variable))
a1.sum$site.num <-factor(a1.sum$site.num, levels = c("9", "8", "7", "6", "5", "4", "3", "2", "1"))




b2.sum <- b2.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975),
                                                           hdi.low = HDInterval::hdi(value)[1], 
                                                           hdi.high = HDInterval::hdi(value)[2] )

b2.sum$variable <- factor(b2.sum$variable, levels = c( "Modern", "Past"))
colnames(b2.sum) <- c("cohort", "mean.val", "Ci.low", "Ci.high", "hdi.low", "hdi.high")


# percent differences:
b2.diffs.pct <- ((b2[,1] - b2[,2])/b2[,2])*100

mean(b2.diffs.pct)
quantile(b2.diffs.pct, 0.025)
quantile(b2.diffs.pct, 0.975)
b3.sum <- b3.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975),
                                                           hdi.low = HDInterval::hdi(value)[1], 
                                                           hdi.high = HDInterval::hdi(value)[2] )
b3.sum$variable <- factor(b3.sum$variable, levels = c( "Modern", "Past"))
colnames(b3.sum) <- c("cohort", "mean.val", "Ci.low", "Ci.high", "hdi.low", "hdi.high")

b4.sum <- b4.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975),
                                                           hdi.low = HDInterval::hdi(value)[1], 
                                                           hdi.high = HDInterval::hdi(value)[2] )


b4.sum$variable <- factor(b4.sum$variable, levels = c( "Modern", "Past"))
colnames(b4.sum) <- c("cohort", "mean.val", "Ci.low", "Ci.high", "hdi.low", "hdi.high")

b5.sum <- b5.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975),
                                                           hdi.low = HDInterval::hdi(value)[1], 
                                                           hdi.high = HDInterval::hdi(value)[2] )
b5.sum$variable <- factor(b5.sum$variable, levels = c( "Modern", "Past"))
colnames(b5.sum) <- c("cohort", "mean.val", "Ci.low", "Ci.high", "hdi.low", "hdi.high")

b6.sum <- b6.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975),
                                                           hdi.low = HDInterval::hdi(value)[1], 
                                                           hdi.high = HDInterval::hdi(value)[2] )
b6.sum$variable <- factor(b6.sum$variable, levels = c( "Modern", "Past"))
colnames(b6.sum) <- c("cohort", "mean.val", "Ci.low", "Ci.high", "hdi.low", "hdi.high")


b7.sum <- b7.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975),
                                                           hdi.low = HDInterval::hdi(value)[1], 
                                                           hdi.high = HDInterval::hdi(value)[2] )
b7.sum$variable <- factor(b7.sum$variable, levels = c( "Modern", "Past"))
colnames(b7.sum) <- c("cohort", "mean.val", "Ci.low", "Ci.high", "hdi.low", "hdi.high")


# write out all the dotplots with 95% ci
int.dot.age <- ggplot(data.frame(a1.sum), aes(x = mean.val, y = site.num, color = structure, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point(alpha = 0.5)+scale_color_manual(values = c("Savanna"='sienna4', 'Forest' = "forestgreen"))+theme_bw(base_size = 14)+theme(legend.position = "none", axis.title.y = element_blank(), panel.grid = element_blank())+xlab(expression(paste("Intercept (", alpha[s], ")")))+xlim(-0.3, 0.8) + geom_vline(xintercept = 0, linetype = "dashed")

b2.dot.age <- ggplot(data.frame(b2.sum), aes(x = mean.val, y = cohort,  size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point(alpha = 0.5)+xlim(-0.1, 0.25)+scale_color_manual(values = c("Past"='#2166ac', 'Modern' = "#b2182b"))+theme_bw(base_size = 14)+theme(legend.position = "none", axis.title.y = element_blank(), panel.grid = element_blank())+xlab(expression(paste("Precip. Sensitivity (", beta[1], ")")))+xlim(-0.3, 0.8)+ geom_vline(xintercept = 0, linetype = "dashed")

b3.dot.age <- ggplot(data.frame(b3.sum), aes(x = mean.val, y = cohort,  size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point(alpha = 0.5)+xlim(-0.1, 0.25)+scale_color_manual(values = c("Past"='#2166ac', 'Modern' = "#b2182b"))+theme_bw(base_size = 14)+theme(legend.position = "none", axis.title.y = element_blank(), panel.grid = element_blank())+xlab(expression(paste("DBH Sensitivity (", beta[6], ")")))+xlim(-0.3, 0.8)+ geom_vline(xintercept = 0, linetype = "dashed")

b4.dot.age <- ggplot(data.frame(b4.sum), aes(x = mean.val, y = cohort,  size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point(alpha = 0.5)+scale_color_manual(values = c("Past"='#2166ac', 'Modern' = "#b2182b"))+theme_bw(base_size = 14)+theme(legend.position = "none", axis.title.y = element_blank(), panel.grid = element_blank())+xlab(expression(paste("lag-1 growth coef. (", beta[4], ")")))+xlim(-0.3, 0.8)+ geom_vline(xintercept = 0, linetype = "dashed")

b5.dot.age <- ggplot(data.frame(b5.sum), aes(x = mean.val, y = cohort,  size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point(alpha = 0.5)+scale_color_manual(values = c("Past"='#2166ac', 'Modern' = "#b2182b"))+theme_bw(base_size = 14)+theme(legend.position = "none", axis.title.y= element_blank(), panel.grid = element_blank())+xlab(expression(paste("lag-2 growth coef. (", beta[5], ")")))+xlim(-0.3, 0.8)+ geom_vline(xintercept = 0, linetype = "dashed")


b6.dot.age <- ggplot(data.frame(b6.sum), aes(x = mean.val, y = cohort,  size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point(alpha = 0.5)+scale_color_manual(values = c("Past"='#2166ac', 'Modern' = "#b2182b"))+theme_bw(base_size = 14)+theme(legend.position = "none", axis.title.y = element_blank(), panel.grid = element_blank())+xlab(expression(paste("Max. Temp. sensitivity (", beta[2], ")")))+xlim(-0.3, 0.8)+ geom_vline(xintercept = 0, linetype = "dashed")

b7.dot.age <- ggplot(data.frame(b7.sum), aes(x = mean.val, y = cohort,  size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point(alpha = 0.5)+scale_color_manual(values = c("Past"='#2166ac', 'Modern' = "#b2182b"))+theme_bw(base_size = 14)+theme(legend.position = "none", axis.title.y = element_blank(), panel.grid = element_blank())+xlab(expression(paste("Temp*Precip sensitivity (", beta[3], ")")))+xlim(-0.3, 0.8)+ geom_vline(xintercept = 0, linetype = "dashed")

mod.past.legend <- get_legend(ggplot(data.frame(b7.sum), aes(x = mean.val, y = cohort, color = cohort))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high,height = 0))+
                                geom_point(alpha = 0.5)+scale_color_manual(values = c("Past"='#2166ac', 'Modern' = "#b2182b"))+theme_bw(base_size = 14)+theme(legend.title = element_blank()))

png(height = 13, width = 5, units = "in", res = 300, "outputs/paper_figures_struct_cohort_scaling/full_dot_plot_cohort_only_CI.png")
cowplot::plot_grid(int.dot.age, 
                   b2.dot.age, 
                   b6.dot.age, 
                   b7.dot.age, 
                   b3.dot.age, 
                   b4.dot.age, 
                   b5.dot.age, ncol = 1, align = "v")
dev.off()



#--------------------------- make figure 3 ----------------------------------------------------
# read in the mcmc samples for all the beta and population level parameters
samps <- readRDS("outputs/growth_model/cohort_struct_scaled_lag2_reg_cohort_only_re_t_pr_dry_yrs_site_rs_inter/samps_v5.rds")

int.mcmc <- as.mcmc(samps)
int.mcmc.mat <- as.matrix(int.mcmc)
int.mcmc.dat <- data.frame(int.mcmc.mat)



# plot the conditional effect of Temperature across MAP:
MAP.sim <- seq(min(train.dry.pair$MAP.scaled), max(train.dry.pair$MAP.scaled), by = 0.1)

## Calculate conditional effect of X1 across the range of X2
#int.mcmc <- as.mcmc(samp.structure.cohort.re[[1]])

int.mcmc.mat <- as.matrix(int.mcmc)
int.mcmc.dat <- data.frame(int.mcmc.mat)

int.1 <- matrix(rep(NA, nrow(int.mcmc.dat)*length(MAP.sim)), nrow = nrow(int.mcmc.dat))
int.2 <- matrix(rep(NA, nrow(int.mcmc.dat)*length(MAP.sim)), nrow = nrow(int.mcmc.dat))
#int.3 <- matrix(rep(NA, nrow(int.mcmc.dat)*length(MAP.sim)), nrow = nrow(int.mcmc.dat))
#int.4 <- matrix(rep(NA, nrow(int.mcmc.dat)*length(MAP.sim)), nrow = nrow(int.mcmc.dat))

# simulate the effect of beta 1 conditional on Tmax
for(i in 1:length(MAP.sim)){
  int.1[, i] <- int.mcmc.dat$beta6.1. + int.mcmc.dat$beta7.1. * MAP.sim[i]
  int.2[, i] <- int.mcmc.dat$beta6.2. + int.mcmc.dat$beta7.2. * MAP.sim[i]
  #int.3[, i] <- int.mcmc.dat$beta6.3. + int.mcmc.dat$beta7.3. * MAP.sim[i]
  #int.4[, i] <- int.mcmc.dat$beta6.4. + int.mcmc.dat$beta7.4. * MAP.sim[i]
  
}

## Note: the variance now comes from the posterior, not the vcov matrix

bayes.c.eff.mean1 <- apply(int.1, 2, mean)
bayes.c.eff.lower1 <- apply(int.1, 2, function(x) quantile(x, probs = c(0.025)))
bayes.c.eff.upper1 <- apply(int.1, 2, function(x) quantile(x, probs = c(0.975)))

bayes.c.eff.mean2 <- apply(int.2, 2, mean)
bayes.c.eff.lower2 <- apply(int.2, 2, function(x) quantile(x, probs = c(0.025)))
bayes.c.eff.upper2 <- apply(int.2, 2, function(x) quantile(x, probs = c(0.975)))



# summarise all the data -> this is ugly but it does the job
one<- train.dry.pair %>% filter(struct.cohort.code %in% "1") %>% dplyr::select(MAP.prism)
two<- train.dry.pair %>% filter(struct.cohort.code %in% "2") %>% dplyr::select(MAP.prism)



plot.dat1 <- data.frame(MAP.sim,MAP =unscale_function(zVar = MAP.sim, myVar = train.dry.pair$MAP.prism) , mean = bayes.c.eff.mean1, Ci.low = bayes.c.eff.lower1, Ci.high = bayes.c.eff.upper1, cohort = "Modern")

plot.dat2 <- data.frame(MAP.sim, MAP = unscale_function(zVar = MAP.sim, myVar = train.dry.pair$MAP.prism), mean = bayes.c.eff.mean2, Ci.low = bayes.c.eff.lower2, Ci.high = bayes.c.eff.upper2, cohort = "Past")

#plot.dat3 <- data.frame(MAP.sim, MAP = unscale_function(zVar = MAP.sim, myVar = three$MAP.prism) , mean = bayes.c.eff.mean3, Ci.low = bayes.c.eff.lower3, Ci.high = bayes.c.eff.upper3, cohort = "Past-Savanna", structure = "Savanna")

#plot.dat4 <- data.frame(MAP.sim, MAP = unscale_function(zVar = MAP.sim, myVar = four$MAP.prism), mean = bayes.c.eff.mean4, Ci.low = bayes.c.eff.lower4, Ci.high = bayes.c.eff.upper4, cohort = "Modern-Savanna", structure = "Savanna")


plot.dat.MAP <- rbind(plot.dat1, plot.dat2)
plot.dat.MAP$cohort <- as.factor(plot.dat.MAP$cohort)

## Foundation for the plot & line for the posterior mean of the Bayesian conditional effect
MAP.conditional <- ggplot(plot.dat.MAP, aes(x = MAP, y = mean, color = cohort)) + geom_line( alpha = 0.8, size = 0.5)+ geom_ribbon(aes(ymin = Ci.low, ymax = Ci.high, fill = cohort),  alpha = 0.2, linetype = "blank") + 
  ylab("Condtional effect of tmax")+xlab("Precipitation (mm)")+scale_color_manual(values = c("Past"='#2166ac', 'Modern' = "#b2182b"))+scale_fill_manual(values = c("Past"='#2166ac', 'Modern' = "#b2182b"))+theme_bw(base_size = 18)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = c(0.85,0.15), legend.title = element_blank())+ylim(-0.4, 0.2)+notoprightpanels

MAP.conditional

png(height = 4, width = 4, units = "in", res = 300, "outputs/paper_figures_struct_cohort_scaling/cohort_only_conditional_effect_tmax_fig3.png")
MAP.conditional
dev.off()
# -------------------------read in cohort X structure model samples:

cohort.struct.params <- readRDS("outputs/growth_model/cohort_struct_scaled_lag2_reg_struct_x_cohort_re_t_pr_dry_yrs_site_rs_inter/samps_v5.rds")
#train.dry.pair <- readRDS("outputs/growth_model/cohort_struct_scaled_lag2_reg_struct_x_cohort_re_t_pr_dry_yrs_site_rs_inter/train.rds")
#test.dry.pair <- readRDS("outputs/growth_model/cohort_struct_scaled_lag2_reg_struct_x_cohort_re_t_pr_dry_yrs_site_rs_inter/test.rds")
pred.pair<-readRDS("outputs/growth_model/cohort_struct_scaled_lag2_reg_struct_x_cohort_re_t_pr_dry_yrs_site_rs_inter/YP.samps_v5.rds")

pred.cs <- data.frame(pred.pair[[1]])
head(pred.cs)
pred.cs.m <- melt(pred.cs)

pred.cohort.summary <- pred.cs.m %>% group_by(variable) %>% dplyr::summarise(Predicted = mean(exp(value), na.rm =TRUE),
                                                                             ci.lo = quantile(exp(value), 0.025, na.rm = TRUE),
                                                                             ci.hi = quantile(exp(value), 0.975, na.rm =TRUE))
pred.cohort.summary$Observed <- full.dry.pair$RWI
pred.cohort.summary$struct.cohort.code <- full.dry.pair$struct.cohort.code
pred.cohort.summary$struct.cohort <- full.dry.pair$struct.cohort
pred.obs.cs <- summary(lm(pred.cohort.summary$Predicted ~ pred.cohort.summary$Observed))
# 

mod2 <- read.csv("/Users/kah/Documents/TreeRings2/outputs/growth_model/model_summary/cohort_struct_scaled_lag2_reg_struct_x_cohort_re_t_pr_dry_yrs_site_rs_inter_summary.csv")


# # this does a poor job representing iWUE values by itself, but explains som of the variation
p.o.plot.growth.cs <- ggplot(pred.cohort.summary, aes(Observed, Predicted))+geom_point(color = "black", size = 0.5)+
  geom_errorbar(data = pred.cohort.summary,aes(ymin=ci.lo, ymax=ci.hi), color = "grey", alpha = 0.5)+
  geom_point(data = pred.cohort.summary, aes(Observed, Predicted), color = "black", size = 0.5)+
  geom_abline(aes(slope = 1, intercept = 0), color = "red", linetype = "dashed")+
  geom_text(data=data.frame(pred.obs.cs$r.squared), aes( label = paste("R^2: ", round(mod2$Rsq, digits = 3), sep="")),parse=T,x=1.5, y=15)+
  theme_bw(base_size = 16)+ylab("Predicted Tree Growth (mm)")+xlab("Observed Tree Growth (mm)")+theme(panel.grid = element_blank())+ylim(0, 18)+xlim(0, 9)





cs.growth.summary<- pred.cohort.summary %>% group_by(struct.cohort) %>% dplyr::summarise(growth = mean (Predicted, na.rm =TRUE),
                                                                                         Ci.lo = quantile(Predicted, 0.025, na.rm = TRUE),
                                                                                         Ci.hi = quantile(Predicted, 0.975, na.rm =TRUE))

cs.growth.summary$struct.cohort<- factor(cs.growth.summary$struct.cohort, levels = c(   "Past-Savanna", "Past-Forest", "Modern-Savanna","Modern-Forest" ))

predicted.growth.cs <- ggplot(cs.growth.summary, aes(struct.cohort, y = growth, fill = struct.cohort))+geom_bar(stat = "identity")+scale_fill_manual(values = c("Past-Savanna"='#a6611a',"Modern-Savanna"='#dfc27d',"Modern-Forest"='#80cdc1', "Past-Forest"='#018571'))+
  geom_errorbar(aes(x = struct.cohort, ymin = Ci.lo, ymax = Ci.hi), alpha = 0.8, size = 0.5, width = 0.2)+ylab("Predicted growth (mm)")+theme_bw(base_size = 16)+theme(panel.grid = element_blank(), axis.title.x = element_blank(), legend.title = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+ylim(0, 5.7)


new_full.cs.pred <- left_join(pred.cs.m, pred.cohort.summary[,c("variable", "struct.cohort")], by = "variable")

cs.growth.full.summary <- new_full.cs.pred %>% group_by(struct.cohort) %>% dplyr::summarise(growth = mean (value, na.rm =TRUE),
                                                                                           Ci.lo = quantile(value, 0.025, na.rm = TRUE),
                                                                                           Ci.hi = quantile(value, 0.975, na.rm =TRUE))


ggplot(cs.growth.full.summary, aes(struct.cohort, y = growth, fill = struct.cohort))+geom_bar(stat = "identity")+#scale_fill_manual(values  = c("Past"='#2166ac', 'Modern' = "#b2182b"))+
  geom_errorbar(aes(x = struct.cohort, ymin = Ci.lo, ymax = Ci.hi), alpha = 0.8, size = 0.5, width = 0.2)+ylab("Predicted growth")+theme_bw(base_size = 16)+theme(panel.grid = element_blank(), axis.title.x = element_blank(), legend.title = element_blank())


samps.cs <- as.data.frame(cohort.struct.params)


alpha.samps.cs  <- samps.cs[,1:9]# one alpha for each of 4 cohort-strcuture groups
beta2.samps.cs <- samps.cs[,10:13]
beta3.samps.cs <- samps.cs[,14:17]
beta4.samps.cs <- samps.cs[,18:21]
beta5.samps.cs <- samps.cs[,22:25]
beta6.samps.cs <- samps.cs[,26:29]
beta7.samps.cs <- samps.cs[,30:33]
mu_beta.samps.cs <- samps.cs[,34:40]
sigma.samps.cs <- samps.cs[,41]


# plot marginal distributions of cohort + structure specific parameters:
a <- data.frame(alpha.samps.cs)
colnames(a) <- unique(train.dry.pair$site)#[order(unique(train.dry.pair[,c("site", "site.num")])[,2])])
a$num <- rownames(a)
a.m <- melt(a, id.vars=c("num"))
alpha.mplots <- ggplot(a.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Random intercepts")


b2 <- data.frame(beta2.samps.cs)
colnames(b2) <-c(unique(train.dry.pair$struct.cohort)[order(unique(train.dry.pair[,c("struct.cohort", "struct.cohort.code")])[,2])])
#colnames(b2) <- c(paste0(c(unique(train.dry.pair$struct.cohort))))
b2$num <- rownames(b2)
b2.m <- melt(b2, id.vars=c("num"))
b2.mplots <- ggplot(b2.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Precipitation Index slope")

b3 <- data.frame(beta3.samps.cs)
colnames(b3) <- c(unique(train.dry.pair$struct.cohort)[order(unique(train.dry.pair[,c("struct.cohort", "struct.cohort.code")])[,2])])
b3$num <- rownames(b3)
b3.m <- melt(b3, id.vars=c("num"))
b3.mplots <- ggplot(b3.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("DBH Index slope")


b4 <- data.frame(beta4.samps.cs)
colnames(b4) <- c(unique(train.dry.pair$struct.cohort)[order(unique(train.dry.pair[,c("struct.cohort", "struct.cohort.code")])[,2])])
b4$num <- rownames(b4)
b4.m <- melt(b4, id.vars=c("num"))
b4.mplots <- ggplot(b4.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Log_RWI-1 Index slope")

b5 <- data.frame(beta5.samps.cs)
colnames(b5) <- c(unique(train.dry.pair$struct.cohort)[order(unique(train.dry.pair[,c("struct.cohort", "struct.cohort.code")])[,2])])
b5$num <- rownames(b5)
b5.m <- melt(b5, id.vars=c("num"))
b5.mplots <- ggplot(b5.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Log_RWI-2 Index slope")

b6 <- data.frame(beta6.samps.cs)
colnames(b6) <- c(unique(train.dry.pair$struct.cohort)[order(unique(train.dry.pair[,c("struct.cohort", "struct.cohort.code")])[,2])])
b6$num <- rownames(b6)
b6.m <- melt(b6, id.vars=c("num"))
b6.mplots <- ggplot(b6.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Summer Temperature slope")

b7 <- data.frame(beta7.samps.cs)
colnames(b7) <- c(unique(train.dry.pair$struct.cohort)[order(unique(train.dry.pair[,c("struct.cohort", "struct.cohort.code")])[,2])])
b7$num <- rownames(b7)
b7.m <- melt(b7, id.vars=c("num"))
b7.mplots <- ggplot(b7.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Precip*Temp slope")



# ------------------------- STRUCT X COHORT DOTPLOT MODEL SUMMARIES -----------------------

# get summaries by struct-cohort class from the melted samples:

a1.sum <- a.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                          Ci.low = quantile(value, 0.025), 
                                                          Ci.high = quantile(value, 0.975))
a1.sum$variable <- unique(train.dry.pair$site)

df.site.struct <- unique(train.dry.pair[,c("site", "structure")])
a1.sum <- merge(a1.sum, df.site.struct, by.x = "variable", by.y = "site")
a1.sum$structure <- factor(a1.sum$structure, levels = c( "Forest", "Savanna"))
a1.sum$site.num <- as.character(1:length(a1.sum$variable))
a1.sum$site.num <-factor(a1.sum$site.num, levels = c("9", "8", "7", "6", "5", "4", "3", "2", "1"))

b2.sum <- b2.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))
b2.sum$variable <- factor(b2.sum$variable, levels = c( "Modern-Forest", "Modern-Savanna", "Past-Forest", "Past-Savanna"))


b3.sum <- b3.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))
b3.sum$variable <- factor(b3.sum$variable, levels = c( "Modern-Forest", "Modern-Savanna", "Past-Forest", "Past-Savanna"))


b4.sum <- b4.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))
b4.sum$variable <- factor(b4.sum$variable, levels = c( "Modern-Forest", "Modern-Savanna", "Past-Forest", "Past-Savanna"))

b5.sum <- b5.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))
b5.sum$variable <- factor(b5.sum$variable, levels = c( "Modern-Forest", "Modern-Savanna", "Past-Forest", "Past-Savanna"))

b6.sum <- b6.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))
b6.sum$variable <- factor(b6.sum$variable, levels = c( "Modern-Forest", "Modern-Savanna", "Past-Forest", "Past-Savanna"))


b7.sum <- b7.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))
b7.sum$variable <- factor(b7.sum$variable, levels = c(  "Modern-Forest", "Modern-Savanna", "Past-Forest", "Past-Savanna"))


# write out all the dotplots
int.dot <- ggplot(data.frame(a1.sum), aes(x = mean.val, y = site.num, color = structure, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+geom_point(alpha = 0.5)+xlim(-0.1, 0.25)+scale_color_manual(values = c("Savanna"='sienna4',
                                                                                                                                                                                                                                         "Forest"='forestgreen'))+theme_bw(base_size = 14)+theme(legend.position = "none", panel.grid = element_blank(), axis.title.y = element_blank())+xlab(expression(paste("Intercept (", alpha[s], ")"))) +xlim(-0.3, 0.8)+ geom_vline(xintercept = 0, linetype = "dashed")

b2.dot <- ggplot(data.frame(b2.sum), aes(x = mean.val, y = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+geom_point(alpha = 0.5)+xlim(-0.15, 0.8)+scale_color_manual(values = c("Past-Savanna"='#a6611a',
                                                                                                                                                                                                                                       "Modern-Savanna"='#dfc27d',
                                                                                                                                                                                                                                       "Modern-Forest"='#80cdc1',
                                                                                                                                                                                                                                       "Past-Forest"='#018571'))+theme_bw(base_size = 14)+theme(legend.position = "none", axis.title.y = element_blank(), panel.grid = element_blank())+xlab(expression(paste("Precip. Sensitivity (", beta[1], ")")))+xlim(-0.3, 0.8)+ geom_vline(xintercept = 0, linetype = "dashed")

b3.dot <- ggplot(data.frame(b3.sum), aes(x = mean.val, y = variable,  size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+geom_point(alpha = 0.5)+xlim(-0.19, 0.71)+scale_color_manual(values = c("Past-Savanna"='#a6611a',
                                                                                                                                                                                                                                        "Modern-Savanna"='#dfc27d',
                                                                                                                                                                                                                                        "Modern-Forest"='#80cdc1',
                                                                                                                                                                                                                                        "Past-Forest"='#018571'))+theme_bw(base_size = 14)+theme(legend.position = "none", axis.title.y = element_blank(), panel.grid = element_blank())+xlab(expression(paste("DBH Sensitivity (", beta[6], ")")))+xlim(-0.3, 0.8)+ geom_vline(xintercept = 0, linetype = "dashed")

b4.dot <- ggplot(data.frame(b4.sum), aes(x = mean.val, y = variable,  size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+geom_point(alpha = 0.5)+scale_color_manual(values = c("Past-Savanna"='#a6611a',
                                                                                                                                                                                                                      "Modern-Savanna"='#dfc27d',
                                                                                                                                                                                                                      "Modern-Forest"='#80cdc1',
                                                                                                                                                                                                                      "Past-Forest"='#018571'))+theme_bw(base_size = 14)+theme(legend.position = "none", axis.title.y = element_blank(), panel.grid = element_blank())+xlab(expression(paste("lag -1 growth coef. (", beta[4], ")")))+xlim(-0.3, 0.8)+ geom_vline(xintercept = 0, linetype = "dashed")

b5.dot <- ggplot(data.frame(b5.sum), aes(x = mean.val, y = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+geom_point(alpha = 0.5)+scale_color_manual(values = c("Past-Savanna"='#a6611a',
                                                                                                                                                                                                                      "Modern-Savanna"='#dfc27d',
                                                                                                                                                                                                                      "Modern-Forest"='#80cdc1',
                                                                                                                                                                                                                      "Past-Forest"='#018571'))+theme_bw(base_size = 14)+theme(legend.position = "none", axis.title.y= element_blank(), panel.grid = element_blank())+xlab(expression(paste("lag -2 growth coef. (", beta[5], ")")))+xlim(-0.3, 0.8)+ geom_vline(xintercept = 0, linetype = "dashed")


b6.dot <- ggplot(data.frame(b6.sum), aes(x = mean.val, y = variable,  size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+geom_point(alpha = 0.5)+scale_color_manual(values = c("Past-Savanna"='#a6611a',
                                                                                                                                                                                                                      "Modern-Savanna"='#dfc27d',
                                                                                                                                                                                                                      "Modern-Forest"='#80cdc1',
                                                                                                                                                                                                                      "Past-Forest"='#018571'))+theme_bw(base_size = 14)+theme(legend.position = "none", axis.title.y = element_blank(), panel.grid = element_blank())+xlab(expression(paste("Max. Temp. sensitivity (", beta[2], ")")))+xlim(-0.3, 0.8)+ geom_vline(xintercept = 0, linetype = "dashed")

b7.dot <- ggplot(data.frame(b7.sum), aes(x = mean.val, y = variable,  size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+geom_point(alpha = 0.5)+scale_color_manual(values = c("Past-Savanna"='#a6611a',
                                                                                                                                                                                                                      "Modern-Savanna"='#dfc27d',
                                                                                                                                                                                                                      "Modern-Forest"='#80cdc1',
                                                                                                                                                                                                                      "Past-Forest"='#018571'))+theme_bw(base_size = 14)+theme(legend.position = "none", axis.title.y = element_blank(), panel.grid = element_blank())+xlab(expression(paste("Temp*Precip sensitivity (", beta[3], ")")))+xlim(-0.3, 0.8)+ geom_vline(xintercept = 0, linetype = "dashed")

cs.legend <- get_legend(ggplot(data.frame(b7.sum), aes(x = mean.val, y = variable, color = variable))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, height = 0))+geom_point(alpha = 0.5)+scale_color_manual(values = c("Past-Savanna"='#a6611a',
                                                                                                                                                                                                                 "Modern-Savanna"='#dfc27d',
                                                                                                                                                                                                                 "Modern-Forest"='#80cdc1',
                                                                                                                                                                                                                 "Past-Forest"='#018571'))+theme_bw(base_size = 14)+theme(legend.title = element_blank(), axis.title.y = element_blank(), panel.grid = element_blank())+xlab("Temp*Precip sensitivity (Beta7)")+xlim(-0.3, 0.8)+ geom_vline(xintercept = 0, linetype = "dashed"))

sav.for.legend <- cowplot::get_legend(ggplot(data.frame(a1.sum), aes(x = mean.val, y = site.num, color = structure))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, height = 0))+geom_point(alpha = 0.5)+xlim(-0.1, 0.25)+scale_color_manual(values = c("Savanna"='sienna4',
                                                                                                                                                                                                                                                 "Forest"='forestgreen'))+theme_bw(base_size = 14)+theme(legend.title = element_blank(), axis.title.y = element_blank(), panel.grid = element_blank())+xlab("Intercept (alpha)")+xlim(-0.3, 0.8)+ geom_vline(xintercept = 0, linetype = "dashed"))



# png(height = 14, width = 10, units = "in", res = 300, "outputs/cpaper_figures_struct_cohort_scaling/full_dot_plot_cohort_cohortXstuct_CI_alllegend.png")
# cowplot::plot_grid(sav.for.legend,
#                    
# cowplot::plot_grid(int.dot.age, int.dot, 
#           b2.dot.age, b2.dot, 
#           b6.dot.age, b6.dot,
#           b7.dot.age, b7.dot, 
#           b3.dot.age, b3.dot, 
#           b4.dot.age, b4.dot,
#           b5.dot.age, b5.dot,
#           mod.past.legend, cs.legend,
#           ncol = 2, align = "v", labels = "AUTO"), 
# 
# 
# nrow = 2, align = "v", rel_heights = c(0.1, 1))
# dev.off()


# code to remove top and right axes perr journal requirements

notoprightpanels <- theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(), axis.line = element_line(color = 'black'))

png(height = 14, width = 10, units = "in", res = 300, "outputs/paper_figures_struct_cohort_scaling/full_dot_plot_cohort_cohortXstuct_CI_formated.png")

                   
                   cowplot::plot_grid(int.dot.age + theme(legend.position = c(0.75, 0.35), legend.title = element_blank()) + guides(size = FALSE) + notoprightpanels  , 
                                      int.dot+ theme(legend.position = c(0.75, 0.35), legend.title = element_blank()) + guides(size = FALSE)+notoprightpanels  , 
                                      b2.dot.age + notoprightpanels, 
                                      b2.dot+ notoprightpanels, 
                                      b6.dot.age + notoprightpanels, 
                                      b6.dot + notoprightpanels, 
                                      b7.dot.age+ notoprightpanels, 
                                      b7.dot+ notoprightpanels,  
                                      b4.dot.age+ notoprightpanels, 
                                      b4.dot+ notoprightpanels, 
                                      b5.dot.age+ notoprightpanels,  
                                      b5.dot+ notoprightpanels, 
                                      b3.dot.age+ notoprightpanels,  b3.dot+ notoprightpanels, 
                                      ncol = 2, align = "hv", labels = c("a)", "b)", 
                                                                        "c)", "d)",
                                                                        "e)", "f)",
                                                                        "g)", "h)",
                                                                        "i)", "j)",
                                                                        "k)", "l)",
                                                                        "m)", "n)"), label_x = 0.28, label_y = 0.9, label_fontface = "plain")
dev.off()
#-------------------------------------------------------------------------------------------------
#                                     Figure 4: 
#-------------------------------------------------------------------------------------------------
# we wannt a figure that plots the change in a () predicted WUE and the change in baseline WUE(b), and then values for predicted tree growth

library(HDInterval)
library(coda)
# get differences in WUE:

wue.samps <- readRDS("outputs/growth_model/iWUE_MAP_TMAX_dbh_cohort/samps.iWUE_age.cohort.re_struct_cohort_scaled_dataset_v4.rds")
#wue.data <- readRDS("outputs/growth_model/iWUE_MAP_TMAX_dbh_cohort/")

samps <- data.frame(wue.samps[[1]])
alpha.samps  <- samps[,1:2]
beta.samps  <- samps[,3:4]
beta2.samps  <- samps[,5:6]
beta3.samps  <- samps[,7:8]
params <- samps[,1:8]
#saveRDS(params, "outputs/growth_model/iWUE_MAP_TMAX_dbh_cohort/params.iWUE_ageclass_v2.rds")
iWUE.samps  <- samps[,9:(8+length(full.iso$iWUE))]

alpha.diff <- ((samps[1] - samps[2])/samps[2])*100

wue.pct.diff <- alpha.diff %>% summarise(
  change = "WUE",
  pct.change = mean(beta1.1.),
  Ci.low = quantile(beta1.1., 0.025), 
  Ci.high = quantile(beta1.1., 0.975), 
  median = median(beta1.1.), 
  hdi.low = hdi(beta1.1.)[1], 
  hdi.high = hdi(beta1.1.)[2])

# predicted differences in WUE:

full.iso <- readRDS("data/full_WUE_struct_cohort_scaled_dataset_v4.rds")
full.iso[full.iso$Cor.d13C.suess <= -21 & full.iso$Cor.d13C.suess >= -27.5, ] %>% group_by( site, ageclass) %>% dplyr::summarise(n = n(), WUE.mean = mean(iWUE, na.rm=TRUE))
full.iso%>% group_by(ageclass, structure) %>% dplyr::summarise(n = n(), WUE.mean = mean(iWUE, na.rm=TRUE), d13 = mean(Cor.d13C.suess))

#full.iso <- full.iso[full.iso$Cor.d13C.suess < -21.9, ]
Yp.samps <- data.frame(iWUE.samps) 

Yp.m <- melt(Yp.samps)
Yp.summary <- Yp.m %>% group_by(variable) %>% dplyr::summarise(Predicted = mean(value),
                                                               ci.hi = quantile(value,0.975),
                                                               ci.lo = quantile(value,0.025))
Yp.summary$Observed <- full.iso$iWUE
Yp.summary$ageclass <- full.iso$ageclass


pred.obs <- summary(lm(colMeans(Yp.samps)~ full.iso$iWUE))

# this does a poor job representing iWUE values by itself, but explains som of the variation
p.o.plot <- ggplot(Yp.summary, aes(Observed, Predicted))+geom_point(color = "black", size = 0.5)+geom_errorbar(data = Yp.summary,aes(ymin=ci.lo, ymax=ci.hi), color = "grey", alpha = 0.5)+geom_point(data = Yp.summary, aes(Observed, Predicted), color = "black", size = 0.5)+geom_abline(aes(slope = 1, intercept = 0), color = "red", linetype = "dashed")+geom_text(data=data.frame(pred.obs$r.squared), aes( label = paste("R^2: ", pred.obs$r.squared, sep="")),parse=T,x=120, y=175)

# note fairly poor model fit
png(width = 6, height = 5, units = "in", res = 300, "outputs/growth_model/iWUE_MAP_TMAX_dbh/pred_vs_obs_cohort_struct_scaled_v4.png")
p.o.plot
dev.off()




Yp.diff <- ((Yp.age.samps[1] - Yp.age.samps[2])/Yp.samps[2])*100

wuepred.pct.diff <- Yp.diff %>% summarise(
  change = "WUE",
  pct.change = mean(iWUE.p.1.),
  Ci.low = quantile(iWUE.p.1., 0.025), 
  Ci.high = quantile(iWUE.p.1., 0.975), 
  median = median(iWUE.p.1.), 
  hdi.low = hdi(iWUE.p.1.)[1], 
  hdi.high = hdi(iWUE.p.1.)[2])


Predicted.wue <- Yp.summary %>% dplyr::group_by(ageclass) %>% dplyr::summarise(mean = mean(Predicted, na.rm = TRUE), 
                                                                               Ci.low = quantile(Predicted,0.025, na.rm = TRUE),
                                                                               Ci.high = quantile(Predicted,0.975, na.rm = TRUE), 
                                                                               hdi.low = hdi(Predicted, na.rm = TRUE)[1],
                                                                               hdi.high = hdi(Predicted,na.rm = TRUE)[2])

Predicted.wue$ageclass <- factor(Predicted.wue$ageclass, levels = c("Past", "Modern"))


pred.WUE <- ggplot(Predicted.wue , aes(ageclass, y = mean,fill = ageclass))+geom_bar(stat = "identity")+scale_fill_manual(values  = c("Past"='#2166ac', 'Modern' = "#b2182b"))+
  geom_errorbar(aes(x = ageclass, ymin = Ci.low, ymax = Ci.high), alpha = 0.8, size = 0.5, width = 0.2)+ylab("Predicted WUE")+theme_bw(base_size = 16)+theme(panel.grid = element_blank(), axis.title.x = element_blank(), legend.title = element_blank())


# make bar plots for baseline WUE increases:
colnames(alpha.samps) <- c("Modern", "Past")
alpha.samps.m <- melt(alpha.samps) 

baseline.growth <- alpha.samps.m %>% dplyr::group_by(variable) %>% dplyr::summarise(mean = mean(value, na.rm = TRUE), 
                                                                                    Ci.low = quantile(value,0.025, na.rm = TRUE),
                                                                                    Ci.high = quantile(value,0.975, na.rm = TRUE), 
                                                                                    hdi.low = hdi(value, na.rm = TRUE)[1],
                                                                                    hdi.high = hdi(value,na.rm = TRUE)[2])

baseline.growth$ageclass <- factor(baseline.growth$variable, levels = c("Past", "Modern"))


baseline.WUE <- ggplot(baseline.growth , aes(ageclass, y = mean,fill = ageclass))+geom_bar(stat = "identity")+scale_fill_manual(values  = c("Past"='#2166ac', 'Modern' = "#b2182b"))+
  geom_errorbar(aes(x = ageclass, ymin = Ci.low, ymax = Ci.high), alpha = 0.8, size = 0.5, width = 0.2)+ylab("Baseline WUE")+theme_bw(base_size = 16)+theme(panel.grid = element_blank(), axis.title.x = element_blank(), legend.title = element_blank())


# ----------------- Plot average predicted difference in iWUE between ageclasses--------------------
Yp.samps <- data.frame(iWUE.samps) 
Yp.m <- melt(Yp.samps)


obs.wue <- full.iso
obs.wue$num <- 1:length(Yp.samps)
obs.wue$variable <- paste0("iWUE.p.", obs.wue$num, ".")

obs.preds <- left_join(obs.wue, Yp.m, by = "variable")
WUE.age <- obs.preds %>% group_by(ageclass) %>% summarise(mean = mean(value, na.rm = TRUE), 
                                                          Ci.low = quantile(value,0.025, na.rm = TRUE),
                                                          Ci.high = quantile(value,0.975, na.rm = TRUE), 
                                                          hdi.low = hdi(value, na.rm = TRUE)[1],
                                                          hdi.high = hdi(value,na.rm = TRUE)[2])

### read in and plot the average predicted change in tree growth:

Yp.summary.cohort <- readRDS("outputs/growth_model/cohort_struct_scaled_lag2_reg_cohort_only_re_t_pr_dry_yrs_site_rs_inter/predicted_growth_YP_summary_v5.rds")

mean.pred.growth <- ggplot(Yp.summary.cohort, aes(ageclass, Predicted, fill = ageclass))+geom_bar(stat = "identity")+
  geom_errorbar(data = Yp.summary.cohort,aes(ymin=ci.lo, ymax=ci.hi), color = "black", alpha = 0.8, size = 0.5, width = 0.2)+ylim(0, 6) + 
  ylab("Mean predicted \n tree growth (mm)")+scale_fill_manual(values = c("Past"='#2166ac', 'Modern' = "#b2182b"))+theme_bw(base_size = 16)+theme(panel.grid = element_blank(), axis.title.x = element_blank(), legend.position = "none")



plot_grid(pred.WUE+theme(legend.position = "none"), mean.pred.growth, ncol = 1, align = "hv")



#beta_diffsm <- readRDS("outputs/growth_model/lag2_reg_cohort_only_re_t_pr_dry_yrs_site_rs_inter/beta_differences_cohorts.rds")
beta_diffs <- readRDS("outputs/growth_model/cohort_struct_scaled_lag2_reg_cohort_only_re_t_pr_dry_yrs_site_rs_inter/beta_diffs_cohorts_v5.rds")


beta_diffsm <- beta_diffs %>% gather(beta, difference) %>% group_by(beta) %>% 
  dplyr::summarise(mean = mean(difference),
                   Ci.low = quantile(difference, 0.025),
                   Ci.high = quantile(difference, 0.975),
                   hdi.low = hdi(difference)[1],
                   hdi.high = hdi(difference)[2]) 

# hard coding this, but need to make it better
beta_diffsm$significant <- c("NS", "NS", "Significant", "Significant", "Significant", "NS")


MAP.beta.diff <- data.frame(
  change = "MAP",
  pct.change = mean(beta_diffs[,"MAP"]),
  Ci.low = quantile(beta_diffs[,"MAP"], 0.025), 
  Ci.high = quantile(beta_diffs[,"MAP"], 0.975), 
  median = median(beta_diffs[,"MAP"]), 
  hdi.low = hdi(beta_diffs[,"MAP"])[1], 
  hdi.high = hdi(beta_diffs[,"MAP"])[2])


Lag2.beta.diff <- data.frame(
  change = "lag_2",
  pct.change = mean(beta_diffs[,"lag_2"]),
  Ci.low = quantile(beta_diffs[,"lag_2"], 0.025), 
  Ci.high = quantile(beta_diffs[,"lag_2"], 0.975), 
  median = median(beta_diffs[,"lag_2"]), 
  hdi.low = hdi(beta_diffs[,"lag_2"])[1], 
  hdi.high = hdi(beta_diffs[,"lag_2"])[2])


# use posterior parameter estimates to generate mean growth
beta_diffsm$beta <- factor(beta_diffsm$beta, levels = c("MAP", "Tmax", "MAPxTmax","DBH", "lag_1", "lag_2"))
beta_diffsm$params <-c("DBH", "lag_1", "lag_2", "Precip", "Precip x Tmax", "Tmax")
beta_diffsm$params <- factor(beta_diffsm$params, levels = c("Precip", "Tmax", "Precip x Tmax","DBH", "lag_1", "lag_2"))

beta.diffs.plot.mod.past <- ggplot(beta_diffsm, aes(params, mean, fill = significant))+geom_bar(stat = "identity", width = 0.8)+geom_errorbar(data = beta_diffsm,aes(ymin=hdi.low, ymax=hdi.high), color = "grey29", alpha = 0.8, size = 0.5, width = 0.1)+
  ylab("Difference in beta parameter \n (Modern - Past)")+xlab("Model Parameters")+theme_bw(base_size = 16)+theme(panel.grid = element_blank(), legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))+scale_fill_manual(values = c("Significant" = "black", "NS" = "lightgrey"))



png(height = 6, width = 12, units = "in", res = 300, "outputs/paper_figures_struct_cohort_scaling/summary_of_differences_cohort_v2.png")
plot_grid(
  plot_grid(pred.WUE+theme(legend.position = "none"), mean.pred.growth, ncol = 1, align = "hv", labels = c("", "B")),
  beta.diffs.plot.mod.past, ncol = 2, labels = c("A", "C"), rel_widths = c(0.75, 1))
dev.off()


# plot overall differences in WUE:
wue.diff.bar <- ggplot(wue.pct.diff, aes(change, pct.change))+geom_bar(stat = "identity")+
  geom_errorbar(data = wue.pct.diff, aes(ymin=hdi.low, ymax=hdi.high), color = "grey", alpha = 0.8, size = 0.5, width = 0.2)+ylab("percent change in iWUE")+theme_bw(base_size = 16)+theme(panel.grid = element_blank(), axis.title.x = element_blank())

drought.bar <- ggplot(MAP.beta.diff, aes(change, pct.change))+geom_bar(stat = "identity")+
  geom_errorbar(data = MAP.beta.diff, aes(ymin=hdi.low, ymax=hdi.high), color = "grey", alpha = 0.8, size = 0.5, width = 0.2)+ylab("change in drought sensitivity")+theme_bw(base_size = 16)+theme(panel.grid = element_blank(), axis.title.x = element_blank())

lag1.bar <- ggplot(Lag2.beta.diff, aes(change, pct.change))+geom_bar(stat = "identity")+
  geom_errorbar(data = Lag2.beta.diff, aes(ymin=hdi.low, ymax=hdi.high), color = "grey", alpha = 0.8, size = 0.5, width = 0.2)+ylab("change in lag (-2) parameter")+theme_bw(base_size = 16)+theme(panel.grid = element_blank(), axis.title.x = element_blank())


mean.pred.growth <- ggplot(growth.cohort, aes(ageclass,Predicted, fill = ageclass))+geom_bar(stat = "identity")+geom_errorbar(data = growth.cohort,aes(ymin=ci.lo, ymax=ci.hi), color = "grey", alpha = 0.8, size = 0.5, width = 0.2)+ylim(0, 6) + ylab("Mean predicted tree growth (mm)")+scale_fill_manual(values = c("Past"='#2166ac', 'Modern' = "#b2182b"))+theme_bw(base_size = 16)+theme(panel.grid = element_blank(), axis.title.x = element_blank(), legend.position = "none")

png(height = 4, width = 10, units = "in", res = 300, "outputs/paper_figures_struct_cohort_scaling/summary_differences_by_cohort.png")
plot_grid(wue.diff.bar, drought.bar, lag1.bar,mean.pred.growth, ncol = 4, align = 'h', rel_widths = c(0.5,0.5,0.5, 1))
dev.off()


png(height = 4, width = 10, units = "in", res = 300, "outputs/paper_figures_struct_cohort_scaling/summary_differences_by_cohort.png")
plot_grid(wue.diff.bar, drought.bar, lag1.bar,mean.pred.growth, ncol = 4, align = 'h', rel_widths = c(0.5,0.5,0.5, 1))
dev.off()

# a better figure:

#-------------------------------------------------------------------------------------------------
#                 Figure 4: WUE and d13C parameter models by structure & cohort
#-------------------------------------------------------------------------------------------------
# ----------get the parameters for the WUE model:
wue.samps <- readRDS("outputs/growth_model/iWUE_MAP_TMAX_dbh_cohort/samps.iWUE_age.cohort.re_struct_cohort_scaled_dataset_v4.rds")
#wue.data <- readRDS("outputs/growth_model/iWUE_MAP_TMAX_dbh_cohort/")

samps <- data.frame(wue.samps[[1]])
alpha.samps  <- samps[,1:2]
beta.samps  <- samps[,3:4]
beta2.samps  <- samps[,5:6]
beta3.samps  <- samps[,7:8]
params <- samps[,1:8]
#saveRDS(params, "outputs/growth_model/iWUE_MAP_TMAX_dbh_cohort/params.iWUE_ageclass_v2.rds")
iWUE.samps  <- samps[,9:(8+length(full.iso$iWUE))]

Yp.samps <- data.frame(iWUE.samps) 
Yp.m <- melt(Yp.samps)
Yp.summary <- Yp.m %>% group_by(variable) %>% dplyr::summarise(Predicted = mean(value),
                                                               ci.hi = quantile(value,0.975),
                                                               ci.lo = quantile(value,0.025))
Yp.summary$Observed <- full.iso$iWUE
Yp.summary$ageclass <- full.iso$ageclass
Yp.summary$struct.cohort <- full.iso$struct.cohort

pred.obs.wue.cohort <- summary(lm(Yp.summary$Predicted~ Yp.summary$Observed))

# this does a poor job representing iWUE values by itself, but explains som of the variation
p.o.plot.wue.cohort <- ggplot(Yp.summary, aes(Observed, Predicted))+geom_point(color = "black", size = 0.5)+
  geom_errorbar(data = Yp.summary,aes(ymin=ci.lo, ymax=ci.hi), color = "grey", alpha = 0.5)+
  geom_point(data = Yp.summary, aes(Observed, Predicted), color = "black", size = 0.5)+
  geom_abline(aes(slope = 1, intercept = 0), color = "red", linetype = "dashed")+
  geom_text(data=data.frame(pred.obs$r.squared), aes( label = paste("R^2: ", round(pred.obs.wue.cohort$r.squared, digits = 3), sep="")),parse=T,x=120, y=175)+
  theme_bw(base_size = 16)+ylab("Predicted WUE")+xlab("Observed WUE")+theme(panel.grid = element_blank())+ylim(90, 190)+xlim(90, 190)


# -----------plot marginal distributions of cohort + specific parameters:
a <- data.frame(alpha.samps)
colnames(a) <- c(paste0(c("Modern", "Past")))
a$num <- rownames(a)
a.m <- melt(a, id.vars=c("num"))
alpha.mplots <- ggplot(a.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+ylab("frequency")+xlab("iWUE")


ggplot(a.m, aes(variable, value, fill = variable))+geom_boxplot(alpha = 0.5)+theme_bw()+ylab("frequency")+xlab("d13C")

a <- data.frame(alpha.samps)
colnames(a) <- c(paste0(c("Modern", "Past")))




# for MAP:
b <- data.frame(beta.samps)
colnames(b) <- c(paste0(c("Modern", "Past")))

b$num <- rownames(b)
b.m <- melt(b, id.vars=c("num"))
b.mplots <-ggplot(b.m, aes(value, 1, fill = variable))+stat_density_ridges(alpha = 0.5, quantile_lines = TRUE)+theme_bw()+xlab("Estimated Precip Sensitivity")+theme_bw(base_size = 16)+scale_fill_manual(values = c("Past"='blue',"Modern"='red'))


# for Tmax:
b2 <- data.frame(beta2.samps)
colnames(b2) <-c(paste0(c("Modern", "Past")))

b2$num <- rownames(b2)
b2.m <- melt(b2, id.vars=c("num"))
b2.mplots <-ggplot(b2.m, aes(value, 1, fill = variable))+stat_density_ridges(alpha = 0.5, quantile_lines = TRUE)+theme_bw()+xlab("Estimated Tmax Sensitivity")+theme_bw(base_size = 16)

# for DBH:
b3 <- data.frame(beta3.samps)
colnames(b3) <- c(paste0(c("Modern", "Past")))

b3$num <- rownames(b3)
b3.m <- melt(b3, id.vars=c("num"))
b3.mplots <-ggplot(b3.m, aes(value, 1, fill = variable))+stat_density_ridges(alpha = 0.5, quantile_lines = TRUE)+theme_bw()+xlab("Estimated DBH Sensitivity")+theme_bw(base_size = 16)+scale_fill_manual(values = c("Past"='blue',"Modern"='red'))

# calculate dot plots
a.mean <- apply(as.matrix(a[,1:2]), 2, mean)
a.lower <- apply(as.matrix(a[,1:2]), 2, function(x) quantile(x, probs = c(0.025)))
a.upper <- apply(as.matrix(a[,1:2]), 2, function(x) quantile(x, probs = c(0.975)))

## Combine both estimates
plot.dat.a <- data.frame(a.mean, a.lower, a.upper)
plot.dat.a$class <- row.names(plot.dat.a)
plot.dat.a$class <- factor(plot.dat.a$class, levels = c(paste0(c( "Past", "Modern"))))


a.dots.2.wue <- ggplot(plot.dat.a, aes(x = a.mean, y = class,  size = 2))+geom_errorbarh( xmin = a.lower, xmax = a.upper, size = 2,height = 0)+geom_point(alpha = 0.5)+theme_bw(base_size = 18)+coord_flip()+xlim(115, 165)+theme(legend.position = "none", axis.title.x = element_blank(), panel.grid = element_blank())+ xlab(expression(paste("Baseline iWUE")))

png(width = 5, height = 4, units = "in", res = 300, "outputs/growth_model/iWUE_MAP_TMAX_dbh/param_marginal_distn_bycohort_struct_v3_iwue_random_slopes_cohort.png")
a.dots.2.wue
dev.off()

# dot plot for MAP
b.mean <- apply(as.matrix(b[,1:2]), 2, mean)
b.lower <- apply(as.matrix(b[,1:2]), 2, function(x) quantile(x, probs = c(0.025)))
b.upper <- apply(as.matrix(b[,1:2]), 2, function(x) quantile(x, probs = c(0.975)))

## Combine both estimates
plot.dat.b <- data.frame(b.mean, b.lower, b.upper)
plot.dat.b$class <- row.names(plot.dat.b)
plot.dat.b$class <- factor(plot.dat.b$class, levels = c(paste0(c("Past", "Modern"))))




b.dots.2.wue <- ggplot(plot.dat.b, aes(x = b.mean, y = class,  size = 2))+geom_errorbarh( xmin = b.lower, xmax = b.upper, size = 2,height = 0)+
  geom_point(alpha = 0.5)+theme_bw(base_size = 18)+scale_color_manual(values = c("Past"='blue',"Modern"='red'))+coord_flip()+theme(legend.position = "none", axis.title.x = element_blank(), panel.grid = element_blank())+
  xlab("Precip effect on iWUE")+geom_vline(xintercept = 0, linetype = "dashed")+xlim(-1,6)

png(width = 5, height = 4, units = "in", res = 300, "outputs/growth_model/iWUE_MAP_TMAX_dbh/param_marginal_distn_bycohort_struct_v3_iwue_random_slopes_Precip_impact_struct_cohort_scaled.png")
b.dots.2.wue
dev.off()




# dot plot for Tmax
b2.mean <- apply(as.matrix(b2[,1:2]), 2, mean)
b2.lower <- apply(as.matrix(b2[,1:2]), 2, function(x) quantile(x, probs = c(0.025)))
b2.upper <- apply(as.matrix(b2[,1:2]), 2, function(x) quantile(x, probs = c(0.975)))

## Combine both estimates
plot.dat.b2 <- data.frame(b2.mean, b2.lower, b2.upper)
plot.dat.b2$class <- c("Modern", "Past")
plot.dat.b2$class <- factor(plot.dat.b2$class, levels = c("Past",  "Modern"))




b2.dots.2.wue <- ggplot(plot.dat.b2, aes(x = b2.mean, y = class, size = 2))+
  geom_errorbarh( xmin = b2.lower, xmax = b2.upper, size = 2,height = 0)+geom_point(alpha = 0.5)+
  theme_bw(base_size = 18)+scale_color_manual(values = c("Past"='blue',"Modern"='red'))+coord_flip()+
  theme(legend.position = "none", axis.title.x = element_blank(), panel.grid = element_blank())+ xlab(expression(paste("Tmax effect on iWUE")))+
  geom_vline(xintercept = 0, linetype = "dashed")+xlim( -4, 2.7)

png(width = 5, height = 4, units = "in", res = 300, "outputs/growth_model/iWUE_MAP_TMAX_dbh/param_marginal_distn_bycohort_v3_iwue_random_slopes_TMAX_impact_struct_cohort_scaled.png")
b2.dots.2.wue
dev.off()



# dot plot for DBH
b3.mean <- apply(as.matrix(b3[,1:2]), 2, mean)
b3.lower <- apply(as.matrix(b3[,1:2]), 2, function(x) quantile(x, probs = c(0.025)))
b3.upper <- apply(as.matrix(b3[,1:2]), 2, function(x) quantile(x, probs = c(0.975)))

## Combine both estimates
plot.dat.b3 <- data.frame(b3.mean, b3.lower, b3.upper)
plot.dat.b3$class <- row.names(plot.dat.b3)
plot.dat.b3$class <- factor(plot.dat.b3$class, levels = c("Past",  "Modern"))



b3.dots.2.wue <- ggplot(plot.dat.b3, aes(x = b3.mean, y = class, size = 2))+
  geom_errorbarh( xmin = b3.lower, xmax = b3.upper, size = 2,height = 0)+geom_point(alpha = 0.5)+theme_bw(base_size = 18)+
  scale_color_manual(values = c("Past"='blue',"Modern"='red'))+coord_flip()+theme(legend.position = "none", axis.title.x = element_blank(), panel.grid = element_blank())+
  xlim(-2.1, 4)+ xlab(expression(paste("DBH effect on iWUE")))+geom_vline(xintercept = 0, linetype = "dashed")

png(width = 5, height = 4, units = "in", res = 300, "outputs/growth_model/iWUE_MAP_TMAX_dbh/param_marginal_distn_bycohort_struct_v3_iwue_random_slopes_DBH_impact_struct_cohort_scaled.png")
b3.dots.2.wue
dev.off()

png(width = 4, height = 14, units = "in", res = 300, "outputs/growth_model/iWUE_MAP_TMAX_dbh/param_marginal_distn_bycohort_d13_all_dotplots_wueV3_struct_cohort_scaled.png")
plot_grid(a.dots.2.wue, b.dots.2.wue, b2.dots.2.wue, b3.dots.2.wue, ncol = 1,align = "hv", labels = "AUTO")
dev.off()




# ----------get the parameters for the d13 model:
samps.d13.params<- readRDS( "outputs/growth_model/iWUE_MAP_TMAX_dbh_cohort/params.d13_ageclass_struct_cohort_scaled_dataset_v4.rds")

alpha.samps  <- data.frame(samps.d13.params[,1:2])
colnames(alpha.samps) <- c( "Modern", "Past") # note this may need to be changed
alphad13.samps.diffs <-alpha.samps$Modern -alpha.samps$Past

alpha.samps.m <- melt(alpha.samps)
colnames(alpha.samps.m) <- c("ageclass", "value")
d13.cohort  <- alpha.samps.m  %>% group_by(ageclass) %>% dplyr::summarise(Predicted = mean(value),
                                                                          ci.hi = quantile(value,0.975),
                                                                          ci.lo = quantile(value,0.025) )


d13.cohort$ageclass <- factor(d13.cohort$ageclass, levels = c("Past", "Modern"))

mean.pred.d13 <- ggplot(d13.cohort, aes(ageclass, Predicted, fill = ageclass))+geom_bar(stat = "identity", width = 0.5)+
  geom_errorbar(data = d13.cohort, aes(ymin=ci.lo, ymax=ci.hi), color = "grey29", alpha = 0.8, size = 0.5, width = 0.1) + ylab(expression(paste("Baseline " ,delta^{13}, "C (\u2030)")))+scale_fill_manual(values = c("Past"='#2166ac', 'Modern' = "#b2182b"))+theme_bw(base_size = 16)+theme(panel.grid = element_blank(), axis.title.x = element_blank(), legend.position = "none")
# make dot plots:

mean.pred.d13.dot <- ggplot(data = d13.cohort, aes(x = Predicted, y = ageclass, color = ageclass, size = 4))+
  geom_errorbarh(data = d13.cohort, aes(xmin = ci.lo, xmax = ci.hi, size = 2,height = 0))+geom_point(alpha = 0.5)+
  theme_bw(base_size = 18)+xlim(-25.5, -23)+scale_color_manual(values = c("Past"='blue',"Modern"='red'))+coord_flip()+theme(legend.position = "none", axis.title.x = element_blank(), panel.grid = element_blank())+ xlab(expression(paste("Baseline " ,delta^{13}, "C (\u2030)")))


beta1d13.samps  <- data.frame(samps.d13.params[,3:4])
colnames(beta1d13.samps) <- c("Modern", "Past")
beta1d13.samps.diffs <- beta1d13.samps$Modern - beta1d13.samps$Past

beta1d13.samps.m <- melt(beta1d13.samps)
colnames(beta1d13.samps.m) <- c("ageclass", "value")
beta1d13.cohort  <- beta1d13.samps.m  %>% group_by(ageclass) %>% dplyr::summarise(Predicted = mean(value),
                                                                                  ci.hi = quantile(value,0.975),
                                                                                  ci.lo = quantile(value,0.025) )

beta1d13.cohort$ageclass <- factor(beta1d13.cohort$ageclass, levels = c("Past", "Modern"))

mean.pred.beta1.d13 <- ggplot(beta1d13.cohort, aes(ageclass, Predicted, fill = ageclass))+geom_bar(stat = "identity", width = 0.5)+
  geom_errorbar(data = beta1d13.cohort, aes(ymin=ci.lo, ymax=ci.hi), color = "grey29", alpha = 0.8, size = 0.5, width = 0.1)+ 
  ylim(-0.7, 0.6)+geom_hline(yintercept = 0, color = "grey", linetype = "dashed")+ ylab(expression(paste("Precip. effect on " ,delta^{13}, "C (\u2030)")))+scale_fill_manual(values = c("Past"='#2166ac', 'Modern' = "#b2182b"))+theme_bw(base_size = 16)+theme(panel.grid = element_blank(), axis.title.x = element_blank(), legend.position = "none")


beta2d13.samps  <- data.frame(samps.d13.params[,5:6])
colnames(beta2d13.samps) <- c("Modern", "Past")
beta2d13.samps.diffs <- beta2d13.samps$Modern - beta2d13.samps$Past
beta2d13.samps.m <- melt(beta2d13.samps)
colnames(beta2d13.samps.m) <- c("ageclass", "value")
beta2d13.cohort  <- beta2d13.samps.m  %>% group_by(ageclass) %>% dplyr::summarise(Predicted = mean(value),
                                                                                  ci.hi = quantile(value,0.975),
                                                                                  ci.lo = quantile(value,0.025) )



beta2d13.cohort$ageclass <- factor(beta2d13.cohort$ageclass, levels = c("Past", "Modern"))

mean.pred.beta2.d13 <- ggplot(beta2d13.cohort, aes(ageclass, Predicted, fill = ageclass))+geom_bar(stat = "identity", width = 0.5)+
  geom_errorbar(data = beta2d13.cohort, aes(ymin=ci.lo, ymax=ci.hi), color = "grey29", alpha = 0.8, size = 0.5, width = 0.1) +
  ylim(-0.7, 0.6)+geom_hline(yintercept = 0, color = "grey", linetype = "dashed")+ylab(expression(paste("Tmax effect on " ,delta^{13}, "C (\u2030)")))+scale_fill_manual(values = c("Past"='#2166ac', 'Modern' = "#b2182b"))+theme_bw(base_size = 16)+theme(panel.grid = element_blank(), axis.title.x = element_blank(), legend.position = "none")

# for Diameter
beta3d13.samps  <- data.frame(samps.d13.params[,7:8])
colnames(beta3d13.samps) <- c("Modern", "Past")
beta3d13.samps.diffs <- beta3d13.samps$Modern - beta3d13.samps$Past


beta3d13.samps.m <- melt(beta3d13.samps)
colnames(beta3d13.samps.m) <- c("ageclass", "value")
beta3d13.cohort  <- beta3d13.samps.m  %>% group_by(ageclass) %>% dplyr::summarise(Predicted = mean(value),
                                                                                  ci.hi = quantile(value,0.975),
                                                                                  ci.lo = quantile(value,0.025) )



beta3d13.cohort$ageclass <- factor(beta3d13.cohort$ageclass, levels = c("Past", "Modern"))

mean.pred.beta3.d13 <- ggplot(beta3d13.cohort, aes(ageclass, Predicted, fill = ageclass))+geom_bar(stat = "identity", width = 0.5)+
  geom_errorbar(data = beta3d13.cohort, aes(ymin=ci.lo, ymax=ci.hi), color = "grey29", alpha = 0.8, size = 0.5, width = 0.1) +
  ylim(-0.7, 0.63)+geom_hline(yintercept = 0, color = "grey", linetype = "dashed")+ylab(expression(paste("DBH effect on " ,delta^{13}, "C (\u2030)")))+scale_fill_manual(values = c("Past"='#2166ac', 'Modern' = "#b2182b"))+theme_bw(base_size = 16)+theme(panel.grid = element_blank(), axis.title.x = element_blank(), legend.position = "none")



# plot out d13 model responses:
png(height = 3, width = 12, units = "in", res = 300, "outputs/paper_figures_struct_cohort_scaling/summary_of_d13_mod.png")
plot_grid(mean.pred.d13.dot, mean.pred.beta1.d13, mean.pred.beta2.d13, mean.pred.beta3.d13, ncol = 4)
dev.off()

# plot overall
png(height = 6, width = 12, units = "in", res = 300, "outputs/paper_figures_struct_cohort_scaling/summary_of_d13_iwue_mod_v3.png")
plot_grid(mean.pred.d13.dot, mean.pred.beta1.d13, mean.pred.beta2.d13, mean.pred.beta3.d13,
          a.dots.2.wue, b.dots.2.wue, b2.dots.2.wue, b3.dots.2.wue, ncol = 4, align = "v")
dev.off()

# plot predicted vs observed

samps.d13.samps<- readRDS( "outputs/growth_model/iWUE_MAP_TMAX_dbh_cohort/samps.d13_ageclass_struct_cohort_scaled_dataset_v4.rds")
samps <- data.frame(samps.d13.samps)
alpha.samps  <- samps[,1:2]
beta.samps  <- samps[,3:4]
beta2.samps  <- samps[,5:6]
beta3.samps  <- samps[,7:8]
params <- samps[,1:8]
#saveRDS(params, "outputs/growth_model/id13_MAP_TMAX_dbh_cohort/params.id13_ageclass_v2.rds")
#id13.samps  <- samps[,17:(16+length(full.iso$id13))]


#saveRDS(params, "outputs/growth_model/id13_MAP_TMAX_dbh_cohort/params.id13_ageclass_v2.rds")
id13.samps  <- samps[,9:(8+length(full.iso$Cor.d13C.suess))]

Yp.samps <- data.frame(id13.samps) 
Yp.m <- melt(Yp.samps)
Yp.summary <- Yp.m %>% group_by(variable) %>% dplyr::summarise(Predicted = mean(value),
                                                               ci.hi = quantile(value,0.975),
                                                               ci.lo = quantile(value,0.025))
Yp.summary$Observed <- full.iso$Cor.d13C.suess
Yp.summary$ageclass <- full.iso$ageclass
Yp.summary$struct.cohort <- full.iso$struct.cohort

pred.obs.cohort <- summary(lm(Yp.summary$Predicted~ Yp.summary$Observed))

# this does a poor job representing iWUE values by itself, but explains som of the variation
p.o.plot.d13c.cohort <- ggplot(Yp.summary, aes(Observed, Predicted))+geom_point(color = "black", size = 0.5)+
  geom_errorbar(data = Yp.summary,aes(ymin=ci.lo, ymax=ci.hi), color = "grey", alpha = 0.5)+
  geom_point(data = Yp.summary, aes(Observed, Predicted), color = "black", size = 0.5)+
  geom_abline(aes(slope = 1, intercept = 0), color = "red", linetype = "dashed")+
  geom_text(data=data.frame(pred.obs$r.squared), aes( label = paste("R^2: ", round(pred.obs.cohort$r.squared, digits = 3), sep="")),parse=T,x=-26, y=-22)+
  theme_bw(base_size = 16)+ylab(expression(paste("Predicted ",delta^{13}, "C (\u2030)")))+xlab(expression(paste("Observed ",delta^{13}, "C (\u2030)")))+theme(panel.grid = element_blank())+ylim(-27, -21)+xlim(-27, -21)#+ylim(-27, -21)+xlim(-27, -21)


# note fairly poor model fit
png(width = 6, height = 5, units = "in", res = 300, "outputs/growth_model/iWUE_MAP_TMAX_dbh/pred_vs_obs_struct_cohort_WUE_v4_struct_cohort_scaled.png")
p.o.plot.d13c.cohort
dev.off()



# beta diffs for tree ring growth cohorts 

# -----------plot marginal distributions of cohort + structure specific parameters:
a <- data.frame(alpha.samps)
colnames(a) <- c(paste0(c("Modern", "Past")))
a$num <- rownames(a)
a.m <- melt(a, id.vars=c("num"))
alpha.mplots <- ggplot(a.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+ylab("frequency")+xlab("id13")


ggplot(a.m, aes(variable, value, fill = variable))+geom_boxplot(alpha = 0.5)+theme_bw()+ylab("frequency")+xlab("d13C")

a <- data.frame(alpha.samps)
colnames(a) <- c(paste0(c("Modern", "Past")))




# for MAP:
b <- data.frame(beta.samps)
colnames(b) <- c(paste0(c("Modern", "Past")))

b$num <- rownames(b)
b.m <- melt(b, id.vars=c("num"))
b.mplots <-ggplot(b.m, aes(value, 1, fill = variable))+stat_density_ridges(alpha = 0.5, quantile_lines = TRUE)+theme_bw()+xlab("Estimated Precip Sensitivity")+theme_bw(base_size = 16)+scale_fill_manual(values = c("Past"='blue',"Modern"='red'))


# for Tmax:
b2 <- data.frame(beta2.samps)
colnames(b2) <-c(paste0(c("Modern", "Past")))

b2$num <- rownames(b2)
b2.m <- melt(b2, id.vars=c("num"))
b2.mplots <-ggplot(b2.m, aes(value, 1, fill = variable))+stat_density_ridges(alpha = 0.5, quantile_lines = TRUE)+theme_bw()+xlab("Estimated Tmax Sensitivity")+theme_bw(base_size = 16)

# for DBH:
b3 <- data.frame(beta3.samps)
colnames(b3) <- c(paste0(c("Modern", "Past")))

b3$num <- rownames(b3)
b3.m <- melt(b3, id.vars=c("num"))
b3.mplots <-ggplot(b3.m, aes(value, 1, fill = variable))+stat_density_ridges(alpha = 0.5, quantile_lines = TRUE)+theme_bw()+xlab("Estimated DBH Sensitivity")+theme_bw(base_size = 16)+scale_fill_manual(values = c("Past"='blue',"Modern"='red'))

# calculate dot plots
a.mean <- apply(as.matrix(a[,1:2]), 2, mean)
a.lower <- apply(as.matrix(a[,1:2]), 2, function(x) quantile(x, probs = c(0.025)))
a.upper <- apply(as.matrix(a[,1:2]), 2, function(x) quantile(x, probs = c(0.975)))

## Combine both estimates
plot.dat.a <- data.frame(a.mean, a.lower, a.upper)
plot.dat.a$class <- row.names(plot.dat.a)
plot.dat.a$class <- factor(plot.dat.a$class, levels = c(paste0(c( "Past", "Modern"))))


a.dots.2.d13 <- ggplot(plot.dat.a, aes(x = a.mean, y = class, color = class, size = 2))+
  geom_errorbarh( xmin = a.lower, xmax = a.upper, size = 2,height = 0)+geom_point(alpha = 0.5)+xlim(-25, -23.5)+theme_bw(base_size = 18)+scale_color_manual(values = c("Past"='blue',"Modern"='red'))+
  coord_flip()+theme(legend.position = "none", axis.title.x = element_blank(), panel.grid = element_blank())+ xlab(expression(paste("Baseline" ,delta^{13}, "C (\u2030)")))+
  
  png(width = 5, height = 4, units = "in", res = 300, "outputs/growth_model/iWUE_MAP_TMAX_dbh/param_marginal_distn_bycohort_struct_v3_d13_random_slopes_cohort_struct_cohort_scaled.png")
a.dots.2.d13
dev.off()

# dot plot for MAP
b.mean <- apply(as.matrix(b[,1:2]), 2, mean)
b.lower <- apply(as.matrix(b[,1:2]), 2, function(x) quantile(x, probs = c(0.025)))
b.upper <- apply(as.matrix(b[,1:2]), 2, function(x) quantile(x, probs = c(0.975)))

## Combine both estimates
plot.dat.b <- data.frame(b.mean, b.lower, b.upper)
plot.dat.b$class <- row.names(plot.dat.b)
plot.dat.b$class <- factor(plot.dat.b$class, levels = c(paste0(c("Past", "Modern"))))




b.dots.2.d13 <- ggplot(plot.dat.b, aes(x = b.mean, y = class, color = class, size = 2))+geom_errorbarh( xmin = b.lower, xmax = b.upper, size = 2,height = 0)+
  geom_point(alpha = 0.5)+theme_bw(base_size = 18)+scale_color_manual(values = c("Past"='blue',"Modern"='red'))+coord_flip()+theme(legend.position = "none", axis.title.x = element_blank(), panel.grid = element_blank())+
  xlab(expression(paste("Precipitation Effect on " ,delta^{13}, "C (\u2030)")))+geom_vline(xintercept = 0, linetype = "dashed")+xlim(-0.5,5)

# png(width = 5, height = 4, units = "in", res = 300, "outputs/growth_model/iWUE_MAP_TMAX_dbh/param_marginal_distn_bycohort_struct_v3_d13_random_slopes_Precip_impact.png")
# b.dots.2.d13
# dev.off()





# dot plot for Tmax
b2.mean <- apply(as.matrix(b2[,1:2]), 2, mean)
b2.lower <- apply(as.matrix(b2[,1:2]), 2, function(x) quantile(x, probs = c(0.025)))
b2.upper <- apply(as.matrix(b2[,1:2]), 2, function(x) quantile(x, probs = c(0.975)))

## Combine both estimates
plot.dat.b2 <- data.frame(b2.mean, b2.lower, b2.upper)
plot.dat.b2$class <- c("Modern", "Past")
plot.dat.b2$class <- factor(plot.dat.b2$class, levels = c("Past",  "Modern"))




b2.dots.2.d13 <- ggplot(plot.dat.b2, aes(x = b2.mean, y = class, color = class, size = 2))+
  geom_errorbarh( xmin = b2.lower, xmax = b2.upper, size = 2,height = 0)+geom_point(alpha = 0.5)+
  theme_bw(base_size = 18)+scale_color_manual(values = c("Past"='blue',"Modern"='red'))+coord_flip()+
  theme(legend.position = "none", axis.title.x = element_blank(), panel.grid = element_blank())+xlab(expression(paste("Tmax Effect on" ,delta^{13}, "C (\u2030)")))+
  geom_vline(xintercept = 0, linetype = "dashed")+xlim( -4, 4)

# png(width = 5, height = 4, units = "in", res = 300, "outputs/growth_model/iWUE_MAP_TMAX_dbh/param_marginal_distn_bycohort_v3_d13_random_slopes_TMAX_impact.png")
# b2.dots.2.d13
# dev.off()



# dot plot for DBH
b3.mean <- apply(as.matrix(b3[,1:2]), 2, mean)
b3.lower <- apply(as.matrix(b3[,1:2]), 2, function(x) quantile(x, probs = c(0.025)))
b3.upper <- apply(as.matrix(b3[,1:2]), 2, function(x) quantile(x, probs = c(0.975)))

## Combine both estimates
plot.dat.b3 <- data.frame(b3.mean, b3.lower, b3.upper)
plot.dat.b3$class <- row.names(plot.dat.b3)
plot.dat.b3$class <- factor(plot.dat.b3$class, levels = c("Past",  "Modern"))



b3.dots.2.d13 <- ggplot(plot.dat.b3, aes(x = b3.mean, y = class, color = class, size = 2))+
  geom_errorbarh( xmin = b3.lower, xmax = b3.upper, size = 2,height = 0)+geom_point(alpha = 0.5)+theme_bw(base_size = 18)+
  scale_color_manual(values = c("Past"='blue',"Modern"='red'))+coord_flip()+theme(legend.position = "none", axis.title.x = element_blank(), panel.grid = element_blank())+
  xlim(-2, 10)+ xlab(expression(paste("DBH effect on " ,delta^{13}, "C (\u2030)")))+geom_vline(xintercept = 0, linetype = "dashed")

#png(width = 5, height = 4, units = "in", res = 300, "outputs/growth_model/iWUE_MAP_TMAX_dbh/param_marginal_distn_bycohort_struct_v3_d13_random_slopes_DBH_impact.png")
b3.dots.2.d13
#dev.off()

# generate one large figure for D13c and iWUE:
png(width = 7, height = 14, units = "in", res = 300, "outputs/paper_figures_struct_cohort_scaling/parameter_distributions_iWUE_d13_v3.png")

plot_grid(a.dots.2.wue, a.dots.2.d13,
          b.dots.2.wue, b.dots.2.d13,
          b2.dots.2.wue, b2.dots.2.d13,
          b3.dots.2.wue, b3.dots.2.d13
          , labels = "AUTO", ncol = 2)
dev.off()



#--------------------------------------------------------------------------------------------
# Plot WUE and d13 Models that also have structure + ageclass (these models fit better than ageclass only)
#--------------------------------------------------------------------------------------------


wue.struct.samps <- readRDS("outputs/growth_model/iWUE_MAP_TMAX_dbh_cohort/samps.iWUE_struct.cohort.re_struct_cohort_scaled_dataset_v4.rds")
#wue.data <- readRDS("outputs/growth_model/iWUE_MAP_TMAX_dbh_cohort/")

samps <- data.frame(wue.struct.samps[[1]])
alpha.samps  <- samps[,1:4]
beta.samps  <- samps[,5:8]
beta2.samps  <- samps[,9:12]
beta3.samps  <- samps[,13:16]
params <- samps[,1:16]
#saveRDS(params, "outputs/growth_model/iWUE_MAP_TMAX_dbh_cohort/params.iWUE_ageclass_v2.rds")
iWUE.samps  <- samps[,17:(16+length(full.iso$iWUE))]



alpha.diff.forest <- ((alpha.samps[,2]-alpha.samps[,1] )/alpha.samps[,1])*100 # (modern - past)/past * 100 = pct increase

wue.pct.diff.forest <- data.frame(
  pct.change = mean(alpha.diff.forest),
  Ci.low = quantile(alpha.diff.forest, 0.025), 
  Ci.high = quantile(alpha.diff.forest, 0.975), 
  median = median(alpha.diff.forest), 
  hdi.low = hdi(alpha.diff.forest)[1], 
  hdi.high = hdi(alpha.diff.forest)[2])

alpha.diff.savanna <- ((alpha.samps[,4]-alpha.samps[,3] )/alpha.samps[,3])*100

wue.pct.diff.savanna <- data.frame(
  pct.change = mean(alpha.diff.savanna),
  Ci.low = quantile(alpha.diff.savanna, 0.025), 
  Ci.high = quantile(alpha.diff.savanna, 0.975), 
  median = median(alpha.diff.savanna), 
  hdi.low = hdi(alpha.diff.savanna)[1], 
  hdi.high = hdi(alpha.diff.savanna)[2])


Yp.samps <- data.frame(iWUE.samps) 
Yp.m <- melt(Yp.samps)
Yp.summary <- Yp.m %>% group_by(variable) %>% dplyr::summarise(Predicted = mean(value),
                                                               ci.hi = quantile(value,0.975),
                                                               ci.lo = quantile(value,0.025))
Yp.summary$Observed <- full.iso$iWUE
Yp.summary$ageclass <- full.iso$ageclass
Yp.summary$struct.cohort <- full.iso$struct.cohort

pred.obs.wue.struct.cohort <- summary(lm(Yp.summary$Predicted~ Yp.summary$Observed))

# this does a poor job representing iWUE values by itself, but explains som of the variation
p.o.plot.wue.struct.cohort <- ggplot(Yp.summary, aes(Observed, Predicted))+geom_point(color = "black", size = 0.5)+
  geom_errorbar(data = Yp.summary,aes(ymin=ci.lo, ymax=ci.hi), color = "grey", alpha = 0.5)+
  geom_point(data = Yp.summary, aes(Observed, Predicted), color = "black", size = 0.5)+
  geom_abline(aes(slope = 1, intercept = 0), color = "red", linetype = "dashed")+
  geom_text(data=data.frame(pred.obs$r.squared), aes( label = paste("R^2: ", round(pred.obs.wue.struct.cohort$r.squared, digits = 3), sep="")),parse=T,x=120, y=175)+
  theme_bw(base_size = 16)+ylab("Predicted WUE")+xlab("Observed WUE")+theme(panel.grid = element_blank())+ylim(90, 190)+xlim(90, 190)

# note fairly poor model fit
png(width = 6, height = 5, units = "in", res = 300, "outputs/growth_model/iWUE_MAP_TMAX_dbh/pred_vs_obs_struct_cohort_WUE_struct_cohort_scaled_v4.png")
p.o.plot.wue.struct.cohort
dev.off()

Predicted.wue <- Yp.summary %>% group_by(struct.cohort) %>% dplyr::summarise(mean = mean(Predicted, na.rm = TRUE), 
                                                                             Ci.low = quantile(Predicted,0.025, na.rm = TRUE),
                                                                             Ci.high = quantile(Predicted,0.975, na.rm = TRUE), 
                                                                             hdi.low = hdi(Predicted, na.rm = TRUE)[1],
                                                                             hdi.high = hdi(Predicted,na.rm = TRUE)[2])

#predicted growth for our sites shows that strongest increas in predicted WUE for forests, bute savannas generally have a higher WUE in the past
ggplot(Predicted.wue , aes(struct.cohort, y = mean))+geom_bar(stat = "identity")+geom_errorbar(aes(ymin = Ci.low, ymax = Ci.high,width = 0.1))

Predicted.wue$struct.cohort<- factor(Predicted.wue$struct.cohort, levels = c(   "Past-Savanna", "Past-Forest", "Modern-Savanna","Modern-Forest" ))

predicted.wue.cs <- ggplot(Predicted.wue, aes(struct.cohort, y = mean, fill = struct.cohort))+geom_bar(stat = "identity")+scale_fill_manual(values = c("Past-Savanna"='#a6611a',"Modern-Savanna"='#dfc27d',"Modern-Forest"='#80cdc1', "Past-Forest"='#018571'))+
  geom_errorbar(aes(x = struct.cohort, ymin = Ci.low, ymax = Ci.high), alpha = 0.8, size = 0.5, width = 0.2)+ylab("Predicted iWUE")+theme_bw(base_size = 16)+theme(panel.grid = element_blank(), axis.title.x = element_blank(), legend.title = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))


Predicted.wue[Predicted.wue$struct.cohort %in% "Modern-Forest",]$mean -Predicted.wue[Predicted.wue$struct.cohort %in% "Past-Forest",]$mean 
Predicted.wue[Predicted.wue$struct.cohort %in% "Modern-Savanna",]$mean -Predicted.wue[Predicted.wue$struct.cohort %in% "Past-Savanna",]$mean 

# -----------plot marginal distributions of cohort + specific parameters:
unique(full.iso[, c("struct.cohort", "struct.cohort.code")])

a <- data.frame(alpha.samps)
colnames(a) <- c(paste0(c("Past-Forest", "Modern-Forest", "Past-Savanna", "Modern-Savanna")))
a$num <- rownames(a)
a.m <- melt(a, id.vars=c("num"))
alpha.mplots <- ggplot(a.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+ylab("frequency")+xlab("iWUE")


ggplot(a.m, aes(variable, value, fill = variable))+geom_boxplot(alpha = 0.5)+theme_bw()+ylab("frequency")+xlab("d13C")

a <- data.frame(alpha.samps)
colnames(a) <- c(paste0(c("Past-Forest", "Modern-Forest", "Past-Savanna", "Modern-Savanna")))




# for MAP:
b <- data.frame(beta.samps)
colnames(b) <- c(paste0(c("Past-Forest", "Modern-Forest", "Past-Savanna", "Modern-Savanna")))

b$num <- rownames(b)
b.m <- melt(b, id.vars=c("num"))
b.mplots <-ggplot(b.m, aes(value, 1, fill = variable))+stat_density_ridges(alpha = 0.5, quantile_lines = TRUE)+theme_bw()+xlab("Estimated Precip Sensitivity")+theme_bw(base_size = 16)#+scale_fill_manual(values = c("Past"='blue',"Modern"='red'))


# for Tmax:
b2 <- data.frame(beta2.samps)
colnames(b2) <-c(paste0(c("Past-Forest", "Modern-Forest", "Past-Savanna", "Modern-Savanna")))

b2$num <- rownames(b2)
b2.m <- melt(b2, id.vars=c("num"))
b2.mplots <-ggplot(b2.m, aes(value, 1, fill = variable))+stat_density_ridges(alpha = 0.5, quantile_lines = TRUE)+theme_bw()+xlab("Estimated Tmax Sensitivity")+theme_bw(base_size = 16)

# for DBH:
b3 <- data.frame(beta3.samps)
colnames(b3) <- c(paste0(c("Past-Forest", "Modern-Forest", "Past-Savanna", "Modern-Savanna")))

b3$num <- rownames(b3)
b3.m <- melt(b3, id.vars=c("num"))
b3.mplots <-ggplot(b3.m, aes(value, 1, fill = variable))+stat_density_ridges(alpha = 0.5, quantile_lines = TRUE)+theme_bw()+xlab("Estimated DBH Sensitivity")+theme_bw(base_size = 16)#+scale_fill_manual(values = c("Past"='blue',"Modern"='red'))

# calculate dot plots
a.mean <- apply(as.matrix(a[,1:4]), 2, mean)
a.lower <- apply(as.matrix(a[,1:4]), 2, function(x) quantile(x, probs = c(0.025)))
a.upper <- apply(as.matrix(a[,1:4]), 2, function(x) quantile(x, probs = c(0.975)))

## Combine both estimates
plot.dat.a <- data.frame(a.mean, a.lower, a.upper)
plot.dat.a$class <- row.names(plot.dat.a)
plot.dat.a$class <- factor(plot.dat.a$class, levels = c(paste0(c("Past-Forest", "Modern-Forest", "Past-Savanna", "Modern-Savanna"))))


a.dots.2.wue <- ggplot(plot.dat.a, aes(x = a.mean, y = class,  size = 2))+geom_errorbarh( xmin = a.lower, xmax = a.upper, size = 2,height = 0)+
  geom_point(alpha = 0.5)+theme_bw(base_size = 16)+scale_color_manual(values = c("Past-Savanna"='#a6611a',"Modern-Savanna"='#dfc27d',"Modern-Forest"='#80cdc1',"Past-Forest"='#018571'))+
  coord_flip()+xlim(90, 165)+theme(legend.position = "none", axis.title.x = element_blank(), panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+ xlab(expression(paste("Baseline iWUE")))

png(width = 5, height = 4, units = "in", res = 300, "outputs/growth_model/iWUE_MAP_TMAX_dbh/param_marginal_distn_bycohort_struct_v3_iwue_random_slopes_cohort_scaled_struct_cohort.png")
a.dots.2.wue
dev.off()

# dot plot for MAP
b.mean <- apply(as.matrix(b[,1:4]), 2, mean)
b.lower <- apply(as.matrix(b[,1:4]), 2, function(x) quantile(x, probs = c(0.025)))
b.upper <- apply(as.matrix(b[,1:4]), 2, function(x) quantile(x, probs = c(0.975)))

## Combine both estimates
plot.dat.b <- data.frame(b.mean, b.lower, b.upper)
plot.dat.b$class <- row.names(plot.dat.b)
plot.dat.b$class <- factor(plot.dat.b$class, levels = c(paste0(c("Past-Forest", "Modern-Forest", "Past-Savanna", "Modern-Savanna"))))




b.dots.2.wue <- ggplot(plot.dat.b, aes(x = b.mean, y = class,  size = 2))+geom_errorbarh( xmin = b.lower, xmax = b.upper, size = 2,height = 0)+
  geom_point(alpha = 0.5)+theme_bw(base_size = 16)+scale_color_manual(values = c("Past-Savanna"='#a6611a',"Modern-Savanna"='#dfc27d',"Modern-Forest"='#80cdc1',"Past-Forest"='#018571'))+coord_flip()+theme(legend.position = "none", axis.title.x = element_blank(), panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab("Precipitation \n effect on iWUE")+geom_vline(xintercept = 0, linetype = "dashed")+xlim(-4.1,9.7)

png(width = 5, height = 4, units = "in", res = 300, "outputs/growth_model/iWUE_MAP_TMAX_dbh/param_marginal_distn_bycohort_struct_v3_iwue_random_slopes_Precip_impact_scaled_struct_cohort.png")
b.dots.2.wue
dev.off()




# dot plot for Tmax
b2.mean <- apply(as.matrix(b2[,1:4]), 2, mean)
b2.lower <- apply(as.matrix(b2[,1:4]), 2, function(x) quantile(x, probs = c(0.025)))
b2.upper <- apply(as.matrix(b2[,1:4]), 2, function(x) quantile(x, probs = c(0.975)))

## Combine both estimates
plot.dat.b2 <- data.frame(b2.mean, b2.lower, b2.upper)
plot.dat.b2$class <- row.names(plot.dat.b2)
plot.dat.b2$class <- factor(plot.dat.b2$class, levels = c("Past-Forest", "Modern-Forest", "Past-Savanna", "Modern-Savanna"))




b2.dots.2.wue <- ggplot(plot.dat.b2, aes(x = b2.mean, y = class,  size = 2))+
  geom_errorbarh( xmin = b2.lower, xmax = b2.upper, size = 2,height = 0)+geom_point(alpha = 0.5)+
  theme_bw(base_size = 16)+scale_color_manual(values = c("Past-Savanna"='#a6611a',"Modern-Savanna"='#dfc27d',"Modern-Forest"='#80cdc1',"Past-Forest"='#018571'))+coord_flip()+
  xlab("Maximum Temperature \n effect on iWUE")+
  theme(legend.position = "none", axis.title.x = element_blank(), panel.grid = element_blank(), axis.text.x = element_text(angle= 45, hjust = 1))+ 
  geom_vline(xintercept = 0, linetype = "dashed")+xlim( -4, 6)

png(width = 5, height = 4, units = "in", res = 300, "outputs/growth_model/iWUE_MAP_TMAX_dbh/param_marginal_distn_bycohort_v3_iwue_random_slopes_TMAX_impact_scaled_struct_cohort.png")
b2.dots.2.wue
dev.off()



# dot plot for DBH
b3.mean <- apply(as.matrix(b3[,1:4]), 2, mean)
b3.lower <- apply(as.matrix(b3[,1:4]), 2, function(x) quantile(x, probs = c(0.025)))
b3.upper <- apply(as.matrix(b3[,1:4]), 2, function(x) quantile(x, probs = c(0.975)))

## Combine both estimates
plot.dat.b3 <- data.frame(b3.mean, b3.lower, b3.upper)
plot.dat.b3$class <- row.names(plot.dat.b3)
plot.dat.b3$class <- factor(plot.dat.b3$class, levels = paste0(c("Past-Forest", "Modern-Forest", "Past-Savanna", "Modern-Savanna")))



b3.dots.2.wue <- ggplot(plot.dat.b3, aes(x = b3.mean, y = class,  size = 2))+
  geom_errorbarh( xmin = b3.lower, xmax = b3.upper, size = 2,height = 0)+geom_point(alpha = 0.5)+theme_bw(base_size = 16)+
  scale_color_manual(values = c("Past-Savanna"='#a6611a',"Modern-Savanna"='#dfc27d',"Modern-Forest"='#80cdc1',"Past-Forest"='#018571'))+coord_flip()+theme(legend.position = "none", axis.title.x = element_blank(), panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+
  xlim(-10, 11)+ xlab(expression(paste("DBH effect on iWUE")))+geom_vline(xintercept = 0, linetype = "dashed")

png(width = 5, height = 4, units = "in", res = 300, "outputs/growth_model/iWUE_MAP_TMAX_dbh/param_marginal_distn_bycohort_struct_v3_iwue_random_slopes_DBH_impact_scaled_struct_cohort.png")
b3.dots.2.wue
dev.off()

png(width = 4, height = 14, units = "in", res = 300, "outputs/growth_model/iWUE_MAP_TMAX_dbh/param_marginal_distn_by age_structure_cohort_iWUE_all_dotplots_wueV3_scaled_struct_cohort.png")
plot_grid(a.dots.2.wue, b.dots.2.wue, b2.dots.2.wue, b3.dots.2.wue, ncol = 1,align = "hv", labels = "AUTO")
dev.off()

#--------------------plot cohort-structure random effects for d13C----------------------------------


d13.struct.samps <- readRDS("outputs/growth_model/iWUE_MAP_TMAX_dbh_cohort/samps.d13_struct.cohort_struct_cohort_scaled_dataset_v4.rds")

#wue.data <- readRDS("outputs/growth_model/iWUE_MAP_TMAX_dbh_cohort/")

samps <- data.frame(d13.struct.samps[[1]])
alpha.samps  <- samps[,1:4]
beta.samps  <- samps[,5:8]
beta2.samps  <- samps[,9:12]
beta3.samps  <- samps[,13:16]
params <- samps[,1:16]
#saveRDS(params, "outputs/growth_model/id13_MAP_TMAX_dbh_cohort/params.id13_ageclass_v2.rds")
#id13.samps  <- samps[,17:(16+length(full.iso$id13))]


#saveRDS(params, "outputs/growth_model/id13_MAP_TMAX_dbh_cohort/params.id13_ageclass_v2.rds")
id13.samps  <- samps[,17:(16+length(full.iso$Cor.d13C.suess))]

Yp.samps <- data.frame(id13.samps) 
Yp.m <- melt(Yp.samps)
Yp.summary <- Yp.m %>% group_by(variable) %>% dplyr::summarise(Predicted = mean(value),
                                                               ci.hi = quantile(value,0.975),
                                                               ci.lo = quantile(value,0.025))
Yp.summary$Observed <- full.iso$Cor.d13C.suess
Yp.summary$ageclass <- full.iso$ageclass
Yp.summary$struct.cohort <- full.iso$struct.cohort

pred.obs.cohort.struct <- summary(lm(Yp.summary$Predicted~ Yp.summary$Observed))

# this does a poor job representing iWUE values by itself, but explains som of the variation
p.o.plot.d13c.struct.cohort <- ggplot(Yp.summary, aes(Observed, Predicted))+geom_point(color = "black", size = 0.5)+
  geom_errorbar(data = Yp.summary,aes(ymin=ci.lo, ymax=ci.hi), color = "grey", alpha = 0.5)+
  geom_point(data = Yp.summary, aes(Observed, Predicted), color = "black", size = 0.5)+
  geom_abline(aes(slope = 1, intercept = 0), color = "red", linetype = "dashed")+
  geom_text(data=data.frame(pred.obs$r.squared), aes( label = paste("R^2: ", round(pred.obs.cohort.struct$r.squared, digits = 3), sep="")),parse=T,x=-26, y=-22)+
  theme_bw(base_size = 16)+ylab(expression(paste("Predicted ",delta^{13}, "C (\u2030)")))+xlab(expression(paste("Observed ",delta^{13}, "C (\u2030)")))+theme(panel.grid = element_blank())+ylim(-27, -21)+xlim(-27, -21)
#expression(paste("Tmax Effect on" ,delta^{13}, "C (\u2030)"))
# note fairly poor model fit
png(width = 6, height = 5, units = "in", res = 300, "outputs/growth_model/iWUE_MAP_TMAX_dbh/pred_vs_obs_struct_cohort_WUE_scaled_struct_cohort_v4.png")
p.o.plot.d13c.struct.cohort
dev.off()


alpha.diff.forest <- ((alpha.samps[,2]-alpha.samps[,1] )/alpha.samps[,1])*100 # (modern - past)/past * 100 = pct increase
alpha.mean.diff.forest <- (alpha.samps[,2]-alpha.samps[,1] )

data.frame(mean = mean(alpha.mean.diff.forest),
           Ci.low = quantile(alpha.mean.diff.forest, 0.025), 
           Ci.high = quantile(alpha.mean.diff.forest, 0.975)
)

d13.pct.diff.forest <- data.frame(
  pct.change = mean(alpha.diff.forest),
  Ci.low = quantile(alpha.diff.forest, 0.025), 
  Ci.high = quantile(alpha.diff.forest, 0.975), 
  median = median(alpha.diff.forest), 
  hdi.low = hdi(alpha.diff.forest)[1], 
  hdi.high = hdi(alpha.diff.forest)[2])

alpha.diff.savanna <- ((alpha.samps[,4]-alpha.samps[,3] )/alpha.samps[,3])*100

alpha.mean.diff.savanna <- (alpha.samps[,4]-alpha.samps[,3] )

data.frame(mean = mean(alpha.mean.diff.savanna),
           Ci.low = quantile(alpha.mean.diff.savanna, 0.025), 
           Ci.high = quantile(alpha.mean.diff.savanna, 0.975)
)


d13.pct.diff.savanna <- data.frame(
  pct.change = mean(alpha.diff.savanna),
  Ci.low = quantile(alpha.diff.savanna, 0.025), 
  Ci.high = quantile(alpha.diff.savanna, 0.975), 
  median = median(alpha.diff.savanna), 
  hdi.low = hdi(alpha.diff.savanna)[1], 
  hdi.high = hdi(alpha.diff.savanna)[2])

# -----------plot marginal distributions of cohort + specific parameters:
unique(full.iso[, c("struct.cohort", "struct.cohort.code")])

a <- data.frame(alpha.samps)
colnames(a) <- c(paste0(c("Past-Forest", "Modern-Forest", "Past-Savanna", "Modern-Savanna")))
a$num <- rownames(a)
a.m <- melt(a, id.vars=c("num"))
alpha.mplots <- ggplot(a.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+ylab("frequency")+xlab("id13")


ggplot(a.m, aes(variable, value, fill = variable))+geom_boxplot(alpha = 0.5)+theme_bw()+ylab("frequency")+xlab("d13C")

a <- data.frame(alpha.samps)
colnames(a) <- c(paste0(c("Past-Forest", "Modern-Forest", "Past-Savanna", "Modern-Savanna")))




# for MAP:
b <- data.frame(beta.samps)
colnames(b) <- c(paste0(c("Past-Forest", "Modern-Forest", "Past-Savanna", "Modern-Savanna")))

b$num <- rownames(b)
b.m <- melt(b, id.vars=c("num"))
b.mplots <-ggplot(b.m, aes(value, 1, fill = variable))+stat_density_ridges(alpha = 0.5, quantile_lines = TRUE)+theme_bw()+xlab("Estimated Precip Sensitivity")+theme_bw(base_size = 16)#+scale_fill_manual(values = c("Past"='blue',"Modern"='red'))


# for Tmax:
b2 <- data.frame(beta2.samps)
colnames(b2) <-c(paste0(c("Past-Forest", "Modern-Forest", "Past-Savanna", "Modern-Savanna")))

b2$num <- rownames(b2)
b2.m <- melt(b2, id.vars=c("num"))
b2.mplots <-ggplot(b2.m, aes(value, 1, fill = variable))+stat_density_ridges(alpha = 0.5, quantile_lines = TRUE)+theme_bw()+xlab("Estimated Tmax Sensitivity")+theme_bw(base_size = 16)

# for DBH:
b3 <- data.frame(beta3.samps)
colnames(b3) <- c(paste0(c("Past-Forest", "Modern-Forest", "Past-Savanna", "Modern-Savanna")))

b3$num <- rownames(b3)
b3.m <- melt(b3, id.vars=c("num"))
b3.mplots <-ggplot(b3.m, aes(value, 1, fill = variable))+stat_density_ridges(alpha = 0.5, quantile_lines = TRUE)+theme_bw()+xlab("Estimated DBH Sensitivity")+theme_bw(base_size = 16)#+scale_fill_manual(values = c("Past"='blue',"Modern"='red'))

# calculate dot plots
a.mean <- apply(as.matrix(a[,1:4]), 2, mean)
a.lower <- apply(as.matrix(a[,1:4]), 2, function(x) quantile(x, probs = c(0.025)))
a.upper <- apply(as.matrix(a[,1:4]), 2, function(x) quantile(x, probs = c(0.975)))

## Combine both estimates
plot.dat.a <- data.frame(a.mean, a.lower, a.upper)
plot.dat.a$class <- row.names(plot.dat.a)
plot.dat.a$class <- factor(plot.dat.a$class, levels = c(paste0(c("Past-Forest", "Modern-Forest", "Past-Savanna", "Modern-Savanna"))))


a.dots.2.d13 <- ggplot(plot.dat.a, aes(x = a.mean, y = class,  size = 2))+geom_errorbarh( xmin = a.lower, xmax = a.upper, size = 2,height = 0)+
  geom_point(alpha = 0.5)+theme_bw(base_size = 16)+scale_color_manual(values = c("Past-Savanna"='#a6611a',"Modern-Savanna"='#dfc27d',"Modern-Forest"='#80cdc1',"Past-Forest"='#018571'))+
  coord_flip()+xlim(-26, -22)+theme(legend.position = "none", axis.title.x = element_blank(), panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+ xlab(expression(paste("Baseline " ,delta^{13}, "C (\u2030)")))

# png(width = 5, height = 4, units = "in", res = 300, "outputs/growth_model/iWUE_MAP_TMAX_dbh/param_marginal_distn_bycohort_struct_v3_id13_random_slopes_cohort.png")
# a.dots.2.d13
# dev.off()

# dot plot for MAP
b.mean <- apply(as.matrix(b[,1:4]), 2, mean)
b.lower <- apply(as.matrix(b[,1:4]), 2, function(x) quantile(x, probs = c(0.025)))
b.upper <- apply(as.matrix(b[,1:4]), 2, function(x) quantile(x, probs = c(0.975)))

## Combine both estimates
plot.dat.b <- data.frame(b.mean, b.lower, b.upper)
plot.dat.b$class <- row.names(plot.dat.b)
plot.dat.b$class <- factor(plot.dat.b$class, levels = c(paste0(c("Past-Forest", "Modern-Forest", "Past-Savanna", "Modern-Savanna"))))




b.dots.2.d13 <- ggplot(plot.dat.b, aes(x = b.mean, y = class,  size = 2))+geom_errorbarh( xmin = b.lower, xmax = b.upper, size = 2,height = 0)+
  geom_point(alpha = 0.5)+theme_bw(base_size = 16)+scale_color_manual(values = c("Past-Savanna"='#a6611a',"Modern-Savanna"='#dfc27d',"Modern-Forest"='#80cdc1',"Past-Forest"='#018571'))+coord_flip()+theme(legend.position = "none", axis.title.x = element_blank(), panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab(expression(atop("Precipitation" , paste("effect on ",delta^{13}, "C (\u2030)"))))+geom_vline(xintercept = 0, linetype = "dashed")+xlim(-0.65,0.4)

#png(width = 5, height = 4, units = "in", res = 300, "outputs/growth_model/iWUE_MAP_TMAX_dbh/param_marginal_distn_bycohort_struct_v3_id13_random_slopes_Precip_impact.png")
#b.dots.2.d13

#dev.off()




# dot plot for Tmax
b2.mean <- apply(as.matrix(b2[,1:4]), 2, mean)
b2.lower <- apply(as.matrix(b2[,1:4]), 2, function(x) quantile(x, probs = c(0.025)))
b2.upper <- apply(as.matrix(b2[,1:4]), 2, function(x) quantile(x, probs = c(0.975)))

## Combine both estimates
plot.dat.b2 <- data.frame(b2.mean, b2.lower, b2.upper)
plot.dat.b2$class <- row.names(plot.dat.b2)
plot.dat.b2$class <- factor(plot.dat.b2$class, levels = c("Past-Forest", "Modern-Forest", "Past-Savanna", "Modern-Savanna"))


#atop("A long string of text for the purpose", paste("of illustrating my point" [reported]))))

b2.dots.2.d13 <- ggplot(plot.dat.b2, aes(x = b2.mean, y = class,  size = 2))+
  geom_errorbarh( xmin = b2.lower, xmax = b2.upper, size = 2,height = 0)+geom_point(alpha = 0.5)+
  theme_bw(base_size = 16)+scale_color_manual(values = c("Past-Savanna"='#a6611a',"Modern-Savanna"='#dfc27d',"Modern-Forest"='#80cdc1',"Past-Forest"='#018571'))+coord_flip()+
  xlab(expression(atop("Maximum Temperature" , paste("effect on ",delta^{13}, "C (\u2030)"))))+
  theme(legend.position = "none", axis.title.x = element_blank(), panel.grid = element_blank(), axis.text.x = element_text(angle= 45, hjust = 1))+ 
  geom_vline(xintercept = 0, linetype = "dashed")+xlim( -0.75, 0.75)

#png(width = 5, height = 4, units = "in", res = 300, "outputs/growth_model/iWUE_MAP_TMAX_dbh/param_marginal_distn_bycohort_v3_id13_random_slopes_TMAX_impact.png")
#b2.dots.2.d13
#dev.off()



# dot plot for DBH
b3.mean <- apply(as.matrix(b3[,1:4]), 2, mean)
b3.lower <- apply(as.matrix(b3[,1:4]), 2, function(x) quantile(x, probs = c(0.025)))
b3.upper <- apply(as.matrix(b3[,1:4]), 2, function(x) quantile(x, probs = c(0.975)))

## Combine both estimates
plot.dat.b3 <- data.frame(b3.mean, b3.lower, b3.upper)
plot.dat.b3$class <- row.names(plot.dat.b3)
plot.dat.b3$class <- factor(plot.dat.b3$class, levels = paste0(c("Past-Forest", "Modern-Forest", "Past-Savanna", "Modern-Savanna")))



b3.dots.2.d13 <- ggplot(plot.dat.b3, aes(x = b3.mean, y = class,  size = 2))+
  geom_errorbarh( xmin = b3.lower, xmax = b3.upper, size = 2,height = 0)+geom_point(alpha = 0.5)+theme_bw(base_size = 16)+
  scale_color_manual(values = c("Past-Savanna"='#a6611a',"Modern-Savanna"='#dfc27d',"Modern-Forest"='#80cdc1',"Past-Forest"='#018571'))+coord_flip()+theme(legend.position = "none", axis.title.x = element_blank(), panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+
  xlim(-1, 2)+ xlab(expression(paste("DBH effect on ",delta^{13}, "C (\u2030)")))+geom_vline(xintercept = 0, linetype = "dashed")

#png(width = 5, height = 4, units = "in", res = 300, "outputs/growth_model/iWUE_MAP_TMAX_dbh/param_marginal_distn_bycohort_struct_v3_id13_random_slopes_DBH_impact.png")
b3.dots.2.d13
#dev.off()

png(width = 4, height = 14, units = "in", res = 300, "outputs/growth_model/iWUE_MAP_TMAX_dbh/param_marginal_distn_by age_structure_cohort_id13_all_dotplots_d13scaled_struct_cohort_v4.png")
plot_grid(a.dots.2.d13, b.dots.2.d13, b2.dots.2.d13, b3.dots.2.d13, ncol = 1,align = "hv", labels = "AUTO")
dev.off()


titled13 <- ggdraw() + 
  draw_label(
    expression(paste(delta^{13}, "C (\u2030)")),
    x = 0.5,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

titleiWUE <- ggdraw() + 
  draw_label(
    "iWUE",
    x = 0.5,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

# plot all dotplots for WUE & d13c side by side
png(height = 13, width = 7, units = "in", res = 300, "outputs/paper_figures_struct_cohort_scaling/summary_of_d13_iwue_struct_cohort_params_v3_format.png")
plot_grid(plot_grid(titled13, titleiWUE, ncol = 2),
  plot_grid(a.dots.2.d13 +xlab("Baseline") + notoprightpanels + theme(plot.title = element_text(hjust = 0.5)), 
          a.dots.2.wue+xlab("Baseline")+ notoprightpanels +theme(plot.title = element_text(hjust = 0.5)),
          b.dots.2.d13+xlab("Precipitation effect")+ notoprightpanels, 
          b.dots.2.wue+xlab("Precipitation effect")+ notoprightpanels,
          b2.dots.2.d13+xlab("Max. temperature effect")+ notoprightpanels, 
          b2.dots.2.wue+xlab("Max. temperature effect")+ notoprightpanels,
          b3.dots.2.d13+xlab("DBH effect")+ notoprightpanels, 
          b3.dots.2.wue+xlab("DBH effect")+ notoprightpanels,
          ncol = 2, align = "v",labels = c("a)", "b)", "c)", "d)", "e)","f)", "g)", "h)"),
          label_x = 0.26, label_y = 0.95, label_fontface = "plain"),nrow = 2, rel_heights = c(0.05, 1))
dev.off()


# ----------------------------------------------------------------------------
# make supplement predicted vs observed plots
#-----------------------------------------------------------------------------

# pred vs observed d13 C
png(height = 4, width = 8, units = "in", res = 300, "outputs/paper_figures_struct_cohort_scaling/p.o.plots.d13C.combined.png")
plot_grid(p.o.plot.d13c.cohort, p.o.plot.d13c.struct.cohort, align = "hv", labels = "AUTO")
dev.off()


# pred vs observed iWUE

png(height = 4, width = 8, units = "in", res = 300, "outputs/paper_figures_struct_cohort_scaling/p.o.plots.iWUE.combined.png")
plot_grid(p.o.plot.wue.cohort, p.o.plot.wue.struct.cohort, align = "hv", labels = "AUTO")
dev.off()

# predicted vs observed growth
png(height = 4, width = 8, units = "in", res = 300, "outputs/paper_figures_struct_cohort_scaling/p.o.plots.growth.combined.png")
plot_grid(p.o.plot.growth.ageclass, p.o.plot.growth.cs, align = "hv", labels = "AUTO")
dev.off()



#----------plot changes in growth an predicted WUE across stand and cohorts:
cs.legend <- get_legend(predicted.wue.cs)

png(height = 7, width = 5.5, units = "in", res = 300, "outputs/paper_figures_struct_cohort_scaling/predicted_wue_growth_change_cohort_structures.png")
plot_grid(plot_grid(predicted.wue.cs+theme_bw(base_size = 14)+theme(legend.position = "none", panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank()), 
                    predicted.growth.cs+ylim(0, 6)+theme_bw(base_size = 14)+theme(legend.position = "none", panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank()), ncol = 1, align = "hv", labels = "AUTO"),
          cs.legend, rel_widths = c(0.5, 0.3))
dev.off()





#---------------------------------Just plot raw stable istope data used in models------------------
d13C.samples <- readRDS("/Users/kah/Documents/TreeRings/outputs/growth_model/iWUE_MAP_TMAX_dbh_cohort/full.iso_v3.rds")

d13C.samples %>% group_by(site, ageclass, structure) %>% summarise(n.tree =length(unique(ID)),
                                                                   n.year =length(unique(year)),
                                                                   WUE = mean(iWUE), 
                                                                   d13C = mean(Cor.d13C.suess))


# lets just make figures of the raw data:
WUE.column <- ggplot(d13C.samples, aes(year, iWUE))+geom_point(size = 0.5)+facet_wrap(~site, ncol = 1)+theme_bw(base_size = 12)+ylab("Raw iWUE")+xlab("Year")

d13.column <- ggplot(d13C.samples, aes(year, Cor.d13C.suess))+geom_point(size = 0.5)+facet_wrap(~site, ncol = 1)+theme_bw(base_size = 12)+ylab(expression(paste("Raw " ,delta^{13}, "C (\u2030 VPDB)")))+xlab("Year")

png(height= 6, width = 6, units ="in", res = 300, "outputs/paper_figures_struct_cohort_scaling/d13_iWUE_raw_data.png")
plot_grid(d13.column, WUE.column, ncol = 2, labels = "AUTO")
dev.off()


# ----------------------------------------------------------------------------------------------
# Make figure 5---moving this code from the RWI_bayes_model_struct_cohort_scaled_outputs
#-----------------------------------------------------------------------------------------------
# read in future temperatures for the region: rds file contains all of the climate model projections
June.rcps <- readRDS("data/future_climate_tas_all/future_june_tmax_all_rcp_models_v1.rds")

#@precip.means <- read.csv("outputs/CCESM_rcp8.5_mean_Pr_TMAX_proj_sites.csv")

unique.clim <- unique(train.dry.pair[, c("site", "year", "JUNTmax", "ageclass")])

Jun.rcps.clim.full <- June.rcps[,c("year", "Tmax", "period", "rcp", "site")]

# subset by sites analyzed here:
Jun.rcps.clim <- Jun.rcps.clim.full %>% filter(site %in% unique(unique.clim$site))
Jun.rcps.clim$period  <- ifelse(Jun.rcps.clim$year < 2050, "2025-2049",
                                ifelse(Jun.rcps.clim$year >=2050 & Jun.rcps.clim$year < 2075, "2050-2074", "2075-2099" ))

colnames(Jun.rcps.clim) <- c("year", "value", "fut.class", "rcp", "site")


summaries <- Jun.rcps.clim %>% group_by(rcp, fut.class)%>% dplyr::summarise(tmax.mean = mean(value), 
                                                                            tmax.sd = sd(value), 
                                                                            min = min(value),
                                                                            max = max(value))


unique.clim$fut.class <- ifelse(unique.clim$year <= 1950, "1895-1950", "1950-2015")
unique.clim2 <- unique.clim[,c("year", "JUNTmax", "fut.class", "ageclass", "site")]
colnames(unique.clim2) <- c("year", "value", "fut.class", "rcp",  "site")


summaries.past <- unique.clim2 %>% group_by(rcp, fut.class)%>% dplyr::summarise(tmax.mean = mean(value), 
                                                                                tmax.sd = sd(value))


clim.full <- rbind(unique.clim2, Jun.rcps.clim)

# making the dataframe to organize the time periods and rcps
reclass <- data.frame(class = c(1.5,2.5, 4,4.5,5,5.5, 8,8.5, 9, 9.5,  12, 12.5, 13, 13.5), 
                      fut.class = c("1895-1950", "1950-2015", 
                                    "2025-2049","2025-2049","2025-2049","2025-2049",
                                    "2050-2074","2050-2074","2050-2074","2050-2074",
                                    "2075-2099","2075-2099","2075-2099","2075-2099" ),
                      rcp = c("Past", "Modern", "rcp2.6", "rcp4.5", "rcp6.0", "rcp8.5", "rcp2.6", "rcp4.5", "rcp6.0", "rcp8.5", "rcp2.6", "rcp4.5", "rcp6.0", "rcp8.5"))

clim.full2 <- merge( reclass,clim.full, by = c("fut.class", "rcp"))

ggplot(clim.full2, aes(x = fut.class, y = value, fill = rcp))+geom_boxplot(width = 0.5, outlier.size = 0.05)+coord_flip()+xlab("Time Period")+ylab(expression("June Mean Maximum Temperature (" * degree * "C)"))


ggplot(clim.full, aes(value, fill = rcp))+geom_histogram()+facet_wrap(~fut.class)

Future.Tmax.summaries <- clim.full %>% group_by(fut.class, rcp) %>% dplyr::summarise(meanTmax = mean(value, na.rm=TRUE),
                                                                                     sd = sd(value, na.rm=TRUE),
                                                                                     Tmax.ci.low = quantile(value, 0.025, na.rm=TRUE),
                                                                                     Tmax.ci.high = quantile(value, 0.975, na.rm=TRUE))

Future.Tmax.summaries.byfut.class <- clim.full %>% group_by(fut.class) %>% dplyr::summarise(meanTmax = mean(value, na.rm=TRUE),
                                                                                            sd = sd(value, na.rm=TRUE),
                                                                                            Tmax.ci.low = quantile(value, 0.025, na.rm=TRUE),
                                                                                            Tmax.ci.high = quantile(value, 0.975, na.rm=TRUE))

# save summary table to output in the tables script
saveRDS(Future.Tmax.summaries, "/Users/kah/Documents/TreeRings/outputs/data/Tmax_future_climate_v2.rds")
clim.full$rcp.2 <- ifelse(clim.full$rcp %in% "rcp26", "rcp 2.6",
                          ifelse(clim.full$rcp %in% "rcp45", "rcp 4.5",
                                 ifelse(clim.full$rcp %in% "rcp60","rcp 6.0",
                                        ifelse(clim.full$rcp %in% "rcp85", "rcp 8.5",
                                               ifelse(clim.full$rcp %in% "Modern", "Modern", "Past")))))


boxplot.tmax <- ggplot(clim.full, aes(x = fut.class, y = value, fill = rcp.2))+geom_boxplot(width = 0.5, outlier.size = 0.05)+coord_flip()+
  xlab("Time Period")+ylab(expression("June Mean Maximum Temperature (" * degree * "C)"))+ scale_y_continuous(name = "Time Period", breaks = c(1.5, 2.5, 4.5, 8.5, 12.5 ), minor_breaks = NULL,labels = sort(unique(clim.full$fut.class)))+
  theme_bw()+theme(legend.title = element_blank(), panel.grid = element_blank()) +
  scale_fill_manual(values = c("Past"='#2166ac', 'Modern' = "#b2182b", "rcp 2.6" = "#ffffb2", "rcp 4.5" = "#984ea3", "rcp 6.0" = "#e0f3f8", "rcp 8.5" ="#fc8d59" ))+ylim(20,40)+ylab("Time Period")

