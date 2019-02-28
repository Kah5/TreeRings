# here we want to pull and extract the projected TMaximum for each month as a timeseries into the future.
# then we plot range of climate for different rcp scenarios:
library(plyr)
library(raster)
library(data.table)
#ibrary(rgdal)
library(reshape2)
library(ncdf4)
library(ncdf4.helpers)
library(PCICt)
library(tidyr)
library(dplyr)

#https://gdo-dcp.ucllnl.org/downscaled_cmip_projections/#Projections:%20Subset%20Request


# set the working dir (where the prism data folder is)
workingdir <- "/Users/kah/Documents/TreeRings/"
setwd(workingdir)

#ccesm.tam <- stack("data/ccesm_2025_2099_tamax/Extraction_tasmax.nc")
climate_output <- nc_open("data/ccesm_2025_2099_tamax/Extraction_tasmax.nc")

lon <- ncvar_get(climate_output, varid = "longitude")
lat <- ncvar_get(climate_output, varid = "latitude")
lat <- ncvar_get(climate_output, varid = "latitude")
#proj <- ncvar_get(climate_output, varid = "projection")
time <- ncvar_get(climate_output, varid = "time")
summary(lon)
summary(lat)
climate_output$dim$time$units
climate_output$dim$time$calendar

tas <- ncvar_get(climate_output, "tasmax") # pull out ta max
head(tas)
dim(tas)
#colnames(tas) <- lon

#tas.m <- as.data.frame(tas, )

tas.df <- as.data.frame(as.table(tas))
tas.df$lon <- factor(tas.df$Var1, labels=lon)
tas.df$lat <- factor(tas.df$Var2, labels=lat)
tas.df$proj <- factor(tas.df$Var4, labels=c("rcp2.6", "rcp4.5", "rcp6.0", "rcp8.5"))
month <- rep(1:12,  74)
year <- rep(2025:2099, each = 12)
year_mo <- paste0(year, "_", month)
tas.df$year_mo <- factor(tas.df$Var3, labels = year_mo)
#tas.df$month <- 

test <- tas.df %>% separate(year_mo, c("key","value"), "_", convert = TRUE) #%>% str
tas.df2 <- test
tas.df2$year_mo <- factor(tas.df2$Var3, labels = year_mo)

pet <- tas.df2[,c("lon", "lat","proj", "year_mo", "key", "value","Freq")]
colnames(pet) <- c("lon", "lat", "proj","year_mo", "year", "month","Tmax")


pet.long2 <- dcast(pet, lon + lat + proj ~ year_mo, fun.aggregate = mean, value.var='Tmax', na.rm = TRUE)
write.csv(pet.long2, "outputs/cmip5_rcp8.5_tmax_long.csv", row.names = FALSE)
nc_close("data/ccesm_2025_2099_tamax/Extraction_tasmax.nc")

pet.long2 <- read.csv( "outputs/cmip5_rcp8.5_tmax_long.csv")


# read in the site level data we are using to model:
#full.ghcn <- read.csv("outputs/data/rwi_age_dbh_ghcn.df")
#sites <- unique(full.ghcn$site)

tr_sites <- read.csv("/Users/kah/Documents/TreeRings/outputs/priority_sites_locs.csv")
tr_sites <- tr_sites[order(tr_sites$code),]
tr_sites$site <- c("AVO", "BAC", "BON","BOO", "CAC", "COR", "DUF", "ENG", "GLL1", "GLL2", "GLL3", "GLL4", 
                   "GLA","GLE","GLL", "HIC", "ITA","LED", "MAP","MOU", "PAM", "PLE", "PVC","STC", "TOW", "UNC", "UNI")

coordinates(tr_sites) <- ~ coords.x1 + coords.x2
proj4string(tr_sites) <- crs('+init=epsg:3175')


tr_sites.lat <- spTransform(tr_sites, CRSobj = CRS('+init=epsg:4269'))
tr_sites.lat.df <- data.frame(tr_sites.lat)

# extract grid cells where we have sites:
temp.long <- pet.long2
# ppet.long$lon <- as.numeric(as.character(ppet.long$lon))
temp.long$lat <- as.numeric(as.character(temp.long$lat))
temp.long$lon <- as.numeric(as.character(temp.long$lon))

temp.long.2.6 <- temp.long[temp.long$proj %in% "rcp2.6",]
temp.long.4.5 <- temp.long[temp.long$proj %in% "rcp4.5",]
temp.long.6.0 <- temp.long[temp.long$proj %in% "rcp6.0",]
temp.long.8.5 <- temp.long[temp.long$proj %in% "rcp8.5",]

# extract for 2.6:
coordinates(temp.long.2.6) <- ~ lon + lat 
gridded(temp.long.2.6) <- TRUE
ppet.rast <- stack(temp.long.2.6)
proj4string(ppet.rast) <-  '+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'


#ppet.rast.alb <- projectRaster(ppet.rast, crs='+init=epsg:3175') # project in great lakes albers

ppet.rast.df <- raster::extract(ppet.rast, tr_sites.lat.df[,c("coords.x1", "coords.x2")], df = TRUE)

ppet.rast.df$x <- tr_sites.lat.df$coords.x1
ppet.rast.df$y <- tr_sites.lat.df$coords.x2
ggplot(ppet.rast.df, aes(x,y, color = X2059_2))+geom_point()

# extract for 4.5:
coordinates(temp.long.4.5) <- ~ lon + lat 
gridded(temp.long.4.5) <- TRUE
ppet.rast.4.5 <- stack(temp.long.4.5)
proj4string(ppet.rast.4.5) <-  '+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'

# extract
ppet.rast.df.4.5 <- raster::extract(ppet.rast.4.5, tr_sites.lat.df[,c("coords.x1", "coords.x2")], df = TRUE)

ppet.rast.df.4.5$x <- tr_sites.lat.df$coords.x1
ppet.rast.df.4.5$y <- tr_sites.lat.df$coords.x2
ggplot(ppet.rast.df.4.5, aes(x,y, color = X2059_2))+geom_point()


# extract for 6.0:
coordinates(temp.long.6.0) <- ~ lon + lat 
gridded(temp.long.6.0) <- TRUE
ppet.rast.6.0 <- stack(temp.long.6.0)
proj4string(ppet.rast.6.0) <-  '+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'

# extract
ppet.rast.df.6.0 <- raster::extract(ppet.rast.6.0, tr_sites.lat.df[,c("coords.x1", "coords.x2")], df = TRUE)

ppet.rast.df.6.0$x <- tr_sites.lat.df$coords.x1
ppet.rast.df.6.0$y <- tr_sites.lat.df$coords.x2
ggplot(ppet.rast.df.6.0, aes(x,y, color = X2059_2))+geom_point()


# extract for 8.5:
coordinates(temp.long.8.5) <- ~ lon + lat 
gridded(temp.long.8.5) <- TRUE
ppet.rast.8.5 <- stack(temp.long.8.5)
proj4string(ppet.rast.8.5) <-  '+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'

# extract
ppet.rast.df.8.5 <- raster::extract(ppet.rast.8.5, tr_sites.lat.df[,c("coords.x1", "coords.x2")], df = TRUE)

ppet.rast.df.8.5$x <- tr_sites.lat.df$coords.x1
ppet.rast.df.8.5$y <- tr_sites.lat.df$coords.x2
ggplot(ppet.rast.df.8.5, aes(x,y, color = X2059_2))+geom_point()




temp.extracted <- rbind(ppet.rast.df, ppet.rast.df.4.5, ppet.rast.df.6.0, ppet.rast.df.8.5)



# get full midwest
pet.long.nona <- pet.long2[!is.na(pet.long2),]
midwest.region <- unique(pet.long.nona[,c("lat", "lon")])
region.8.5 <- raster::extract(ppet.rast.8.5, midwest.region[,c("lat", "lon")], df = TRUE)



# 
June_index <- colnames(temp.extracted) %like% "_6" 
temp.June <- temp.extracted[,June_index]
temp.June$x <- temp.extracted$x
temp.June$y <- temp.extracted$y
temp.June$proj <- temp.extracted$proj
Junes.melt <- melt(temp.June, id.vars = c("x", "y", "proj"))

Junes.melt2 <- Junes.melt %>% separate(variable, c("yr","month"), "_", convert = TRUE) 
Junes.melt3 <- Junes.melt2 %>% separate(yr, c("yr2","year"), "X", convert = TRUE) 

ggplot(Junes.melt3, aes(year, value, color = proj))+geom_point()
Junes.melt3$fut.class <- ifelse(Junes.melt3$year <= 2060, "2025-2060", "2060-2099")

ggplot(Junes.melt3, aes(year, value, color = fut.class))+geom_point()+facet_wrap(~proj)

Junes.melt3$proj <- as.factor(Junes.melt3$proj)

ggplot(Junes.melt3, aes(x = proj, y = value, fill = fut.class))+geom_boxplot(width = 0.5)#+facet_wrap(~proj)
Junes.melt3$rcp <- revalue(Junes.melt3$proj, c("1"="rcp2.6", "2"="rcp4.5", "3"= "rcp6.0", "4"="rcp8.5"))

ggplot(Junes.melt3, aes(x = rcp, y = value, fill = fut.class))+geom_boxplot(width = 0.5)#+facet_wrap(~proj)

write.csv(Junes.melt3, "outputs/June_summer_cmip5_preds_allyears_2025_2099.csv", row.names = FALSE)
