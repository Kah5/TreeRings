# script to readin in the downscaled climate model projections from https://gdo-dcp.ucllnl.org/downscaled_cmip_projections/#Projections:%20Complete%20Archives
# Under Subset request, I selected downscaled projections for Jan 2025 - Dec 2099 and highlighted the region of the midwest for the domain
# I used the projection set: BCSD-CMIP5-Hydrology-monthly and selected maximum temperature and precipiation
# then for all the rcps, I selected "all"
# then I selected "no analysis" and "netcdf" on the last page...it took less than an hour for them to email me with a link to download the zipped data
# #######################more information from the product:
# Product:               Bias-corrected statistically downscaled GCM data
# Variables:             tasmax    
# NetCDF type:          float     
# NetCDF missing value:  1e+20     
# Period:                2018Jan through 2099Dec
# Resolution:            1/8 degree
# Latitude bounds:       (29.875, 38.125)
# Longitude bounds:      (-115.125, -108.0)
# Area within bounds:    602058 km^2 (approx)
# Dimensions:         
#   Times:                984
# Latitudes:            66
# Longitudes:           57
# Projections:          97
# 
# 
# Global attributes
# -----------------
#   Conventions:           GDT 1.2
# authors:               Bridget Thrasher, Ed Maurer
# description:           Bias-corrected and downscaled GCM data
# creation_date:         2012
# institution:           Climate Analytics Group, Santa Clara U.
# SurfSgnConvention:     Traditional


# Selected 
# overview:
# 1. Read in the lat long data we need to extract climate data over
# 2. create function to open netcdf, generate a raster stack of all the projections, then extract by our lat long data
# 3. output & repeat for the next climate variable

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


# set the working dir (where the prism data folder is)
workingdir <- "/Users/kah/Documents/TreeRings2/"
setwd(workingdir)

alb_proj <- '+init=epsg:3175' # pls and fia data projection
ncdf_proj <- '+init=epsg:4269'

tr_sites <- read.csv("/Users/kah/Documents/TreeRings/outputs/priority_sites_locs.csv")
tr_sites <- tr_sites[order(tr_sites$code),]
tr_sites$site <- c("AVO", "BAC", "BON","BOO", "CAC", "COR", "DUF", "ENG", "GLL1", "GLL2", "GLL3", "GLL4", 
                   "GLA", "HIC","LED", "MOU", "PAM", "PLE", "PVC","STC", "TOW", "UNC", "UNI")

coordinates(tr_sites) <- ~ coords.x1 + coords.x2
proj4string(tr_sites) <- crs(alb_proj )


tr_sites.lat <- spTransform(tr_sites, CRSobj = CRS(ncdf_proj))
tr_sites.lat.df <- data.frame(tr_sites.lat)


#-------------------------------------------------------------------------
# Read in the netcdf for maximum temperatures 
#-------------------------------------------------------------------------
# get the filenames of the netcdfs:
# note that the second file is max temp
temp.files <- list.files("data/future_climate_tas_all/bcsd5/", pattern = ".nc")
temp.ncs <- paste0(getwd(),"/data/future_climate_tas_all/bcsd5/", temp.files) # full filenames


climate_output <- temp.ncs[2]

nc <- nc_open(climate_output)
variableofinterest <- names(nc$var) # get the variable of interest
tave <- ncvar_get(nc,variableofinterest) # this extracts a 4 dimensional array of data
dim(tave)

lon <- ncvar_get(nc, varid = "longitude") - 360
lat <- ncvar_get(nc, varid = "latitude")
lat <- ncvar_get(nc, varid = "latitude")
#proj <- ncvar_get(nc, varid = "projection")
time <- ncvar_get(nc, varid = "time")
summary(360 - lon)
summary(lat)
nc$dim$time$units
nc$dim$time$calendar

#tas <- ncvar_get(nc, "tas") # pull out ta max
#head(tas)
#dim(tas)
#colnames(tas) <- lon

nc_close(nc)

# dim(tave)# look at the dimensions
nmonths <- dim(tave)[3] # 3rd dimension is the number of months in the downscaled projections
# ntave <- dim(tave)



rlist <- list()

proj <- 1

tas <- tave
cov.data.ll <- tr_sites.lat.df

extract.tmax.july <- function(proj, tas,  tr_sites.lat.df , nmonths ){
# for the 1st model + projection, get raster lists of all the months
# make a raster for each month 
  
  
years <-  c(rep(2025:2098, each = 12),2099)

months <- c(rep(1:12, 74), 1)
ymonths <- 1:889
julys <- ymonths[ months%in% "6"]

for(m in julys){ 
  rlist[[m]] <- raster(as.matrix(t(tas[,101:1,m,proj])), xmn = min(lon), xmx = max(lon), 
                            ymn = min(lat) , ymx = max(lat) , 
                            crs = CRS('+init=epsg:4269'))
}

rast.stack.tave <- stack(rlist)

extracted.pts.tave <- data.frame(raster::extract(rast.stack.tave, tr_sites.lat.df[, c("coords.x1", "coords.x2")]))

#ll.data <- as.data.frame(cov.data.ll)
extracted.pts.tave$lat <-tr_sites.lat.df$coords.x2 # get the lat and long
extracted.pts.tave$lon <-tr_sites.lat.df$coords.x1

#system.time(removed.nas<- extracted.pts.tave[!rowSums(is.na(  extracted.pts.tave[cols])), ])

# do the same for the extracted.pts.t


colnames(extracted.pts.tave)[1:74] <- c(paste0("tave_", 2025:2098, "_", c(rep(6, 74)) )) #, "tavet_2099_1") # note may need to change this to make more customizable
extracted.pts.tave.m <- melt(extracted.pts.tave, id.vars = c("lat", "lon"))
extracted.pts.tave.m$value <- ifelse(extracted.pts.tave.m$value >= 1e+20, NA, extracted.pts.tave.m$value)
ext.tave.sep <- extracted.pts.tave.m %>% tidyr::separate(variable, sep = "_", into = c("climate", "year", "month"))
colnames(ext.tave.sep)[6] <- "Tave"





saveRDS(ext.tave.sep, paste0("data/future_climate_tas_all/rdsfiles/july_tave_summary_proj_", proj, "_v1.rds"))
rm(rast.stack.tave)
}


extract.tmax.july(proj = 2, tas = tas, tr_sites.lat.df = tr_sites.lat.df, nmonths = nmonths)


all.proj.names <- read.delim("data/future_climate_tas_all/bcsd5/Projections5.txt", header = FALSE)
proj.allnum<- 1:length(all.proj.names$V1)
# get the rcp values for all the projections
all.proj.names$rcp <- substr(all.proj.names$V1, nchar(as.character(all.proj.names$V1))-4, nchar(as.character(all.proj.names$V1)))

all.proj.names$proj.allnum <- 1:length(all.proj.names$V1)

# every 4th model is rcp 2.6

rcp.2.6.index <- all.proj.names %>% filter(rcp %in% "rcp26") %>% select(proj.allnum)

rcp.8.5.index <- all.proj.names %>% filter(rcp %in% "rcp85") %>% select(proj.allnum)
rcp.4.5.index <-  all.proj.names %>% filter(rcp %in% "rcp45") %>% select(proj.allnum)
rcp.6.0.index <-  all.proj.names %>% filter(rcp %in% "rcp60") %>% select(proj.allnum)


# then apply this funcation across all 97 projections downloaded in the netcdf
all.future.ppt <- list()

#system.time(all.future.ppt <- lapply(1:2, FUN = extract.yearly.ppt)) 
# user  system elapsed 
# 11.062   0.749  12.938 

# for loop with 2 :
#   user  system elapsed 
# 10.004   0.422  10.508 

# for loop is slightly faster, so we will use that, but this takes a bit
system.time(
  for(i in unique(rcp.2.6.index)){ # extracts for all 97 projections 
    extract.tmax.july(proj = i,  tas = tave, tr_sites.lat.df = tr_sites.lat.df  , nmonths = nmonths)
    print(i)
  }
)

system.time(
  for(i in unique(rcp.4.5.index)){ # extracts for all 97 projections 
    extract.tmax.july(proj = i,  tas = tave, tr_sites.lat.df = tr_sites.lat.df  , nmonths = nmonths)
    print(i)
  }
)

system.time(
  for(i in unique(rcp.6.0.index)){ # extracts for all 97 projections 
    extract.tmax.july(proj = i,  tas = tave, tr_sites.lat.df = tr_sites.lat.df  , nmonths = nmonths)
    print(i)
  }
)

system.time(
  for(i in unique(rcp.8.5.index)){ # extracts for all 97 projections 
    extract.tmax.july(proj = i,  tas = tave, tr_sites.lat.df = tr_sites.lat.df  , nmonths = nmonths)
    print(i)
  }
)


