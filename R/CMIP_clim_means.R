library(raster)
library(sp)
library(rgeos)
library(ggplot2)
library(reshape2)
library(ncdf4)
library(lubridate)

setwd("/Users/kah/Documents/TreeRings/")
# get average future climate projections for RCP 8.5:

rcp <- "85"
climate <- "pr"

# read in the site level data we are using to model:
full.ghcn <- read.csv("outputs/data/rwi_age_dbh_ghcn.df")
sites <- unique(full.ghcn$site)
  
# define a function to extract rcp data for each of these sites:
extract.site.rcps <- function(climate, rcp, sites, time, model){
  
  setwd(paste0('/Users/kah/Documents/bimodality/data/',model,rcp,climate,time,'/'))
  tr_sites <- read.csv("/Users/kah/Documents/TreeRings/outputs/priority_sites_locs.csv")
  tr_sites$site <- c("AVO", "BAC", "BON","BOO", "CAC", "COR", "DUF", "ENG", "GLL1", "GLL2", "GLL3", "GLL4", 
                     "GLA", "HIC", "LED", "MOU", "PAM", "PLE", "PVC","STC", "TOW", "UNC", "UNI")
  # select sites of interest:
  tr_sites <- tr_sites[tr_sites$site %in% sites,]
  
  coordinates(tr_sites) <- ~coords.x1 +coords.x2
  proj4string(tr_sites) <- '+init=epsg:3175' # tr_sites is currently in great lakes albers + we need to re-project
  tr_sites.ll <- spTransform(tr_sites, crs('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0 '))
  
  month <- sprintf("%02d", 1:12)
  month.abb <- c('Jan', 'Oct', 'Nov', "Dec","Feb","Mar","Apr", "May", 
                 'Jun', "Jul", "Aug", "Sep")
  filenames <- list.files(pattern=paste0(model,rcp,climate,time,".*\\.tif$", sep = ""))
  s <- stack(filenames)
  #t <- crop(s, extent(tr_sites.ll))#make all into a raster
  t <- crop(s, extent((matrix(c(-97,  40, -87,48), nrow=2))))#make all into a raster
  s <- projectRaster(t, crs='+init=epsg:3175') # project in great lakes albers
  #crop to the extent of indiana & illinois 
  y <- data.frame(rasterToPoints(s)) #covert to dataframe
  
  colnames(y) <- c("x", "y", month.abb)
  y$gridNumber <- cellFromXY(s, y[, 1:2])
  
  full <- y
  
  if(climate == 'pr'){
    full$total<- rowSums(full[,3:14], na.rm=TRUE)
    full$SI <- rowSums(abs(full[,3:14]-(full[,16]/12)))/full[,16]
    
  }else{
    full[,3:14] <- full[,3:14]/10
    full$mean <- rowMeans((full[,3:14]), na.rm = TRUE)
    full$meanGS <- rowMeans((full[,c( "Jun", "Jul")]), na.rm = TRUE)
    mean.corr <- full$mean
    mean.corr[abs(mean.corr) < 0.8 ] <- 0.8 # assign all mean values near 0 to 0.8 to avoid the cv blowing up
    full$SI <- (abs(apply((full[,3:14]),1, sd, na.rm = TRUE))/abs((mean.corr)))
    full$cv <- (apply(full[,3:14],1, sd, na.rm = TRUE)/full[,15])*100
  }
  
  coordinates(full) <- ~x + y
  gridded(full) <- TRUE
  avgs <- stack(full) 
  
  #plot(avgs) #plots averages
  
  tr_sites <- data.frame(tr_sites)
  avgs.df <- data.frame(x = tr_sites$coords.x1, y =tr_sites$coords.x2)
  if(climate == "pr"){
    avgs.df$total <- raster::extract(avgs$total, avgs.df[,c("x","y")])
    avgs.df$SI <- raster::extract(avgs$SI, avgs.df[,c("x","y")])
    colnames(avgs.df) <- c('x', "y", paste0(climate,"-", rcp), paste0(climate,'-',rcp,'SI')) 
  }else{
    avgs.df$mean <- raster::extract(avgs$mean, avgs.df[,c("x","y")])
    
    avgs.df$SI <- raster::extract(avgs$SI, avgs.df[,c("x","y")])
    avgs.df$meanGS <- raster::extract(avgs$meanGS, avgs.df[,c("x","y")])
    colnames(avgs.df) <- c('x', "y", paste0(climate,"-", rcp), paste0(climate,'-',rcp,'cv'), paste0(climate,'-',rcp,'GS')) 
    
  }
  avgs.df$site <- tr_sites$site
  avgs.df
}


# -----extract model projections for CCESM-----
# predictions for the 2050s:
pr85.50 <- extract.site.rcps ("pr", "85", sites = sites, time = "50", model = "cc")
Tmax.50 <- extract.site.rcps ("tx", "85", sites = sites, time = "50", model = "cc")
Tmin.50 <- extract.site.rcps ("tn", "85", sites = sites, time = "50", model = "cc")

# predictions for the 2070s
pr85.70 <- extract.site.rcps ("pr", "85", sites = sites, time = "70", model = "cc")
Tmax.70 <- extract.site.rcps ("tx", "85", sites = sites, time = "70", model = "cc")
Tmin.70 <- extract.site.rcps ("tn", "85", sites = sites, time = "70", model = "cc")

# need to convert to C and then F
toFahrenheit = function(celsius) {
  f = (9/5) * celsius + 32; 
}

Tmax.50$`tx-85` <- toFahrenheit(Tmax.50$`tx-85`)
Tmax.50$`tx-85cv` <- toFahrenheit(Tmax.50$`tx-85cv`)
Tmax.50$`tx-85GS` <- toFahrenheit(Tmax.50$`tx-85GS`)

Tmax.70$`tx-85` <- toFahrenheit(Tmax.70$`tx-85`)
Tmax.70$`tx-85cv` <- toFahrenheit(Tmax.70$`tx-85cv`)
Tmax.70$`tx-85GS` <- toFahrenheit(Tmax.70$`tx-85GS`)

Tmin.50$`tn-85` <- toFahrenheit(Tmin.50$`tn-85`)
Tmin.50$`tn-85cv` <- toFahrenheit(Tmin.50$`tn-85cv`)
Tmin.50$`tn-85GS` <- toFahrenheit(Tmin.50$`tn-85GS`)

Tmin.70$`tn-85` <- toFahrenheit(Tmin.70$`tn-85`)
Tmin.70$`tn-85cv` <- toFahrenheit(Tmin.70$`tn-85cv`)
Tmin.70$`tn-85GS` <- toFahrenheit(Tmin.70$`tn-85GS`)


# compile Tmaxes and precips into one data fram with a site column:
rcp8.5_2070 <- merge(Tmax.70, pr85.70, by = c("site", "x","y"))
rcp8.5_2050 <- merge(Tmax.50, pr85.50, by = c("site", "x","y"))
colnames(rcp8.5_2050)[4:8] <- c("Tx_85_50", "Tx_85_50cv", "Tx_85_50GS",
                                "Pr_85_50", "Pr_85_50si")

colnames(rcp8.5_2070)[4:8] <- c("Tx_85_70", "Tx_85_70cv", "Tx_85_70GS",
                                "Pr_85_70", "Pr_85_70si")

setwd("/Users/kah/Documents/TreeRings/")
rcp85 <- merge(rcp8.5_2050, rcp8.5_2070, by = c("site", "x","y"))

ggplot(rcp85, aes(Tx_85_70GS, Pr_85_70, color = site))+geom_point()

write.csv(rcp85, "outputs/CCESM_rcp8.5_mean_Pr_TMAX_proj_sites.csv", row.names = FALSE)



# -----extract model projections for IND-----
# predictions for the 2050s:
IN_pr85.50 <- extract.site.rcps ("pr", "85", sites = sites, time = "50", model = "in")
IN_Tmax.50 <- extract.site.rcps ("tx", "85", sites = sites, time = "50", model = "in")
IN_Tmin.50 <- extract.site.rcps ("tn", "85", sites = sites, time = "50", model = "in")

# predictions for the 2070s
IN_pr85.70 <- extract.site.rcps ("pr", "85", sites = sites, time = "70", model = "in")
IN_Tmax.70 <- extract.site.rcps ("tx", "85", sites = sites, time = "70", model = "in")
IN_Tmin.70 <- extract.site.rcps ("tn", "85", sites = sites, time = "70", model = "in")

# # need to convert to C and then F
# toFahrenheit = function(celsius) {
#   f = (9/5) * celsius + 32; 
# }
# 
# IN_Tmax.50$`tx-85` <- toFahrenheit(IN_Tmax.50$`tx-85`)
# IN_Tmax.50$`tx-85cv` <- toFahrenheit(IN_Tmax.50$`tx-85cv`)
# IN_Tmax.50$`tx-85GS` <- toFahrenheit(IN_Tmax.50$`tx-85GS`)
# 
# IN_Tmax.70$`tx-85` <- toFahrenheit(IN_Tmax.70$`tx-85`)
# IN_Tmax.70$`tx-85cv` <- toFahrenheit(IN_Tmax.70$`tx-85cv`)
# IN_Tmax.70$`tx-85GS` <- toFahrenheit(IN_Tmax.70$`tx-85GS`)
# 
# IN_Tmin.50$`tn-85` <- toFahrenheit(IN_Tmin.50$`tn-85`)
# IN_Tmin.50$`tn-85cv` <- toFahrenheit(IN_Tmin.50$`tn-85cv`)
# IN_Tmin.50$`tn-85GS` <- toFahrenheit(IN_Tmin.50$`tn-85GS`)
# 
# IN_Tmin.70$`tn-85` <- toFahrenheit(IN_Tmin.70$`tn-85`)
# IN_Tmin.70$`tn-85cv` <- toFahrenheit(IN_Tmin.70$`tn-85cv`)
# IN_Tmin.70$`tn-85GS` <- toFahrenheit(IN_Tmin.70$`tn-85GS`)


# compile Tmaxes and precips into one data fram with a site column:
IN_rcp8.5_2070 <- merge(IN_Tmax.70, IN_pr85.70, by = c("site", "x","y"))
IN_rcp8.5_2050 <- merge(IN_Tmax.50, IN_pr85.50, by = c("site", "x","y"))
colnames(IN_rcp8.5_2050)[4:8] <- c("Tx_85_50", "Tx_85_50cv", "Tx_85_50GS",
                                "Pr_85_50", "Pr_85_50si")

colnames(IN_rcp8.5_2070)[4:8] <- c("Tx_85_70", "Tx_85_70cv", "Tx_85_70GS",
                                "Pr_85_70", "Pr_85_70si")

setwd("/Users/kah/Documents/TreeRings/")
IN_rcp85 <- merge(IN_rcp8.5_2050, IN_rcp8.5_2070, by = c("site", "x","y"))

ggplot(IN_rcp85, aes(Tx_85_70GS, Pr_85_70, color = site))+geom_point()

write.csv(IN_rcp85, "outputs/INMCM4_rcp8.5_mean_Pr_TMAX_proj_sites.csv", row.names = FALSE)

# -----extract model projections for mcD-----
# predictions for the 2050s:
mc_pr85.50 <- extract.site.rcps ("pr", "85", sites = sites, time = "50", model = "mc")
mc_Tmax.50 <- extract.site.rcps ("tx", "85", sites = sites, time = "50", model = "mc")
mc_Tmmc.50 <- extract.site.rcps ("tn", "85", sites = sites, time = "50", model = "mc")

# predictions for the 2070s
mc_pr85.70 <- extract.site.rcps ("pr", "85", sites = sites, time = "70", model = "mc")
mc_Tmax.70 <- extract.site.rcps ("tx", "85", sites = sites, time = "70", model = "mc")
mc_Tmmc.70 <- extract.site.rcps ("tn", "85", sites = sites, time = "70", model = "mc")

# # need to convert to C and then F
# toFahrenheit = function(celsius) {
#   f = (9/5) * celsius + 32; 
# }
# 
# mc_Tmax.50$`tx-85` <- toFahrenheit(mc_Tmax.50$`tx-85`)
# mc_Tmax.50$`tx-85cv` <- toFahrenheit(mc_Tmax.50$`tx-85cv`)
# mc_Tmax.50$`tx-85GS` <- toFahrenheit(mc_Tmax.50$`tx-85GS`)
# 
# mc_Tmax.70$`tx-85` <- toFahrenheit(mc_Tmax.70$`tx-85`)
# mc_Tmax.70$`tx-85cv` <- toFahrenheit(mc_Tmax.70$`tx-85cv`)
# mc_Tmax.70$`tx-85GS` <- toFahrenheit(mc_Tmax.70$`tx-85GS`)
# 
# mc_Tmmc.50$`tn-85` <- toFahrenheit(mc_Tmmc.50$`tn-85`)
# mc_Tmmc.50$`tn-85cv` <- toFahrenheit(mc_Tmmc.50$`tn-85cv`)
# mc_Tmmc.50$`tn-85GS` <- toFahrenheit(mc_Tmmc.50$`tn-85GS`)
# 
# mc_Tmmc.70$`tn-85` <- toFahrenheit(mc_Tmmc.70$`tn-85`)
# mc_Tmmc.70$`tn-85cv` <- toFahrenheit(mc_Tmmc.70$`tn-85cv`)
# mc_Tmmc.70$`tn-85GS` <- toFahrenheit(mc_Tmmc.70$`tn-85GS`)


# compile Tmaxes and precips mcto one data fram with a site column:
mc_rcp8.5_2070 <- merge(mc_Tmax.70, mc_pr85.70, by = c("site", "x","y"))
mc_rcp8.5_2050 <- merge(mc_Tmax.50, mc_pr85.50, by = c("site", "x","y"))
colnames(mc_rcp8.5_2050)[4:8] <- c("Tx_85_50", "Tx_85_50cv", "Tx_85_50GS",
                                   "Pr_85_50", "Pr_85_50si")

colnames(mc_rcp8.5_2070)[4:8] <- c("Tx_85_70", "Tx_85_70cv", "Tx_85_70GS",
                                   "Pr_85_70", "Pr_85_70si")

setwd("/Users/kah/Documents/TreeRings/")
mc_rcp85 <- merge(mc_rcp8.5_2050, mc_rcp8.5_2070, by = c("site", "x","y"))

ggplot(mc_rcp85, aes(Tx_85_70GS, Pr_85_70, color = site))+geom_point()

write.csv(mc_rcp85, "outputs/mcMCM4_rcp8.5_mean_Pr_TMAX_proj_sites.csv", row.names = FALSE)



# -----extract model projections for HE-----
# predictions for the 2050s:
he_pr85.50 <- extract.site.rcps ("pr", "85", sites = sites, time = "50", model = "he")
he_Tmax.50 <- extract.site.rcps ("tx", "85", sites = sites, time = "50", model = "he")
he_Tmhe.50 <- extract.site.rcps ("tn", "85", sites = sites, time = "50", model = "he")

# predictions for the 2070s
he_pr85.70 <- extract.site.rcps ("pr", "85", sites = sites, time = "70", model = "he")
he_Tmax.70 <- extract.site.rcps ("tx", "85", sites = sites, time = "70", model = "he")
he_Tmhe.70 <- extract.site.rcps ("tn", "85", sites = sites, time = "70", model = "he")

# # need to convert to C and then F
# toFahrenheit = function(celsius) {
#   f = (9/5) * celsius + 32; 
# }
# 
# he_Tmax.50$`tx-85` <- toFahrenheit(he_Tmax.50$`tx-85`)
# he_Tmax.50$`tx-85cv` <- toFahrenheit(he_Tmax.50$`tx-85cv`)
# he_Tmax.50$`tx-85GS` <- toFahrenheit(he_Tmax.50$`tx-85GS`)
# 
# he_Tmax.70$`tx-85` <- toFahrenheit(he_Tmax.70$`tx-85`)
# he_Tmax.70$`tx-85cv` <- toFahrenheit(he_Tmax.70$`tx-85cv`)
# he_Tmax.70$`tx-85GS` <- toFahrenheit(he_Tmax.70$`tx-85GS`)
# 
# he_Tmhe.50$`tn-85` <- toFahrenheit(he_Tmhe.50$`tn-85`)
# he_Tmhe.50$`tn-85cv` <- toFahrenheit(he_Tmhe.50$`tn-85cv`)
# he_Tmhe.50$`tn-85GS` <- toFahrenheit(he_Tmhe.50$`tn-85GS`)
# 
# he_Tmhe.70$`tn-85` <- toFahrenheit(he_Tmhe.70$`tn-85`)
# he_Tmhe.70$`tn-85cv` <- toFahrenheit(he_Tmhe.70$`tn-85cv`)
# he_Tmhe.70$`tn-85GS` <- toFahrenheit(he_Tmhe.70$`tn-85GS`)


# compile Tmaxes and precips heto one data fram with a site column:
he_rcp8.5_2070 <- merge(he_Tmax.70, he_pr85.70, by = c("site", "x","y"))
he_rcp8.5_2050 <- merge(he_Tmax.50, he_pr85.50, by = c("site", "x","y"))
colnames(he_rcp8.5_2050)[4:8] <- c("Tx_85_50", "Tx_85_50cv", "Tx_85_50GS",
                                   "Pr_85_50", "Pr_85_50si")

colnames(he_rcp8.5_2070)[4:8] <- c("Tx_85_70", "Tx_85_70cv", "Tx_85_70GS",
                                   "Pr_85_70", "Pr_85_70si")

setwd("/Users/kah/Documents/TreeRings/")
he_rcp85 <- merge(he_rcp8.5_2050, he_rcp8.5_2070, by = c("site", "x","y"))

ggplot(he_rcp85, aes(Tx_85_70GS, Pr_85_70, color = site))+geom_point()

write.csv(he_rcp85, "outputs/heMCM4_rcp8.5_mean_Pr_TMAX_proj_sites.csv", row.names = FALSE)


# merge into one big df for rcp8.5:
#rcp85$model <- "CCESM"
IN_rcp85$model <- "INMCM4"
mc_rcp85$model <- "MIROC"
he_rcp85$model <- "HadGEM-ES2"

rcp85.full <- rbind( IN_rcp85, mc_rcp85, he_rcp85)
write.csv(rcp85.full, "outputs/rcp8.5_mean_Pr_TMAX_proj_sites.csv", row.names = FALSE)

# plot rcpfull:

P.50 <- ggplot(rcp85.full, aes( model, Pr_85_50))+geom_boxplot()+geom_point()+ylab("Total Annual Precipitation (mm) \n 2050 - 2069")
P.70 <- ggplot(rcp85.full, aes( model, Pr_85_70))+geom_boxplot()+geom_point()+ylab("Total Annual Precipitation (mm) \n 2070 - 2099")

t.50 <- ggplot(rcp85.full, aes( model, Tx_85_50GS))+geom_boxplot()+geom_point()+ylab("July Maximum Temperature (DegF)  \n 2050 - 2069")+ylim(84, 95)
t.70 <- ggplot(rcp85.full, aes( model, Tx_85_70GS))+geom_boxplot()+geom_point()+ylab("July Maximum Temperature (DegF) \n 2070 - 2099")+ylim(84, 95)

png(height = 8, width = 9, units = "in", res = 300, "/Users/kah/Documents/TreeRings/outputs/growth_model/future_climate_model_scenarios.png")
plot_grid(P.50,  t.50, P.70, t.70, ncol = 2, labels = "AUTO", label_x = 0.1)
dev.off()


# -------------------------Get the change between current PRISM and future projections--------------------

prism.df <- read.csv( "outputs/full_prism_all_months.csv")
prism.df$MAP <- rowSums(prism.df[,c("PRISM_pcp_1","PRISM_pcp_2", "PRISM_pcp_3","PRISM_pcp_4","PRISM_pcp_5","PRISM_pcp_6", "PRISM_pcp_7", "PRISM_pcp_8", "PRISM_pcp_9", "PRISM_pcp_10", "PRISM_pcp_11", "PRISM_pcp_12")])


prism.df$Tmean <- rowMeans(prism.df[,c("PRISM_tavg_1","PRISM_tavg_2", "PRISM_tavg_3","PRISM_tavg_4","PRISM_tavg_5","PRISM_tavg_6", "PRISM_tavg_7", "PRISM_tavg_8", "PRISM_tavg_9", "PRISM_tavg_10", "PRISM_tavg_11", "PRISM_tavg_12")])

prism.df$Tmax_gs <- rowMeans(prism.df[,c("PRISM_tmax_6","PRISM_tmax_7")])


full.current.summary <- prism.df %>% group_by(site) %>% summarise(MAP = mean(MAP, na.rm = TRUE), 
                                                                  Tmax = mean(Tmax_gs, na.rm = TRUE))

rcp85.full.C <- rcp85.full[,c("site", "x", "y", "Tx_85_50GS", "Tx_85_70GS", "Pr_85_50","Pr_85_70", "model")]
#rcp85.full.C$Tx_85_50GS <- 5/9 * (rcp85.full.C$Tx_85_50GS - 32)
#rcp85.full.C$Tx_85_70GS <- 5/9 * (rcp85.full.C$Tx_85_70GS - 32)

# get the differences between projected and mean of current climate
merged.full <- merge(rcp85.full.C, full.current.summary, by = "site")
merged.full$Tx_85_50_GS_diff <- merged.full$Tx_85_50GS - merged.full$Tmax
merged.full$Tx_85_70_GS_diff <- merged.full$Tx_85_70GS - merged.full$Tmax
merged.full$Pr_85_50_diff <- merged.full$Pr_85_50 - merged.full$MAP
merged.full$Pr_85_70_diff <- merged.full$Pr_85_70 - merged.full$MAP

ggplot(merged.full, aes(Tx_85_50_GS_diff, Tmax, color = model))+geom_point()
ggplot(merged.full, aes(Tx_85_70_GS_diff, Tmax, color = model))+geom_point()

ggplot(merged.full, aes(Pr_85_50_diff, Tmax, color = model))+geom_point()
ggplot(merged.full, aes(Pr_85_70_diff, Tmax, color = model))+geom_point()

P.50.diff <- ggplot()+geom_hline(yintercept = 0, color = "grey", linetype = "dashed")+geom_boxplot(data = merged.full, aes( model, Pr_85_50_diff))+geom_point(data = merged.full, aes( model, Pr_85_50_diff))+ylab("Changes in Total Annual Precipitation (mm) \n future - historical (2050 - 2069)")+ylim(-135, 100)
P.70.diff <- ggplot()+geom_hline(yintercept = 0, color = "grey", linetype = "dashed")+geom_boxplot(data = merged.full, aes( model, Pr_85_70_diff))+geom_point(data = merged.full, aes( model, Pr_85_70_diff))+ylab("Changes in Total Annual Precipitation (mm) \n future - historical (2070 - 2099)")+ylim(-135, 100)

t.50.diff <- ggplot(merged.full, aes( model, Tx_85_50_GS_diff))+geom_boxplot()+geom_point()+ylab("Changes in July Maximum Temperature (DegC)  \n future - historical (2050 - 2069)")+ylim(0, 8)
t.70.diff <- ggplot(merged.full, aes( model, Tx_85_70_GS_diff))+geom_boxplot()+geom_point()+ylab("Changes in July Maximum Temperature (DegC) \n future - historical (2070 - 2099)")+ylim(0, 8)


png(height = 10, width = 9, units = "in", res = 300, "/Users/kah/Documents/TreeRings/outputs/growth_model/future_climate_model_scenarios_differences.png")
plot_grid(P.50.diff,  t.50.diff, P.70.diff, t.70.diff, ncol = 2, labels = "AUTO", label_x = 0.1)
dev.off()


# precip comparison:
mc <- extract.site.rcps ("pr", "85", sites = sites, time = "50", model = "mc")
ccsm <- extract.site.rcps ("pr", "85", sites = sites, time = "50", model = "cc")
mr <- extract.site.rcps ("pr", "85", sites = sites, time = "50", model = "mr")
bc <- extract.site.rcps ("pr", "85", sites = sites, time = "50", model = "bc")
he <- extract.site.rcps ("pr", "85", sites = sites, time = "50", model = "he")
gs <- extract.site.rcps ("pr", "85", sites = sites, time = "50", model = "gs")
ip <- extract.site.rcps ("pr", "85", sites = sites, time = "50", model = "ip")
ind <- extract.site.rcps ("pr", "85", sites = sites, time = "50", model = "in")
no <- extract.site.rcps ("pr", "85", sites = sites, time = "50", model = "no")
mg <- extract.site.rcps ("pr", "85", sites = sites, time = "50", model = "mg")


mc$model <- "mc"
ccsm$model <- "cc"
mr$model <- "mr"
bc$model <- "bc"
he$model <- "he"
gs$model <- "gs"
ip$model <- "ip"
ind$model <- "ind"
no$model <- "no"
mg$model <- "mg"

pr50<- rbind(mc, ccsm, mr, bc, he, gs, ip, ind, no, mg)
colnames(pr50) <- c("x", "y", "pr85", "pr85si", "site", "model")
ggplot(pr50, aes( model, pr85,color = site))+geom_point()+geom_line()


# do the same for temmac
mc <- extract.site.rcps ("tx", "85", sites = sites, time = "50", model = "mc")
ccsm <- extract.site.rcps ("tx", "85", sites = sites, time = "50", model = "cc")
mr <- extract.site.rcps ("tx", "85", sites = sites, time = "50", model = "mr")
bc <- extract.site.rcps ("tx", "85", sites = sites, time = "50", model = "bc")
he <- extract.site.rcps ("tx", "85", sites = sites, time = "50", model = "he")
gs <- extract.site.rcps ("tx", "85", sites = sites, time = "50", model = "gs")
ip <- extract.site.rcps ("tx", "85", sites = sites, time = "50", model = "ip")
ind <- extract.site.rcps ("tx", "85", sites = sites, time = "50", model = "in")
no <- extract.site.rcps ("tx", "85", sites = sites, time = "50", model = "no")
mg <- extract.site.rcps ("tx", "85", sites = sites, time = "50", model = "mg")


mc$model <- "mc"
ccsm$model <- "cc"
mr$model <- "mr"
bc$model <- "bc"
he$model <- "he"
gs$model <- "gs"
ip$model <- "ip"
ind$model <- "ind"
no$model <- "no"
mg$model <- "mg"

tx50<- rbind(mc, ccsm, mr, he, gs, ip, ind, no, mg)
colnames(tx50) <- c("x", "y", "tx85", "tx85si", "tx85GS","site", "model")
ggplot(tx50, aes( model, tx85GS, color = site))+geom_point()
