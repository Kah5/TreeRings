# code to clean up and generate testing and training data for the RWI, WUE and d13 models:
# run after running RW_trends
# Author: Kelly A. Heilman
# Last Checked: March 25th, 2020

library(rjags)
library(ggplot2)
library(caTools)
library(ggridges)
library(tidyr)
library(reshape2)
library(dplyr)
library(cowplot)


# some initial data checking:
directory <- "Users/kah/Documents/TreeRings2"
full.ghcn <- read.csv(paste0("outputs/data/rwi_age_dbh_ghcn.csv"))
hist(full.ghcn$RWI)

hist(log(full.ghcn$RWI))

summary(full.ghcn)

# get all records that have all RWI and don't have negative diams or NA diams
full.ghcn <- full.ghcn[!is.na(full.ghcn$RWI) & full.ghcn$DBH > 0 & !is.na(full.ghcn$DBH),]



#DI <- as.vector( full.ghcn$JJA.pdsi )
ageclass <- as.numeric( full.ghcn$ageclass)
Age <- as.numeric( full.ghcn$Age)
n     <- length(full.ghcn$RWI)
DBH <- full.ghcn$DBH
full.ghcn$DBH.scaled = as.vector(scale(full.ghcn$DBH, center = TRUE, scale = TRUE))
site <- full.ghcn$site
SpecCode <- full.ghcn$SpecCode

# read in the prism data and the ghcn data
rwi.ghcn <- read.csv("outputs/full_ghcn_all_months_rwi.csv")
rwi.prism <- read.csv("outputs/full_prism_all_months_rwi.csv")


# calculate JJA VPD for each year
rwi.prism$jja.VPDmax <- rowMeans(rwi.prism[,c("Month_vpdmax_6", "Month_vpdmax_7", "Month_vpdmax_8")])
rwi.prism$jja.VPDmin <- rowMeans(rwi.prism[,c("Month_vpdmin_6", "Month_vpdmin_7", "Month_vpdmin_8")])

# calculate JJA BAL for each year
rwi.prism$jja.BAL <- rowMeans(rwi.prism[,c("Month_BAL_6", "Month_BAL_7", "Month_BAL_8")])

# calculate total precip for each year
rwi.ghcn$MAP.prism <- rowSums(rwi.prism[,c("Month_pcp_1","Month_pcp_2", "Month_pcp_3","Month_pcp_4","Month_pcp_5","Month_pcp_6", "Month_pcp_7", "Month_pcp_8", "Month_pcp_9", "Month_pcp_10", "Month_pcp_11", "Month_pcp_12")])


# get moving window of past 5, 10, 15 years of climate

prism.df <- read.csv( "outputs/full_prism_all_months.csv")

# create function to get the water year
wtr_yr <- function(df, start_month=9) {
  # Convert dates into POSIXlt
  #dates.posix = as.POSIXlt(dates)
  # Year offset
  offset = ifelse(df$month >= start_month - 1, 1, 0)
  # Water year
  adj.year = df$Year + offset
  # Return the water year
  adj.year
}


long.prism <- unique(prism.df[,c("site", "Year","PRISM_pcp_1","PRISM_pcp_2", "PRISM_pcp_3","PRISM_pcp_4","PRISM_pcp_5","PRISM_pcp_6", "PRISM_pcp_7", "PRISM_pcp_8", "PRISM_pcp_9", "PRISM_pcp_10", "PRISM_pcp_11", "PRISM_pcp_12"),]) %>%
  group_by(site, Year) %>%
  arrange(site, Year) %>% gather(key = precip, value = value, PRISM_pcp_1:PRISM_pcp_12)

long.prism$ month <- do.call(rbind, stringr::str_split(string = long.prism$precip, pattern = "_"))[,3]# get the month value
long.prism$climate <- do.call(rbind, stringr::str_split(string = long.prism$precip, pattern = "_"))[,2]# get the cliimate variable 

# use wtr_year function to get water year as a column
long.prism$wtr.year <- wtr_yr(long.prism)

# get total precipitation for each water year:
long.prism.wy.MAP <- long.prism %>% group_by(site, wtr.year) %>% summarise(MAP.wy = sum(value, na.rm=TRUE) )

colnames(long.prism.wy.MAP) <- c("site", "Year", "MAP.wy")

prism.df$MAP <- rowSums(prism.df[,c("PRISM_pcp_1","PRISM_pcp_2", "PRISM_pcp_3","PRISM_pcp_4","PRISM_pcp_5","PRISM_pcp_6", "PRISM_pcp_7", "PRISM_pcp_8", "PRISM_pcp_9", "PRISM_pcp_10", "PRISM_pcp_11", "PRISM_pcp_12")])

prism.df <- merge(prism.df, long.prism.wy.MAP, by = c("site", "Year"))
# plot wy vs map, its pretty much a 1:1 relationship, with noise, so it shouldnt change results that much
ggplot(prism.df, aes(MAP, MAP.wy, color = site))+geom_point()+geom_abline(yintercept = 0, slope = 1)

prism.df <- prism.df %>% select(-MAP) 
prism.df <- prism.df %>% rename(MAP = MAP.wy) # rename so we use WY MAP from now on

require(dplyr)
MAP.roll.means <- unique(prism.df[,c("site", "Year", "MAP"),]) %>%
  group_by(site, Year) %>%
  arrange(site, Year) %>%
  dplyr::mutate(MAP.5 = zoo::rollmean(x = MAP, 5, align = "right", fill = NA),
                MAP.10 = zoo::rollmean(x = MAP, 10, align = "right", fill = NA), 
                MAP.15 = zoo::rollmean(x = MAP, 15, align = "right", fill = NA))

colnames(MAP.roll.means) <- c("site", "year", "MAP", "MAP.5", "MAP.10", "MAP.15")


prism.df$Tmean <- rowMeans(prism.df[,c("PRISM_tavg_1","PRISM_tavg_2", "PRISM_tavg_3","PRISM_tavg_4","PRISM_tavg_5","PRISM_tavg_6", "PRISM_tavg_7", "PRISM_tavg_8", "PRISM_tavg_9", "PRISM_tavg_10", "PRISM_tavg_11", "PRISM_tavg_12")])


require(dplyr)
Tmean.roll.means <- unique(prism.df[,c("site", "Year", "Tmean"),]) %>%
  group_by(site, Year) %>%
  arrange(site, Year) %>% 
  dplyr:: mutate(Tmean.5 = zoo::rollmean(x = Tmean, 5, align = "right", fill = NA),
                 Tmean.10 = zoo::rollmean(x = Tmean, 10, align = "right", fill = NA), 
                 Tmean.15 = zoo::rollmean(x = Tmean, 15, align = "right", fill = NA))

colnames(Tmean.roll.means) <- c("site", "year", "Tmean", "Tmean.5", "Tmean.10", "Tmean.15")

ggplot(Tmean.roll.means, aes(year, Tmean.15, color = site))+geom_point()+geom_line()+stat_smooth()





rwi.ghcn$MAP.ghcn <- rowSums(rwi.ghcn[,c("Month_pcp_1","Month_pcp_2", "Month_pcp_3","Month_pcp_4","Month_pcp_5","Month_pcp_6", "Month_pcp_7", "Month_pcp_8", "Month_pcp_9", "Month_pcp_10", "Month_pcp_11", "Month_pcp_12")])

# make sure the prism pcp, temp values are specified in columns, so when we merge df, it won't be a huge deal:
colnames(rwi.prism)[12:59] <- paste0("prism_",colnames(rwi.prism)[12:59])
rwi.prism.sub <- rwi.prism[,!colnames(rwi.prism) %in% c("DBH", "dbhclass", "ageclass", "SpecCode", "RWI", "RWI_1", "RWI_2", "RWI_3")]
rwi.ghcn.sub <- rwi.ghcn[,!colnames(rwi.ghcn) %in% c("DBH", "dbhclass", "ageclass", "SpecCode", "RWI", "RWI_1", "RWI_2", "RWI_3")]
# merge rwi.ghcn and rwi.prism
full.clim <- merge(rwi.ghcn.sub, rwi.prism.sub, by = c("year", "site", "ID"))

# merge to full.ghcn
full.ghcn <- merge(full.ghcn, full.clim, by = c("ID", "year", "site" ))

full.ghcn$site_age <- paste0(full.ghcn$site, "-", full.ghcn$ageclass)
full.ghcn$site_age.code <- as.numeric(as.factor(full.ghcn$site_age))

# merge the lagged mean prism climates to full.ghcn
full.ghcn <- merge(full.ghcn, MAP.roll.means[,c("site", "year", "MAP.5", "MAP.10", "MAP.15")], by = c("site", "year"))

#DI <- as.vector( full.ghcn$JJA.pdsi )
ageclass <- as.numeric( full.ghcn$ageclass)
Age <- as.numeric( full.ghcn$Age)
n     <- length(full.ghcn$RWI)
DBH <- full.ghcn$DBH.x
full.ghcn$DBH.scaled = as.vector(scale(full.ghcn$DBH, center = TRUE, scale = TRUE))
site <- full.ghcn$site
SpecCode <- full.ghcn$SpecCode
plot <- unique(full.ghcn$site)

# convert Tmax to Celcius
#full.ghcn$JUNTmax <- 5/9 * (full.ghcn$JUNTmax - 32)


# standardise predictor variables to have mean 0 and sd = 1
DI.scaled = scale(full.ghcn$JJA.pdsi, center= TRUE, scale=TRUE)
DBH.scaled = scale(full.ghcn$DBH, center= TRUE, scale=TRUE)
full.ghcn$T.scaled = as.vector(scale(full.ghcn$JUNTmax, center= TRUE, scale=TRUE))
full.ghcn$DI.scaled = as.vector(scale(full.ghcn$JJA.pdsi, center = TRUE, scale = TRUE))
full.ghcn$DBH.scaled = as.vector(scale(full.ghcn$DBH, center = TRUE, scale = TRUE))
full.ghcn$SP6.scaled = as.vector(scale(full.ghcn$SP06_6, center = TRUE, scale = TRUE))
full.ghcn$SP6.scaled = as.vector(scale(full.ghcn$SP06_6, center = TRUE, scale = TRUE))
full.ghcn$SP1.scaled = as.vector(scale(full.ghcn$SP01_6, center = TRUE, scale = TRUE))

full.ghcn$MAP.5.scaled = as.vector(scale(full.ghcn$MAP.5, center = TRUE, scale = TRUE))
full.ghcn$MAP.15.scaled = as.vector(scale(full.ghcn$MAP.15, center = TRUE, scale = TRUE))
full.ghcn$MAP.10.scaled = as.vector(scale(full.ghcn$MAP.10, center = TRUE, scale = TRUE))


full.ghcn$jja.VPDmax.scaled <- as.vector(scale(full.ghcn$jja.VPDmax, center = TRUE, scale = TRUE))
full.ghcn$jja.BAL.scaled <- as.vector(scale(full.ghcn$jja.BAL, center = TRUE, scale = TRUE))
full.ghcn$MAP.scaled = as.vector(scale(full.ghcn$MAP.prism, center = TRUE, scale = TRUE))

MAP.scaled <- scale(full.ghcn$MAP.prism, center = TRUE, scale = TRUE)
T.scaled <- scale(full.ghcn$JUNTmax, center= TRUE, scale=TRUE)
MAP.5.scaled <- scale(full.ghcn$MAP.5, center = TRUE, scale = TRUE)
MAP.15.scaled <- scale(full.ghcn$MAP.15, center = TRUE, scale = TRUE)
MAP.10.scaled<-  scale(full.ghcn$MAP.10, center = TRUE, scale = TRUE)

SP1.scaled <- scale(full.ghcn$SP01_6, center = TRUE, scale = TRUE)
SP6.scaled <- scale(full.ghcn$SP06_6, center = TRUE, scale = TRUE)



# alternative scaling-> center on site level means:
full.ghcn$prism_tmax_jja <- rowMeans(full.ghcn[, c("prism_Month_tmax_6", "prism_Month_tmax_7", "prism_Month_tmax_8")])
full.ghcn.unique <- unique(full.ghcn[, c("site", "JUNTmax", "MAP.prism", "prism_Month_tmax_6", "prism_tmax_jja", "jja.VPDmax")])
site.means <- full.ghcn.unique %>% group_by(site) %>% summarise(site.tmax = mean(JUNTmax, na.rm = TRUE),
                                                                site.sd.tmax = sd(JUNTmax, na.rm = TRUE),
                                                                site.MAP = mean(MAP.prism, na.rm = TRUE), 
                                                                site.sd.MAP = sd (MAP.prism, na.rm = TRUE),
                                                                site.DBH = mean(DBH, na.rm=TRUE), 
                                                                site.tmax.prism = mean(prism_Month_tmax_6), 
                                                                site.tmax.prism.jja = mean(prism_tmax_jja), 
                                                                site.VPDmax = mean(jja.VPDmax))


full.ghcn <- left_join(full.ghcn, site.means, by = "site")

# scale data by site means for climate variables 
full.ghcn$T.scaled2 <- (full.ghcn$JUNTmax - full.ghcn$site.tmax)/full.ghcn$site.sd.tmax
full.ghcn$MAP.scaled2 <- (full.ghcn$MAP.prism - full.ghcn$site.MAP)/full.ghcn$site.sd.MAP
full.ghcn$T.prism.scaled2 <- (full.ghcn$prism_Month_tmax_6 - full.ghcn$site.tmax.prism)/full.ghcn$site.tmax.prism
full.ghcn$T.prism.JJA.scaled2 <- (full.ghcn$prism_tmax_jja - full.ghcn$site.tmax.prism.jja)/full.ghcn$site.tmax.prism.jja
full.ghcn$jja.VPDmax.scaled2 <- (full.ghcn$jja.VPDmax - full.ghcn$site.VPDmax)/full.ghcn$site.VPDmax


ggplot(full.ghcn, aes(Year, jja.VPDmax.scaled, color = site))+geom_point()+stat_smooth(method = "lm")
ggplot(full.ghcn, aes(Year, JUNTmin, color = site))+geom_point()+stat_smooth(method = "lm")
ggplot(full.ghcn, aes(Year, MAP.scaled, color = site))+geom_point()+stat_smooth(method = "lm")


ggplot(full.ghcn, aes(T.scaled, log(RWI), color = ID))+stat_smooth(method = "lm", se = FALSE)+theme(legend.position = "none")
ggplot(full.ghcn, aes(T.prism.JJA.scaled, log(RWI), color = ageclass))+geom_point()+stat_smooth(method = "lm")
ggplot(full.ghcn, aes(T.prism.scaled, log(RWI), color = ageclass))+geom_point()+stat_smooth(method = "lm")



#full.ghcn$MAP.scaled <- (full.ghcn$MAP.prism - full.ghcn$site.MAP)/full.ghcn$site.MAP

#full.ghcn$DBH.scaled <- (full.ghcn$DBH - full.ghcn$site.DBH)/full.ghcn$site.DBH

# need to define site level structures, if not already defined:

if(! "structure" %in% colnames(full.ghcn)){
  
  structure <- data.frame(site = c("AVO","BON","COR",  "ENG",  "GLA",  "GLL1", "GLL2", "GLL3","GLL4", "HIC",  "MOU",  "PLE",  "PVC",  "STC",  "TOW", "UNC" ),
                          structure = c("Forest", "Savanna", "Forest", "Forest", "Savanna", "Forest", "Savanna", "Savanna", "Forest", "Savanna", "Forest","Savanna", "Savanna", "Savanna", "Forest", "Savanna"))
  full.ghcn <- merge(full.ghcn, structure, by = "site")
}



#struct.cohort and struct.cohort.code (if it doesnt alread exist)

if(! "struct.cohort" %in% colnames(full.ghcn)){
  
  full.ghcn$struct.cohort <- paste0(full.ghcn$ageclass,"-", full.ghcn$structure)
  full.ghcn$struct.cohort.code <- ifelse(full.ghcn$struct.cohort %in% "Past-Forest", 1,
                                         ifelse(full.ghcn$struct.cohort %in% "Modern-Forest", 2, 
                                                ifelse(full.ghcn$struct.cohort %in% "Past-Savanna", 3,
                                                       ifelse(full.ghcn$struct.cohort %in% "Modern-Savanna", 4,"NA" ))))
  
}


write.csv(full.ghcn, "outputs/full.ghcn.sites.struct.before.splitting.csv", row.names = FALSE)


# omit NA values for RWI - 1:
ghcn.clean <- full.ghcn[!is.na(full.ghcn$RWI_1) & !is.na(full.ghcn$RWI_2) & !is.na(full.ghcn$DBH),]

# save ghcn.clean for later use:
saveRDS(ghcn.clean, "outputs/growth_model/ghcn.clean.full.data.rds")

# split training and testing datasets:
msk <- caTools::sample.split( ghcn.clean, SplitRatio = 3/4, group = NULL )

train.full <- ghcn.clean[msk,]
test.full <- ghcn.clean[!msk,]

# create a dataste the elimates yrs 1900-1950 for the modern and 1950-present for past:

mod.post <- ghcn.clean[ghcn.clean$ageclass %in% "Modern" & ghcn.clean$Year >= 1950,]
past.pre <- ghcn.clean[ghcn.clean$ageclass %in% "Past" & ghcn.clean$Year < 1950,]

sub.ghcn<- rbind(mod.post, past.pre)

msk <- caTools::sample.split( sub.ghcn, SplitRatio = 3/4, group = NULL )

train <- sub.ghcn[msk,]
test <- sub.ghcn[!msk,]

# get dry and wet years and separate by ageclass:
dry <- quantile(ghcn.clean$JJA.pdsi, 0.25) # value of the driest years
wet <- quantile(ghcn.clean$JJA.pdsi, 0.75) # value of the wettest years

pre.dry <- ghcn.clean[ghcn.clean$year < 1950 & ghcn.clean$Jul.pdsi <= dry & ghcn.clean$ageclass %in% "Past",]
pre.dry$class <- "pre-1950"
pre.dry$climclass <- "Dry_0.25"

post.dry <- ghcn.clean[ghcn.clean$year >=1950 & ghcn.clean$Jul.pdsi <= dry & ghcn.clean$ageclass %in% "Modern" ,]
post.dry$class <- "post-1950"
post.dry$climclass <- "Dry_0.25"

pre.wet <- ghcn.clean[ghcn.clean$year < 1950 & ghcn.clean$Jul.pdsi >= wet & ghcn.clean$ageclass %in% "Past",]
pre.wet$class <- "pre-1950"
pre.wet$climclass <- "Wet_0.25"
post.wet <- ghcn.clean[ghcn.clean$year >=1950 & ghcn.clean$Jul.pdsi >= wet & ghcn.clean$ageclass %in% "Modern" ,]
post.wet$class <- "post-1950"
post.wet$climclass <- "Wet_0.25"

# combine the pre and post dry data sets:
dry.yrs <- rbind(post.dry, pre.dry)

msk <- sample.split( dry.yrs, SplitRatio = 3/4, group = NULL )

train.dry <- dry.yrs[msk,]
test.dry <- dry.yrs[!msk,]

# now get dry years for sites where we have both ageclasses

dry.yrs.paired <- dry.yrs[!dry.yrs$site %in% c("COR","GLL4", "HIC", "PLE", "PVC", "STC", "TOW"),]
msk <- sample.split( dry.yrs.paired, SplitRatio = 3/4, group = NULL )

train.dry.pair <- dry.yrs.paired[msk,]
test.dry.pair <- dry.yrs.paired[!msk,]

# save the split testing and training data for use later on:
saveRDS(dry.yrs.paired, 'data/full_dry_paired_dataset.rds')
saveRDS(train.dry.pair, 'data/train_dry_paired_dataset.rds')
saveRDS(test.dry.pair, 'data/test_dry_paired_dataset.rds')



#---------------Develop test and training sets for the stable isotope data-------------------
# read in the d13 data:


#d13 <- read.csv("outputs/stable_isotopes/merged_d13_growth.csv")
d13 <- read.csv("outputs/stable_isotopes/merged_d13_growth_v3.csv")

d13 <- d13[!is.na(d13$DBH),]

 # if there a multiple values, take the mean d13C_12_cCorr
d13.values <- d13 %>% group_by(site, year, ID, ageclass) %>% dplyr::summarise(Cor.d13C.suess = mean(Cor.d13C.suess), 
                                               iWUE = mean(iWUE),
                                               Age = mean(Age),
                                               RWI= mean(RWI), 
                                               RWI_1 = mean(RWI_1),
                                               RWI_2 =mean (RWI_2), 
                                               PCP = mean (PCP), 
                                               TMIN = mean (TMIN),
                                               JUNTmax = mean (JUNTmax),
                                               DBH = mean (DBH))

d13.summary<- d13.values %>% group_by( site, ageclass, ID)%>% dplyr::summarise(n = n(), 
                                                   iWUE.mean = mean(iWUE, na.rm =TRUE))

d13.values %>% group_by(ageclass)%>% dplyr::summarise(n = n(), 
                                        iWUE.mean = mean(iWUE, na.rm =TRUE))

climate.wue <- ghcn.clean %>% dplyr::select(-ageclass)

full.iso <- merge(unique(climate.wue ), d13.values[,c("site", "ID", "year","Cor.d13C.suess", "iWUE", "ageclass")], by = c("site", "ID", "year"), all.y = TRUE)

full.iso %>% group_by(ageclass)%>% dplyr::summarise(n = n(), 
                                             iWUE.mean = mean(iWUE, na.rm =TRUE))


full.iso$iWUE.scaled <- as.numeric(scale(full.iso$iWUE))
d13index <- !is.na(full.ghcn$Cor.d13C.suess)

# split into testing and training:
library(caTools)
#full.iso <- full.ghcn[!is.na(full.ghcn$iWUE) & !full.ghcn$site %in% "BON",]
#full.iso <- full.ghcn
iWUE.med <- full.iso%>% group_by(struct.cohort, struct.cohort.code) %>% dplyr::summarise(iWUEmean = median(iWUE, na.rm =TRUE), WUE.lo = quantile(iWUE, 0.025, na.rm=TRUE),WUE.hi = quantile(iWUE, 0.975, na.rm = TRUE))
full.iso %>% group_by(structure, ageclass) %>% dplyr::summarise(iWUEmean = mean(iWUE, na.rm =TRUE),
                                                                d13mean = mean (Cor.d13C.suess),
                                                                n = n())



full.iso$struct.cohort <- paste0(full.iso$ageclass, "-", full.iso$structure)
full.iso$struct.cohort.code <- ifelse(full.iso$struct.cohort %in% "Past-Forest", 1,
                                   ifelse(full.iso$struct.cohort %in% "Modern-Forest", 2, 
                                          ifelse(full.iso$struct.cohort %in% "Past-Savanna", 3,
                                                 ifelse(full.iso$struct.cohort %in% "Modern-Savanna", 4,"NA" ))))


full.iso %>% group_by(struct.cohort.code) %>% dplyr::summarise(iWUEmean = mean(iWUE, na.rm =TRUE),
                                                                d13mean = mean (Cor.d13C.suess),
                                                                n = n())


full.iso<- full.iso[!is.na(full.iso$DBH),]
full.iso<- full.iso[!is.na(full.iso$ageclass),]

msk <- sample.split( full.iso, SplitRatio = 3/4, group = NULL )

train.iso <- full.iso[msk,]
test.iso <- full.iso[!msk,]

saveRDS(full.iso, 'data/full_WUE_dataset_v3.rds')
saveRDS(train.iso, 'data/train_WUE_dataset_v3.rds')
saveRDS(test.iso, 'data/test_WUE_dataset_v3.rds')

# ------read in estimates of future total precipiation & tmax to project tree growth in the future:
rcp85 <- read.csv("outputs/rcp8.5_mean_Pr_TMAX_proj_sites.csv")

# scale to the same scale as t.scaled
rcp85$Tx_50.scaled <- as.numeric(scale(rcp85$Tx_85_50GS, attr(T.scaled, "scaled:center"), attr(T.scaled, "scaled:scale")))

rcp85$Tx_70.scaled <- as.numeric(scale(rcp85$Tx_85_70GS, attr(T.scaled, "scaled:center"), attr(T.scaled, "scaled:scale")))

# scale to the same scale as MAP.scaled

#scaled.new <- scale(rcp85$Tx_85_50GS, center = mean(full.ghcn$JUNTmax), scale = attr(T.scaled, "scaled:scale"))
#scaled.new2 <- scale(rcp85$Tx_85_50GS, attr(full.ghcn$JUNTmax, "scaled:center"))

rcp85$Pr_85_50.scaled <- as.numeric(scale(rcp85$Pr_85_50, attr(MAP.scaled, "scaled:center"), attr(MAP.scaled, "scaled:scale")))
rcp85$PR_85_70.scaled <- as.numeric(scale(rcp85$Pr_85_70, attr(MAP.scaled, "scaled:center"), attr(MAP.scaled, "scaled:scale")))


if(! "structure" %in% colnames(rcp85)){
  
  structure <- data.frame(site = c("AVO","BON","COR",  "ENG",  "GLA",  "GLL1", "GLL2", "GLL3","GLL4", "HIC",  "MOU",  "PLE",  "PVC",  "STC",  "TOW", "UNC" ),
                          structure = c("Forest", "Savanna", "Forest", "Forest", "Savanna", "Forest", "Savanna", "Savanna", "Forest", "Savanna", "Forest","Savanna", "Savanna", "Savanna", "Forest", "Savanna"))
  rcp85 <- merge(rcp85, structure, by = "site")
  
}
rcp85$ageclass <- "Modern"

if(! "struct.cohort" %in% colnames(rcp85)){
  
  rcp85$struct.cohort <- paste0(rcp85$ageclass,"-", rcp85$structure)
  rcp85$struct.cohort.code <- ifelse(rcp85$struct.cohort %in% "Past-Forest", 1,
                                     ifelse(rcp85$struct.cohort %in% "Modern-Forest", 2, 
                                            ifelse(rcp85$struct.cohort %in% "Past-Savanna", 3,
                                                   ifelse(rcp85$struct.cohort %in% "Modern-Savanna", 4,"NA" ))))
  
}

