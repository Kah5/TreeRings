library(lme4)
library(dplR)
library(reshape2)
library(ggplot2)
library(treeclim)


#read in rwl & add site + year codes
#read in records from my collections:
Bonanza <- read.tucson("./cofecha/BONww.rwl", header = TRUE)
Hickory <- read.tucson ("./cofecha/HICww.rwl", header = FALSE)
PleasantWolf <- read.tucson('data/wi006.rwl') #Pleasant prairie in southeast WI, from ITRDB
StCroix <- read.tucson("./cofecha/STCww.rwl") #saint croix savanna, MN
Sand <- read.tucson("data/il001.rwl", header = TRUE) #Sandwich, il. Cook tree rings from the 1980's
#Pulaski <- read.tucson("./in001.rwl", header = TRUE)
Townsend <- read.tucson('./cofecha/tow/TOWww.rwl', header = TRUE)#townsedn woods
YellowRiver <- read.tucson('data/ia029.rwl', header = TRUE) # had to fix a wrong year
Pleasant <- read.tucson('./cofecha/PLEww.rwl', header = TRUE) #Pleasant valley conservency
Desouix <- read.tucson('data/mn029.rwl', header = TRUE) #close to BONanza
Coral <- read.tucson('/Users/kah/Documents/crossdating/data/cofecha/COR.rwl')
Uncas <- read.tucson("/Users/kah/Documents/crossdating/data/cofecha/UNC.rwl")
Glacial <- read.tucson("/Users/kah/Documents/crossdating/data/cofecha/GLA.rwl")
Englund <- read.tucson("/Users/kah/Documents/crossdating/data/cofecha/ENG.rwl")
Mound <- read.tucson("/Users/kah/Documents/crossdating/data/cofecha/MOU.rwl")
Glaciallk1 <- read.tucson("cleanrwl/GLL1ww.rwl")
Glaciallk2 <- read.tucson("cleanrwl/GLL1ww.rwl")
Glaciallk3 <- read.tucson("cleanrwl/GLL2ww.rwl")
Glaciallk4 <- read.tucson("cleanrwl/GLL3ww.rwl")
PVC <- read.tucson("cleanrwl/GLL4ww.rwl")


#this function uses dplr to read rwl files,plot spaghetti plots, detrend and plot chronologies
read_detrend_rwl <- function(rwl, name, det.method){
  #pdf(paste0('outputs/spagplots/',name,'.pdf'))
  #plot(rwl, plot.type = 'spag')
  #dev.off()
  stats <- rwi.stats(rwl)
  #detrend
  rwl.rwi <- detrend(rwl = rwl, method = det.method)
  rwl <- chron(rwl.rwi)
  png(paste0('./outputs/cronplots/', name, '.png'))
  plot(rwl)
  dev.off()
  #rwl.bai <- chron(bai.out(rwl.rwi))
  #rwl.bai$Year <- min(as.numeric(rownames(rwl))):max(as.numeric(rownames(rwl)))
  #rwl.bai$Site <- name
  rwl$Year <- min(as.numeric(rownames(rwl))):max(as.numeric(rownames(rwl)))
  rwl$Site <- name
  rwl
}


Glacial <- read_detrend_rwl(Glacial, "Glacial", "Spline")# spline bettern than modneg
Hickory <- read_detrend_rwl(Hickory, "Hickory","Spline")
Bonanza <- read_detrend_rwl(Bonanza, "Bonanza","Spline")
Pleasant <- read_detrend_rwl(Pleasant, "Pleasant","Spline")
Townsend <- read_detrend_rwl(Townsend, "Townsend","Spline")
StCroix <- read_detrend_rwl(StCroix, "StCroix","Spline")
Coral <- read_detrend_rwl(Coral, "Coral","Spline")
Uncas <- read_detrend_rwl(Uncas, "Uncas","Spline")
Englund <- read_detrend_rwl(Englund, "Englund","Spline")
Mound <- read_detrend_rwl(Mound, "Mound","Spline")
Glaciallk1 <- read_detrend_rwl(Glaciallk1, "GLL1","Spline")
Glaciallk2 <- read_detrend_rwl(Glaciallk2, "GLL2","Spline")
Glaciallk3 <- read_detrend_rwl(Glaciallk3, "GLL3","Spline")
Glaciallk4 <- read_detrend_rwl(Glaciallk4, "GLL4","Spline")
PVC <- read_detrend_rwl(PVC, "PVC", "Spline")
Uncas <- read_detrend_rwl(Uncas, "Uncas", "Spline")

#############################################
#plot detrended time series across all sites#
#############################################


Bonanza$type <- 'Savanna'
Hickory$type <- 'Savanna'
StCroix$type <- 'Savanna'
#Sandwich$type <- 'Forest'
Pleasant$type <- 'Savanna'
#PleasantPrairie$type <- 'Savanna'
Townsend$type <- 'Forest'
Glacial$type <- "Savanna"
Coral$type <- "Forest"
Uncas$type <- "Savanna"
Englund$type <- "Forest"
Mound$type <- "Savanna"
Glaciallk1$type <- "Woodland"
Glaciallk2$type <- "Savanna"
Glaciallk3$type <- "Savanna"
Glaciallk4$type <- "Forest"
PVC$type <- "Savanna"

crns <- rbind(Bonanza, Hickory, StCroix, Pleasant, Mound, #PleasantPrairie, 
              Townsend, Glacial, Coral, Uncas, Englund, Glaciallk1, Glaciallk2, Glaciallk3, Glaciallk4, PVC)
quartz(width = 14)
ggplot(crns, aes(x = Year,y=xxxstd, colour = Site)) +geom_point() + geom_smooth()+xlim(1900,2020) +ylim(0.5, 1.5)
ggplot(crns, aes(x = Year,y=xxxstd, colour = type)) +geom_point() + geom_smooth()+xlim(1900,2020) +ylim(0.5, 1.5)

# create a df with year, and crons
crn.cast <- dcast(crns[,c('xxxstd', "Year", "Site")], formula =  Year ~ Site,value.var = "xxxstd", na.rm=TRUE)
write.csv(crn.cast, "outputs/Cronology_full_by_yr.csv", row.names = FALSE)

#read climate
IL.clim <- read.csv("data/NE_illinois_climdiv.csv") #Hickory Grove, Sandwich, Glacial park
WIse.clim <- read.csv("data/south_east_wi_climdiv.csv") #pleasant prairie 
MNwc.clim <- read.csv("data/West_central_MN_nclimdiv.csv") #Bonanza praire, Duboix
WIsc.clim <- read.csv("data/south_central_WI_climdiv.csv") #pleasant valley conservancy
MNec.clim <- read.csv("data/East_Central_MN_CDODiv5039587215503.csv") #townsend woods 
MNec.clim <- read.csv("data/East_Central_MN_CDODiv5039587215503.csv") #St. Croix savanna
MNse.clim <- read.csv("data/South_East_MN_CDO.csv") # for mound prairie


#this function merges relevant climate parameters with the rwi site chronologies
merge.clim.chron <- function(MNcd.clim, chron, site){
MNcd.clim$PCP <- MNcd.clim$PCP*25.54

keeps <- c("Year", "Month",  "PCP")
keepstavg <- c("Year", "Month", "TAVG")
keepst <- c("Year", "Month",  "TMAX")
keepstmin <- c("Year", "Month",  "TMIN")
keepspdsi <- c("Year", "Month",  "PDSI")
keepsspi <- c("Year", "Month","SP01", "SP02", "SP06", "SP09", "SP12", "SP24")
#create a dataset for Precip
MNp.df <- MNcd.clim[,keeps]
MNp.df[MNp.df == -9999]<- NA

#for tmax
MNt.df <- MNcd.clim[,keepst]
MNt.df[MNt.df == -9999]<- NA

#for tmin
MNtmin.df<- MNcd.clim[,keepstmin]
MNtmin.df[MNtmin.df == -9999]<- NA

#for tavg
MNtavg.df <- MNcd.clim[,keepstavg]
MNtavg.df[MNtavg.df == -9999]<- NA

MNpdsi.df<- MNcd.clim[,keepspdsi]
MNpdsi.df[MNpdsi.df == -9999]<- NA

# for SPI
MNspi.df <- MNcd.clim[, keepsspi]
MNspi.df[MNspi.df == "-99.99"]<- NA
#for precipitation


total.p <- aggregate(PCP ~ Year + Month, data=MNp.df, FUN=sum, na.rm = T) 
months <- 6:9
MNpjja.df <- MNp.df[MNp.df$Month %in% months,]
jja.p <- aggregate(PCP ~ Year, data = MNpjja.df, FUN = sum, na.rm = T)

total.p <- aggregate(PCP ~ Year + Month, data=MNp.df, FUN=sum, na.rm = T) 
may.p <- total.p[total.p$Month == 5, ]

tavg.m <- aggregate(TAVG ~ Year + Month, data=MNtavg.df, FUN=sum, na.rm = T) 
jun.tavg <- tavg.m[tavg.m$Month == 6,]

tmin.m <- aggregate(TMIN ~ Year + Month, data = MNtmin.df, FUN = sum, na.rm = T)
jun.tmin <- tmin.m[tmin.m$Month == 6, ]

tmax.m <- aggregate(TMAX ~ Year + Month, data = MNt.df, FUN = sum, na.rm = T)
jun.tmax <- tmax.m[tmax.m$Month == 6, ]

spi01.m <- aggregate(SP01 ~ Year + Month, data = MNspi.df, FUN = sum, na.rm = T)
colnames(spi01.m) <- c("Year", "Month", paste0(site, '-SP01'))
jun.spi01 <- spi01.m[spi01.m$Month == 6, ]

spi02.m <- aggregate(SP02 ~ Year + Month, data = MNspi.df, FUN = sum, na.rm = T)
colnames(spi02.m) <- c("Year", "Month", paste0(site, '-SP02'))
jun.spi02 <- spi02.m[spi02.m$Month == 6, ]

spi06.m <- aggregate(SP06 ~ Year + Month, data = MNspi.df, FUN = sum, na.rm = T)
colnames(spi06.m) <- c("Year", "Month", paste0(site, '-SP06'))
jun.spi06 <- spi06.m[spi06.m$Month == 6, ]

spi09.m <- aggregate(SP09 ~ Year + Month, data = MNspi.df, FUN = sum, na.rm = T)
colnames(spi09.m) <- c("Year", "Month", paste0(site, '-SP09'))
jun.spi09 <- spi09.m[spi09.m$Month == 6, ]

spi12.m <- aggregate(SP12 ~ Year + Month, data = MNspi.df, FUN = sum, na.rm = T)
colnames(spi12.m) <- c("Year", "Month", paste0(site, '-SP12'))
jun.spi12 <- spi12.m[spi12.m$Month == 6, ]

spi24.m <- aggregate(SP24 ~ Year + Month, data = MNspi.df, FUN = sum, na.rm = T)
colnames(spi24.m) <- c("Year", "Month", paste0(site, '-SP24'))
jun.spi24 <- spi24.m[spi24.m$Month == 6, ]


SPI.data <- Reduce(function(x, y) merge(x, y, all=TRUE), list(spi01.m,spi02.m, spi06.m, spi09.m, spi12.m, spi24.m))
write.csv(SPI.data, paste0("outputs/data/", site, "-SPI_data.csv"), row.names = FALSE)

#precip <- dcast(total.p, Year  ~ Month)
annual.p <- aggregate(PCP~Year, data = MNp.df[1:1440,], FUN = sum, na.rm=T)
annual.t <- aggregate(TAVG ~ Year, data = MNtavg.df[1:1440,], FUN = 'mean', na.rm=T)
annual.mint <- aggregate(TMIN ~Year, data = MNtmin.df[1:1440,], FUN = 'mean', na.rm = T)
annual.pdsi <- aggregate(PDSI ~ Year, data = MNpdsi.df[1:1440,], FUN = 'mean', na.rm = T)
annual.pdsi.m <- aggregate(PDSI ~ Year + Month, data = MNpdsi.df[1:1440,], FUN = 'mean', na.rm = T)
jul.pdsi <- annual.pdsi.m[annual.pdsi.m$Month == 7,] 

annuals <- data.frame(Year = annual.p$Year, 
                      PCP = annual.p$PCP,
                      TMIN = annual.mint$TMIN,
                      TAVG = annual.t$TAVG,
                      PDSI = annual.pdsi$PDSI,
                      MAY.p = may.p[1:120,]$PCP,
                      JJA.p = jja.p[1:120,]$PCP,
                      JUNTmin = jun.tmin[1:120,]$TMIN,
                      JUNTavg = jun.tavg[1:120,]$TAVG, 
                      JUNTmax = jun.tmax[1:120,]$TMAX,
                      Jul.pdsi = jul.pdsi[1:120,]$PDSI)

#merge annuals with rwl
annuals.crn <- merge(annuals, chron[c('Year', 'xxxstd', 'Site')], by = "Year")
melt(annuals.crn, id = c('Year','Site', 'PCP', "TMIN", "TAVG", "PDSI","MAY.p","JJA.p", 
                                       "JUNTmin","JUNTavg", 'JUNTmax',"Jul.pdsi"))
}

#create molten dataframes with climate and rwi chronologies
molten.HIC <- merge.clim.chron(IL.clim, Hickory, "HIC")
molten.COR <- merge.clim.chron(IL.clim, Coral, "COR")
molten.GLA <- merge.clim.chron(IL.clim, Glacial, "GLA")
#molten.SAN <- merge.clim.chron(IL.clim, Sandwich)
#molten.PLP <- merge.clim.chron(WIse.clim, PleasantPrairie)
molten.BON <- merge.clim.chron(MNwc.clim, Bonanza, "BON")
#molten.DES <- merge.clim.chron(MNwc.clim, Desoix)
molten.PLE <- merge.clim.chron(WIsc.clim, Pleasant, "BON")
molten.TOW <- merge.clim.chron(MNec.clim, Townsend, "TOW")
molten.STC <- merge.clim.chron(MNec.clim, StCroix, "STC")
molten.UNC <- merge.clim.chron(MNec.clim, Uncas, "UNC")
molten.ENG <- merge.clim.chron(MNec.clim, Englund, "ENG")
molten.MOU <- merge.clim.chron(MNse.clim, Mound, "MOU")
molten.GL1 <- merge.clim.chron(MNwc.clim, Glaciallk1, "GL1")
molten.GL2 <- merge.clim.chron(MNwc.clim, Glaciallk2, "GL2")
molten.GL3 <- merge.clim.chron(MNwc.clim, Glaciallk3, "GL3")
molten.GL4 <- merge.clim.chron(MNwc.clim, Glaciallk4, "GL4")
molten.UNC <- merge.clim.chron(MNec.clim, Uncas, "UNC")
molten.PVC <- merge.clim.chron(IL.clim, PVC, "PVC")
write.csv(molten.HIC, "data/molten.hic.csv")

#want to plot all 
molten.full <- rbind(molten.HIC, molten.BON, molten.STC, #molten.PLE,
                     molten.TOW, molten.ENG, molten.UNC, molten.COR, molten.GLA, molten.MOU, molten.GL1, molten.GL2,
                     molten.GL3, molten.GL4, molten.PLE, molten.PVC)

write.csv(molten.full, "outputs/full_molten_chron.csv")

###############################################################
#using the treeclim R pacakge to calculate moving correlations#
###############################################################
clim.cor<- function(climate, chron, site.name){

PREC <- climate[,c('Year', 'Month', 'PCP')]
PREC$PCP <- PREC$PCP*25.54
PREC <- PREC[1:1452,]


hic.pdsi.static <- dcc(chron, PREC, dynamic = 'static', win_size = 35, win_offset = 5)
pdf(paste0('outputs/correlations/PREC_', site.name,'dynamic.pdf'))
print(plot(hic.pdsi.static))
#g_test(hic.pdsi.moving)
#traceplot(hic.pdsi.moving)
#plot(skills(hic.pdsi.moving))


hic.pdsi.moving <- dcc(chron, PREC, dynamic = 'moving', win_size = 35, win_offset = 5)

print(plot(hic.pdsi.moving))
#g_test(hic.pdsi.moving)
print(traceplot(hic.pdsi.moving))
#plot(skills(hic.pdsi.moving))
dev.off()

#PDSI
PDSI <- climate[,c('Year', 'Month', 'PDSI')]

PDSI <- PDSI[1:1452,]

pdf(paste0('outputs/correlations/PDSI_', site.name,'dynamic.pdf'))
hic.pdsi.static <- dcc(chron, PDSI, dynamic = 'static', win_size = 35, win_offset = 5)

print(plot(hic.pdsi.static))
#g_test(hic.pdsi.moving)
#traceplot(hic.pdsi.moving)
#plot(skills(hic.pdsi.moving))

hic.pdsi.moving <- dcc(chron, PDSI, dynamic = 'moving', win_size = 35, win_offset = 5)

print(plot(hic.pdsi.moving))
#g_test(hic.pdsi.moving)
print(traceplot(hic.pdsi.moving))
#plot(skills(hic.pdsi.moving))
dev.off()

#TAVG
TAVG <- climate[,c('Year', 'Month', 'TAVG')]

TAVG <- TAVG[1:1452,]

pdf(paste0('outputs/correlations/TAVG_', site.name,'dynamic.pdf'))
hic.pdsi.static <- dcc(chron, TAVG, dynamic = 'static', win_size = 35, win_offset = 5)

print(plot(hic.pdsi.static))


hic.pdsi.moving <- dcc(chron, TAVG, dynamic = 'moving', win_size = 35, win_offset = 5)

print(plot(hic.pdsi.moving))
#g_test(hic.pdsi.moving)
print(traceplot(hic.pdsi.moving))
dev.off()


#TMAX
TMAX <- climate[,c('Year', 'Month', 'TMAX')]

TMAX <- TMAX[1:1452,]

pdf(paste0('outputs/correlations/TMAX_', site.name,'dynamic.pdf'))
hic.pdsi.static <- dcc(chron, TMAX, dynamic = 'static', win_size = 35, win_offset = 5)

print(plot(hic.pdsi.static))


hic.pdsi.moving <- dcc(chron, TMAX, dynamic = 'moving', win_size = 35, win_offset = 5)

print(plot(hic.pdsi.moving))
#g_test(hic.pdsi.moving)
print(traceplot(hic.pdsi.moving))
#plot(skills(hic.pdsi.moving))
dev.off()

#TMIN
TMIN <- climate[,c('Year', 'Month', 'TMIN')]

TMIN <- TMIN[1:1452,]

pdf(paste0('outputs/correlations/TMIN_', site.name,'dynamic.pdf'))
hic.pdsi.static <- dcc(chron, TMIN, dynamic = 'static', win_size = 35, win_offset = 5)

print(plot(hic.pdsi.static))


hic.pdsi.moving <- dcc(chron, TMIN, dynamic = 'moving', win_size = 35, win_offset = 5)

print(plot(hic.pdsi.moving))
#g_test(hic.pdsi.moving)
print(traceplot(hic.pdsi.moving))
#plot(skills(hic.pdsi.moving))
dev.off()
dev.off()
}

clim.cor(IL.clim, Hickory, 'Hickory_Grove_')
clim.cor(IL.clim, Sandwich, 'Sandwich_')
clim.cor(WIse.clim, PleasantPrairie, 'Pleasant_Prairie_')
clim.cor(MNwc.clim, Bonanza, 'Bonanza_Prairie_')
clim.cor(MNwc.clim, Desoix, 'Desoix_')
clim.cor(WIsc.clim, Pleasant, 'Pleasant_Valley_Conservancy_')
clim.cor(MNec.clim, Townsend, 'Townsend_woods_')
clim.cor(MNec.clim, StCroix, 'StCroix_savanna_')
clim.cor(MNse.clim, Mound, 'Mound_prairie_')



###############################################################
#looking at age related correlation trends
###############################################################
#create a general function that makes two age related plots
age.trends <- function(rwl, stats, bkpt_yr){
df.list<- list(rwi= as.list(rwl), age = stats$year, record = colnames(rwl), year = rownames(rwl))
ages <- t(stats$year)

#repeat to have the same number as in the rwl
ages <- ages[rep(seq_len(nrow(ages)), each=nrow(rwl)),]

#df.list$means <- lapply(df.list$rwi, mean, na.rm=TRUE)
#plot(df.list$age,df.list$means)

#need to calculate tree age in each year, but this isnt righ 

df.list$yearssince2015 <- max(stats$last)-as.numeric(df.list$year)

#to find age for the for a given tree in df.list
calcyears<- function(x){
testage <- x-rev(df.list$yearssince2015)
testage[testage < 0] <- NA
rev(testage)
}


treeages <- apply(ages, FUN = calcyears, MARGIN = 2 )

colnames(treeages) <- colnames(rwl)
rownames(treeages) <- rownames(rwl)

pre1900 <- complete.cases(treeages[bkpt_yr,]) # finds columns that are NA's in 1890
indexpre1900 <- colnames(treeages[,pre1900])

ages.melt <- melt(treeages)
colnames(ages.melt)<- c('year', 'variable', 'age')
ages.melt$class <- ages.melt$variable %in% indexpre1900

rwl$year <- rownames(rwl)
rwi.melt <- melt(rwl)
colnames(rwi.melt) <- c('year', 'variable','rwi')
  
df.new <- merge(rwi.melt, ages.melt, by = c('year', 'variable'))

summary(dcast(df.new, rwi~., mean,var.value = age, na.rm=TRUE))

mean.rwi.age <- aggregate(rwi ~ age ,df.new, mean, na.rm = TRUE )
plot(mean.rwi.age, ylim = c(0, 4))
std <- aggregate(rwi ~ age, df.new, sd, na.rm = TRUE)
arrows(mean.rwi.age$age, mean.rwi.age$rwi-std$rwi, mean.rwi.age$age, mean.rwi.age$rwi+std$rwi, length=0.05, angle=90, code=3)

ggplot(data = mean.rwi.age, aes(x = age, y = rwi)) + geom_point()

mean.rwi.class <- aggregate(rwi ~ age + class ,df.new, mean, na.rm = TRUE )
mean.rwi.class[mean.rwi.class$class == FALSE,]$class <- 'post1900'
mean.rwi.class[mean.rwi.class$class == TRUE,]$class <- 'pre1900'

ggplot(data=mean.rwi.class, aes(x=age, y=rwi, colour=class)) + geom_point()

}

#need to read in data again: 
Bonanza <- read.tucson("./cofecha/BONww.rwl")
BON.stats <- rwl.stats(Bonanza)
#detrend
Bonanza.rwi <- detrend(rwl = Bonanza, method = "ModNegExp")

StCroix <- read.tucson('./cofecha/STCww.rwl')
STC.stats <- rwl.stats(StCroix)

PleasantPrairie <- read.tucson('data/wi006.rwl')
PLP.stats <- rwl.stats(PleasantPrairie)

Pleasant <- read.tucson('./cofecha/PLEww.rwl')
PLE.stats <- rwl.stats(Pleasant)

Sandwich <- read.tucson ("data/il002.rwl")
SAN.stats <- rwl.stats(Sandwich)

Townsend <- read.tucson('./cofecha/tow/TOWww.rwl')
TOW.stats <- rwl.stats(Townsend)

Yellow <- read.tucson ("data/ia029.rwl")
Yellow.stats <- rwl.stats(Yellow)

age.trends(Desoix, DES.stats, '1900')
age.trends(Hickory, HIC.stats, '1900')
age.trends(Bonanza, BON.stats, '1900')
age.trends(StCroix, STC.stats, '1900')
age.trends(PleasantPrairie, PLP.stats, '1830')
age.trends(Pleasant, PLE.stats, '1830')
age.trends(Sandwich, SAN.stats, '1850')
age.trends(Townsend, TOW.stats, '1900')
age.trends(Yellow, Yellow.stats, '1800')






#find age dependant trends in correlation with climate
df.list$PCP <- IL.clim$PCP
plot(rev(testage), df.list$rwi$HICa397)


merge.clim.list <- function(MNcd.clim){
  MNcd.clim$PCP <- MNcd.clim$PCP*25.54
  
  keeps <- c("Year", "Month",  "PCP")
  keepstavg <- c("Year", "Month", "TAVG")
  keepst <- c("Year", "Month",  "TMAX")
  keepstmin <- c("Year", "Month",  "TMIN")
  keepspdsi <- c("Year", "Month",  "PDSI")
  #create a dataset for Precip
  MNp.df <- MNcd.clim[,keeps]
  MNp.df[MNp.df == -9999]<- NA
  
  #for tmax
  MNt.df <- MNcd.clim[,keepst]
  MNt.df[MNt.df == -9999]<- NA
  
  #for tmin
  MNtmin.df<- MNcd.clim[,keepstmin]
  MNtmin.df[MNtmin.df == -9999]<- NA
  
  #for tavg
  MNtavg.df <- MNcd.clim[,keepstavg]
  MNtavg.df[MNtavg.df == -9999]<- NA
  
  MNpdsi.df<- MNcd.clim[,keepspdsi]
  MNpdsi.df[MNpdsi.df == -9999]<- NA
  #for precipitation
  
  
  total.p <- aggregate(PCP ~ Year + Month, data=MNp.df, FUN=sum, na.rm = T) 
  months <- 6:9
  MNpjja.df <- MNp.df[MNp.df$Month %in% months,]
  jja.p <- aggregate(PCP ~ Year, data = MNpjja.df, FUN = sum, na.rm = T)
  
  total.p <- aggregate(PCP ~ Year + Month, data=MNp.df, FUN=sum, na.rm = T) 
  may.p <- total.p[total.p$Month == 5, ]
  
  tavg.m <- aggregate(TAVG ~ Year + Month, data=MNtavg.df, FUN=sum, na.rm = T) 
  jun.tavg <- tavg.m[tavg.m$Month == 6,]
  
  tmin.m <- aggregate(TMIN ~ Year + Month, data = MNtmin.df, FUN = sum, na.rm = T)
  jun.tmin <- tmin.m[tmin.m$Month == 6, ]
  
  tmax.m <- aggregate(TMAX ~ Year + Month, data = MNt.df, FUN = sum, na.rm = T)
  jun.tmax <- tmax.m[tmax.m$Month == 6, ]
  
  
  
  
  #pr.yr <- aggregate(PCP ~ Year , data=MNp.df, FUN=sum, na.rm = T) 
  #plot(pr.yr[1:120,1], pr.yr[1:120,2], type = "l", xlab = "Year", ylab = "Annual Precip (mm)")
  
  
  #precip <- dcast(total.p, Year  ~ Month)
  annual.p <- aggregate(PCP~Year, data = MNp.df[1:1440,], FUN = sum, na.rm=T)
  annual.t <- aggregate(TAVG ~ Year, data = MNtavg.df[1:1440,], FUN = 'mean', na.rm=T)
  annual.mint <- aggregate(TMIN ~Year, data = MNtmin.df[1:1440,], FUN = 'mean', na.rm = T)
  annual.pdsi <- aggregate(PDSI ~ Year, data = MNpdsi.df[1:1440,], FUN = 'mean', na.rm = T)
  annual.pdsi.m <- aggregate(PDSI ~ Year + Month, data = MNpdsi.df[1:1440,], FUN = 'mean', na.rm = T)
  jul.pdsi <- annual.pdsi.m[annual.pdsi.m$Month == 7,] 
  
  annuals <- data.frame(Year = annual.p$Year, 
                        PCP = annual.p$PCP,
                        TMIN = annual.mint$TMIN,
                        TAVG = annual.t$TAVG,
                        PDSI = annual.pdsi$PDSI,
                        MAY.p = may.p[1:120,]$PCP,
                        JJA.p = jja.p[1:120,]$PCP,
                        JUNTmin = jun.tmin[1:120,]$TMIN,
                        JUNTavg = jun.tavg[1:120,]$TAVG, 
                        JUNTmax = jun.tmax[1:120,]$TMAX,
                        Jul.pdsi = jul.pdsi[1:120,]$PDSI)
  n <- 1895-as.numeric(df.list$year)[1]
  annuals.na <- data.frame(Year = as.numeric(df.list$year)[1:n], 
                        PCP = NA,
                        TMIN = NA,
                        TAVG = NA, 
                        PDSI = NA,
                        MAY.p = NA,
                        JJA.p = NA,
                        JUNTmin = NA,
                        JUNTavg = NA, 
                        JUNTmax = NA,
                        Jul.pdsi = NA)
  annuals.na2 <- data.frame(Year = 2015, 
                           PCP = NA,
                           TMIN = NA,
                           TAVG = NA, 
                           PDSI = NA,
                           MAY.p = NA,
                           JJA.p = NA,
                           JUNTmin = NA,
                           JUNTavg = NA, 
                           JUNTmax = NA,
                           Jul.pdsi = NA)
  #merge annuals with hickory
 annuals <- rbind(annuals.na, annuals, annuals.na2)
 annuals
}
df.list$annualclim <- merge.clim.list(IL.clim)
plot(df.list$annualclim$TMIN, df.list$rwi$HIC397)


#want to plot all 
molten.full <- rbind(molten.HIC, molten.BON, molten.STC, #molten.PLE,
                     molten.TOW, molten.ENG, molten.UNC, molten.COR, molten.GLA, molten.MOU)




#molten.full comes from climate_growth_reg_chron.R
###################################################
#compare climate corelations 
plot.cor.clim <- function(x, Clim, xlab, Site){
  # x <- x[x$Site %in% site,]
  yr <- 1895:1950
  x$class <- '9999'
  x[x$Year %in% yr,]$class <- 'clim_record'
  
  
  #if the dummy variable is significant, then the two slopes are different
  print(summary( cor( x[,Clim], x$value)))
  
  # Extend the regression lines beyond the domain of the data
  df <- data.frame(value <-  x$value, 
                   Climate <- x[,Clim], 
                   Year <- x$Year)
  colnames(df)<- c("value", "Climate", "Year")
  print(summary(lm(value ~ Climate, data = df)))
  ggplot(df, aes(x=Climate, y= value)) + geom_point(shape=1) +
    # scale_colour_hue(l=50) +
    geom_smooth(method = 'lm') + #,   # Add linear regression lines
    #se=TRUE,    # add shaded confidence region
    #fullrange=FALSE)+# Extend regression lines
    
    #scale_color_manual(values=c('Pre-1950'="red",'Post-1950'="blue"))+
    xlim(-8, 8)+
    ylim(0.5, 1.5) +
    theme_bw()+
    theme(text = element_text(size = 30))+
    ylab('Detrended Ring width Index') +
    xlab( xlab ) +
    ggtitle(Site)
  
}

molten <- read.csv("./outputs/full_molten_chron.csv")
plot.cor.clim(x = molten[molten$Site %in% "Bonanza",], Clim = 'PDSI', xlab = "PDSI", Site = "Bonanza Prairie")
plot.cor.clim(molten[molten$Site %in% "Hickory",], "PDSI", "PDSI", "Hickory Grove")
plot.cor.clim(molten[molten$Site %in% "Coral",], "PDSI", "PDSI", "Coral Woods")
plot.cor.clim(molten[molten$Site %in% "Glacial",], "PDSI", "PDSI", "Glacial Park")
plot.cor.clim(molten[molten$Site %in% "StCroix",], "PDSI", "PDSI", "St. Croix Savanna")
plot.cor.clim(molten[molten$Site %in% "Townsend",], "PDSI", "PDSI", "Townsend Woods")
plot.cor.clim(molten[molten$Site %in% "Uncas",], "PDSI", "PDSI", "Uncas Dunes")
plot.cor.clim(molten[molten$Site %in% "Mound",], "PDSI", "PDSI", "Mound Prairie")
plot.cor.clim(molten[molten$Site %in% "Englund",], "PDSI", "PDSI", "Englund")
plot.cor.clim(molten[molten$Site %in% "GLL1",], "PDSI", "PDSI", "GLL1")
plot.cor.clim(molten[molten$Site %in% "GLL2",], "PDSI", "PDSI", "GLL2")
plot.cor.clim(molten[molten$Site %in% "GLL3",], "PDSI", "PDSI", "GLL3")
plot.cor.clim(molten[molten$Site %in% "GLL4",], "PDSI", "PDSI", "GLL4")
plot.cor.clim(molten[molten$Site %in% "PVC",], "PDSI", "PDSI", "PVC")
plot.cor.clim(molten[molten$Site %in% "Pleasant",], "PDSI", "PDSI", "Pleasant Valley")



#let's see if wyckoff and bower's findings of a decreased relationship between PDSI & growth are correct

# conduct f-test to see if the relationship pre-1950 is same as post 1950

yr <- 1895:1950
yr.post <- 1950:2014

#this function runs the stats and makes plots for pre-1950 vs. post-1950
# additionally plots are saved to outputs/correlations within the function
plot.pre.post <- function(x, Climate, xlab, Site){
  yr <- 1895:1950
  yr.post <- 1950:2014
  x$class <- '9999'
  x[x$Year %in% yr,]$class <- 'Pre-1950'
  x[x$Year %in% yr.post,]$class <- 'Post-1950'
  #create dummy variable
  x$group <- 0
  x[x$Year %in% yr,]$group <- 1
  
  #yr <- 1895:1950
  #x$grow <- '9999'
  #x[x$Year %in% yr,]$class <- 'clim_record'
  
  
  #if the dummy variable is significant, then the two slopes are different
  print(summary( cor( x[,Clim], x$value)))
  df <- data.frame(value <-  x$value, 
                   Climate <- x[,Clim], 
                   Year <- x$Year, 
                   class <- x$class)
  colnames(df)<- c("value", "Climate", "Year", "class")
  
  
  #if the dummy variable is significant, then the two slopes are different
  print(summary(aov(value ~ Climate * group, data = df)))
  #print(summary(lm(value ~ Climate:group, data = x)))
  #print(summary(aov(value~Climate*class, data=x)))
  print(anova(lm(value ~ Climate*group, data = df), lm(value ~ Climate, data = df))
  )#print(summary(lm(value~Climate/group-1, data=x)))
  #print(summary(aov(value~Climate/group, data = x)))
  # Extend the regression lines beyond the domain of the data
  
  p<- ggplot(df, aes(x=Climate, y=value, colour=class)) + geom_point(shape=1) +
    scale_colour_hue(l=50) +
    #+ylim(-1.0,1.0)
    #+xlim(-4,4)# Use a slightly darker palette than normal
    geom_smooth(method='lm',   # Add linear regression lines
                se=TRUE,    # add shaded confidence region
                fullrange=FALSE)+# Extend regression lines
    
    scale_color_manual(values=c('Pre-1950'="red",'Post-1950'="blue"))+
    xlim(-8, 8)+
    ylim(0.5, 1.5) +
    theme_bw()+
    theme(text = element_text(size = 10), plot.title = element_text(hjust = 0.5))+
    ylab('Detrended RWI') +
    xlab( xlab ) +
    ggtitle(Site)
  p
  #ggsave(filename = paste0('outputs/correlations/pre_post_jul_pdsi_',Site,".png"), plot = p, width = 10, height = 7 )
}
yr <- 1895:1950
yr.post <- 1950:2014
molten$class <- '9999'
molten[molten$Year %in% yr,]$class <- 'Pre-1950'
molten[molten$Year %in% yr.post,]$class <- 'Post-1950'
#create dummy variable
molten$group <- 0
molten[molten$Year %in% yr,]$group <- 1

sav<- aov(value ~ PDSI*class, data = molten[molten$ecotype %in% "Savanna",])

fores<- aov(value ~ PDSI*class, data = molten[molten$ecotype %in% "Forest",])

plot.pre.post(x = molten[molten$Site %in% "Hickory",], "PDSI", "PDSI", "Hickory Grove")#sig
plot.pre.post(x = molten[molten$Site %in% "Bonanza",], "PDSI", "PDSI", "Bonanaza")
plot.pre.post(x = molten[molten$Site %in% "StCroix",], "PDSI", "PDSI", "St. Croix Savanna")#sig
plot.pre.post(x = molten[molten$Site %in% "Townsend",], "PDSI", "PDSI", "Townsend")
plot.pre.post(x = molten[molten$Site %in% "Englund",], "PDSI", "PDSI", "Englund")
plot.pre.post(x = molten[molten$Site %in% "Uncas",], "PDSI", "PDSI", "Uncas")
plot.pre.post(x = molten[molten$Site %in% "Coral",], "PDSI", "PDSI", "Coral")#sig
plot.pre.post(x = molten[molten$Site %in% "Glacial",], "PDSI", "PDSI", "Glacial Park")#sig at 0.1
plot.pre.post(x = molten[molten$Site %in% "Mound",], "PDSI", "PDSI", "Mound Prairie")
plot.pre.post(x = molten[molten$Site %in% "GLL1",], "PDSI", "PDSI", "Glacial Lakes 1")#sig
plot.pre.post(x = molten[molten$Site %in% "GLL2",], "PDSI", "PDSI", "Glacial Lakes 2")#sig
plot.pre.post(x = molten[molten$Site %in% "GLL3",], "PDSI", "PDSI", "Glacial Lakes 3")
plot.pre.post(x = molten[molten$Site %in% "GLL4",], "PDSI", "PDSI", "Glacial Lakes 4")#sig
plot.pre.post(x = molten[molten$Site %in% "Pleasant",], "PDSI", "PDSI", "Pleasant")#sig
plot.pre.post(x = molten[molten$Site %in% "PVC",], "PDSI", "PDSI", "PVC")




e <- plot.pre.post(molten.HIC, molten.HIC$Jul.pdsi, 'July PDSI', "Hickory Grove, IL") #significant
a <- plot.pre.post(molten.BON, molten.BON$Jul.pdsi, 'July PDSI', "Bonanza Prairie, MN") #significant
d <- plot.pre.post(molten.PLE, molten.PLE$Jul.pdsi, 'July PDSI', "Pleasant Valley Conservancy, WI") #not significant (only sig @ 0.15 )
h <- plot.pre.post(molten.TOW, molten.TOW$Jul.pdsi, 'July PDSI', "Townsend Woods, MN") #not significant
c <- plot.pre.post(molten.STC, molten.STC$Jul.pdsi, 'July PDSI', "St.Croix Savanna, MN") #significant
f <- plot.pre.post(molten.GLA, molten.GLA$Jul.pdsi, 'July PDSI', "Glacial Park, IL") #significant
g <- plot.pre.post(molten.COR, molten.COR$Jul.pdsi, 'July PDSI', "Coral Woods, IL") #significant
b <- plot.pre.post(molten.UNC, molten.UNC$Jul.pdsi, 'July PDSI', "Uncas Dunes, MN") #significant
i <- plot.pre.post(molten.ENG, molten.ENG$Jul.pdsi, 'July PDSI', "Englund Ecotone, MN") #significant
j <- plot.pre.post(molten.MOU, molten.MOU$Jul.pdsi, 'July PDSI', "Mound Prairie, MN") #significant

source("R/grid_arrange_shared_legend.R")
png(width = 6, height = 9, units = 'in', res = 300, 'outputs/correlations/all_site_pre_post_fig3-ModNegExp.png')
grid_arrange_shared_legend(a,b,c,d,e,f,g,h,i,j,nrow = 5, ncol = 2 )
dev.off()

# plot the 
#png(width = 6, height = 9, units = 'in', res = 300, 'outputs/correlations/all_site_pre_post_fig3.png')
#grid_arrange_shared_legend(a,b,c,d,e,f,g,h,i,j,nrow = 5, ncol = 2 )
#dev.off()

# plot out sites with slope change:
# St Croix savanna (c),
png(width = 6, height = 5, units = 'in', res = 300, 'outputs/correlations/slopechange_site_pre_post_fig3-ModNegExp.png')
grid_arrange_shared_legend(c,d,nrow = 2, ncol = 2 )
dev.off()

# plot out sites with intercept change:
# hickory grove (e), bonanza prairie (a), 
# pleasant valley, glacial park, coral woods

png(width = 6, height = 5, units = 'in', res = 300, 'outputs/correlations/interceptchange_site_pre_post_fig3-ModNegExp.png')
grid_arrange_shared_legend(a,e,f,g,nrow = 2, ncol = 2 )
dev.off()

# plot out sites with no changes:
# mound prairie (j), Townsend woods (h), 
#englund ecotone(i), Uncas dunes (b)
png(width = 6, height = 5, units = 'in', res = 300, 'outputs/correlations/nochange_site_pre_post_fig3-ModNegExp.png')
grid_arrange_shared_legend(j,b,h,i,nrow = 2, ncol = 2 )
dev.off()



plot.pre.post(molten.HIC, molten.HIC$JULTavg, 'July Average Temperature (DegF)', "Hickory Grove, IL") #significant




pdf("outputs/pre_post_1950_plots.pdf")
plot.pre.post(molten.HIC, molten.HIC$PDSI, 'PDSI', "Hickory Grove, IL") #significant
plot.pre.post(molten.HIC, molten.HIC$PCP, "Annual Precipitation", "Hickory Grove, IL") #sig
plot.pre.post(molten.HIC, molten.HIC$TMIN/10, "Mean Minimum Temperature", "Hickory Grove, IL") #sig
plot.pre.post(molten.HIC, molten.HIC$TAVG/10, "Mean Temperature", "Hickory Grove, IL") #sig
plot.pre.post(molten.BON, molten.BON$PDSI, "PDSI", "Bonanza Prairie, MN") #sig *
plot.pre.post(molten.BON, molten.BON$PCP, "Annual Precipitation", "Bonanza Prairie, MN") #sig 
plot.pre.post(molten.BON, molten.BON$TMIN, "Mean Minimum Temperature", "Bonanza Prairie, MN") #sig
plot.pre.post(molten.BON, molten.BON$TAVG, "Mean Temperature", "Bonanza Prairie, MN") #***
plot.pre.post(molten.PLE, molten.PLE$PDSI, "PDSI", "Pleasant Valley Conservancy, WI") #**
plot.pre.post(molten.PLE, molten.PLE$PCP, "Annual Precipitation", "Pleasant Valley Conservancy, WI") #***
plot.pre.post(molten.PLE, molten.PLE$TMIN,"Mean Minimum", "Pleasant Valley Conservancy, WI")#***
plot.pre.post(molten.PLE, molten.PLE$TAVG,"Mean Temperature","Pleasant Valley Conservancy, WI")#***
plot.pre.post(molten.TOW, molten.TOW$PDSI, "PDSI", "Townsend Woods, MN") #**
plot.pre.post(molten.TOW, molten.TOW$PCP, "Annual Precipitaiton", "Townsend Woods, MN") #not signficant
plot.pre.post(molten.TOW, molten.TOW$TMIN, "Minimum Temperature", "Townsend Woods, MN") #not significant
plot.pre.post(molten.TOW, molten.TOW$TAVG, "Average Temperature" ,"Townsend Woods, MN") #not significant
dev.off()



#################################################
# Linear regressions by group with bootstrapping#
#################################################
library(boot)

# are the residuals increasing over time?
test <- lm(molten.HIC$value~molten.HIC$PDSI)
plot(molten.HIC$Year, test$residuals, type = "l")
summary(lm(test$residuals ~ molten.HIC$Year))

print(summary(aov(data$value~data$PDSI*data$class)))

growth_reg = function(data, indices){
  climate <- "Jul.pdsi"
  yr <- 1895:1950
  yr.post <- 1950:2014
  data$class <- '9999'
  data[data$Year %in% yr,]$class <- 'Pre-1950'
  data[data$Year %in% yr.post,]$class <- 'Post-1950'
  #create dummy variable
  data$group <- 0
  data[data$Year %in% yr,]$group <- 1
  d = data[indices, ]
  H_relationship = lm(d$value~d[,c(climate)], data = d)
  H_r_sq = coefficients(H_relationship)
  #H_p = summary(H_relationship)$coefficients[,4]
  G_relationship = lm(d$value~d[,c(climate)]*d$group, data = d)
  G_r_sq = coefficients(G_relationship)
  #G_p = summary(G_relationship)$coefficients[,4]
  relationships = c(H_r_sq, #H_p, 
                    G_r_sq #, G_p
  )
  return(relationships)
}

# bootstrapping
results = boot(data = molten.HIC, statistic = growth_reg, R = 5000)
print (results) # view bootstrapped coefficients


plot(results, index = 1) # plot boot strapped intercept
plot(results, index = 2) # beta reg. coefficient
plot(results, index = 3) # boot strapped intercept with grouping
plot(results, index = 4) # beta reg coefficient
plot(results, index = 5) # class 1 reg coefficient
plot(results, index = 6) # class 2 reg coefficient

# still not sure which class is which in this

# the idea is to compare distributions of slope to 0 (if CI doesnt include 0, it should be different from 0)
# also compare the two slope distributions to determine if they are statistically different from each other (ftest/anova)



confidence_interval_H = boot.ci(results, index = 2, conf = 0.95, type = 'bca')
print(confidence_interval_H)
ci_H = confidence_interval_H$bca[ , c(4, 5)]
print(ci_H) # if ciH doesnt contain 0, then the reg coefficient is >0 significant

hist(results$t[,1], main = 'intercept', xlab = 'int',
     col = 'grey')
hist(results$t[,2], main = 'Beta: PDSI', xlab = 'beta',
     col = 'grey', prob = T)
lines(density(results$t[,2]), col = 'blue')
abline(v = ci_H, col = 'red')

#####################################
#plot correlations against soil type#
#####################################'

#read in soil rasters from gssurgo data
library(raster)
ksat <- raster('C:/Users/JMac/Box Sync/GSSURGOtifs/8km_UMW_ksat1.tif')
ksat.alb <- projectRaster(ksat, crs='+init=epsg:3175')

awc <- raster('C:/Users/JMac/Box Sync/GSSURGOtifs/8km_UMW_awc1.tif')
awc.alb <- projectRaster(awc, crs = '+init=epsg:3175')


priority <- readOGR('data/Treecores.kml', layer = "NAPCsites")
priority <- spTransform(priority, CRSobj = CRS('+init=epsg:3175'))
priority <- data.frame(priority)
priority$code <- c("PVC", "STC", "TOW", "HIC", "BON")
priority$Names <- c('Pleasant Valley', 'St. Croix Savanna',"Townsend Woods", "Hickory Grove", "Bonanza Prairie")
places <- c('St. Croix Savanna',"Townsend Woods", "Hickory Grove", "Bonanza Prairie")



priority$ksat <- extract(ksat.alb, priority[,c("coords.x1", "coords.x2")])
priority$awc <- extract(awc.alb, priority[,c("coords.x1", "coords.x2")])

BON.pdsi <- read.csv("BON-WWPDSIcor.csv")
HIC.pdsi <- read.csv("HIC-WWPDSIcor.csv")
STC.pdsi <- read.csv("STC-WWPDSIcor.csv")
TOW.pdsi <- read.csv("TOW-WWPDSIcor.csv")
PLE.pdsi <- read.csv("PLE-WWPDSIcor.csv")



priority$pdsiJul <- 0
priority[priority$code %in% "BON", ]$pdsiJul <- BON.pdsi[19,2]
priority[priority$code %in% "HIC", ]$pdsiJul <- HIC.pdsi[19,2]
priority[priority$code %in% "STC", ]$pdsiJul <- STC.pdsi[19,2]
priority[priority$code %in% "TOW", ]$pdsiJul <- TOW.pdsi[19,2]
priority[priority$code %in% "PVC", ]$pdsiJul <- PLE.pdsi[19,2]


plot(priority$ksat, priority$pdsiJul)
plot(priority$awc, priority$pdsiJul)


#dataset from http://scrippsco2.ucsd.edu/data/atmospheric_co2
#CO2 <- read.csv('data/merged_ice_core_yearly.csv')

CO2 <- read.csv('data/spline_merged_ice_core_yearly2.csv', header = TRUE)

#colnames(CO2) <- c('YearCE', 'ppm', 'Year')

compare.CO2<- function(CO2, x){
  yr <- 1895:1950
  yr.post <- 1950:2014
  x$class <- '9999'
  x[x$Year %in% yr,]$class <- 'Pre-1950'
  x[x$Year %in% yr.post,]$class <- 'Post-1950'
  #create dummy variable
  x$group <- 0
  x[x$Year %in% yr,]$group <- 1
  
  CO2.m <- merge(x, CO2, by = 'Year')
  print(summary( lm(value ~ ppm, data = CO2.m)))
  
  
  # Extend the regression lines beyond the domain of the data
  ggplot(CO2.m, aes(x=ppm, y=value)) + geom_point(shape=1) +
    scale_colour_hue(l=50) + # Use a slightly darker palette than normal
    geom_smooth(method=loess,   # Add linear regression lines
                se=TRUE,    # add shaded confidence region
                fullrange=FALSE)+# Extend regression lines
    ylab('Detrended Ring width Index') +
    xlab( 'ppm') +
    ggtitle('CO2 vs growth')
}

pdf('outputs/CO2_growth_splines.pdf')
compare.CO2(CO2, molten.BON)
compare.CO2(CO2, molten.HIC)
compare.CO2(CO2, molten.TOW)
compare.CO2(CO2, molten.PLE)
compare.CO2(CO2, molten.STC)
compare.CO2(CO2, molten.SAN)
compare.CO2(CO2, molten.DES)
compare.CO2(CO2, molten.COR)
compare.CO2(CO2, molten.ENG)
compare.CO2(CO2, molten.MOU)
compare.CO2(CO2, molten.GLA)
dev.off()


compare.CO2.PDSI<- function(CO2, x){
  CO2.m <- merge(x, CO2, by = 'Year')
  plot(CO2.m$PDSI, CO2.m$ppm)
}



compare.CO2.PDSI(CO2, molten.BON)
compare.CO2.PDSI(CO2, molten.HIC)
compare.CO2.PDSI(CO2, molten.TOW)
compare.CO2.PDSI(CO2, molten.PLE)
compare.CO2.PDSI(CO2, molten.STC)

#plot theses
plot(molten.HIC[molten.HIC$Year== 1895:1950,]$PDSI, molten.HIC[molten.HIC$Year== 1895:1950,]$value)
abline(preHIClm)
points(molten.HIC[molten.HIC$Year== 1951:2014,]$PDSI, molten.HIC[molten.HIC$Year== 1951:2014,]$value, col = 'red')
abline(postHIClm, col = 'red')

var.test( lm(value ~ PDSI, data = molten.BON[molten.BON$Year %in% yr,]),
          lm(value ~ PDSI, data = molten.BON[molten.BON$Year %in% yr.post,]))
#F = 1.5127, num df = 222, denom df = 190, p-value = 0.003411

var.test( lm(value ~ PDSI, data = molten.PLE[molten.PLE$Year %in% yr,]),
          lm(value ~ PDSI, data = molten.PLE[molten.PLE$Year %in% yr.post,]))
# F = 0.82271, num df = 166, denom df = 126, p-value = 0.2389

var.test( lm(value ~ PDSI, data = molten.TOW[molten.TOW$Year %in% yr,]),
          lm(value ~ PDSI, data = molten.TOW[molten.TOW$Year %in% yr,]))

# F = 1.7999, num df = 173, denom df = 190, p-value = 8.101e-05






