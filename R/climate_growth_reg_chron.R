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
merge.clim.chron <- function(MNcd.clim, chron){
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

#merge annuals with rwl
annuals.crn <- merge(annuals, chron[c('Year', 'xxxstd', 'Site')], by = "Year")
melt(annuals.crn, id = c('Year','Site', 'PCP', "TMIN", "TAVG", "PDSI","MAY.p","JJA.p", 
                                       "JUNTmin","JUNTavg", 'JUNTmax',"Jul.pdsi"))
}

#create molten dataframes with climate and rwi chronologies
molten.HIC <- merge.clim.chron(IL.clim, Hickory)
molten.COR <- merge.clim.chron(IL.clim, Coral)
molten.GLA <- merge.clim.chron(IL.clim, Glacial)
molten.SAN <- merge.clim.chron(IL.clim, Sandwich)
molten.PLP <- merge.clim.chron(WIse.clim, PleasantPrairie)
molten.BON <- merge.clim.chron(MNwc.clim, Bonanza)
molten.DES <- merge.clim.chron(MNwc.clim, Desoix)
molten.PLE <- merge.clim.chron(WIsc.clim, Pleasant)
molten.TOW <- merge.clim.chron(MNec.clim, Townsend)
molten.STC <- merge.clim.chron(MNec.clim, StCroix)
molten.UNC <- merge.clim.chron(MNec.clim, Uncas)
molten.ENG <- merge.clim.chron(MNec.clim, Englund)
molten.MOU <- merge.clim.chron(MNse.clim, Mound)
molten.GL1 <- merge.clim.chron(MNwc.clim, Glaciallk1)
molten.GL2 <- merge.clim.chron(MNwc.clim, Glaciallk2)
molten.GL3 <- merge.clim.chron(MNwc.clim, Glaciallk3)
molten.GL4 <- merge.clim.chron(MNwc.clim, Glaciallk4)
molten.UNC <- merge.clim.chron(MNec.clim, Uncas)
molten.PVC <- merge.clim.chron(IL.clim, PVC)
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

