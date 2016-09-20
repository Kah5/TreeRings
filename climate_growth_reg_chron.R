library(lme4)
library(dplR)
library(reshape2)
#read in rwl & add site + year codes
Hickory <- read.tucson ("./cofecha/HICww.rwl")
plot(Hickory)
HIC.stats <- rwl.stats(Hickory)
#detrend 
Hickory.rwi <- detrend(rwl = Hickory, method = "Spline")

Hickory <- chron(Hickory.rwi)            
plot(Hickory)

Hickory$Year <- 1850:2015
Hickory$Site <- 'Hickory Grove'

#Pleasant prairie site from IRTDB--not to be confused with KH collected from pleasant valley conservancy
PleasantPrairie <- read.tucson('data/wi006.rwl')
PLP.stats <- rwi.stats(PleasantPrairie)
plot(chron(PleasantPrairie))
#detrend 
PleasantPrairie.rwi <- detrend(rwl = PleasantPrairie, method = "Spline")
PleasantPrairie <- chron(PleasantPrairie.rwi)            
plot(PleasantPrairie)

PleasantPrairie$Year <- 1807:2000
PleasantPrairie$Site <- 'Pleasant Prairie, WI'

#Sandwich--south of Hickory grove *ed cook record from 1980's
#note there are two records--one for Quercus macrocarpa (il002) and one for Quercus alba (il001)
#here we use il002
Sandwich <- read.tucson ("data/il002.rwl")
SAN.stats <- rwi.stats(Sandwich)
#detrend 
Sandwich.rwi <- detrend(rwl = Sandwich, method = "ModNegExp")
Sandwich <- chron(Sandwich.rwi)            


Sandwich$Year <- 1752:1980
Sandwich$Site <- 'Sandwich'


#Dubois de Souix record--north of bonanaza
Desoix <- read.tucson ("data/mn029.rwl")
DES.stats <- rwi.stats(Desoix)
#detrend 
Desoix.rwi <- detrend(rwl = Desoix, method = "ModNegExp")
Desoix <- chron(Desoix.rwi)            


Desoix$Year <- 1877:2010
Desoix$Site <- 'Bois de soix'


#Yellow River record--iowa
Yellow <- read.tucson ("data/ia029.rwl")
Yellow.stats <- rwi.stats(Yellow)
plot(chron(Yellow))
#detrend 
Yellow.rwi <- detrend(rwl = Yellow, method = "Spline")
Yellow <- chron(Yellow.rwi)            
plot(Yellow)

Yellow$Year <- 1650:1980
Yellow$Site <- 'Yellow River, IA'



Bonanza <- read.tucson("./cofecha/BONww.rwl")
BON.stats <- rwi.stats(Bonanza)
#detrend
Bonanza.rwi <- detrend(rwl = Bonanza, method = "ModNegExp")
Bonanza <- chron(Bonanza.rwi)

Bonanza$Year <- 1818:2015
Bonanza$Site <- "Bonanza Prairie"


Pleasant <- read.tucson('./cofecha/PLEww.rwl')
PLE.stats <- rwi.stats(Pleasant)
#detrend
Pleasant.rwi <- detrend(rwl = Pleasant, method = "ModNegExp")
Pleasant <- chron (Pleasant.rwi)

#sample figures for NAPC conference
pdf('outputs/chron_NAPC_fig.pdf')
plot(Pleasant$Year, Pleasant$xxxstd, type = 'l', col = 'black', xlab = 'Year',ylab = "Detrended Ring Width Index", cex = 5)
abline(a= 1, b = 0)

plot(Pleasant$Year, Pleasant$xxxstd, type = 'l', col = 'black', xlab = 'Year',ylab = "Detrended Ring Width Index", cex = 5)
abline(a= 1, b = 0)
rect(1895,0, 1950,3, col = rgb(0.3,0.5,0.5,1/4))
rect(1950,0, 2014,3, col = rgb(0.9,0.1,0.1,1/4))
dev.off()

Pleasant$Year <- 1768:2015
Pleasant$Site <- "Pleasant Valley Conservancy"


Townsend <- read.tucson('./cofecha/tow/TOWww.rwl')
TOW.stats <- rwi.stats(Townsend)
#detrend
Townsend.rwi <- detrend(rwl = Townsend, method = "ModNegExp")
Townsend <- chron(Townsend.rwi)
Townsend$Year <- 1880: 2015
Townsend$Site <- 'Townsend Woods'

StCroix <- read.tucson('./cofecha/STCww.rwl')
STC.stats <- rwi.stats(StCroix)
#detrend
STC.rwi <- detrend(rwl = StCroix, method = "ModNegExp")
StCroix <- chron(STC.rwi)
StCroix$Year <- 1879: 2015
StCroix$Site <- 'St. Croix Savanna'

#read climate
IL.clim <- read.csv("data/NE_illinois_climdiv.csv") #Hickory Grove, Sandwich, Glacial park
WIse.clim <- read.csv("data/south_east_wi_climdiv.csv") #pleasant prairie 
MNwc.clim <- read.csv("data/West_central_MN_nclimdiv.csv") #Bonanza praire, Duboix
WIsc.clim <- read.csv("data/south_central_WI_climdiv.csv") #pleasant valley conservancy
MNse.clim <- read.csv("data/CDODiv2154347072867.csv") #townsend woods 
MNse.clim <- read.csv("data/CDODiv2154347072867.csv") #St. Croix savanna

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

#merge annuals with hickory
annuals.crn <- merge(annuals, chron[c('Year', 'xxxstd', 'Site')], by = "Year")
melt(annuals.crn, id = c('Year','Site', 'PCP', "TMIN", "TAVG", "PDSI","MAY.p","JJA.p", 
                                       "JUNTmin","JUNTavg", 'JUNTmax',"Jul.pdsi"))
}

#create molten dataframes with climate and rwi chronologies
molten.HIC <- merge.clim.chron(IL.clim, Hickory)
molten.SAN <- merge.clim.chron(IL.clim, Sandwich)
molten.PLP <- merge.clim.chron(WIse.clim, PleasantPrairie)
molten.BON <- merge.clim.chron(MNwc.clim, Bonanza)
molten.DES <- merge.clim.chron(MNwc.clim, Desoix)
molten.PLE <- merge.clim.chron(WIsc.clim, Pleasant)
molten.TOW <- merge.clim.chron(MNse.clim, Townsend)
molten.STC <- merge.clim.chron(MNse.clim, StCroix)

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
clim.cor(MNse.clim, Townsend, 'Townsend_woods_')
clim.cor(MNse.clim, StCroix, 'StCroix_savanna_')



###############################################################
#looking at age related correlation trends
###############################################################
df.list<- list(rwi= as.list(Hickory.rwi), age = HIC.stats$year, record = colnames(Hickory.rwi), year = rownames(Hickory.rwi))
ages <- t(HIC.stats$year)

#repeat to have the same number as in the Hickory.rwi
ages <- ages[rep(seq_len(nrow(ages)), each=nrow(Hickory)),]

df.list$means <- lapply(df.list$rwi, mean, na.rm=TRUE)
plot(df.list$age,df.list$means)

#need to calculate tree age in each year, but this isnt righ 

df.list$yearssince2015 <- 2015-as.numeric(df.list$year)

#to find age for the for a given tree in df.list
calcyears<- function(x){
testage <- x-rev(df.list$yearssince2015)
testage[testage < 0] <- NA
rev(testage)
}



treeages <- apply(ages, FUN = calcyears, MARGIN = 2 )
pre1890 <- complete.cases(treeages['1890',]) # finds columns that are NA's in 1890
indexpre1890 <- colnames(treeages[,pre1890])

colnames(treeages) <- colnames(Hickory)
rownames(treeages) <- rownames(Hickory)
ages.melt <- melt(treeages)
colnames(ages.melt)<- c('year', 'variable', 'age')
ages.melt$class <- ages.melt$variable %in% indexpre1890

Hickory$year <- rownames(Hickory)
rwi.melt <- melt(Hickory)
colnames(rwi.melt) <- c('year', 'variable','rwi')
  
df.new <- merge(rwi.melt, ages.melt, by = c('year', 'variable'))

summary(dcast(df.new, rwi~., mean,var.value = age, na.rm=TRUE))

mean.rwi.age <- aggregate(rwi ~ age ,df.new, mean, na.rm = TRUE )
plot(mean.rwi.age, ylim = c(0, 4))
std <- aggregate(rwi ~ age, df.new, sd, na.rm = TRUE)
arrows(mean.rwi.age$age, mean.rwi.age$rwi-std$rwi, mean.rwi.age$age, mean.rwi.age$rwi+std$rwi, length=0.05, angle=90, code=3)

ggplot(data = mean.rwi.age, aes(x = age, y = rwi)) + geom_point()

mean.rwi.class <- aggregate(rwi ~ age + class ,df.new, mean, na.rm = TRUE )
mean.rwi.class[mean.rwi.class$class == FALSE,]$class <- 'post1890'
mean.rwi.class[mean.rwi.class$class == TRUE,]$class <- 'pre1890'

ggplot(data=mean.rwi.class, aes(x=age, y=rwi, colour=class)) + geom_point()












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
                     molten.TOW)

