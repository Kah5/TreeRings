library(lme4)
library(dplR)
#####################################
#read in rwl & add site + year codes#
#####################################

#Hickory Grove
Hickory <- read.tucson ("./cofecha/HICww.rwl")
HIC.stats <- rwi.stats(Hickory)



#detrend 
Hickory.rwi <- detrend(rwl = Hickory, method = "ModNegExp")
Hickory.rwi$year <- rownames(Hickory.rwi)
# calculate record age

treedata <- data.frame(ID = colnames(Hickory.rwi),
                       sampleyr = 2015)


Hic <- data.frame(Hickory.rwi)

# Find tree age for each tree at the time of sampling
for(i in unique(colnames(Hic))){
  treedata[treedata$ID==i, "age"] <- treedata[treedata$ID == i, "sampleyr"] - as.numeric( min(Hic[!is.na(Hic[,i]), "year"], na.rm=T))
}
summary(treedata)  

treeages <- matrix(NA, nrow(Hic),
                       )
Hic <- t(Hic)
Hic.age <- data.frame(Hic)

# code for calculating tree age in each year

for(i in unique(colnames(Hic))){
  
firstyr <- as.numeric( min(Hic[!is.na(Hic[,i]), "year"], na.rm=T))
#Hic.age[Hic.age$year == firstyr,i] <- 1
yridx <- firstyr:2015
for (yr in firstyr:2015){
Hic.age[as.numeric(Hic.age$year)== yr,i] <- yr-firstyr
}
}

Hic.age$year <- rownames(Hic)

# plot rwi vs. tree age:
Age.m <- melt(Hic.age)
colnames(Age.m) <- c("year", "ID", "Age")

RWI.m <- melt(Hic)
colnames(RWI.m) <- c("year", "ID", "RWI")

site.m <- merge(Age.m, RWI.m, by = c('year', "ID"))

ggplot(site.m, aes(x = Age, y = RWI, color = ID))+geom_line()




# find the # of young and old trees in a record

Hic.young <- Hickory.rwi[,is.na(Hickory.rwi[Hickory.rwi$year == "1890",])]

Hickory <- chron(Hickory.rwi)            


Hickory$Year <- 1850:2015
Hickory$Site <- 'Hickory Grove'

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




#Bonanza prairie
Bonanza <- read.tucson("./cofecha/BONww.rwl")
BON.stats <- rwi.stats(Bonanza)
#detrend
Bonanza.rwi <- detrend(rwl = Bonanza, method = "ModNegExp")
Bonanza <- chron(Bonanza.rwi)

Bonanza$Year <- 1818:2015
Bonanza$Site <- "Bonanza Prairie"

#Pleasant valley Conservancy record
Pleasant <- read.tucson('./cofecha/PLEww.rwl')
PLE.stats <- rwi.stats(Pleasant)
#detrend
Pleasant.rwi <- detrend(rwl = Pleasant, method = "ModNegExp")
Pleasant <- chron (Pleasant.rwi)
Pleasant$Year <- 1768:2015
Pleasant$Site <- "Pleasant Valley Conservancy"


#townsend woods
Townsend <- read.tucson('./cofecha/tow/TOWww.rwl')
TOW.stats <- rwi.stats(Townsend)
#detrend
Townsend.rwi <- detrend(rwl = Townsend, method = "ModNegExp")
Townsend <- chron(Townsend.rwi)
Townsend$Year <- 1880: 2015
Townsend$Site <- 'Townsend Woods'


# wolfsfield woods ***note this is an Acer Saccharim record # Comparing townsend woods
Wolfsfield <- read.tucson('data/mn018.rwl')
WOL.stats <- rwi.stats(Wolfsfield)
#detrend
Wolfsfield.rwi <- detrend(rwl = Wolfsfield, method = "ModNegExp")
Wolfsfield <- chron(Wolfsfield.rwi)
Wolfsfield$Year <- 1820:1983
Wolfsfield$Site <- 'Wolfsfield Woods'


################################################
#Merge chronologies with local climate datasets#
################################################


#read in hickory grove climate (also climate for Sandwich site)
MNcd.clim <- read.csv("data/NE_illinois_climdiv.csv")
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

#pr.yr <- aggregate(PCP ~ Year , data=MNp.df, FUN=sum, na.rm = T) 
#plot(pr.yr[1:120,1], pr.yr[1:120,2], type = "l", xlab = "Year", ylab = "Annual Precip (mm)")


#precip <- dcast(total.p, Year  ~ Month)
annual.p <- aggregate(PCP~Year, data = MNp.df[1:1440,], FUN = sum, na.rm=T)
annual.t <- aggregate(TAVG ~ Year, data = MNtavg.df[1:1440,], FUN = 'mean', na.rm=T)
annual.mint <- aggregate(TMIN ~Year, data = MNtmin.df[1:1440,], FUN = 'mean', na.rm = T)
annual.pdsi <- aggregate(PDSI ~ Year, data = MNpdsi.df[1:1440,], FUN = 'mean', na.rm = T)
annuals <- data.frame(Year = annual.p$Year, 
                      PCP = annual.p$PCP,
                      TMIN = annual.mint$TMIN,
                      TAVG = annual.t$TAVG,
                      PDSI = annual.pdsi$PDSI)

#merge annuals with hickory
annuals.HIC <- merge(annuals, Hickory[c('Year', 'xxxstd', 'Site')], by = "Year")
molten.HIC <- melt(annuals.HIC, id = c('Year','Site', 'PCP', "TMIN", "TAVG", "PDSI"))

annuals.SAN <- merge(annuals, Sandwich[c('Year', "xxxstd", 'Site')], by = "Year")
molten.SAN <- melt(annuals.SAN, id = c('Year','Site', 'PCP', "TMIN", "TAVG", "PDSI"))

###########################
#read in Bonanza climate (also climate for desoix)
MNcd.clim <- read.csv("data/West_central_MN_nclimdiv.csv")
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

#pr.yr <- aggregate(PCP ~ Year , data=MNp.df, FUN=sum, na.rm = T) 
#plot(pr.yr[1:120,1], pr.yr[1:120,2], type = "l", xlab = "Year", ylab = "Annual Precip (mm)")


#precip <- dcast(total.p, Year  ~ Month)
annual.p <- aggregate(PCP~Year, data = MNp.df[1:1440,], FUN = sum, na.rm=T)
annual.t <- aggregate(TAVG ~ Year, data = MNtavg.df[1:1440,], FUN = 'mean', na.rm=T)
annual.mint <- aggregate(TMIN ~Year, data = MNtmin.df[1:1440,], FUN = 'mean', na.rm = T)
annual.pdsi <- aggregate(PDSI ~ Year, data = MNpdsi.df[1:1440,], FUN = 'mean', na.rm = T)
annuals <- data.frame(Year = annual.p$Year, 
                      PCP = annual.p$PCP,
                      TMIN = annual.mint$TMIN,
                      TAVG = annual.t$TAVG,
                      PDSI = annual.pdsi$PDSI)

#merge annuals with Bonanza
annuals.BON <- merge(annuals, Bonanza[c('Year', 'xxxstd', 'Site')], by = 'Year')
molten.BON <- melt(annuals.BON, id = c('Year','Site', 'PCP', "TMIN", "TAVG", "PDSI"))

annuals.DES <- merge(annuals, Desoix[c('Year', "xxxstd", 'Site')], by = "Year")
molten.DES <- melt(annuals.DES, id = c('Year','Site', 'PCP', "TMIN", "TAVG", "PDSI"))

##############################
#read in Pleasant valley conservancy climate
################################
MNcd.clim <- read.csv("data/NE_illinois_climdiv.csv")
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

#pr.yr <- aggregate(PCP ~ Year , data=MNp.df, FUN=sum, na.rm = T) 
#plot(pr.yr[1:120,1], pr.yr[1:120,2], type = "l", xlab = "Year", ylab = "Annual Precip (mm)")


#precip <- dcast(total.p, Year  ~ Month)
annual.p <- aggregate(PCP~Year, data = MNp.df[1:1440,], FUN = sum, na.rm=T)
annual.t <- aggregate(TAVG ~ Year, data = MNtavg.df[1:1440,], FUN = 'mean', na.rm=T)
annual.pdsi <- aggregate(PDSI ~ Year, data = MNpdsi.df[1:1440,], FUN = 'mean', na.rm = T)
annuals <- data.frame(Year = annual.p$Year, 
                      PCP = annual.p$PCP,
                      TMIN = annual.mint$TMIN,
                      TAVG = annual.t$TAVG,
                      PDSI = annual.pdsi$PDSI)
#merge annuals with hickory
annuals.PLE <- merge(annuals, Pleasant[c('Year', 'xxxstd', 'Site')], by = 'Year')
molten.PLE <- melt(annuals.PLE, id = c('Year','Site', 'PCP', "TMIN", "TAVG", "PDSI"))

#now merge all the molten frames together

molten.full <- rbind(molten.HIC, molten.BON, molten.PLE)



##############################
#read in Townsend climate
################################
MNcd.clim <- read.csv("data/CDODiv2154347072867.csv")
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

#pr.yr <- aggregate(PCP ~ Year , data=MNp.df, FUN=sum, na.rm = T) 
#plot(pr.yr[1:120,1], pr.yr[1:120,2], type = "l", xlab = "Year", ylab = "Annual Precip (mm)")


#precip <- dcast(total.p, Year  ~ Month)
annual.p <- aggregate(PCP~Year, data = MNp.df[1:1440,], FUN = sum, na.rm=T)
annual.t <- aggregate(TAVG ~ Year, data = MNtavg.df[1:1440,], FUN = 'mean', na.rm=T)
annual.pdsi <- aggregate(PDSI ~ Year, data = MNpdsi.df[1:1440,], FUN = 'mean', na.rm = T)
annuals <- data.frame(Year = annual.p$Year, 
                      PCP = annual.p$PCP,
                      TMIN = annual.mint$TMIN,
                      TAVG = annual.t$TAVG,
                      PDSI = annual.pdsi$PDSI)

#merge annuals with hickory
annuals.TOW <- merge(annuals, Townsend[c('Year', 'xxxstd', 'Site')], by = 'Year')
molten.TOW <- melt(annuals.TOW, id = c('Year','Site', 'PCP', "TMIN", "TAVG", "PDSI"))


#now merge all the molten frames together

molten.full <- rbind(molten.HIC, molten.BON, molten.PLE, molten.TOW)

