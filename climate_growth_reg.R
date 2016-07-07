library(lme4)
library(dplR)
#read in rwl & add site + year codes
Hickory <- read.tucson ("./cofecha/HICww.rwl")
HIC.stats <- rwi.stats(Hickory)
#detrend 
Hickory.rwi <- detrend(rwl = Hickory, method = "ModNegExp")
Hickory <- chron(Hickory.rwi)            


Hickory$Year <- 1850:2015
Hickory$Site <- 'Hickory Grove'



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
Pleasant$Year <- 1768:2015
Pleasant$Site <- "Pleasant Valley Conservancy"

Townsend <- read.tucson('./cofecha/tow/TOWww.rwl')
TOW.stats <- rwi.stats(Townsend)
#detrend
Townsend.rwi <- detrend(rwl = Townsend, method = "ModNegExp")
Townsend <- chron(Townsend.rwi)
Townsend$Year <- 1880: 2015
Townsend$Site <- 'Townsend Woods'

#read in hickory grove climate
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



###########################
#read in Bonanza climate
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

#merge annuals with hickory
annuals.BON <- merge(annuals, Bonanza[c('Year', 'xxxstd', 'Site')], by = 'Year')
molten.BON <- melt(annuals.BON, id = c('Year','Site', 'PCP', "TMIN", "TAVG", "PDSI"))


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

