#Correlating ring widths to climate factors across sites
# Author: Kelly Heilman

# note: need to update the rwl sources to the files with corrected headers
rm(list=ls())

library(dplR)
library(reshape2)
library(ggplot2)
library(plyr)

wood <- "WW"
#read in whole ring width file for site
#Bonanza <L- read.tucson("./cofecha/BON_out/BONall.rwl", header = T)
#for EW
if(wood == "EW"){
  Bonanza <- read.tucson("./cofecha/BONew.rwl", header = T)
  Hickory <- read.tucson("./cofecha/HICew.rwl", header = F)
  #Glacial <- read.tucson("./cofecha/GLAew.rwl")
  Townsend <- read.tucson('./cofecha/tow/TOWew.rwl', header = T)
  Pleasant <- read.tucson('./cofecha/PLEew.rwl', header = T)
}else{if(wood == "LW"){
  Bonanza <- read.tucson("./cofecha/BONlw.rwl")
  Hickory <- read.tucson("./cofecha/HIClw.rwl", header = F)
  #Glacial <- read.tucson("./cofecha/GLAlw.rwl")
  Townsend <- read.tucson('./cofecha/tow/TOWlw.rwl', header = T)
  Pleasant <- read.tucson('./cofecha/PLElw.rwl', header = T)
}else{
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
  Coral <- read.tucson('C:/Users/JMac/Documents/Kelly/crossdating/data/cofecha/COR.rwl')
  Uncas <- read.tucson("C:/Users/JMac/Documents/Kelly/crossdating/data/cofecha/UNC.rwl")
  Glacial <- read.tucson("C:/Users/JMac/Documents/Kelly/crossdating/data/cofecha/GLA.rwl")
  Englund <- read.tucson("C:/Users/JMac/Documents/Kelly/crossdating/data/cofecha/ENG.rwl")
  Mound <- read.tucson("C:/Users/JMac/Documents/Kelly/crossdating/data/cofecha/MOU.rwl")
  GLL1 <- read.tucson("cleanrwl/GLL1ww.rwl")
  GLL2 <- read.tucson("cleanrwl/GLL1ww.rwl")
  GLL3 <- read.tucson("cleanrwl/GLL2ww.rwl")
  GLL4 <- read.tucson("cleanrwl/GLL3ww.rwl")
  PVC <- read.tucson("cleanrwl/GLL4ww.rwl")
  }}

#change site --need to run this script for each site. It will output correlation coeffeiencts and save them in csv
site <- PVC
site.code <- "PVC"


##################################################
#################################################
################################################
################################################

stats.rwl <- rwl.stats(site)
write.csv(stats.rwl, paste0("data/site_stats/",site.code, "-site_stats.csv"))

site.code.rwi <- detrend(rwl = site, method = "Spline")
plot(site)
#create chronology of sites
site.code.crn <- chron(site.code.rwi, prefix = paste(site.code))
#write chronology to text 
crn.trend <- chron(site, prefix= paste(site.code), prewhiten = TRUE)
crn.prewhiten <- chron(site,prefix= paste(site.code), prewhiten = TRUE ) #also has residuals

write.csv(site.code.crn, paste0(site.code, "-crn.csv"))
crn.plot(site.code.crn, add.spline = TRUE)
site.code.stats <- rwi.stats(site)

site.code.crn$Year <- rownames(site.code.crn)
site.code.crn$freq <- 12
monthlys<- site.code.crn[rep(rownames(site.code.crn),
                  site.code.crn$freq),]
write.csv(monthlys, paste0(site.code, "monthly-crn.csv"))






##############################################################################################
#now using climate division data from the respective climate division for each tree ring site#
##############################################################################################
# need to add climate for additional sites as needed*******

if(site.code %in% c("BON", "GL1", "GL2", "GL3", "GL4")){
MNcd.clim <- read.csv("data/West_central_MN_nclimdiv.csv")
} else{ if(site.code == "HIC" ){
  MNcd.clim <- read.csv("data/NE_illinois_climdiv.csv")
} else{ if(site.code %in% c("GLA", "PVC") ){
  MNcd.clim <- read.csv("data/NE_illinois_climdiv.csv")
} else{ if(site.code == "COR" ){
    MNcd.clim <- read.csv("data/NE_illinois_climdiv.csv")
} else{ if(site.code == "W-R" ){
  MNcd.clim <- read.csv("data/West_central_MN_nclimdiv.csv")
} else{ if(site.code == 'SAW'){
  MNcd.clim <- read.csv("data/NE_illinois_climdiv.csv")
}else{ if(site.code == "STC"){
  MNcd.clim <- read.csv("data/East_Central_MN_CDODiv5039587215503.csv")
}else{ if(site.code == "ENG"){
  MNcd.clim <- read.csv("data/Central_MN_CDO.csv")
}else{ if(site.code == "TOW"){
  MNcd.clim <- read.csv("data/South_central_MN_CDO.csv")
}else{ if(site.code == "MOU"){
  MNcd.clim <- read.csv("data/South_East_MN_CDO.csv")
}else{ if(site.code == "UNC"){
  MNcd.clim <- read.csv("data/East_Central_MN_CDODiv5039587215503.csv")
}else { if(site.code == 'PLE'){
  MNcd.clim <- read.csv('data/south_central_WI_climdiv.csv')
}else { if(site.code == 'YRF'){
  MNcd.clim <- read.csv('IA_nclim_div_northeast.csv')}
  #MNcd.clim <-read.csv('data/CDODiv2154347072867.csv')}
}
}
}
}
}
}
}
}
}
}
}
}

MNcd.clim$PCP <- MNcd.clim$PCP*25.54 # convert to mm
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
total.p

pr.yr <- aggregate(PCP ~ Year , data=MNp.df, FUN=sum, na.rm = T) 
plot(pr.yr[1:120,1], pr.yr[1:120,2], type = "l", xlab = "Year", ylab = "Annual Precip (mm)")


precip <- dcast(total.p, Year  ~ Month)

annual.p <- aggregate(PCP~Year, data = MNp.df[1:1440,], FUN = sum, na.rm=T)
annual.t <- aggregate(TAVG ~ Year, data = MNcd.clim[1:1440,], FUN = mean, na.rm=T)
annual.mint <- aggregate(TMIN ~Year, data = MNcd.clim[1:1440,], FUN = mean, na.rm = T)
annual.maxt <- aggregate(TMAX ~Year, data = MNcd.clim[1:1440,], FUN = mean, na.rm = T)
annual.PDSI <- aggregate(PDSI ~Year, data = MNcd.clim[1:1440,], FUN = mean, na.rm = T)

write.csv(annual.p, paste0(site.code, '-annualP.csv'))
write.csv(annual.t, paste0(site.code, '-annualtavg.csv'))
write.csv(annual.mint, paste0(site.code, '-annualtmin.csv'))
write.csv(annual.maxt, paste0(site.code, '-annualtmax.csv'))
write.csv(annual.PDSI, paste0(site.code, '-annualPDSI.csv'))

par(mfrow=c(2,1))
plot(annual.p, type = "l")
plot(annual.t, type = "l")
plot(annual.mint, type = "l")
tmin.lm <- lm(annual.mint$TMIN ~ annual.mint$Year)
dev.off()
#create violin plot of monthly precip
ggplot(total.p, aes(x = factor(Month), y = PCP))+ geom_violin(fill = "orange") +
  geom_point( colour= "blue")

prmeans <- aggregate(PCP ~ Month, data = MNp.df, FUN=mean, na.rm = T) 
tmean <- aggregate(TAVG ~ Month, data = MNcd.clim, FUN = mean, na.rm = T)



#plot mean monthly precipitation & Temperature
pdf(paste0("outputs/",site.code, "mean.climate.pdf"))
plot(pr.yr[1:120,1], pr.yr[1:120,2], type = "l", xlab = "Year", ylab = "Annual Precip (mm)")

op <- par(mar=c(5, 4, 4, 6) + 0.1)
b.plot <- barplot(height = prmeans$PCP, names.arg = prmeans$Month,
                  xlab="Month", ylab="Mean Precipitation (mm)")
bar.x <- b.plot[prmeans$Month]

par(new = TRUE)
plot(x = bar.x, y = tmean$TAVG, xlab = "", ylab = "", pch = 16,
     ylim = c(0, 100),
     axes = FALSE, col = "red")
par(new = TRUE)
plot(x = bar.x, y = tmean$TAVG, xlab = "", ylab = "", type = "l",
     ylim = c(0, 100),
     axes = FALSE, col = "red")
axis(4, col = "red", col.axis = "red")
mtext("Temperature (degF)", side = 4, line=3, cex = par("cex.lab"), col = "red")
dev.off()

#plot annual precip




record.MN <- merge(precip, site.code.crn, by = "Year")

site.code.Pcors <- cor(record.MN[,2:13], record.MN[,14], use = "pairwise.complete.obs")
site.code.Pprev <- cor(record.MN[1:120,2:13], record.MN[2:121,14], use = "pairwise.complete.obs")
site.code.Pcors
site.code.Pprev

precip <- rbind(site.code.Pcors, site.code.Pprev)
write.csv(precip, paste0(site.code, "-", wood, "Precipcor.csv"))
#plot the correlations by month 
barplot(t(site.code.Pcors),ylim = c(-0.25, 0.5), main = paste(site.code,"Correlation with Precip."))
barplot(t(site.code.Pprev), ylim = c(-0.25, 0.5), main = paste(site.code, "Correlation with previous year Precip"))
##
#now with max T
mean.t <- aggregate(TMAX~Year + Month, data=MNt.df, FUN=sum, na.rm = T) 
mean.t

temp <- dcast(mean.t, Year  ~ Month)


#create violin plot of monthly Tmax
ggplot(mean.t, aes(x = factor(Month), y = TMAX))+ geom_violin(fill = "orange") +
  geom_point( colour= "blue")

site.code.crn$Year <- rownames(site.code.crn)


temp.MN <- merge(temp, site.code.crn, by = "Year")

#correlate the chronology with temperature
site.code.Tcors <- cor(temp.MN[,2:13], temp.MN[,14], use = "pairwise.complete.obs")
site.code.Tmaxprev<- cor(temp.MN[1:120,2:13], temp.MN[2:121,14], use = "pairwise.complete.obs")

temps <- rbind(site.code.Tmaxprev, site.code.Tcors)
write.csv(temps, paste0(site.code, "-", wood, "tmaxcor.csv"))

#plot the correlations by month 
barplot(t(site.code.Tcors), ylim = c(-0.25, 0.5), main = paste(site.code,"Correlation with Maximum Temperature"))
barplot(t(site.code.Tmaxprev), ylim = c(-0.25, 0.5), main = paste(site.code, "Correlation with previous year Maximum Temperature"))

#now with Tmin
min.t <- aggregate(TMIN~Year + Month, data=MNtmin.df, FUN=sum, na.rm = T) 
min.t

temp.min <- dcast(min.t, Year  ~ Month)


#create violin plot of monthly minimum temperature
ggplot(min.t, aes(x = factor(Month), y = TMIN))+ geom_violin(fill = "orange") +
  geom_point( colour= "blue")

site.code.crn$Year <- rownames(site.code.crn)


temp.MN <- merge(temp.min, site.code.crn, by = "Year")

#correlate the chronology with temperature
site.code.Tmincors <- cor(temp.MN[,2:13], temp.MN[,14], use = "pairwise.complete.obs")
site.code.Tminprev <- cor(temp.MN[1:120,2:13], temp.MN[2:121,14], use = "pairwise.complete.obs")

tmin <- rbind( site.code.Tminprev, site.code.Tmincors)
write.csv(tmin, paste0(site.code, "-", wood, "tmincor.csv"))


#plot the correlations by month 
site.codetmin.p <- barplot(t(site.code.Tmincors), ylim= c(-0.25, 0.5), main = paste(site.code, "Correlation with Minimum Temperature"))
site.codetminprev.p<- barplot(t(site.code.Tminprev),ylim = c(-0.25, 0.5), main = paste(site.code, "Correlation with previous year Minimum Temperature"))



#average temperature
#now with Tmin
avg.t <- aggregate(TAVG~Year + Month, data=MNtavg.df, FUN=sum, na.rm = T) 
avg.t

temp.avg <- dcast(avg.t, Year  ~ Month)


#create violin plot of monthly minimum temperature
ggplot(avg.t, aes(x = factor(Month), y = TAVG))+ geom_violin(fill = "orange") +
  geom_point( colour= "blue")

site.code.crn$Year <- rownames(site.code.crn)


temp.MN <- merge(temp.avg, site.code.crn, by = "Year")

#correlate the chronology with temperature
site.code.Tavgcors <- cor(temp.MN[,2:13], temp.MN[,14], use = "pairwise.complete.obs")
site.code.Tavgprev <- cor(temp.MN[1:120,2:13], temp.MN[2:121,14], use = "pairwise.complete.obs")

tavg <- rbind(site.code.Tavgprev, site.code.Tavgcors )
write.csv(tavg, paste0(site.code, "-", wood, "tavgcor.csv"))


##now with PDSI
pdsi <- aggregate(PDSI~Year + Month, data=MNpdsi.df, FUN=sum, na.rm = T) 
pdsi

drought <- dcast(pdsi, Year  ~ Month)


#create violin plot of monthly precip
ggplot(pdsi, aes(x = factor(Month), y = PDSI))+ geom_violin(fill = "orange") +
  geom_point( colour= "blue")

site.code.crn$Year <- rownames(site.code.crn)


pdsi.MN <- merge(drought, site.code.crn, by = "Year")

#correlate the chronology with temperature
site.code.PDSIcors <- cor(pdsi.MN[,2:13], pdsi.MN[,14], use = "pairwise.complete.obs")
site.code.PDSIprev <- cor(pdsi.MN[1:120,2:13], pdsi.MN[2:121,14], use = "pairwise.complete.obs")

pdsis <- rbind(site.code.PDSIprev,site.code.PDSIcors)
write.csv(pdsis, paste0(site.code, "-", wood, "PDSIcor.csv"))

#plot the correlations by month 
PDSI.b<- barplot(t(site.code.PDSIcors),ylim = c(-0.25, 0.5), main = paste(site.code,"Correlation with PDSI"))
pdsi.prev <- barplot(t(site.code.PDSIprev),ylim = c(-0.25, 0.5), main = paste(site.code, "Correlation with previous year PDSI"))



