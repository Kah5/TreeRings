#for MN clim
MN.clim <- read.csv("MINNESOTA_sites.csv.csv")

Orton.clim <- MN.clim[MN.clim$STATION_NAME == "MILAN 1 NW MN US",]
keeps <- c("STATION_NAME", "Year", "Month",  "TPCP")
keepst <- c("STATION_NAME", "Year", "Month",  "MNTM")
MNp.df <- Orton.clim[,keeps]
MNp.df[MNp.df == -9999]<- NA

MNt.df <- Orton.clim[,keepst]
MNt.df[MNt.df == -9999]<- NA

###for IL marengo county


#read in Illinois precipitation file
ILp <- read.csv("IL_monthly_PT.csv")

ILp.Marengo <- ILp[ILp$STATION_NAME == "MARENGO IL US",]

ILp.Marengo 
keeps <- c("STATION_NAME", "YEAR", "Month",  "TPCP")
ILp.df <- ILp.Marengo[,keeps]
ILp.df[ILp.df == -9999]<- NA

library(plyr)
total.p <- aggregate(TPCP~YEAR, data=ILp.df, FUN=sum, na.rm = T) 
HIC.test

#spagetti plot of ring width for site
plot(site, plot.type= "spag")
###detrending
#detrends the entire series
site.code.rwi <- detrend(rwl = site, method = "ModNegExp")
summary(total.p)
summary(site.code.rwi)

#add year to dataframe
#site.code.rwi$Year<- as.numeric(rownames(site.code.rwi))


site.code.crn <- chron(site.code.rwi, prefix = paste(site.code))
site.cron.plot<- crn.plot(site.code.crn, add.spline = TRUE)
site.code.stats <- rwl.stats(site)

#add year to dataframe
site.code.rwi$Year<- as.numeric(rownames(site.code.rwi))


MN.clim <- read.csv("MINNESOTA_sites.csv.csv")

Orton.clim <- MN.clim[MN.clim$STATION_NAME == "MILAN 1 NW MN US",]
keeps <- c("STATION_NAME", "Year", "Month",  "TPCP")
keepst <- c("STATION_NAME", "Year", "Month",  "MNTM")
MNp.df <- Orton.clim[,keeps]
MNp.df[MNp.df == -9999]<- NA

MNt.df <- Orton.clim[,keepst]
MNt.df[MNt.df == -9999]<- NA

#for precipitation

library(plyr)
total.p <- aggregate(TPCP~Year + Month, data=MNp.df, FUN=sum, na.rm = T) 
total.p

precip <- dcast(total.p, Year  ~ Month)

library(ggplot2)
#create violin plot of monthly precip
ggplot(total.p, aes(x = factor(Month), y = TPCP))+ geom_violin(fill = "orange") +
  geom_point( colour= "blue")

site.code.crn$Year <- rownames(site.code.crn)


record.MN <- merge(precip, site.code.crn, by = "Year")

site.code.Pcors <- cor(record.MN[,2:13], record.MN[,14], use = "pairwise.complete.obs")
#plot the correlations by month 
barplot(t(site.code.Pcors), main = "site Correlation with Milan Precip.")


#for monthly mean temp
library(plyr)
mean.t <- aggregate(MNTM~Year + Month, data=MNt.df, FUN=sum, na.rm = T) 
mean.t
#Correlating ring widths to precip in Illinois
library(dplR)


#read in whole ring width file for site
Bonanza <- read.tucson("./cofecha/BON_out/BONall.rwl", header = T)
Hickory <- read.tucson ("./cofecha/HICall.rwl", header = T)




site <- Hickory
site.code <- "HIC"

temp <- dcast(mean.t, Year  ~ Month)

library(ggplot2)
#create violin plot of monthly precip
ggplot(mean.t, aes(x = factor(Month), y = MNTM))+ geom_violin(fill = "orange") +
  geom_point( colour= "blue")

site.code.crn$Year <- rownames(site.code.crn)


temp.MN <- merge(temp, site.code.crn, by = "Year")

#correlate the chronology with temperature
site.code.Tcors <- cor(temp.MN[,2:13], temp.MN[,14], use = "pairwise.complete.obs")
#plot the correlations by month 
barplot(t(site.code.Tcors), main = "site Correlation with milan Temperature.")
