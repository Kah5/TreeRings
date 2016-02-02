#Correlating ring widths to precip in Illinois
library(dplR)


#read in whole ring width file for bonanza
Bonanza <- read.tucson("./cofecha/BONall.rwl", header = T)

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

#spagetti plot of ring width for HIC
plot(Bonanza, plot.type= "spag")
BON.crn <- chron(BON.rwi, prefix = "BON")
crn.plot(BON.crn, add.spline = TRUE)
Bon.stats <- rwl.stats(Bonanza)

###detrending
#detrends the entire series
BON.rwi <- detrend(rwl = Bonanza, method = "ModNegExp")
summary(total.p)
summary(BON.rwi)

#add year to dataframe
BON.rwi$Year<- as.numeric(rownames(BON.rwi))


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

BON.crn$Year <- rownames(BON.crn)


record.MN <- merge(precip, BON.crn, by = "Year")

BON.Pcors <- cor(record.MN[,2:13], record.MN[,14], use = "pairwise.complete.obs")
#plot the correlations by month 
barplot(t(BON.Pcors), main = "Bonanza Correlation with Precip.")


#for monthly mean temp
library(plyr)
mean.t <- aggregate(MNTM~Year + Month, data=MNt.df, FUN=sum, na.rm = T) 
mean.t

temp <- dcast(mean.t, Year  ~ Month)

library(ggplot2)
#create violin plot of monthly precip
ggplot(mean.t, aes(x = factor(Month), y = MNTM))+ geom_violin(fill = "orange") +
  geom_point( colour= "blue")

BON.crn$Year <- rownames(BON.crn)


temp.MN <- merge(temp, BON.crn, by = "Year")

#correlate the chronology with temperature
BON.Tcors <- cor(temp.MN[,2:13], temp.MN[,14], use = "pairwise.complete.obs")
#plot the correlations by month 
barplot(t(BON.Tcors), main = "Bonanza Correlation with Temperature.")


##
#now using climate division data from west central minnesota
MNcd.clim <- read.csv("WestCenMNcd.csv")

keeps <- c("Year", "Month",  "PCP")
keepst <- c("Year", "Month",  "TMAX")
keepst <- c("Year", "Month",  "TMIN")
keepspdsi <- c("Year", "Month",  "PDSI")
#create a dataset for Precip
MNp.df <- MNcd.clim[,keeps]
MNp.df[MNp.df == -9999]<- NA

#for tmax
MNt.df <- MNcd.clim[,keepst]
MNt.df[MNt.df == -9999]<- NA

#for tmin
MNtmin.df<- MNcd.clim[,keepst]
MNtmin.df[MNtmin.df == -9999]<- NA

MNpdsi.df<- MNcd.clim[,keepspdsi]
MNpdsi.df[MNpdsi.df == -9999]<- NA
#for precipitation

library(plyr)
total.p <- aggregate(PCP~Year + Month, data=MNp.df, FUN=sum, na.rm = T) 
total.p

precip <- dcast(total.p, Year  ~ Month)

library(ggplot2)
#create violin plot of monthly precip
ggplot(total.p, aes(x = factor(Month), y = PCP))+ geom_violin(fill = "orange") +
  geom_point( colour= "blue")

BON.crn$Year <- rownames(BON.crn)


record.MN <- merge(precip, BON.crn, by = "Year")

BON.Pcors <- cor(record.MN[,2:13], record.MN[,14], use = "pairwise.complete.obs")
#plot the correlations by month 
barplot(t(BON.Pcors), main = "Bonanza Correlation with Precip.")

##
#now with max T
mean.t <- aggregate(TMAX~Year + Month, data=MNt.df, FUN=sum, na.rm = T) 
mean.t

temp <- dcast(mean.t, Year  ~ Month)

library(ggplot2)
#create violin plot of monthly precip
ggplot(mean.t, aes(x = factor(Month), y = TMAX))+ geom_violin(fill = "orange") +
  geom_point( colour= "blue")

BON.crn$Year <- rownames(BON.crn)


temp.MN <- merge(temp, BON.crn, by = "Year")

#correlate the chronology with temperature
BON.Tcors <- cor(temp.MN[,2:13], temp.MN[,14], use = "pairwise.complete.obs")
#plot the correlations by month 
barplot(t(BON.Tcors), main = "Bonanza Correlation with Maximum Temperature.")

#now with Tmin
min.t <- aggregate(TMIN~Year + Month, data=MNtmin.df, FUN=sum, na.rm = T) 
min.t

temp <- dcast(min.t, Year  ~ Month)

library(ggplot2)
#create violin plot of monthly minimum temperature
ggplot(min.t, aes(x = factor(Month), y = TMIN))+ geom_violin(fill = "orange") +
  geom_point( colour= "blue")

BON.crn$Year <- rownames(BON.crn)


temp.MN <- merge(temp, BON.crn, by = "Year")

#correlate the chronology with temperature
BON.Tmincors <- cor(temp.MN[,2:13], temp.MN[,14], use = "pairwise.complete.obs")
#plot the correlations by month 
barplot(t(BON.Tmincors), main = "Bonanza Correlation with Minimum Temperature.")

##now with PDSI
pdsi <- aggregate(PDSI~Year + Month, data=MNpdsi.df, FUN=sum, na.rm = T) 
pdsi

drought <- dcast(pdsi, Year  ~ Month)

library(ggplot2)
#create violin plot of monthly precip
ggplot(pdsi, aes(x = factor(Month), y = PDSI))+ geom_violin(fill = "orange") +
  geom_point( colour= "blue")

BON.crn$Year <- rownames(BON.crn)


pdsi.MN <- merge(drought, BON.crn, by = "Year")

#correlate the chronology with temperature
BON.PDSIcors <- cor(pdsi.MN[,2:13], pdsi.MN[,14], use = "pairwise.complete.obs")
#plot the correlations by month 
barplot(t(BON.PDSIcors), main = "Bonanza Correlation with PDSI")



##need to work on processing individual records
series <- BON.rwi[, "BON411a"] # extract the series
names(series) <- rownames(BON.rwi) # give it years as rownames
series.rwi <- detrend.series(y = series, y.name = "BON411a",
                               verbose=TRUE)

series <- BON.rwi[, "BON511a"] # extract the series
names(series) <- rownames(BON.rwi) # give it years as rownames
series.rwi <- detrend.series(y = series, y.name = "BON411a",
                             verbose=TRUE)

series <- BON.rwi[, "BON1211a"] # extract the series
names(series) <- rownames(BON.rwi) # give it years as rownames
series.rwi <- detrend.series(y = series, y.name = "BON411a",
                             verbose=TRUE)

data.names<- colnames(Bonanza)
#trying to detrend all the data

#make plots of each detrending and spline
pdf("detrending_splines.pdf")
for(i in 1:length(data_names)) {
  nam <- paste0(data_names[i], ".rwi")
  assign(nam, detrend(rwl=Bonanza, y.name = colnames(get(data_names[i])), make.plot = TRUE,method = "Spline" ))}
dev.off()

detrend(Bonanza)


def.par <- par(no.readonly=TRUE)
eps.cut <- 0.85 # An arbitrary EPS cutoff for demonstration
 ## Plot the chronology showing a potential cutoff year
   ## based on EPS. Running stats on the rwi with a window.
foo <- rwi.stats.running(BON.rwi,
                              window.length = 80)
yrs <- as.numeric(rownames(BON.crn))
bar <- data.frame(yrs = c(min(yrs), foo$mid.year, max(yrs)),
                   eps = c(NA, foo$eps, NA))

par(mar = c(2, 2, 2, 2), mgp = c(1.1, 0.1, 0), tcl = 0.25,
     mfcol = c(2, 1), xaxs='i')
 plot(yrs, BON.crn[, 1], type = "n", xlab = "Year",
       ylab = "RWI", axes=FALSE)
 cutoff <- max(bar$yrs[bar$eps < eps.cut], na.rm = TRUE)
 xx <- c(500, 500, cutoff, cutoff)
 yy <- c(-1, 3, 3, -1)
 polygon(xx, yy, col = "grey80")
 abline(h = 1, lwd = 1.5)
 lines(yrs, BON.crn[, 1], col = "grey50")
 lines(yrs, ffcsaps(BON.crn[, 1], nyrs = 32), col = "red",
          lwd = 2)
 axis(1); axis(2); axis(3);
 par(new = TRUE)
 plot(bar$yrs, bar$eps, type = "b", xlab = "", ylab = "",
       axes = FALSE, pch = 20, col = "blue")
 axis(4, at = pretty(foo$eps))
 mtext("EPS", side = 4, line = 1.1)
 
 box()
 > ## Second plot is the chronology after the cutoff only
   > ## Chronology is rebuilt using just years after cutoff but
   > ## that difference is essentially nil.
  yr.mask <- yrs > cutoff
  yrs2 <- yrs[yr.mask]
  BON.crn2 <- chron(BON.rwi[yr.mask, ])
 plot(yrs2, BON.crn2[, 1], type = "n",
         xlab = "Year", ylab = "RWI", axes=FALSE)
 abline(h = 1, lwd = 1.5)
 lines(yrs2, BON.crn2[, 1], col = "grey50")
 lines(yrs2, ffcsaps(BON.crn2[, 1], nyrs = 32),
          col = "red", lwd = 2)
  axis(1); axis(2); axis(3); axis(4)
  box()
  par(def.par)
 
 
 
