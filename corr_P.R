#Correlating ring widths to precip in Illinois
rm(list=ls())

library(dplR)
library(reshape2)
library(ggplot2)
library(plyr)

wood <- "WW"
#read in whole ring width file for site
#Bonanza <- read.tucson("./cofecha/BON_out/BONall.rwl", header = T)
#for EW
if(wood == "EW"){
  Bonanza <- read.tucson("./cofecha/BON/BONallEW.rwl", header = T)
  Hickory <- read.tucson("./cofecha/HICallEW.rwl", header = T)
  Glacial <- read.tucson("./cofecha/GLAew.rwl")
}else{if(wood == "LW"){
  Bonanza <- read.tucson("./cofecha/BON/BONallLW.rwl")
  Hickory <- read.tucson("./cofecha/HICallLW.rwl", header = T)
  Glacial <- read.tucson("./cofecha/GLAlw.rwl")
}else{
  Bonanza <- read.tucson("./cofecha/BON_out/BONall.rwl", header = T)
  Hickory <- read.tucson ("./cofecha/HICall3.rwl", header = T)
  Glacial <- read.tucson("./cofecha/GLA.rwl")
  Sand <- read.tucson("./il001.rwl", header = T)
  Pulaski <- read.tucson("./in001.rwl", header = T)
}}

#change site
site <- Hickory
site.code <- "HIC"

##################################################
#################################################
################################################
################################################
site.code.rwi <- detrend(rwl = site, method = "Spline")
#create chronology of sites
site.code.crn <- chron(site.code.rwi, prefix = paste(site.code))
#write chronology to text 
crn.trend <- chron(site, prefix= paste(site.code))
write.csv(site.code.crn, paste0(site.code, "-crn.csv"))
site.cron.plot<- crn.plot(site.code.crn, add.spline = TRUE)
site.code.stats <- rwi.stats(site)

site.code.crn$Year <- rownames(site.code.crn)
site.code.crn$freq <- 12
monthlys<- site.code.crn[rep(rownames(site.code.crn),
                  site.code.crn$freq),]
write.csv(monthlys, paste0(site.code, "monthly-crn.csv"))


#now looking at the runoff rank for the state of indiana:
runoff <- read.csv("IL_runoff_rank.txt")
cfs <- read.table("Algonquin_ILmonthly.txt", header = T)
yrs <- 1901:2015
runoff.cor <- cor(runoff$runoff_mmd, site.code.crn[site.code.crn$Year %in% yrs,1])
pct.cor <- cor(runoff$percentile, site.code.crn[site.code.crn$Year %in% yrs,1])

cfs.yr <- aggregate(mean_va ~ year_nu + month_nu, data = cfs, FUN = sum)

#create monthly column for all 
cfs.mo <- dcast(cfs.yr, year_nu ~ month_nu)

yrs.2 <- 1915: 2009 # years over which we have cfs streamflow data
cfs.cor <- cor(cfs.mo[,2:13], site.code.crn[site.code.crn$Year %in% yrs.2,1], use = "pairwise.complete")

barplot(t(cfs.cor))

write.csv(cfs.cor, paste0(site.code, "-", wood, "cfs.cor.csv"))

plot(cfs.yr$mean_va, site.code.crn[site.code.crn$Year %in% yrs.2,1])
plot(cfs.yr$year_nu, cfs.yr$mean_va, type = "l")

###########################################################
#now using climate division data from west central minnesota
if(site.code == "BON"){
MNcd.clim <- read.csv("WestCenMNcd.csv")
} else{
  MNcd.clim <- read.csv("IL_cd.csv")
}

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
total.p

pr.yr <- aggregate(PCP ~ Year , data=MNp.df, FUN=sum, na.rm = T) 
plot(pr.yr[1:120,1], pr.yr[1:120,2], type = "l", xlab = "Year", ylab = "Annual Precip (mm)")


precip <- dcast(total.p, Year  ~ Month)

annual.p <- aggregate(PCP~Year, data = MNp.df[1:1440,], FUN = sum, na.rm=T)
annual.t <- aggregate(TAVG ~ Year, data = MNcd.clim[1:1440,], FUN = mean, na.rm=T)
par(mfrow=c(2,1))
plot(annual.p, type = "l")
plot(annual.t, type = "l")
dev.off()
#create violin plot of monthly precip
ggplot(total.p, aes(x = factor(Month), y = PCP))+ geom_violin(fill = "orange") +
  geom_point( colour= "blue")

prmeans <- aggregate(PCP ~ Month, data = MNp.df, FUN=mean, na.rm = T) 
tmean <- aggregate(TAVG ~ Month, data = MNcd.clim, FUN = mean, na.rm = T)



#plot mean monthly precipitation & Temperature
pdf(paste0(site.code, "mean.climate.pdf"))
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



##########
#looking at individual tree response to climate variables
###########
#first need to individually detrend  
#for hickory grove
ser <- names(site)
site.series <- site[,1]
library(corrplot)
corrplot(M, method="circle")


HIC1180 <- detrend.series(site[,1], y.name = "HIC1180", method = "Spline",
                          verbose= TRUE)
HIC1349 <- detrend.series(site[,2], y.name = "HIC1349", method = "Spline",
                          verbose= TRUE)

HIC1396 <- detrend.series(site[,3], y.name = "HIC1396", method = "Spline",
                          verbose= TRUE)

HIC1398 <- detrend.series(site[,4], y.name = "HIC1398", method = "Spline",
                          verbose= TRUE)

HIC1424 <- detrend.series(site[,5], y.name = "HIC1424", method = "Spline",
                          verbose= TRUE)

HIC1896 <- detrend.series(site[,6], y.name = "HIC1896", method = "Spline",
                          verbose= TRUE)

HIC1699 <- detrend.series(site[,7], y.name = "HIC1699", method = "Spline",
                          verbose= TRUE)
HIC1895 <- detrend.series(site[,8], y.name = "HIC1895", method = "Spline",
                          verbose= TRUE)
HIC1984 <- detrend.series(site[,9], y.name = "HIC1984", method = "Spline",
                          verbose= TRUE)
HIC1891 <- detrend.series(site[,10], y.name = "HIC1891", method = "Spline",
                          verbose= TRUE)
HIC1892 <- detrend.series(site[,11], y.name = "HIC1892", method = "Spline",
                         verbose= TRUE)
HICfrag <- detrend.series(site[,12], y.name = "HICfrag", method = "Spline",
                         verbose= TRUE)

HICcrn <- site.code.crn$HICstd
#for bonanza prairie #bonanaza has 12 variables
BON111a <- detrend.series(Bonanza[,1],y.name = "BON111a", method = "Spline",
                          verbose= TRUE)
BON411a <- detrend.series(Bonanza[,2],y.name = "BON411a", method = "Spline",
                          verbose= TRUE)
BON511a <- detrend.series(Bonanza[,3],y.name = "BON511a", method = "Spline",
                          verbose= TRUE)
BON611a <- detrend.series(Bonanza[,4],y.name = "BON611a", method = "Spline",
                          verbose= TRUE)
BON711a <- detrend.series(Bonanza[,5],y.name = "BON711a", method = "Spline",
                          verbose= TRUE)
BON811a <- detrend.series(Bonanza[,6],y.name = "BON811a", method = "Spline",
                          verbose= TRUE)
BON911a <- detrend.series(Bonanza[,7],y.name = "BON911a", method = "Spline",
                          verbose= TRUE)
BON1011a <- detrend.series(Bonanza[,8],y.name = "BON1011a", method = "Spline",
                          verbose= TRUE)
BON1111a <- detrend.series(Bonanza[,9],y.name = "BON1111a", method = "Spline",
                          verbose= TRUE)
BON1211a <- detrend.series(Bonanza[,10],y.name = "BON1211a", method = "Spline",
                          verbose= TRUE)
BON1311a <- detrend.series(Bonanza[,11],y.name = "BON1311a", method = "Spline",
                          verbose= TRUE)
BON1411a <- detrend.series(Bonanza[,12],y.name = "BON1411a", method = "Spline",
                          verbose= TRUE)
BON1502 <- detrend.series(Bonanza[,13],y.name = "BONNew E", method = "Spline",
                          verbose= TRUE)
BONcrn <- site.code.crn$BONstd

#if statement that determines site detrending to use for corrplots
if(site.code == "HIC"){
all.detrended <- data.frame(Year = as.numeric(row.names(site)),HIC1180, HIC1349, HIC1396,
                            HIC1398, HIC1424, HIC1896, HIC1699, HIC1895, 
                            HIC1984, HIC1891, HIC1892, HICfrag, HICcrn)}else{
all.detrended <- data.frame(Year = as.numeric(row.names(site)),BON111a,BON411a, BON511a, BON611a, BON711a, 
                            BON811a, BON911a, BON1011a, BON1111a, BON1211a,BON1311a, BON1411a,
                            BON1502, BONcrn)
                            }

#renaming and reformatting climate variables
drought <- data.frame(drought)
colnames(drought) <- c("Year", "Jan", "Feb", "Mar", "Apr",
                       "May", "Jun", "Jul", "Aug", "Sep", 
                       "Oct", "Nov", "Dec")
temp <- data.frame(temp)
colnames(temp) <- c("Year", "Jan", "Feb", "Mar", "Apr",
                    "May", "Jun", "Jul", "Aug", "Sep", 
                    "Oct", "Nov", "Dec")
tmax.p <- data.frame(Year = temp[2:121,1], 
                       temp[1:120,2:13])
colnames(tmax.p) <- c("Year", "pJan", "pFeb", "pMar", "pApr",
                      "pMay", "pJun", "pJul", "pAug", "pSep", 
                      "pOct", "pNov", "pDec")
full.tmax <- merge( temp,tmax.p, by = "Year")

tmax.series<- merge(all.detrended,full.tmax, by = "Year")

M.tmax <- cor(tmax.series, use = "pairwise.complete.obs")


temp.min<- data.frame(temp.min)
colnames(temp.min) <- c("Year", "Jan", "Feb", "Mar", "Apr",
                        "May", "Jun", "Jul", "Aug", "Sep", 
                        "Oct", "Nov", "Dec")
precip<- data.frame(precip)
colnames(precip) <- c("Year", "Jan", "Feb", "Mar", "Apr",
                      "May", "Jun", "Jul", "Aug", "Sep", 
                      "Oct", "Nov", "Dec")

precip.p <- data.frame(Year = precip[2:121,1], 
                       precip[1:120,2:13])
colnames(precip) <- c("Year", "pJan", "pFeb", "pMar", "pApr",
                      "pMay", "pJun", "pJul", "pAug", "pSep", 
                      "pOct", "pNov", "pDec")
full.precip <- merge(precip.p, precip, by = "Year")

pr.series<- merge(all.detrended,full.precip, by = "Year")

M <- cor(pr.series, use = "pairwise.complete.obs")
  

library(lattice)
levelplot(M)
corrplot(M, type = "upper", method = "circle")


if(site.code == "HIC"){
M2 <- M[15:37,2:14]
M.tmax2 <- M.tmax[15:37,2:14] ##reduce size of matrix to exclude correlations of the same variables
}else{M2 <- M[16:nrow(M),2:15]
M.tmax2 <- M.tmax[16:nrow(M.tmax),2:15] }


corrplot(M2, method = "color", sig.level=0.05)
levelplot(M2 ,main=list('Precipitation',side=1,line=0.5))

###easy corrplot for plotting only significant values
cor.mtest <- function(mat, conf.level = 0.95) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], conf.level = conf.level)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      lowCI.mat[i, j] <- lowCI.mat[j, i] <- tmp$conf.int[1]
      uppCI.mat[i, j] <- uppCI.mat[j, i] <- tmp$conf.int[2]
    }
  }
  return(list(p.mat, lowCI.mat, uppCI.mat))
}

res1 <- cor.mtest(pr.series, 0.95)
res2 <- cor.mtest(pr.series, 0.99)
rest1 <- cor.mtest(tmax.series, 0.95)
## specialized the insignificant value according to the significant level
if(site.code == "HIC"){
precip.corrplot <-corrplot(M[2:14,15:ncol(M), drop=FALSE], p.mat = res1[[1]], insig="blank", method = "square",
         title="Precipitation")
tmax.corrplot <- corrplot(M.tmax[1:14,15:ncol(M.tmax), drop = FALSE], p.mat = rest1[[1]], insig="blank", method = "square",
                          title = "Max Temperature")
}else{

precip.corrplot <- corrplot(M[2:15,16:ncol(M), drop=FALSE], p.mat = res1[[1]], insig="blank", method = "square",
           title= "Precipitation")
tmax.corrplot <- corrplot(M.tmax[2:15,16:ncol(M.tmax), drop = FALSE], p.mat = rest1[[1]], insig="blank", method = "square",
                          title = "Max Temperature")
}





pdf(paste0(site.code,wood,"_summary_feb3.pdf"))
plot(site, plot.type= "spag")
crn.plot(site.code.crn, add.spline = TRUE)
#plot the correlations by month 
barplot(t(site.code.Pcors),ylim = c(-0.25, 0.5), main = paste(site.code,"Correlation with Precip."))
barplot(t(site.code.Pprev), ylim = c(-0.25, 0.5), main = paste(site.code, "Correlation with previous year Precip"))
##
barplot(t(site.code.Tcors), ylim = c(-0.25, 0.5), main = paste(site.code,"Correlation with Maximum Temperature"))
barplot(t(site.code.Tmaxprev), ylim = c(-0.25, 0.5), main = paste(site.code, "Correlation with previous year Maximum Temperature"))

site.codetmin.p <- barplot(t(site.code.Tmincors), ylim= c(-0.25, 0.5), main = paste(site.code, "Correlation with Minimum Temperature"))
site.codetminprev.p<- barplot(t(site.code.Tminprev),ylim = c(-0.25, 0.5), main = paste(site.code, "Correlation with previous year Minimum Temperature"))

PDSI.b<- barplot(t(site.code.PDSIcors),ylim = c(-0.25, 0.5), main = paste(site.code,"Correlation with PDSI"))
pdsi.prev <- barplot(t(site.code.PDSIprev),ylim = c(-0.25, 0.5), main = paste(site.code, "Correlation with previous year PDSI"))

precip.corrplot
levelplot(M2,main=list('Precipitation',side=1,line=0.5),scales=list(x=list(rot=90)))
tmax.corrplot
levelplot(M.tmax2, ,main=list('Max. Temperature',side=1,line=0.5),scales=list(x=list(rot=90)))
if(site.code == "HIC"){
  corrplot(M[2:14,15:ncol(M), drop=FALSE], method = "square",
           title="Precipitation")
  corrplot(M[2:14,15:ncol(M), drop=FALSE], p.mat = res1[[1]], insig="blank", method = "square",
                             title="Precipitation")
  corrplot(M.tmax[1:14,15:ncol(M.tmax), drop = FALSE], method = "square",
           title = "Max Temperature")
  corrplot(M.tmax[1:14,15:ncol(M.tmax), drop = FALSE], p.mat = rest1[[1]], insig="blank", method = "square",
                            title = "Max Temperature")
}else{
  corrplot(M[2:15,16:ncol(M), drop=FALSE], method = "square",
           title= "Precipitation")
  corrplot(M[2:15,16:ncol(M), drop=FALSE], p.mat = res1[[1]], insig="blank", method = "square",
                              title= "Precipitation")
  corrplot(M.tmax[2:15,16:ncol(M.tmax), drop = FALSE], method = "square",
           title = "Max Temperature")
  corrplot(M.tmax[2:15,16:ncol(M.tmax), drop = FALSE], p.mat = rest1[[1]], insig="blank", method = "square",
                            title = "Max Temperature")
}

def.par <- par(no.readonly=TRUE)
eps.cut <- 0.75 # An arbitrary EPS cutoff for demonstration
## Plot the chronology showing a potential cutoff year
## based on EPS. Running stats on the rwi with a window.
site.code.ids<- read.ids(site.code.rwi, stc = c(3, 2, 3))
#site.code.ids$tree <- 1:12
foo <- rwi.stats.running(site.code.rwi, site.code.ids,
                         window.length = 15)
yrs <- as.numeric(rownames(site.code.crn))

bar <- data.frame(yrs = c(min(yrs), foo$mid.year, max(yrs)),
                  eps = c(NA, foo$eps, NA),
                  rbar = c(NA, foo$rbar.tot, NA))
par(mar = c(2, 2, 2, 2), mgp = c(1.1, 0.1, 0), tcl = 0.25,
    mfcol = c(2, 1), xaxs='i')
plot(yrs, site.code.crn[, 1], type = "n", xlab = "Year",
     ylab = "RWI", axes=FALSE)
#cutoff <- max(bar$yrs[bar$eps < eps.cut], na.rm = TRUE)
#xx <- c(500, 500, cutoff, cutoff)
#yy <- c(-1, 3, 3, -1)
#polygon(xx, yy, col = "grey80")
abline(h = 1, lwd = 1.5)
lines(yrs, site.code.crn[, 1], col = "grey50")
lines(yrs, ffcsaps(site.code.crn[, 1], nyrs = 32), col = "red",
      lwd = 2)
axis(1); axis(2); axis(3);
box()
#par(new = TRUE)
## Add EPS
plot(bar$yrs, bar$eps, type = "b", xlab = "", ylab = "", 
     axes = FALSE, ylim = c(0.5, 0.95),pch = 20, col = "blue")
axis(4, at = pretty(c(0.5,0.95)), col ="blue")
mtext("EPS", side = 4, line = 1.1)
par(new = TRUE)
plot(bar$yrs, bar$rbar, type = "b", ylim= c(0,1),
     xlab = "Years",axes = FALSE, ylab = "Rbar", pch = 20, col = "forestgreen")

box()

dev.off()



corstarsl <- function(x){ 
  require(Hmisc) 
  x <- as.matrix(x) 
  R <- rcorr(x)$r 
  p <- rcorr(x)$P 
  
  ## define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))
  
  ## trunctuate the matrix that holds the correlations to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1] 
  
  ## build a new matrix that includes the correlations with their apropriate stars 
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x)) 
  diag(Rnew) <- paste(diag(R), " ", sep="") 
  rownames(Rnew) <- colnames(x) 
  colnames(Rnew) <- paste(colnames(x), "", sep="") 
  
  ## remove upper triangle
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew) 
  
  ## remove last column and return the matrix (which is now a data frame)
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  return(Rnew) 
}





