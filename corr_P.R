#Correlating ring widths to precip in Illinois
rm(list=ls())

library(dplR)
library(reshape2)
library(ggplot2)
library(plyr)


#read in whole ring width file for site
Bonanza <- read.tucson("./cofecha/BON_out/BONall.rwl", header = T)
Hickory <- read.tucson ("./cofecha/HICall.rwl", header = T)




site <- Hickory
site.code <- "HIC"

##################################################
#################################################
################################################
################################################
site.code.rwi <- detrend(rwl = site, method = "ModNegExp")
#create chronology of sites
site.code.crn <- chron(site.code.rwi, prefix = paste(site.code))
site.cron.plot<- crn.plot(site.code.crn, add.spline = TRUE)
site.code.stats <- rwl.stats(site)
site.code.crn$Year <- rownames(site.code.crn)





###########################################################
#now using climate division data from west central minnesota
if(site.code == "BON"){
MNcd.clim <- read.csv("WestCenMNcd.csv")
} else{
  MNcd.clim <- read.csv("IL_cd.csv")
}

MNcd.clim$PCP <- MNcd.clim$PCP*25.54

keeps <- c("Year", "Month",  "PCP")
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

MNpdsi.df<- MNcd.clim[,keepspdsi]
MNpdsi.df[MNpdsi.df == -9999]<- NA
#for precipitation


total.p <- aggregate(PCP ~ Year + Month, data=MNp.df, FUN=sum, na.rm = T) 
total.p

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

#plot the correlations by month 
barplot(t(site.code.Pcors),ylim = c(-0.25, 0.5), main = paste(site.code,"Correlation with Precip."))
barplot(t(site.code.Pprev), ylim = c(-0.25, 0.5), main = paste(site.code, "Correlation with previous year Precip"))
##
#now with max T
mean.t <- aggregate(TMAX~Year + Month, data=MNt.df, FUN=sum, na.rm = T) 
mean.t

temp <- dcast(mean.t, Year  ~ Month)


#create violin plot of monthly precip
ggplot(mean.t, aes(x = factor(Month), y = TMAX))+ geom_violin(fill = "orange") +
  geom_point( colour= "blue")

site.code.crn$Year <- rownames(site.code.crn)


temp.MN <- merge(temp, site.code.crn, by = "Year")

#correlate the chronology with temperature
site.code.Tcors <- cor(temp.MN[,2:13], temp.MN[,14], use = "pairwise.complete.obs")
site.code.Tmaxprev<- cor(temp.MN[1:120,2:13], temp.MN[2:121,14], use = "pairwise.complete.obs")

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

#plot the correlations by month 
site.codetmin.p <- barplot(t(site.code.Tmincors), ylim= c(-0.25, 0.5), main = paste(site.code, "Correlation with Minimum Temperature"))
site.codetminprev.p<- barplot(t(site.code.Tminprev),ylim = c(-0.25, 0.5), main = paste(site.code, "Correlation with previous year Minimum Temperature"))



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

#plot the correlations by month 
PDSI.b<- barplot(t(site.code.PDSIcors),ylim = c(-0.25, 0.5), main = paste(site.code,"Correlation with PDSI"))
pdsi.prev <- barplot(t(site.code.PDSIprev),ylim = c(-0.25, 0.5), main = paste(site.code, "Correlation with previous year PDSI"))


##########
#looking at individual tree response to climate variables
#############
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
BON411a <- detrend.series(Bonanza[,1],y.name = "BON411a", method = "Spline",
                          verbose= TRUE)
BON511a <- detrend.series(Bonanza[,2],y.name = "BON511a", method = "Spline",
                          verbose= TRUE)
BON611a <- detrend.series(Bonanza[,3],y.name = "BON611a", method = "Spline",
                          verbose= TRUE)
BON711a <- detrend.series(Bonanza[,4],y.name = "BON711a", method = "Spline",
                          verbose= TRUE)
BON811a <- detrend.series(Bonanza[,5],y.name = "BON811a", method = "Spline",
                          verbose= TRUE)
BON911a <- detrend.series(Bonanza[,6],y.name = "BON911a", method = "Spline",
                          verbose= TRUE)
BON1011a <- detrend.series(Bonanza[,7],y.name = "BON1011a", method = "Spline",
                          verbose= TRUE)
BON1111a <- detrend.series(Bonanza[,8],y.name = "BON1111a", method = "Spline",
                          verbose= TRUE)
BON1211a <- detrend.series(Bonanza[,9],y.name = "BON1211a", method = "Spline",
                          verbose= TRUE)
BON1311a <- detrend.series(Bonanza[,10],y.name = "BON1311a", method = "Spline",
                          verbose= TRUE)
BON1411a <- detrend.series(Bonanza[,11],y.name = "BON1411a", method = "Spline",
                          verbose= TRUE)
BONcrn <- site.code.crn$BONstd

#if statement that determines site detrending to use for corrplots
if(site.code == "HIC"){
all.detrended <- data.frame(Year = as.numeric(row.names(site)),HIC1180, HIC1349, HIC1396,
                            HIC1398, HIC1424, HIC1896, HIC1699, HIC1895, 
                            HIC1984, HIC1891, HIC1892, HICfrag, HICcrn)}else{
all.detrended <- data.frame(Year = as.numeric(row.names(site)),BON411a, BON511a, BON611a, BON711a, 
                            BON811a, BON911a, BON1011a, BON1111a, BON1211a,BON1311a, BON1411a,
                            BONcrn)
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
full.tmax <- merge(tmax.p, temp, by = "Year")

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

#create function to calculate p values for correlation matrix
cor.prob <- function (X, dfr = nrow(X) - 2) {
  R <- cor(X, use="pairwise.complete.obs")
  above <- row(R) < col(R)
  r2 <- R[above]^2
  Fstat <- r2 * dfr/(1 - r2)
  R[above] <- 1 - pf(Fstat, 1, dfr)
  R[row(R) == col(R)] <- NA
  R
}

## Use this to dump the cor.prob output to a 4 column matrix
## with row/column indices, correlation, and p-value.
## See StackOverflow question: http://goo.gl/fCUcQ
flattenSquareMatrix <- function(m) {
  if( (class(m) != "matrix") | (nrow(m) != ncol(m))) stop("Must be a square matrix.") 
  if(!identical(rownames(m), colnames(m))) stop("Row and column names must be equal.")
  ut <- upper.tri(m)
  data.frame(i = rownames(m)[row(m)[ut]],
             j = rownames(m)[col(m)[ut]],
             cor=t(m)[ut],
             p=m[ut])
}
p.values <- flattenSquareMatrix(cor.prob(pr.series))


library(lattice)
levelplot(M)
corrplot(M, type = "upper", method = "circle")

if(site.code == "HIC"){
M2 <- M[15:37,2:14]
M.tmax2 <- M.tmax[15:37,2:14] ##reduce size of matrix to exclude correlations of the same variables
}else{M2 <- M[14:nrow(M),2:13]
M.tmax2 <- M.tmax[14:nrow(M.tmax),2:13] }


corrplot(M2, method = "color", sig.level=0.05)
levelplot(M2 ,main=list('Precipitation',side=1,line=0.5))

z.m <- melt(M2)

#a ggplot way of visualizing
precip.corrplot<- ggplot(z.m, aes(Var1, Var2, fill = value)) + geom_tile() + 
  scale_fill_gradient(low = "red",  high = "blue")+
  ylab("Tree Series") + xlab("Precpitation") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

##correlation for tmax
#

z.mt <- melt(M.tmax2)

#a ggplot way of visualizing
tmax.corrplot<- ggplot(z.mt, aes(Var1, Var2, fill = value)) + geom_tile() + 
  scale_fill_gradient(low = "red",  high = "blue")+
  ylab("Tree Serires") + xlab("Max. temperature") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

pdf(paste0(site.code,"_summary_feb3.pdf"))
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
dev.off()
