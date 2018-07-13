# Date: July 12, 2018
# Authors Kelly Heilman, Mary Jane Walter
# Climate response function analysis
# This is a basic R script to take detrended RWI for each tree and correlates it with climate
# We generate a climate response function for each tree as our "climate sensitivity"
# then we can compare the climate sensitivities

# load libraries (these provide various functions we will use in this script)
library(dplR)
library(ggplot2)
library(plyr)
library(tidyr)

# before running this script, be sure to run the site rwl files through DPLR_detrend.R

# set the working directory:

setwd("/Users/kah/Documents/TreeRings")

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> load in climate data: <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< 
# climate data is from climate division data here: https://www7.ncdc.noaa.gov/CDO/CDODivisionalSelect.jsp#

#An overview of the variables and their units can be found here*: ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/divisional-readme.txt
#Note that precipitation is reported in Inches, but we will need to convert to mm.

# Citation for this data set is: NOAA's Gridded Climate Divisional Dataset (CLIMDIV). [indicate subset used]. NOAA National Climatic Data Center. [access date]."
# The subset used would be "Upper West Michigan 1895-2017, and the access date: 7/12/18

# the climate data should be saved in a folder labeled "climate" for this to read in 

# this reads in the climate file, which is a text file with a header and comma "," separating columns. We create an object in the R environment called WUP:
WUP <- read.delim("climate/CDODiv2925377682961.txt", header = TRUE, sep = ",")
head(WUP) # view the first few lines of the data:

# we need to separate YearMonth into year and month
WUP$Year <- as.numeric( substring(WUP$YearMonth, first = 1, last = 4) )
WUP$Month <- factor(as.numeric( substring(WUP$YearMonth, first = 5, last = 6)), labels = c("1", "2", "3", "4","5", "6", "7","8","9","10","11","12"))

# convert inches of precipiation (PCP) into mm of preciptation 
WUP$PCP.cm <- WUP$PCP * 25.4


library(dplyr)
# get total yearly precipitation (Precip) and mean annual temperatures (MAT)
WUP.yr <- WUP %>% dplyr::group_by(Year) %>% summarise(Precip = sum(PCP.cm), MAT = mean(TAVG))

# lets make separate dataframes for all climate variables of interest that have a column for year, and colums for each month
Precip.m <- WUP %>% select(Year, Month, PCP.cm) %>% spread(Month, PCP.cm)
Tavg.m <- WUP %>% select(Year, Month, TAVG) %>% spread(Month, TAVG)
Tmin.m <- WUP %>% select(Year, Month, TMIN) %>% spread(Month, TMIN)
Tmax.m <- WUP %>% select(Year, Month, TMAX) %>% spread(Month, TMAX)
PDSI.m <- WUP %>% select(Year, Month, PDSI) %>% spread(Month, PDSI)

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Basic Plots of Mean climate <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# I use the library ggplot2 to make most of my plots, but you could also use the regular plot function:
# here I show the syntax of how to plot in base R and ggplot

# plot total precipitation using base R
plot(WUP.yr$Year, WUP.yr$Precip, type = "l", main = "Total Yearly Precipitation", xlab = "Year", ylab = "Precipitation (mm)")

# plot total precipitation using ggplot2:
ggplot(data = WUP.yr, aes(Year, Precip))+geom_line() + ggtitle("Total Yearly Precipitation") + ylab("Precipitation (mm)")+xlab("Year")

# remove the data from 2018, since it is not a full year
ggplot(data = WUP.yr[WUP.yr$Year %in% 1895:2017,], aes(Year, Precip))+geom_line()+ ggtitle("Total Yearly Precipitation") + ylab("Precipitation (mm)")+xlab("Year")+theme_bw()

# plot Mean Annual Temperature here:

# remove the data from 2018, since it is not a full year
ggplot(data = WUP.yr[WUP.yr$Year %in% 1895:2017,], aes(Year, MAT))+geom_line(color = "red")+ ggtitle("Mean Annual Temperature") + ylab("Mean Temperature (DegF)")+xlab("Year")+theme_bw()

# draw a line through T to observe change in temperature trend
ggplot(data = WUP.yr[WUP.yr$Year %in% 1895:2017,], aes(Year, MAT))+geom_line(color = "red")+ ggtitle("Mean Annual Temperature") + ylab("Mean Temperature (DegF)")+xlab("Year")+theme_bw()+stat_smooth(method = "lm", color = "black")


# example of a more complex ggplot:
ggplot(WUP, aes(x=Month, y=PCP.cm, fill = "yellow")) + geom_violin() + stat_summary(fun.y=mean, geom="point", shape=23, size=2, color = 'yellow') + ylab("Monthly Precipitation (mm)")+theme_bw()+theme(legend.position = "none")


# >>>>>>>>>>>>>>>>>>>>>>>>>> load in the detrended ring width data: <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

site1 <- read.rwl("cleanrwl/BONww.rwl") # will need to replace with your detrended data:

site1<- detrend(rwl = site1, method = "Spline") # basic detrending (UNDERC will already be detrended)

site1$Year <- rownames(site1)
Prec <- merge(site1, Precip.m, by = "Year")
TAVG <- merge(site1, Tavg.m, by = "Year")
TMAX <- merge(site1, Tmax.m, by = "Year")
TMIN <- merge(site1, Tmin.m, by = "Year")
PDSI <- merge(site1, PDSI.m, by = "Year")


# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Sample correlations <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# do a basic climate correlation comparing 1 metric of climate (May max temperature) and 1 tree at 1 site:

# a simple relationship of one single tree with a single climate variable would look like:
plot( Prec[,17], Prec[,2], ylab = "Precip", xlab = "Ring width index")

# get a correlation coefficient:
cor(Prec[,17], Prec[,2], use = "pairwise.complete.obs")

# now do this for all the trees at the site and correlate with all the climate variables:
treeIDS <- colnames(site1)[!colnames(site1) %in% "Year"]

# for PRECIPITATION:
# make a matrix:
Prec.matrix <- as.matrix(Prec[,2:length(Prec)])
Prec.mat <- Prec.matrix[rownames(Prec.matrix) %in% treeIDS, colnames(Prec.matrix) %in% 1:12]
library(Hmisc)
cormatrix <- rcorr(Prec.matrix, type='spearman')



cordata = melt(cormatrix$r)
cordata_sub_prec <- cordata[cordata$Var1 %in% treeIDS & cordata$Var2 %in% 1:12,]

ggplot(cordata_sub_prec, aes(x=Var2, y=Var1, fill=value)) + 
  geom_tile() + xlab("Month") + ylab("")+ggtitle("Correlation with Precipitaiton")+scale_fill_gradient2(high="red",mid="white",low="blue", 
                                                         na.value="yellow", midpoint=0)

# for TMAX
# make a matrix:
Tmax.matrix <- as.matrix(TMAX[,2:length(TMAX)])
Tmax.mat <- Tmax.matrix[rownames(Tmax.matrix) %in% treeIDS, colnames(Tmax.matrix) %in% 1:12]
library(Hmisc)
cormatrix <- rcorr(Tmax.matrix, type='spearman')




cordata = melt(cormatrix$r)
cordata_sub_tmax <- cordata[cordata$Var1 %in% treeIDS & cordata$Var2 %in% 1:12,]

ggplot(cordata_sub_tmax, aes(x=Var2, y=Var1, fill=value)) + 
  geom_tile() + xlab("Month") + ylab("")+ggtitle("Correlation with Tmax")+scale_fill_gradient2(high="red",mid="white",low="blue", 
                                                                                                        na.value="yellow", midpoint=0)


# left to do:
# correlation plots for all trees with the rest of the climate variables

# based on these plots, lets decide which climate variables to compare (typically we want to do a climate variable that most trees are somewhat sensitive to and seems ecologically relevant)

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Extract sensitivities <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# run a linear regression for each tree with each climate variable. The slope of this line will be our sensitivity.
# alternatively, we could compare tree level correlations
