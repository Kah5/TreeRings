# This script does some basic climate correlation of the site chronologies
# Uses the data from 'data/crn/BON-crn.csv'

library(dplR)
library(reshape2)
library(ggplot2)
library(plyr)


site <- Bonanza # assigns site as the same object as bonanaza rwl file
site.code <- "BON" # for naming purposes
site.code.crn <- read.csv(paste0('data/crn/', site.code,'-crn.csv'))


# read in the .csv files that have GHCN climate data (global historical climate network) in them:
# this changes the file that is read in based on the site.code
if(site.code == "BON"){
  MNcd.clim <- read.csv("data/West_central_MN_nclimdiv.csv")
}else{ if(site.code == "HIC" ){
  MNcd.clim <- read.csv("data/NE_illinois_climdiv.csv")
} else{ if(site.code == "GLA" ){
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

# do some conversions of units & make a separate Dataframe for each climate variable
MNcd.clim$PCP <- MNcd.clim$PCP*25.54 # convert to mm
keeps <- c("Year", "Month",  "PCP")
keepstavg <- c("Year", "Month", "TAVG")
keepst <- c("Year", "Month",  "TMAX")
keepstmin <- c("Year", "Month",  "TMIN")
keepspdsi <- c("Year", "Month",  "PDSI")

#create a dataset for Precip with only th columns in 'keeps'
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

# Calculate the total precipitation for each month in each year using the aggregate() function
total.p <- aggregate(PCP ~ Year + Month, data=MNp.df, FUN=sum, na.rm = T) 
total.p

# Calculate the total precipitation for each year using the aggregate function 
pr.yr <- aggregate(PCP ~ Year , data=MNp.df, FUN=sum, na.rm = T) 

# plot the precipitation using baseplot
plot(pr.yr[1:120,1], pr.yr[1:120,2], type = "l", xlab = "Year", ylab = "Annual Precip (mm)")

#we can also plot using ggplot
ggplot(data= pr.yr[1:120,], aes(x = Year, y = PCP)) + geom_line()+
  ylab('Annual Precip (mm)')+xlab('Year')+theme_bw()

# use decast to reorder the dataframe...
# now we have each month as a column
precip <- dcast(total.p, Year  ~ Month)

# we can aggreate/average each variable of climate for each year (except Precip, which we sum)
annual.p <- aggregate(PCP~Year, data = MNp.df[1:1440,], FUN = sum, na.rm=T)
annual.t <- aggregate(TAVG ~ Year, data = MNcd.clim[1:1440,], FUN = mean, na.rm=T)
annual.mint <- aggregate(TMIN ~Year, data = MNcd.clim[1:1440,], FUN = mean, na.rm = T)
annual.maxt <- aggregate(TMAX ~Year, data = MNcd.clim[1:1440,], FUN = mean, na.rm = T)
annual.PDSI <- aggregate(PDSI ~Year, data = MNcd.clim[1:1440,], FUN = mean, na.rm = T)

# make plots of these variables
# note the trend in temperature
ggplot(data= annual.t, aes(x = Year, y = TAVG)) + geom_line()+
  ylab('Average Temp (degF)')+xlab('Year')+theme_bw()

ggplot(data= annual.mint, aes(x = Year, y = TMIN)) + geom_line()+
  ylab('Average Min. Temp')+xlab('Year')+theme_bw()

ggplot(data= annual.maxt, aes(x = Year, y = TMAX)) + geom_line()+
  ylab('Average Max. Temp')+xlab('Year')+theme_bw()

#now write these to a file so that we can reuse them for the sites
write.csv(annual.p, paste0('data/site_clims/',site.code, '-annualP.csv'))
write.csv(annual.t, paste0('data/site_clims/',site.code, '-annualtavg.csv'))
write.csv(annual.mint, paste0('data/site_clims/',site.code, '-annualtmin.csv'))
write.csv(annual.maxt, paste0('data/site_clims/',site.code, '-annualtmax.csv'))
write.csv(annual.PDSI, paste0('data/site_clims/',site.code, '-annualPDSI.csv'))


#create violin plot of monthly precipitation:
ggplot(total.p, aes(x = factor(Month), y = PCP))+ geom_violin(fill = "orange") +
  geom_point( colour= "blue") + xlab ('Month') + ylab("Precipitation (mm)") +theme_bw()

prmeans <- aggregate(PCP ~ Month, data = MNp.df, FUN=mean, na.rm = T) 
tmean <- aggregate(TAVG ~ Month, data = MNcd.clim, FUN = mean, na.rm = T)

# This section of code plots the mean monthly temperature and mean annual precipitation on the same plot
#pdf(paste0(site.code, "mean.climate.pdf"))
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
#dev.off()


##########################################################
# Correlations of Tree ring index with Climate
#########################################################

# we now join the precip data frame and site.code.crn df together using merge
record.MN <- merge(precip, site.code.crn, by = "Year")

head(record.MN)


# we use cor to correlate precipitation with the tree ring chronology
site.code.Pcors <- cor(record.MN[,2:13], record.MN[,c(paste0(site.code,'std'))], use = "pairwise.complete.obs") # this correlates with current year
site.code.Pprev <- cor(record.MN[1:120,2:13], record.MN[2:121,c(paste0(site.code,'std'))], use = "pairwise.complete.obs") # this correlates with previous year
site.code.Pcors
site.code.Pprev

# bind the correlation coefficients together in a dataframe
precip <- data.frame(cor.pr = rbind(site.code.Pcors, site.code.Pprev))
write.csv(precip, paste0(site.code, "-", wood, "Precipcor.csv"))

#basic base barplot corelations
barplot(t(site.code.Pcors),ylim = c(-0.25, 0.5), main = paste(site.code,"Correlation with Precip."))
barplot(t(site.code.Pprev), ylim = c(-0.25, 0.5), main = paste(site.code, "Correlation with previous year Precip"))



# make them pretty using a custom plotting function:
climate.barplot <- function(df, site.code, clim){
# add months to the df with precipitation correlations
df$months <- c("pJan", "pFeb", "pMar", "pApr", "pMay", "pJun", "pJul",
            "pAug", "pSep", "pOct", "pNov", "pDec",
            "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
            "Aug", "Sep", "Oct", "Nov", "Dec")

cors.melt <- melt(df, id.vars = c('months'))
cors.melt$months <- factor(cors.melt$months, levels=df$months)
cors.melt[order(cors.melt$months),]
output<- ggplot(data = cors.melt, aes(months, value, fill = variable))+
  geom_bar(stat = 'identity', position = position_dodge()) + 
  theme_bw()+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1)) + ggtitle(paste(site.code, clim ," Correlations"))
output
}

climate.barplot(precip, site.code, "Precipitation")
# based on this barplot, precipitation from the previous july has the strongest correlation with climate