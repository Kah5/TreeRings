library(lme4)
library(dplR)
library(reshape2)
library(ggplot2)
library(treeclim)

#read in rwl & add site + year codes
Hickory <- read.tucson ("./cofecha/HICww.rwl")
HIC.df <- data.frame(Hickory.rwi)
HIC.df$yr <- row.names(HIC.df)

#average cores between individuals
individual<-substring(colnames(Hickory), first = 5, last = 7)
avg.ind <- data.frame(sapply(paste0(unique(individual),'$'), function(x) rowMeans(Hickory.rwi[grep(x, names(Hickory))])))
colnames(avg.ind) <- paste0('HIC',unique(individual))
avg.ind$yr <- row.names(Hickory)
em <- melt(avg.ind, id = "yr")

X11(width = 12)
ggplot(data = em, aes(x = yr, y = value, group = variable, colour = variable))+ geom_line()

#reorder the rwi with mislabeling
num<- substring(colnames(Townsend), first = 4, last = 4)
cnum <- substring(colnames(Townsend), first = 5, last = 5)
colnames(Townsend) <- paste0('TOW',  cnum, num, '11')


#create general function to average cores from the same tree
#can average raw data or rwis using this function, but should probably use raw
avg.individuals <- function (rw, code) {
  x.df <- data.frame(rw)
  x.df$yr <- row.names(x.df)
  
  #average cores between individuals
  individual<-substring(colnames(x.df), first = 5, last = 7)
  avg.ind <- data.frame(sapply(paste0(unique(individual),'$'), function(x) rowMeans(x.df[grep(x, names(rw))])))
  colnames(avg.ind) <- paste0(code,unique(individual))
  avg.ind$yr <- as.numeric(as.character(row.names(rw)))
  
  em <- melt(avg.ind, id = "yr")
  ggplot(data = em, aes(x = yr, y = value, group = variable, colour = variable))+ geom_line()
  
}
avg.individuals(Hickory, 'HIC')
avg.individuals(Bonanza, 'BON')
avg.individuals(Pleasant, 'PLE')
avg.individuals(StCroix, 'STC')
avg.individuals(Yellow, 'Yel')
avg.individuals(Townsend, 'TOW')

pdf('outputs/spagplots/HIC.pdf')
plot(Hickory, plot.type = 'spag')
dev.off()
Hickory.bai <- chron(bai.out(Hickory))
HIC.stats <- rwl.stats(Hickory)
#detrend 
Hickory.rwi <- detrend(rwl = Hickory, method = "ModNegExp")

Hickory <- chron(Hickory.rwi)            
plot(Hickory.rwi)

yrs <- as.numeric(rownames(Hickory))
dat <- Hickory[,1]

#playing around with wavelet and red noise
out.wave <- morlet(y1 = dat, x1 = yrs, p2 = 8, dj = 0.1,
                   siglvl = 0.99)
wavelet.plot(out.wave, useRaster=NA)


redf.dat <- redfit(dat, nsim = 1000)
par(tcl = 0.5, mar = rep(2.2, 4), mgp = c(1.1, 0.1, 0))
plot(redf.dat[["freq"]], redf.dat[["gxxc"]],
     ylim = range(redf.dat[["ci99"]], redf.dat[["gxxc"]]),
     type = "n", ylab = "Spectrum (dB)", xlab = "Frequency (1/yr)",
     axes = FALSE)
grid()
lines(redf.dat[["freq"]], redf.dat[["gxxc"]], col = "#1B9E77")
lines(redf.dat[["freq"]], redf.dat[["ci99"]], col = "#D95F02")
lines(redf.dat[["freq"]], redf.dat[["ci95"]], col = "#7570B3")
lines(redf.dat[["freq"]], redf.dat[["ci90"]], col = "#E7298A")
freqs <- pretty(redf.dat[["freq"]])
pers <- round(1 / freqs, 2)
axis(1, at = freqs, labels = TRUE)
axis(3, at = freqs, labels = pers)
mtext(text = "Period (yr)", side = 3, line = 1.1)
axis(2); axis(4)
legend("topright", c("dat", "CI99", "CI95", "CI90"), lwd = 2,
       col = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A"),
       bg = "white")
box()
par(op)

par(mar=rep(2.5,4),mgp=c(1.2,0.25,0),tcl=0.5,
    xaxs="i",yaxs="i")
plot(yrs,dat,type="n",xlab="Year",ylab="RWI",axes=FALSE)
grid(col="black",lwd=0.5)
abline(h=1)
lines(yrs,dat,col="grey",lwd=1)

my.cols <- c("#A6611A","#DFC27D","#80CDC1","#018571")
lines(yrs,ffcsaps(dat,nyrs=256),col=my.cols[1],lwd=3)
lines(yrs,ffcsaps(dat,nyrs=128),col=my.cols[2],lwd=2)
lines(yrs,ffcsaps(dat,nyrs=64),col=my.cols[3],lwd=2)
lines(yrs,ffcsaps(dat,nyrs=32),col=my.cols[4],lwd=2)
legend("topright", c("dat", "256yrs", "128yrs", "64yrs", "32yrs"),
       lwd = 2, col = c("grey",my.cols),bg = "white")
axis(1);axis(2);axis(3);axis(4)
box()
par(op)



Hickory.bai$Year <- 1850:2015
Hickory.bai$Site <- 'Hickory Grove'

Hickory$Year <- 1850:2015
Hickory$Site <- 'Hickory Grove'

#Pleasant prairie site from IRTDB--not to be confused with KH collected from pleasant valley conservancy
PleasantPrairie <- read.tucson('data/wi006.rwl')


pdf('outputs/spagplots/PleasantPrairie.pdf')
plot(PleasantPrairie, plot.type = 'spag')
dev.off()
PLP.stats <- rwl.stats(PleasantPrairie)
PP.bai <- chron(bai.out(PleasantPrairie))
plot(chron(PleasantPrairie))
#detrend 
PleasantPrairie.rwi <- detrend(rwl = PleasantPrairie, method = "Spline")
PleasantPrairie <- chron(PleasantPrairie.rwi)            
plot(PleasantPrairie)

PP.bai$Year <- 1807:2000
PP.bai$Site <- 'Pleasant Prairie, WI'
PleasantPrairie$Year <- 1807:2000
PleasantPrairie$Site <- 'Pleasant Prairie, WI'

#Sandwich--south of Hickory grove *ed cook record from 1980's
#note there are two records--one for Quercus macrocarpa (il002) and one for Quercus alba (il001)
#here we use il002
Sandwich <- read.tucson ("data/il002.rwl")
pdf('outputs/spagplots/Sandwich.pdf')
plot(Sandwich, plot.type = 'spag')
dev.off()
Sandwich.bai <- chron(bai.out(Sandwich))
SAN.stats <- rwl.stats(Sandwich)
#detrend 
Sandwich.rwi <- detrend(rwl = Sandwich, method = "ModNegExp")
Sandwich <- chron(Sandwich.rwi)            

Sandwich.bai$Year <- 1752:1980
Sandwich.bai$Site <- 'Sandwich'
Sandwich$Year <- 1752:1980
Sandwich$Site <- 'Sandwich'


#Dubois de Souix record--north of bonanaza
Desoix <- read.tucson ("data/mn029.rwl")
Desoix <- chron(bai.out(Desoix))
DES.stats <- rwl.stats(Desoix)
#detrend 
Desoix.rwi <- detrend(rwl = Desoix, method = "ModNegExp")
Desoix <- chron(Desoix.rwi)            

Desoix.bai$Year <- 1877:2010
Desoix.bai$Site <- 'Bois de soix'

Desoix$Year <- 1877:2010
Desoix$Site <- 'Bois de soix'


#Yellow River record--iowa
Yellow <- read.tucson ("data/ia029.rwl")
Yellow.stats <- rwl.stats(Yellow)
plot(chron(Yellow))
#detrend 
Yellow.rwi <- detrend(rwl = Yellow, method = "Spline")
Yellow <- chron(Yellow.rwi)            
plot(Yellow)

Yellow.bai <- chron(bai.out(Yellow.rwi))
Yellow.bai$Year <- 1650:1980
Yellow.bai$Site <- 'Yellow River, IA'

Yellow$Year <- 1650:1980
Yellow$Site <- 'Yellow River, IA'



Bonanza <- read.tucson("./cofecha/BONww.rwl")
pdf('outputs/spagplots/Bonanza.pdf')
plot(Bonanza, plot.type = 'spag')
dev.off()
BON.stats <- rwi.stats(Bonanza)
#detrend
Bonanza.rwi <- detrend(rwl = Bonanza, method = "ModNegExp")
Bonanza <- chron(Bonanza.rwi)
Bonanza.bai <- chron(bai.out(Bonanza.rwi))
Bonanza.bai$Year <- 1818:2015
Bonanza.bai$Site <- "Bonanza Prairie"
Bonanza$Year <- 1818:2015
Bonanza$Site <- "Bonanza Prairie"

#record from glacial park, il
Glacial <- read.tucson("C:/Users/JMac/Documents/Kelly/crossdating/data/cofecha/GLA.rwl")
read_detrend_rwl <- function(rwl, name){
  pdf(paste0('outputs/spagplots/',name,'.pdf'))
  plot(rwl, plot.type = 'spag')
  dev.off()
  stats <- rwi.stats(rwl)
  #detrend
  rwl.rwi <- detrend(rwl = rwl, method = "ModNegExp")
  rwl <- chron(rwl.rwi)
  #rwl.bai <- chron(bai.out(rwl.rwi))
  #rwl.bai$Year <- min(as.numeric(rownames(rwl))):max(as.numeric(rownames(rwl)))
  #rwl.bai$Site <- name
  rwl$Year <- min(as.numeric(rownames(rwl))):max(as.numeric(rownames(rwl)))
  rwl$Site <- name
  rwl
}


Glacial.crn<- read_detrend_rwl(Glacial, "Glacial")


Pleasant <- read.tucson('./cofecha/PLEww.rwl')
pdf('outputs/spagplots/PleasantValley.pdf')
plot(Pleasant, plot.type = 'spag')
dev.off()
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
Pleasant.bai <- chron(bai.out(Pleasant.rwi))
Pleasant.bai$Year <- 1768:2015
Pleasant.bai$Site <- "Pleasant Valley Conservancy"
Pleasant$Year <- 1768:2015
Pleasant$Site <- "Pleasant Valley Conservancy"


Townsend <- read.tucson('./cofecha/tow/TOWww.rwl')
pdf('outputs/spagplots/Townsend.pdf')
plot(Townsend, plot.type = 'spag')
dev.off()
TOW.stats <- rwi.stats(Townsend)
TOW.meta <- read.csv('data/TOW_metadata.csv')

tellervo.names <- data.frame(colnames(Townsend))

tellervo.names$meta <- strtrim(tellervo.names$colnames.Townsend., 4)
colnames(tellervo.names) <- c('Tellervo_full', 'Tellervo')
dbh.tow <-merge(tellervo.names, TOW.meta[,c('Tellervo', 'DBH..cm.')], by = 'Tellervo')
dbh.tow <- dbh.tow[,2:3]
colnames(dbh.tow) <- c('ID', "DBH")

Townsend.bai <- chron(bai.out(Townsend.rwi))
#detrend
Townsend.rwi <- detrend(rwl = Townsend, method = "ModNegExp")
Townsend <- chron(Townsend.rwi)

Townsend.bai$Year <- 1880: 2015
Townsend.bai$Site <- 'Townsend Woods'
Townsend$Year <- 1880: 2015
Townsend$Site <- 'Townsend Woods'

StCroix <- read.tucson('./cofecha/STCww.rwl')
pdf('outputs/spagplots/StCroix.pdf')
plot(StCroix, plot.type = 'spag')
dev.off()
STC.stats <- rwi.stats(StCroix)
#detrend
STC.rwi <- detrend(rwl = StCroix, method = "ModNegExp")
StCroix <- chron(STC.rwi)
StCroix.bai <- chron(bai.out(STC.rwi))
StCroix.bai$Year <- 1879: 2015
StCroix.bai$Site <- 'St. Croix Savanna'

StCroix$Year <- 1879: 2015
StCroix$Site <- 'St. Croix Savanna'


#############################################
#plot detrended time series across all sites#
#############################################


Bonanza$type <- 'Savanna'
Hickory$type <- 'Savanna'
StCroix$type <- 'Savanna'
Sandwich$type <- 'Forest'
Pleasant$type <- 'Savanna'
PleasantPrairie$type <- 'Savanna'
Townsend$type <- 'Forest'




crns <- rbind(Bonanza, Hickory, StCroix, Pleasant, PleasantPrairie, Townsend, Sandwich)
X11(width = 14)
ggplot(crns, aes(x = Year,y=xxxstd, colour = Site)) +geom_point() + geom_smooth()+xlim(1900,2020) +ylim(0.5, 1.5)



Bonanza.bai$type <- 'Savanna'
Hickory.bai$type <- 'Savanna'
StCroix.bai$type <- 'Savanna'
Sandwich.bai$type <- 'Forest'
Pleasant.bai$type <- 'Savanna'
PP.bai$type <- 'Savanna'
Townsend.bai$type <- 'Forest'




bais <- rbind(Bonanza.bai, Hickory.bai, StCroix.bai, Pleasant.bai, Townsend.bai, Sandwich.bai,PP.bai)
X11(width = 14)
ggplot(bais, aes(x = Year,y=xxxstd, colour = Site)) + geom_point() + geom_smooth() +xlim(1900,2020)





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
#create a general function that makes two age related plots
age.trends <- function(rwl, stats, bkpt_yr){
  df.list<- list(rwi= as.list(rwl), age = stats$year, record = colnames(rwl), year = rownames(rwl))
  ages <- t(stats$year)
  
  #repeat to have the same number as in the rwl
  ages <- ages[rep(seq_len(nrow(ages)), each=nrow(rwl)),]
  
  #df.list$means <- lapply(df.list$rwi, mean, na.rm=TRUE)
  #plot(df.list$age,df.list$means)
  
  #need to calculate tree age in each year, but this isnt righ 
  
  df.list$yearssince2015 <- max(stats$last)-as.numeric(df.list$year)
  
  #to find age for the for a given tree in df.list
  calcyears<- function(x){
    testage <- x-rev(df.list$yearssince2015)
    testage[testage < 0] <- NA
    rev(testage)
  }
  
  
  treeages <- apply(ages, FUN = calcyears, MARGIN = 2 )
  
  colnames(treeages) <- colnames(rwl)
  rownames(treeages) <- rownames(rwl)
  
  pre1900 <- complete.cases(treeages[bkpt_yr,]) # finds columns that are NA's in 1890
  indexpre1900 <- colnames(treeages[,pre1900])
  
  ages.melt <- melt(treeages)
  colnames(ages.melt)<- c('year', 'variable', 'age')
  ages.melt$class <- ages.melt$variable %in% indexpre1900
  
  rwl$year <- rownames(rwl)
  rwi.melt <- melt(rwl)
  colnames(rwi.melt) <- c('year', 'variable','rwi')
  
  df.new <- merge(rwi.melt, ages.melt, by = c('year', 'variable'))
  
  summary(dcast(df.new, rwi~., mean,var.value = age, na.rm=TRUE))
  
  mean.rwi.age <- aggregate(rwi ~ age ,df.new, mean, na.rm = TRUE )
  plot(mean.rwi.age, ylim = c(0, 4))
  std <- aggregate(rwi ~ age, df.new, sd, na.rm = TRUE)
  arrows(mean.rwi.age$age, mean.rwi.age$rwi-std$rwi, mean.rwi.age$age, mean.rwi.age$rwi+std$rwi, length=0.05, angle=90, code=3)
  
  ggplot(data = mean.rwi.age, aes(x = age, y = rwi)) + geom_point()
  
  mean.rwi.class <- aggregate(rwi ~ age + class ,df.new, mean, na.rm = TRUE )
  mean.rwi.class[mean.rwi.class$class == FALSE,]$class <- 'post1900'
  mean.rwi.class[mean.rwi.class$class == TRUE,]$class <- 'pre1900'
  
  ggplot(data=mean.rwi.class, aes(x=age, y=rwi, colour=class)) + geom_point()
  
}

#need to read in data again: 
Bonanza <- read.tucson("./cofecha/BONww.rwl")
BON.stats <- rwl.stats(Bonanza)
#detrend
Bonanza.rwi <- detrend(rwl = Bonanza, method = "ModNegExp")

StCroix <- read.tucson('./cofecha/STCww.rwl')
STC.stats <- rwl.stats(StCroix)

PleasantPrairie <- read.tucson('data/wi006.rwl')
PLP.stats <- rwl.stats(PleasantPrairie)

Pleasant <- read.tucson('./cofecha/PLEww.rwl')
PLE.stats <- rwl.stats(Pleasant)

Sandwich <- read.tucson ("data/il002.rwl")
SAN.stats <- rwl.stats(Sandwich)

Townsend <- read.tucson('./cofecha/tow/TOWww.rwl')
TOW.stats <- rwl.stats(Townsend)

Yellow <- read.tucson ("data/ia029.rwl")
Yellow.stats <- rwl.stats(Yellow)

age.trends(Desoix, DES.stats, '1900')
age.trends(Hickory, HIC.stats, '1900')
age.trends(Bonanza, BON.stats, '1900')
age.trends(StCroix, STC.stats, '1900')
age.trends(PleasantPrairie, PLP.stats, '1830')
age.trends(Pleasant, PLE.stats, '1830')
age.trends(Sandwich, SAN.stats, '1850')
age.trends(Townsend, TOW.stats, '1900')
age.trends(Yellow, Yellow.stats, '1800')






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

