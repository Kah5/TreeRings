library(ggplot2)

# this script will calculate WUE from delta13C tree ring isotope measurements
# we need: the delta ATM:
# note that these delta values need to be checked---i used mccarroll and loader until 2003 and filled in to 2011 with data from https://www.nature.com/ngeo/journal/v8/n1/extref/ngeo2313-s1.pdf
# then assume that delta hasnot changed since 2011--to get the code running

deltaATM <- read.csv("data/stable_isotopes/Mccarrol_loader_deltaC_atm.csv") # data from mccarroll and loader patched with recent ppm an dneed to check the delta13atm values
deltaTR<- read.csv("data/stable_isotopes/BON_7a_1996_2011.csv")
deltaTR2 <- read.csv("data/stable_isotopes/BON_9a_13a_1996_2015.csv")
deltaTR3 <- read.csv("data/stable_isotopes/BON_8b_14b_1996_2015.csv")

deltaTR <- rbind(deltaTR, deltaTR2, deltaTR3)

png(height = 4, width = 4, units = 'in', res=300, "outputs/stable_isotopes/Bon_d13_time.png")
ggplot(deltaTR, aes(x = Year, y = Corr.d13C, color = Tree))+
  geom_point()+geom_line(data = deltaTR, aes(x = Year, y = Corr.d13C, color = Tree))+
  theme_bw()+ylab( expression(paste(delta, "13 C tree ring")))+ ylim(-30,-20)
dev.off()


deltas <- merge(deltaTR, deltaATM, by = "Year")

# now calculate the WUE:
a <- 4.4
b <- 27

deltas$iWUE <- deltas$ppm*(1-(deltas$Corr.d13C-deltas$d13atm + a))/(b-a)*0.625
summary(deltas$iWUE)

# make initial plots of the data
png(height = 4, width = 4, units = 'in', res=300, "outputs/stable_isotopes/Bon_iWUE_time.png")
ggplot(deltas, aes(x = Year, y = iWUE, color = Tree))+geom_point()+geom_line(data = deltas, aes(x = Year, y = iWUE, color = Tree))+theme_bw()
dev.off()

png(height = 4, width = 4, units = 'in', res=300, "outputs/stable_isotopes/Bon_iWUE_ppm.png")
ggplot(deltas, aes(x = ppm, y = iWUE,color = Tree))+geom_point()+geom_line(data = deltas, aes(x = ppm, y = iWUE,color = Tree))+theme_bw()
dev.off()


#ggplot(deltas, aes(x = ppm, y = Corr.d13C, color = Tree))+geom_point()+geom_line(data= deltas, aes(x = ppm, y = Corr.d13C, color = Tree))+theme_bw()


# read in the climate and tree ring growth for Bonanza prairie (only place where we have data currently):

bon <- read.csv("data/tree_growth_age/BON-RWI_ModNegExp_detrended.csv")

get.clim <- function(site.code, site.df){
  if(site.code == "BON"){
    MNcd.clim <- read.csv("data/West_central_MN_nclimdiv.csv")
  } else{ if(site.code == "HIC" ){
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
                        Jul.pdsi = jul.pdsi[1:120,]$PDSI, 
                        WUE.fake = seq(0,15, by = 15/119))
  
  #merge annuals with rwl
  #annuals.crn <- merge(annuals, chron, by = "Year")
  #melt(annuals.crn, id = c('ear','Site', 'PCP', "TMIN", "TAVG", "PDSI","MAY.p","JJA.p", 
  #                        "JUNTmin","JUNTavg", 'JUNTmax',"Jul.pdsi"))
  df<- merge(site.df, annuals, by = "Year")
  df$site <- site.code
  df
}
bon.delt <- get.clim("BON",deltas)


ggplot(bon.delt, aes(x = PCP, y = Corr.d13C,color = Tree))+geom_point()+theme_bw()
ggplot(bon.delt, aes(x = JJA.p, y = Corr.d13C,color = Tree))+geom_point()+theme_bw()
ggplot(bon.delt, aes(x = Jul.pdsi, y = Corr.d13C,color = Tree))+geom_point()+theme_bw()
