# lets look at relationship to climate with age:


#####################################
#read in rwl & add site + year codes#
#####################################

#Hickory Grove
Hickory <- read.tucson ("./cofecha/HICww.rwl")
HIC.stats <- rwi.stats(Hickory)

Hickory.rwi <- Hickory.bai
Hickory.bai <- bai.out(Hickory)
#detrend 
Hickory.rwi <- detrend(rwl = Hickory, method = "ModNegExp")
Hickory.rwi$year <- rownames(Hickory.rwi)

Hickory.bai$year <- rownames(Hickory.bai)

# grab from another site
StCroix <- read.tucson("./cofecha/STCww.rwl")
STC.stats <- rwi.stats(StCroix)

StCroix.rwi <- StCroix.bai
StCroix.bai <- bai.out(StCroix)
#detrend 
StCroix.rwi <- detrend(rwl = StCroix, method = "ModNegExp")
StCroix.rwi$year <- rownames(StCroix.rwi)

StCroix.bai$year <- rownames(StCroix.bai)


source("R/tree_age_agg.R")


Hic <- tree_age_agg(Hickory.rwi, 2015, "HIC", "RWI_ModNegExp_detrended")
tree_age_agg(Hickory.bai, 2015, "HIC", "RWI_BAI_test")

Stc <- tree_age_agg(StCroix.rwi, 2015, "STC", "RWI_ModNegExp_detrended")

Hic$year <- as.numeric(Hic$year)

ggplot(Hic, aes(x = Age, y = RWI))+geom_point()
ggplot(Hic, aes(x = year, y = RWI))+geom_point()
ggplot(Hic, aes(x = year, y = Age))+geom_point()

Stc$year <- as.numeric(Stc$year)

ggplot(Stc, aes(x = Age, y = RWI))+geom_point()
ggplot(Stc, aes(x = year, y = RWI))+geom_point()
ggplot(Stc, aes(x = year, y = Age))+geom_point()


trends <- lm(RWI ~ year, data = Hic)

IL.clim <- read.csv("data/NE_illinois_climdiv.csv") #Hickory Grove, Sandwich, Glacial park
MNcd.clim <- read.csv("data/East_Central_MN_CDODiv5039587215503.csv")

get.clim <- function(MNcd.clim){
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
  
  annuals <- data.frame(year = annual.p$Year, 
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
  
  #merge annuals with rwl
  #annuals.crn <- merge(annuals, chron, by = "Year")
  #melt(annuals.crn, id = c('ear','Site', 'PCP', "TMIN", "TAVG", "PDSI","MAY.p","JJA.p", 
   #                        "JUNTmin","JUNTavg", 'JUNTmax',"Jul.pdsi"))
  annuals
  }
Hic.clim <- get.clim(IL.clim)
Stc.clim <- get.clim(MNcd.clim)

HIC_climate <- merge(Hic, Hic.clim, by = "year")
ggplot(HIC_climate, aes(x = Age, y = RWI))+geom_point()

STC_climate <- merge(Stc, Stc.clim, by = "year")
STC_climate$site <- "STC"
HIC_climate$site <- "HIC"


all <- rbind(STC_climate, HIC_climate)
#HIC_climate$ age.class <-cut(HIC_climate$Age, breaks = seq(1,165, by = 20))
#ggplot(HIC_climate, aes(x = RWI, y = PCP, color = age.class))+geom_point()
ggplot(all, aes(x = PDSI, y = RWI, color = site))+geom_point()+stat_smooth()
all$year <- as.numeric(all$year)

summary(lm(RWI~PDSI, data = all))
summary(lm(RWI~PDSI:site, data = all))
summary(lm(RWI~year, data = all))
summary(lm(RWI~year:site, data = all))

ggplot(all, aes(x = year, y = RWI, color = site))+geom_point()+stat_smooth(method = "lm")


gam1 <- gam(RWI~ s(PCP)+
              s(TMIN) +
              s(TAVG) +
              s(PDSI) + s(Age) + s(site),
            data=all)


gam1
plot(gam1)

summary(gam1)$r.sq # R-squared
summary(gam1)$dev.expl # explained deviance
anova(gam1)
AIC(gam1)
