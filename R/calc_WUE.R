library(ggplot2)

# this script will calculate WUE from delta13C tree ring isotope measurements
# we need: the delta ATM:
# note that these delta values need to be checked---i used mccarroll and loader until 2003 and filled in to 2011 with data from https://www.nature.com/ngeo/journal/v8/n1/extref/ngeo2313-s1.pdf
# then assume that delta hasnot changed since 2011--to get the code running

deltaATM <- read.csv("data/stable_isotopes/Mccarrol_loader_deltaC_atm.csv") # data from mccarroll and loader patched with recent ppm an dneed to check the delta13atm values
deltaTR<- read.csv("data/stable_isotopes/BON_7a_1996_2011.csv")
deltaTR2 <- read.csv("data/stable_isotopes/BON_9a_13a_1996_2015.csv")
deltaTR3 <- read.csv("data/stable_isotopes/BON_8b_14b_1996_2015.csv")
deltaTR4 <- read.csv("data/stable_isotopes/Isotopes_BON13a_1969_1954_171115.csv")
deltaTR5 <- read.csv("data/stable_isotopes/BON13_1993_1986_UNI5b_2009.csv")
deltaTR <- rbind(deltaTR, deltaTR2, deltaTR3, deltaTR4, deltaTR5)

deltaTR <- deltaTR[! is.na(deltaTR$Year), ]
deltaTR[deltaTR$Tree %in% "BON13a", ]$Tree <- "Bon13a"
# plot bon13:
ggplot(data = deltaTR[deltaTR$Tree == "Bon13a",],aes(x = Year, y = Corr.d13C, color = Wood))+geom_point()+geom_line()

ggplot(data = deltaTR[deltaTR$Year > 1990,],aes(x = Year, y = Corr.d13C, color = Tree))+geom_point()+geom_line()


png(height = 4, width = 4, units = 'in', res=300, "outputs/stable_isotopes/Bon_d13_time.png")
ggplot(deltaTR[deltaTR$Wood %in% "LW" & deltaTR$Year >= 1990,], aes(x = Year, y = Corr.d13C, color = Tree))+
  geom_point()+geom_line(data = deltaTR[deltaTR$Wood %in% "LW" & deltaTR$Year >= 1990,], aes(x = Year, y = Corr.d13C, color = Tree))+
  theme_bw()+ylab( expression(paste(delta, "13 C tree ring")))+ ylim(-29.5,-25.5)+xlim(1990, 2015)
dev.off()


deltas <- merge(deltaTR, deltaATM, by = "Year")


# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Read in Isotopde df from read_plot_delatC.R >>>>>>>>>>>>>>>>>>>>>>>
deltas <- read.csv("outputs/stable_isotopes/full_std_suess_corrected_d13C.csv")
deltas <- deltas[!is.na(deltas$d13C_12C_corr),]

# make some preliminary plots of the data:

ggplot(deltas[!deltas$Site %in% "UNI", ], aes(x = Year, y = d13C_12C_corr, color = Site))+geom_point()+theme_bw()+stat_smooth(method = "gam" )+scale_color_manual(values = c("red", "blue"))+ylim(-32, -23)

ggplot(deltas[!deltas$Site %in% "UNI", ], aes(x = Year, y = Cor.d13C.suess, color = Site))+geom_point()+theme_bw()+stat_smooth(method = "gam" )+scale_color_manual(values = c("red", "blue"))+ylim(-32, -23)
#ggplot(deltas[!deltas$Site %in% "UNI", ], aes(x = Year, y = Cor.d13C.suess, color = Site))+geom_point()+theme_bw()+geom_line(alpha = 0.5)+scale_color_manual(values = c("red", "blue"))


# get the means and sd of each year for each site
d13.avgs <- aggregate(d13C_12C_corr ~ Year + Site, data=deltas, FUN=mean, na.rm = T) 
d13.sds <- aggregate(d13C_12C_corr ~ Year + Site, data=deltas, FUN=sd, na.rm = T) 
colnames(d13.sds) <- c("Year", "Site", "sd")
d13.avgs <- merge(d13.avgs, d13.sds, by = c("Year", "Site"))

# plot with errorbars
ggplot(d13.avgs[!d13.avgs$Site %in% "UNI", ], aes(x = Year, y = d13C_12C_corr, color = Site))+geom_point()+geom_errorbar(aes(ymin=d13C_12C_corr - sd, ymax = d13C_12C_corr + sd), size = 0.2, width = 0.9)+theme_bw()+scale_color_manual(values = c("red", "blue"))



# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> calculate the WUE:  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
a <- 4.4
b <- 27

deltas$iWUE <- deltas$ppm*(1-(deltas$d13C_12C_corr-deltas$d13atm + a))/(b-a)*0.625
summary(deltas$iWUE)

# just make plots of all the tree replicates:
ggplot(deltas[!deltas$Site %in% "UNI", ], aes(x = Year, y = iWUE, color = Site))+geom_point()+theme_bw()+stat_smooth(method = "gam" )+scale_color_manual(values = c("red", "blue"))
ggplot(deltas[!deltas$Site %in% "UNI", ], aes(x = Year, y = iWUE, color = Site))+geom_point()+theme_bw()+geom_line(alpha = 0.5)+scale_color_manual(values = c("red", "blue"))

wue.avgs <- aggregate(iWUE ~ Year + Site, data=deltas, FUN=mean, na.rm = T) 
wue.sds <- aggregate(iWUE ~ Year + Site, data=deltas, FUN=sd, na.rm = T) 
colnames(wue.sds) <- c("Year", "Site", "sd")
wue.avgs <- merge(wue.avgs, wue.sds, by = c("Year", "Site"))

ggplot(wue.avgs[!wue.avgs$Site %in% "UNI", ], aes(x = Year, y = iWUE, color = Site))+geom_point() +theme_bw()+stat_smooth(method = "gam" )+scale_color_manual(values = c("red", "blue"))
ggplot(wue.avgs[!wue.avgs$Site %in% "UNI", ], aes(x = Year, y = iWUE, color = Site))+geom_point()+theme_bw()+geom_line(alpha = 0.5)+scale_color_manual(values = c("red", "blue"))

# plot with errorbars
ggplot(wue.avgs[!wue.avgs$Site %in% "UNI", ], aes(x = Year, y = iWUE, color = Site))+geom_point()+geom_errorbar(aes(ymin=iWUE - sd, ymax = iWUE + sd), size = 0.2, width = 0.9)+theme_bw()+geom_line(alpha = 0.5)+scale_color_manual(values = c("red", "blue"))

# plot only the years where we have multiple sample estimates
ggplot(wue.avgs[!wue.avgs$Site %in% "UNI" & ! is.na(wue.avgs$sd), ], aes(x = Year, y = iWUE, color = Site))+geom_point()+geom_errorbar(aes(ymin=iWUE - sd, ymax = iWUE + sd), size = 0.2, width = 0.9)+theme_bw()+geom_line(alpha = 0.5)+scale_color_manual(values = c("red", "blue"))


ggplot(deltas, aes(x = Year, y = iWUE, color = Tree))+geom_point()+theme_bw()+facet_wrap(~Site)



#>>>>>>>>>>>>>>>>>>>>>>>>>>>> Comparing the paired years of interest <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# highlight the comparison years in each site
#young.yrs.bon <- c(1955, 1959, 1961,  1964, 1976, 1977,1981, 1987, 1988,1989, 1980, 2006, 2012)
young.yrs.bon <- c(1955, 1959, 1961,  1964, 1976, 1977,1981, 1987, 1988,1989, 1980, 2012)
old.yrs.bon <- c(1921, 1929, 1911, 1940, 1900, 1931, 1934, 1922, 1931, 1929, 1914, 1910, 1933, 1934, 1936, 1926)

young.yrs.gll <- c(1985:1980, 1976:1978, 1972, 1964, 1959:1962, 1953, 1959,
                   2014, 2012, 2011, 2006, 2005, 2001, 1997,1995, 1991, 1993)
old.yrs.gll <- c(1900, 1910, 1911,1915, 1918, 1919, 1920, 1921, 1922, 1924, 1925,1926,1932, 
                 1933, 1934, 1936, 1940, 1943, 1945)


# If the data is 
deltas$class <- NA
deltas[deltas$Site %in% "BON" & deltas$Year %in% old.yrs.bon,]$class <- "Past"
deltas[deltas$Site %in% "BON" & deltas$Year %in% young.yrs.bon,]$class <-  "Modern"
deltas[deltas$Site %in% "GLL" & deltas$Year %in% old.yrs.gll,]$class <-  "Past"
deltas[deltas$Site %in% "GLL" & deltas$Year %in% young.yrs.gll,]$class <- "Modern"

# plot based on groups:

# get colors that match tree growth plots
ageColors <- c( "#009E73", "#D55E00")
#ageColors <- c( "blue", "#D55E00")
deltas$class <- factor(deltas$class, levels = c("Past", "Modern", "NA"))
names(ageColors) <- levels(deltas$class)[1:2]

ggplot(na.omit(deltas), aes(x = ppm, y = iWUE, color = Tree))+geom_point()+theme_bw()+facet_wrap(~Site + class, scales = "free_x")+ylim(90, 200)+xlim(290, 410)
ggplot(na.omit(deltas), aes(x = class, y = Cor.d13C.suess, color = class))+geom_jitter()+theme_bw()+facet_wrap(~Site) #, scales = "free_x")+ylim(90, 200)+xlim(290, 410)

ggplot(na.omit(deltas), aes(x = class, y = Cor.d13C.suess, color = class))+geom_jitter()+theme_bw()+facet_wrap(~Site) #, scales = "free_x")+ylim(90, 200)+xlim(290, 410)
ggplot(na.omit(deltas), aes(x = class, y = iWUE, color = class))+geom_jitter()+scale_color_manual(values = ageColors)+facet_wrap(~Site)+theme_black(base_size = 20)#, scales = "free_x")+ylim(90, 200)+xlim(290, 410)


png(width = 6, height = 4, units = "in", res = 300, "outputs/stable_isotopes/d13C_cor_by_age_class_sites.png")
ggplot(na.omit(deltas), aes(x = class, y = Cor.d13C.suess, color = class))+geom_boxplot(fill = NA, color = "white")+geom_jitter()+scale_color_manual(values = ageColors)+facet_wrap(~Site)+theme_black(base_size = 20)+ylab("d13C corrected")+xlab(" ")
dev.off()

png(width = 6, height = 4, units = "in", res = 300, "outputs/stable_isotopes/iWUE_by_age_class_sites.png")
ggplot(na.omit(deltas), aes(x = class, y = iWUE, color = class))+geom_boxplot(fill = NA, color = "white")+geom_jitter()+scale_color_manual(values = ageColors)+facet_wrap(~Site)+theme_black(base_size = 20)+ylab("iWUE")+xlab(" ")
dev.off()

ggplot(site.df, aes(sand, slope.est))+geom_point(color = "white")+geom_errorbar(aes(ymin=slope.min, ymax = slope.max), color = 'white')+theme_black(base_size = 20)+ylab("Sensitivity to July PDSI")+xlab("% sand ")+stat_smooth(method = "lm", se = FALSE)


t.test(na.omit(deltas[deltas$Site %in% "BON" & deltas$Year %in% old.yrs.bon,]$Cor.d13C.suess), na.omit(deltas[deltas$Site %in% "BON" & deltas$Year %in% young.yrs.bon,]$Cor.d13C.suess))

t.test(na.omit(deltas[deltas$Site %in% "BON" & deltas$Year %in% old.yrs.bon,]$iWUE), na.omit(deltas[deltas$Site %in% "BON" & deltas$Year %in% young.yrs.bon,]$iWUE))

t.test(na.omit(deltas[deltas$Site %in% "GLL" & deltas$Year %in% old.yrs.bon,]$Cor.d13C.suess), na.omit(deltas[deltas$Site %in% "GLL" & deltas$Year %in% young.yrs.bon,]$Cor.d13C.suess))

t.test(na.omit(deltas[deltas$Site %in% "GLL" & deltas$Year %in% old.yrs.bon,]$iWUE), na.omit(deltas[deltas$Site %in% "GLL" & deltas$Year %in% young.yrs.bon,]$iWUE))








# make initial plots of the data
png(height = 4, width = 4, units = 'in', res=300, "outputs/stable_isotopes/Bon_iWUE_time.png")
ggplot(deltas[deltas$Wood %in% "LW" & deltas$Year >= 1990,], aes(x = Year, y = iWUE, color = Tree))+geom_point()+geom_line(data = deltas[deltas$Wood %in% "LW" & deltas$Year >= 1990,], aes(x = Year, y = iWUE, color = Tree))+theme_bw()
dev.off()

png(height = 4, width = 4, units = 'in', res=300, "outputs/stable_isotopes/Bon_iWUE_ppm.png")
ggplot(deltas[deltas$Wood %in% "LW" & deltas$Year >= 1990,], aes(x = ppm, y = iWUE,color = Tree))+geom_point()+geom_line(data = deltas[deltas$Wood %in% "LW" & deltas$Year >= 1990,], aes(x = ppm, y = iWUE,color = Tree))+theme_bw()
dev.off()

png(height = 4, width = 4, units = 'in', res=300, "outputs/stable_isotopes/Bon_delta13C_ppm.png")
ggplot(deltas[deltas$Wood %in% "LW"& deltas$Year >= 1990,], aes(x = ppm, y = Corr.d13C, color = Tree))+geom_point()+geom_line(data = deltas[deltas$Wood %in% "LW"& deltas$Year >= 1990,], aes(x = ppm, y = Corr.d13C,color = Tree))+theme_bw()
dev.off()

#ggplot(deltas, aes(x = ppm, y = Corr.d13C, color = Tree))+geom_point()+geom_line(data= deltas, aes(x = ppm, y = Corr.d13C, color = Tree))+theme_bw()


# read in the climate and tree ring growth for Bonanza prairie (only place where we have data currently):

bon <- read.csv("data/tree_growth_age/BON-RWI_Spline_detrended.csv")

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
  annual.p <- aggregate(PCP~Year, data = MNp.df[1:1452,], FUN = sum, na.rm=T)
  annual.t <- aggregate(TAVG ~ Year, data = MNtavg.df[1:1452,], FUN = 'mean', na.rm=T)
  annual.mint <- aggregate(TMIN ~Year, data = MNtmin.df[1:1452,], FUN = 'mean', na.rm = T)
  annual.pdsi <- aggregate(PDSI ~ Year, data = MNpdsi.df[1:1452,], FUN = 'mean', na.rm = T)
  annual.pdsi.m <- aggregate(PDSI ~ Year + Month, data = MNpdsi.df[1:1452,], FUN = 'mean', na.rm = T)
  jul.pdsi <- annual.pdsi.m[annual.pdsi.m$Month == 7,] 
  
  annuals <- data.frame(Year = annual.p$Year, 
                        PCP = annual.p$PCP,
                        TMIN = annual.mint$TMIN,
                        TAVG = annual.t$TAVG,
                        PDSI = annual.pdsi$PDSI,
                        MAY.p = may.p$PCP,
                        JJA.p = jja.p$PCP,
                        JUNTmin = jun.tmin$TMIN,
                        JUNTavg = jun.tavg$TAVG, 
                        JUNTmax = jun.tmax$TMAX,
                        Jul.pdsi = jul.pdsi$PDSI)
  
  #merge annuals with rwl
  #annuals.crn <- merge(annuals, chron, by = "Year")
  #melt(annuals.crn, id = c('ear','Site', 'PCP', "TMIN", "TAVG", "PDSI","MAY.p","JJA.p", 
  #                        "JUNTmin","JUNTavg", 'JUNTmax',"Jul.pdsi"))
  df<- merge(site.df, annuals, by = "Year")
  df$site <- site.code
  df
}
bon.delt <- get.clim("BON",deltas)

quartz()
ggplot(bon.delt, aes(x = Year, y = Corr.d13C,color = Tree))+geom_line()+theme_bw()
quartz()
ggplot(bon.delt, aes(x = Year, y = TMIN,color = Tree))+geom_line()+theme_bw()

ggplot(bon.delt, aes(x = PCP, y = Corr.d13C,color = Tree))+geom_point()+theme_bw()+stat_smooth(method= "lm")
ggplot(bon.delt, aes(x = JJA.p, y = Corr.d13C,color = Tree))+geom_point()+theme_bw()+stat_smooth(method = "lm")
ggplot(bon.delt, aes(x = Jul.pdsi, y = Corr.d13C,color = Tree))+geom_point()+theme_bw()+stat_smooth(method="lm")

