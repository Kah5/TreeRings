library(ggplot2)

# this script will calculate WUE from delta13C tree ring isotope measurements
# we need: the delta ATM:
# note that these delta values need to be checked---i used mccarroll and loader until 2003 and filled in to 2011 with data from https://www.nature.com/ngeo/journal/v8/n1/extref/ngeo2313-s1.pdf
# then assume that delta hasnot changed since 2011--to get the code running


# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Read in Isotopde df from read_plot_delatC.R >>>>>>>>>>>>>>>>>>>>>>>
deltas <- read.csv("outputs/stable_isotopes/full_std_suess_corrected_d13C.csv")
deltas <- deltas[!is.na(deltas$d13C_12C_corr),]

# make some preliminary plots of the data:

ggplot(deltas[!deltas$Site %in% c("UNI","BON"), ], aes(x = Year, y = d13C_12C_corr, color = Site))+geom_point()+theme_bw()+stat_smooth(method = "gam" )+scale_color_manual(values = c("red", "blue", "forestgreen"))+ylim(-29, -23)

ggplot(deltas[!deltas$Site %in% "UNI", ], aes(x = Year, y = Cor.d13C.suess, color = Site))+geom_point()+theme_bw()+stat_smooth(method = "gam" )+scale_color_manual(values = c("red", "blue", "forestgreen"))+ylim(-29, -23)
#ggplot(deltas[!deltas$Site %in% "UNI", ], aes(x = Year, y = Cor.d13C.suess, color = Site))+geom_point()+theme_bw()+geom_line(alpha = 0.5)+scale_color_manual(values = c("red", "blue"))


# get the means and sd of each year for each site
d13.avgs <- aggregate(Cor.d13C.suess ~ Year + Site, data=deltas, FUN=mean, na.rm = T) 
d13.sds <- aggregate(Cor.d13C.suess ~ Year + Site, data=deltas, FUN=sd, na.rm = T) 
colnames(d13.sds) <- c("Year", "Site", "sd")
d13.avgs <- merge(d13.avgs, d13.sds, by = c("Year", "Site"))

# plot with errorbars
ggplot(d13.avgs[!d13.avgs$Site %in% "UNI", ], aes(x = Year, y = Cor.d13C.suess, color = Site))+geom_point()+geom_errorbar(aes(ymin=Cor.d13C.suess - sd, ymax = Cor.d13C.suess + sd), size = 0.2, width = 0.9)+theme_bw()+scale_color_manual(values = c("red", "blue", "forestgreen"))



# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> calculate the WUE:  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
a <- 4.4
b <- 27

deltas$iWUE <- deltas$ppm*(1-(deltas$d13C_12C_corr-deltas$d13atm + a))/(b-a)*0.625
summary(deltas$iWUE)

# just make plots of all the tree replicates:
png(height = 4, width = 4, units = "in", res = 300, "outputs/stable_isotopes/IWUE_over_time_by_site_v2.png")
ggplot(deltas[!deltas$Site %in% "UNI", ], aes(x = Year, y = iWUE, color = Site))+geom_point()+theme_bw()+stat_smooth(method = "gam" )+scale_color_manual(values = c("red", "blue", "forestgreen", "orange"))+theme_black()
dev.off()

png(height = 4, width = 4, units = "in", res = 300, "outputs/stable_isotopes/d13_cor_suess_over_time_by_site_v2.png")
ggplot(deltas[!deltas$Site %in% "UNI", ], aes(x = Year, y = Cor.d13C.suess, color = Site))+geom_point()+theme_bw()+stat_smooth(method = "gam" )+scale_color_manual(values = c("red", "blue", "forestgreen", "orange"))+theme_black()+ylab(expression(paste(delta^{13}, "C corrected (\u2030)")))
dev.off()

png(height = 4, width = 4, units = "in", res = 300, "outputs/stable_isotopes/d13_over_time_by_site_v2.png")
ggplot(deltas[!deltas$Site %in% "UNI", ], aes(x = Year, y = d13C_12C_corr, color = Site))+geom_point()+theme_bw()+stat_smooth(method = "gam" )+scale_color_manual(values = c("red", "blue", "forestgreen", "orange"))+theme_black()+ylab(expression(paste(delta^{13}, "C (\u2030)")))
dev.off()


png(height = 4, width = 4, units = "in", res = 300, "outputs/stable_isotopes/d13_cor_suess_vs_ppm_by_site_v2.png")
ggplot(deltas[!deltas$Site %in% "UNI", ], aes(x = ppm, y = Cor.d13C.suess, color = Site))+geom_point()+theme_bw()+stat_smooth(method = "gam" )+scale_color_manual(values = c("red", "blue", "forestgreen", "orange"))+theme_black()+ylab(expression(paste(delta^{13}, "C corrected (\u2030)")))
dev.off()

png(height = 4, width = 4, units = "in", res = 300, "outputs/stable_isotopes/d13_cor_vs_ppm_by_site_v2.png")
ggplot(deltas[!deltas$Site %in% "UNI", ], aes(x = ppm, y = d13C_12C_corr, color = Site))+geom_point()+theme_bw()+stat_smooth(method = "gam" )+scale_color_manual(values = c("red", "blue", "forestgreen", "orange"))+theme_black()+ylab(expression(paste(delta^{13}, "C (\u2030)")))
dev.off()


ggplot(deltas[!deltas$Site %in% "UNI", ], aes(x = Year, y = iWUE, color = Site))+geom_point()+theme_bw()+geom_line(alpha = 0.5)+scale_color_manual(values = c("red", "blue", "forestgreen", "orange"))

wue.avgs <- aggregate(iWUE ~ Year + Site, data=deltas, FUN=mean, na.rm = T) 
wue.sds <- aggregate(iWUE ~ Year + Site, data=deltas, FUN=sd, na.rm = T) 
colnames(wue.sds) <- c("Year", "Site", "sd")
wue.avgs <- merge(wue.avgs, wue.sds, by = c("Year", "Site"))

ggplot(wue.avgs[!wue.avgs$Site %in% "UNI", ], aes(x = Year, y = iWUE, color = Site))+geom_point() +theme_bw()+stat_smooth(method = "gam" )+scale_color_manual(values = c("red", "blue", "forestgreen", "orange"))
ggplot(wue.avgs[!wue.avgs$Site %in% "UNI", ], aes(x = Year, y = iWUE, color = Site))+geom_point()+theme_bw()+geom_line(alpha = 0.5)+scale_color_manual(values = c("red", "blue", "forestgreen", "orange"))

# plot with errorbars
ggplot(wue.avgs[!wue.avgs$Site %in% "UNI", ], aes(x = Year, y = iWUE, color = Site))+geom_point()+geom_errorbar(aes(ymin=iWUE - sd, ymax = iWUE + sd), size = 0.2, width = 0.9)+theme_bw()+geom_line(alpha = 0.5)+scale_color_manual(values = c("red", "blue", "forestgreen", "orange"))

# plot only the years where we have multiple sample estimates
ggplot(wue.avgs[!wue.avgs$Site %in% "UNI" & ! is.na(wue.avgs$sd), ], aes(x = Year, y = iWUE, color = Site))+geom_point()+geom_errorbar(aes(ymin=iWUE - sd, ymax = iWUE + sd), size = 0.2, width = 0.9)+theme_bw()+geom_line(alpha = 0.5)+scale_color_manual(values = c("red", "blue", "forestgreen", "orange"))


ggplot(deltas, aes(x = Year, y = iWUE, color = Tree))+geom_point()+theme_bw()+facet_wrap(~Site)



#>>>>>>>>>>>>>>>>>>>>>>>>>>>> Comparing the paired years of interest <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# highlight the comparison years in each site
#young.yrs.bon <- c(1955, 1959, 1961,  1964, 1976, 1977,1981, 1987, 1988,1989, 1980, 2006, 2012)
young.yrs.bon <- c(1955, 1959, 1961,  1964, 1976, 1977,1981, 1987, 1988,1989, 1980, 2012)
young.trees.bon <- c("BON6", "BON12", "BON7", "BON8")

old.yrs.bon <- c(1921, 1929, 1911, 1940, 1900, 1931, 1934, 1922, 1931, 1929, 1914, 1910, 1933, 1934, 1936, 1926)

young.yrs.gll <- c(1985:1980, 1976:1978, 1972, 1964, 1959:1962, 1953, 1959,
                   2014, 2012, 2011, 2006, 2005, 2001, 1997,1995, 1991, 1993)
old.yrs.gll <- c(1900, 1910, 1911,1915, 1918, 1919, 1920, 1921, 1922, 1924, 1925,1926,1932, 
                 1933, 1934, 1936, 1940, 1943, 1945)
old.yrs.mou<- c(1910, 1916, 1923, 1931, 1932 ,1933, 1937, 1942, 1946 ,1948, 1949)
young.yrs.mou <- c(1950, 1958, 1964, 1976, 1980 ,1985 ,1988, 1989 ,2001, 2002, 2012)

old.yrs.gla <- c(1900, 1901, 1902, 1906, 1907, 1915, 1920, 1923, 1927, 1930, 1938, 1947, 1950)
young.yrs.gla <- c(1964, 1965 ,1971, 1975, 1979, 1980 ,1981, 1987 ,1989 ,1991, 2001, 2005, 2012)

# If the data is 
deltas$class <- NA
#deltas[deltas$Site %in% "BON" & deltas$Year %in% old.yrs.bon & !deltas$Tree %in% young.trees.bon,]$class <- "Past"
#deltas[deltas$Site %in% "BON" & deltas$Year %in% young.yrs.bon & deltas$Tree %in% young.trees.bon,]$class <-  "Modern"
deltas[deltas$Site %in% "BON" & deltas$Year %in% old.yrs.bon ,]$class <- "Past"
deltas[deltas$Site %in% "BON" & deltas$Year %in% young.yrs.bon ,]$class <-  "Modern"


deltas[deltas$Site %in% "GLL" & deltas$Year %in% old.yrs.gll,]$class <-  "Past"
deltas[deltas$Site %in% "GLL" & deltas$Year %in% young.yrs.gll,]$class <- "Modern"
deltas[deltas$Site %in% "MOU" & deltas$Year %in% old.yrs.mou,]$class <-  "Past"
deltas[deltas$Site %in% "MOU" & deltas$Year %in% young.yrs.mou,]$class <- "Modern"

deltas[deltas$Site %in% "GLA" & deltas$Year %in% old.yrs.gla,]$class <-  "Past"
deltas[deltas$Site %in% "GLA" & deltas$Year %in% young.yrs.gla,]$class <- "Modern"
# plot based on groups:

# get colors that match tree growth plots
ageColors <- c( "#009E73", "#D55E00")
#ageColors <- c( "blue", "#D55E00")
deltas$class <- factor(deltas$class, levels = c("Past", "Modern", "NA"))
names(ageColors) <- levels(deltas$class)[1:2]

ggplot(na.omit(deltas), aes(x = ppm, y = iWUE, color = Tree))+geom_point()+theme_bw()+facet_wrap(~Site + class, scales = "free_x")+ylim(90, 200)+xlim(290, 410)
ggplot(na.omit(deltas[!deltas$Tree %in% c("BON7", "BON6"),]), aes(x = class, y = Cor.d13C.suess, color = class))+geom_jitter()+theme_bw()+facet_wrap(~Site) #, scales = "free_x")+ylim(90, 200)+xlim(290, 410)

ggplot(na.omit(deltas), aes(x = class, y = Cor.d13C.suess, color = class))+geom_jitter()+theme_bw()+facet_wrap(~Site) #, scales = "free_x")+ylim(90, 200)+xlim(290, 410)

ggplot(na.omit(deltas[!deltas$Tree %in% c("BON13", "BON9"),]), aes(x = class, y = Cor.d13C.suess, color = class))+geom_jitter()+theme_bw()#+facet_wrap(~Site) #, scales = "free_x")+ylim(90, 200)+xlim(290, 410)
ggplot(na.omit(deltas), aes(x = class, y = iWUE, color = class))+geom_jitter()+scale_color_manual(values = ageColors)+facet_wrap(~Site)+theme_black(base_size = 20)#, scales = "free_x")+ylim(90, 200)+xlim(290, 410)

deltas$Site<- factor(deltas$Site, levels = c("GLL", "GLA", "MOU"))
png(width = 8, height = 4, units = "in", res = 300, "outputs/stable_isotopes/d13C_cor_by_age_class_sites_v2.png")
ggplot(na.omit(deltas[!deltas$Site %in% "BON",]), aes(x = class, y = Cor.d13C.suess, fill = class))+geom_boxplot( color = "white")+scale_fill_manual(values = ageColors)+facet_wrap(~Site)+theme_black(base_size = 20)+ylab(expression(paste(delta^{13}, "C corrected (\u2030)")))+xlab(" ")
dev.off()

delts.df <- deltas[!deltas$Site %in% "BON",]
head(delts.df)



ggplot(na.omit(deltas[!deltas$Site %in% "BON",]), aes(x = class, y = Cor.d13C.suess))
ggplot(na.omit(deltas), aes(x = class, y = Cor.d13C.suess, color = Tree))+geom_boxplot(fill = NA, color = "white")+geom_jitter()+facet_wrap(~Site)+theme_black(base_size = 20)+ylab(expression(paste(delta^{13}, "C corrected (\u2030)")))+xlab(" ")


png(width = 6, height = 4, units = "in", res = 300, "outputs/stable_isotopes/iWUE_by_age_class_sites_v2.png")
ggplot(na.omit(deltas[!deltas$Site %in% "BON",]), aes(x = class, y = iWUE, fill = class))+geom_boxplot( color = "white")+scale_fill_manual(values = ageColors)+facet_wrap(~Site)+theme_black(base_size = 20)+ylab("iWUE")+xlab(" ")
dev.off()

ggplot(na.omit(deltas), aes(x = ppm, y = Cor.d13C.suess, color = class))+geom_point()+scale_color_manual(values = ageColors)+facet_wrap(~Site)+theme_black(base_size = 20)+ylab("d13C corrected")+xlab(" ")



png(width = 6, height = 4, units = "in", res = 300, "outputs/stable_isotopes/d13C_cor_by_age_class_sites_v2.png")
ggplot(na.omit(deltas[deltas$Site %in% c("BON", "GLL"),]), aes(x = class, y = Cor.d13C.suess, color = class))+geom_boxplot(fill = NA, color = "white")+geom_jitter()+scale_color_manual(values = ageColors)+theme_black(base_size = 20)+ylab(expression(paste(delta^{13}, "C corrected (\u2030)")))+xlab(" ")
dev.off()

png(width = 6, height = 4, units = "in", res = 300, "outputs/stable_isotopes/iWUE_by_age_class_sites_v2.png")
ggplot(na.omit(deltas), aes(x = class, y = iWUE, color = class))+geom_boxplot(fill = NA, color = "white")+geom_jitter()+scale_color_manual(values = ageColors)+facet_wrap(~Site)+theme_black(base_size = 20)+ylab("iWUE")+xlab(" ")
dev.off()


t.test(na.omit(deltas[deltas$Site %in% "BON" & deltas$Year %in% old.yrs.bon,]$Cor.d13C.suess), na.omit(deltas[deltas$Site %in% "BON" & deltas$Year %in% young.yrs.bon,]$Cor.d13C.suess))

t.test(na.omit(deltas[deltas$Site %in% "BON" & deltas$Year %in% old.yrs.bon,]$iWUE), na.omit(deltas[deltas$Site %in% "BON" & deltas$Year %in% young.yrs.bon,]$iWUE))

t.test(na.omit(deltas[deltas$Site %in% "GLL" & deltas$Year %in% old.yrs.bon,]$Cor.d13C.suess), na.omit(deltas[deltas$Site %in% "GLL" & deltas$Year %in% young.yrs.bon,]$Cor.d13C.suess))

t.test(na.omit(deltas[deltas$Site %in% "GLL" & deltas$Year %in% old.yrs.bon,]$iWUE), na.omit(deltas[deltas$Site %in% "GLL" & deltas$Year %in% young.yrs.bon,]$iWUE))


# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> How Much does VPD affect delta C 13? >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# read in the tree ring data and climate from these sites and make correlations:

BON.clim <- read.csv("data/climate/PRISM/BONfull.clim.csv")
GLL2.clim <- read.csv("data/climate/PRISM/GLL2full.clim.csv")

BON.clim$Site <- "BON"
GLL2.clim$Site <- "GLL"

climates <- rbind(BON.clim, GLL2.clim)
colnames(climates)[2] <- "Year"
deltas <- merge(deltas, climates, by = c("Year", "Site"))


png(height = 4, width = 4, units = "in", res = 300, "outputs/stable_isotopes/d13_cor_vs_JJAVPDmax_by_site.png")
ggplot(deltas[!deltas$Site %in% "UNI", ], aes(x = jja.VPDmax, y = Cor.d13C.suess, color = Site))+geom_point()+theme_bw()+stat_smooth(method = "gam" )+scale_color_manual(values = c("red", "blue"))+theme_black()+ylab(expression(paste(delta^{13}, "C corrected (\u2030)")))+xlab("Mean JJA VPDmax")
dev.off()

png(height = 4, width = 4, units = "in", res = 300, "outputs/stable_isotopes/d13_cor_vs_JulVPDmax_by_site.png")
ggplot(deltas[!deltas$Site %in% "UNI", ], aes(x = jul.VPDmax, y = Cor.d13C.suess, color = Site))+geom_point()+theme_bw()+stat_smooth(method = "gam" )+scale_color_manual(values = c("red", "blue"))+theme_black()+ylab(expression(paste(delta^{13}, "C corrected (\u2030)")))+xlab("Mean JJA VPDmax")
dev.off()

png(height = 4, width = 4, units = "in", res = 300, "outputs/stable_isotopes/d13_cor_vs_junTmax_by_site.png")
ggplot(deltas[!deltas$Site %in% "UNI", ], aes(x = JUNTmax, y = Cor.d13C.suess, color = Site))+geom_point()+theme_bw()+stat_smooth(method = "gam" )+scale_color_manual(values = c("red", "blue"))+theme_black()+ylab(expression(paste(delta^{13}, "C corrected (\u2030)")))+xlab("Mean JJA VPDmax")
dev.off()

ggplot(deltas[!deltas$Site %in% "UNI", ], aes(x = jul.BAL, y = Cor.d13C.suess, color = Site))+geom_point()+theme_bw()+stat_smooth(method = "gam" )+scale_color_manual(values = c("red", "blue"))+theme_black()+ylab(expression(paste(delta^{13}, "C corrected (\u2030)")))
ggplot(deltas[!deltas$Site %in% "UNI", ], aes(x = jul.VPDmax, y = Cor.d13C.suess, color = Site))+geom_point()+theme_bw()+stat_smooth(method = "gam" )+scale_color_manual(values = c("red", "blue"))+theme_black()+ylab(expression(paste(delta^{13}, "C corrected (\u2030)")))
ggplot(deltas[!deltas$Site %in% "UNI", ], aes(x = jul.VPDmax, y = Cor.d13C.suess, color = Site))+geom_point()+theme_bw()+stat_smooth(method = "gam" )+scale_color_manual(values = c("red", "blue"))+theme_black()+ylab(expression(paste(delta^{13}, "C corrected (\u2030)")))
ggplot(deltas[!deltas$Site %in% "UNI", ], aes(x = JUNTmax, y = Cor.d13C.suess, color = Site))+geom_point()+theme_bw()+stat_smooth(method = "gam" )+scale_color_manual(values = c("red", "blue"))+theme_black()+ylab(expression(paste(delta^{13}, "C corrected (\u2030)")))


png(height = 4, width = 4, units = "in", res = 300, "outputs/stable_isotopes/iWUE_vs_JJAVPDmax_by_site.png")
ggplot(deltas[!deltas$Site %in% "UNI", ], aes(x = jja.VPDmax, y = iWUE, color = Site))+geom_point()+theme_bw()+stat_smooth(method = "gam" )+scale_color_manual(values = c("red", "blue"))+theme_black()+ylab("iWUE")+xlab("Mean JJA VPDmax")
dev.off()

png(height = 4, width = 4, units = "in", res = 300, "outputs/stable_isotopes/iWUE_cor_vs_JulVPDmax_by_class.png")
ggplot(deltas[!deltas$Site %in% "UNI" & ! is.na(deltas$class), ], aes(x = jja.VPDmax, y = iWUE, color = class))+geom_point()+theme_bw()+stat_smooth(method = "gam" )+scale_color_manual(values = c("red", "blue"))+theme_black()+ylab("iWUE")+xlab("Mean JJA VPDmax")
dev.off()

png(height = 4, width = 4, units = "in", res = 300, "outputs/stable_isotopes/Cor_d13_suess_cor_vs_JulVPDmax_by_class.png")
ggplot(deltas[!deltas$Site %in% "UNI" & ! is.na(deltas$class), ], aes(x = jja.VPDmax, y = Cor.d13C.suess, color = class))+geom_point()+theme_bw()+stat_smooth(method = "gam" )+scale_color_manual(values = c("red", "blue"))+theme_black()+ylab(expression(paste(delta^{13}, "C corrected (\u2030)")))+xlab("Mean JJA VPDmax")
dev.off()

png(height = 4, width = 4, units = "in", res = 300, "outputs/stable_isotopes/iWUE_cor_vs_JulVPDmax_by_site.png")
ggplot(deltas[!deltas$Site %in% "UNI", ], aes(x = jul.VPDmax, y = iWUE, color = Site))+geom_point()+theme_bw()+stat_smooth(method = "gam" )+scale_color_manual(values = c("red", "blue"))+theme_black()+ylab("iWUE")+xlab("Mean JJA VPDmax")
dev.off()

png(height = 4, width = 4, units = "in", res = 300, "outputs/stable_isotopes/iWUE_cor_vs_junTmax_by_site.png")
ggplot(deltas[!deltas$Site %in% "UNI", ], aes(x = JUNTmax, y = iWUE, color = Site))+geom_point()+theme_bw()+stat_smooth(method = "gam" )+scale_color_manual(values = c("red", "blue"))+theme_black()+ylab("iWUE")+xlab("Mean JJA VPDmax")
dev.off()

ggplot(deltas[!deltas$Site %in% "UNI", ], aes(x = jja.VPDmax, y = iWUE, color = Site))+geom_point()+theme_bw()+stat_smooth(method = "gam" )+scale_color_manual(values = c("red", "blue"))+theme_black()+ylab(expression(paste(delta^{13}, "C corrected (\u2030)")))
ggplot(deltas[!deltas$Site %in% "UNI", ], aes(x = jul.BAL, y = iWUE, color = Site))+geom_point()+theme_bw()+stat_smooth(method = "gam" )+scale_color_manual(values = c("red", "blue"))+theme_black()+ylab(expression(paste(delta^{13}, "C corrected (\u2030)")))
ggplot(deltas[!deltas$Site %in% "UNI", ], aes(x = jul.VPDmax, y = iWUE, color = Site))+geom_point()+theme_bw()+stat_smooth(method = "gam" )+scale_color_manual(values = c("red", "blue"))+theme_black()+ylab(expression(paste(delta^{13}, "C corrected (\u2030)")))
ggplot(deltas[!deltas$Site %in% "UNI", ], aes(x = JJA.p, y = iWUE, color = Site))+geom_point()+theme_bw()+stat_smooth(method = "gam" )+scale_color_manual(values = c("red", "blue"))+theme_black()+ylab(expression(paste(delta^{13}, "C corrected (\u2030)")))
ggplot(deltas[!deltas$Site %in% "UNI", ], aes(x = JUNTmax, y = iWUE, color = Site))+geom_point()+theme_bw()+stat_smooth(method = "gam" )+scale_color_manual(values = c("red", "blue"))+theme_black()+ylab(expression(paste(delta^{13}, "C corrected (\u2030)")))

ggplot(deltas[!deltas$Site %in% "UNI", ], aes(x = ppm, y = jja.VPDmax, color = Site))+geom_point()+theme_bw()+stat_smooth(method = "gam" )+scale_color_manual(values = c("red", "blue"))+theme_black()+ylab(expression(paste(delta^{13}, "C corrected (\u2030)")))

# basic linear models for climate and isotope data
summary(glm(Cor.d13C.suess ~ jja.VPDmax, data = deltas[deltas$Site %in% "GLL",]))
summary(glm(Cor.d13C.suess ~ jul.BAL, data = deltas[deltas$Site %in% "GLL",]))
summary(glm(Cor.d13C.suess ~ ppm, data = deltas[deltas$Site %in% "GLL",]))
summary(glm(Cor.d13C.suess ~ JJA.p, data = deltas[deltas$Site %in% "GLL",]))
summary(glm(Cor.d13C.suess ~ JUNTmax, data = deltas[deltas$Site %in% "GLL",]))

summary(glm(Cor.d13C.suess ~ jja.VPDmax, data = deltas[deltas$Site %in% "BON",]))
summary(glm(Cor.d13C.suess ~ jul.BAL, data = deltas[deltas$Site %in% "BON",]))
summary(glm(Cor.d13C.suess ~ ppm, data = deltas[deltas$Site %in% "BON",]))
summary(glm(Cor.d13C.suess ~ JJA.p, data = deltas[deltas$Site %in% "BON",]))
summary(glm(Cor.d13C.suess ~ JUNTmax, data = deltas[deltas$Site %in% "BON",]))

summary(gam(jja.VPDmax ~ ppm, data = deltas[deltas$Site %in% "GLL",]))


# basic linear models for climate and iWUE data:
summary(glm(iWUE ~ jja.VPDmax, data = deltas[deltas$Site %in% "GLL",]))
summary(glm(iWUE ~ jul.BAL, data = deltas[deltas$Site %in% "GLL",]))
summary(glm(iWUE ~ ppm, data = deltas[deltas$Site %in% "GLL",]))
summary(glm(iWUE ~ JJA.p, data = deltas[deltas$Site %in% "GLL",]))
summary(glm(iWUE ~ JUNTmax, data = deltas[deltas$Site %in% "GLL",]))

summary(glm(iWUE ~ jja.VPDmax, data = deltas[deltas$Site %in% "BON",]))
summary(glm(iWUE ~ jul.BAL, data = deltas[deltas$Site %in% "BON",]))
summary(glm(iWUE ~ ppm, data = deltas[deltas$Site %in% "BON",]))
summary(glm(iWUE ~ JJA.p, data = deltas[deltas$Site %in% "BON",]))
summary(glm(iWUE ~ JUNTmax, data = deltas[deltas$Site %in% "BON",]))

summary(gam(iWUE ~ ppm + jja.VPDmax, data = deltas[deltas$Site %in% "BON",]))
summary(glm(Cor.d13C.suess ~ ppm + jja.VPDmax, data = deltas[deltas$Site %in% "BON",]))



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

