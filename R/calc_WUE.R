library(ggplot2)

# this script will calculate WUE from delta13C tree ring isotope measurements
# we need: the delta ATM:
# note that these delta values need to be checked---i used mccarroll and loader until 2003 and filled in to 2011 with data from https://www.nature.com/ngeo/journal/v8/n1/extref/ngeo2313-s1.pdf
# then assume that delta hasnot changed since 2011--to get the code running


# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Read in Isotopde df from read_plot_delatC.R >>>>>>>>>>>>>>>>>>>>>>>
deltas <- read.csv("outputs/stable_isotopes/full_std_suess_corrected_d13C.csv")
deltas <- deltas[!is.na(deltas$d13C_12C_corr),]


# calculate discrimination:
deltas$bigD <- (deltas$d13atm-deltas$Cor.d13C.suess)/(1+deltas$Cor.d13C.suess/1000)

# make some preliminary plots of the data:
ggplot(deltas, aes(x = year, y = bigD, color = site))+geom_point()+theme_bw()+stat_smooth(method = "gam" )#+scale_color_manual(values = c("red", "blue", "forestgreen"))


ggplot(deltas[!deltas$site %in% c("UNI", "BON"), ], aes(x = year, y = d13C_12C_corr, color = site))+geom_point()+theme_bw()+stat_smooth(method = "gam" )+scale_color_manual(values = c("red", "blue", "forestgreen"))+ylim(-29, -23)

ggplot(deltas[!deltas$site %in% "UNI", ], aes(x = year, y = Cor.d13C.suess, color = site))+geom_point()+theme_bw()+stat_smooth(method = "gam" )+scale_color_manual(values = c("red", "blue", "forestgreen"))+ylim(-29, -23)
#ggplot(deltas[!deltas$site %in% "UNI", ], aes(x = year, y = Cor.d13C.suess, color = site))+geom_point()+theme_bw()+geom_line(alpha = 0.5)+scale_color_manual(values = c("red", "blue"))


# get the means and sd of each year for each site
d13.avgs <- aggregate(Cor.d13C.suess ~ year + site, data=deltas, FUN=mean, na.rm = T) 
d13.sds <- aggregate(Cor.d13C.suess ~ year + site, data=deltas, FUN=sd, na.rm = T) 
colnames(d13.sds) <- c("year", "site", "sd")
d13.avgs <- merge(d13.avgs, d13.sds, by = c("year", "site"))

# plot with errorbars
ggplot(d13.avgs[!d13.avgs$site %in% "UNI", ], aes(x = year, y = Cor.d13C.suess, color = site))+geom_point()+geom_errorbar(aes(ymin=Cor.d13C.suess - sd, ymax = Cor.d13C.suess + sd), size = 0.2, width = 0.9)+theme_bw()+scale_color_manual(values = c("red", "blue", "forestgreen", "grey"))



# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> calculate the WUE:  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
a <- 4.4
b <- 27

deltas$iWUE <- deltas$ppm*(1-(deltas$d13C_12C_corr-deltas$d13atm + a))/(b-a)*0.625
summary(deltas$iWUE)

# just make plots of all the tree replicates:
png(height = 4, width = 4, units = "in", res = 300, "outputs/stable_isotopes/IWUE_over_time_by_site_v2.png")
ggplot(deltas[!deltas$site %in% "UNI", ], aes(x = year, y = iWUE, color = site))+geom_point()+theme_bw()+stat_smooth(method = "gam" )+scale_color_manual(values = c("red", "blue", "forestgreen", "orange", "grey"))+theme_black()
dev.off()

png(height = 4, width = 4, units = "in", res = 300, "outputs/stable_isotopes/d13_cor_suess_over_time_by_site_v2.png")
ggplot(deltas[!deltas$site %in% "UNI", ], aes(x = year, y = Cor.d13C.suess, color = site))+geom_point()+theme_bw()+stat_smooth(method = "gam" )+scale_color_manual(values = c("red", "blue", "forestgreen", "orange"))+theme_black()+ylab(expression(paste(delta^{13}, "C corrected (\u2030)")))
dev.off()

png(height = 4, width = 4, units = "in", res = 300, "outputs/stable_isotopes/d13_over_time_by_site_v2.png")
ggplot(deltas[!deltas$site %in% "UNI", ], aes(x = year, y = d13C_12C_corr, color = site))+geom_point()+theme_bw()+stat_smooth(method = "gam" )+scale_color_manual(values = c("red", "blue", "forestgreen", "orange"))+theme_black()+ylab(expression(paste(delta^{13}, "C (\u2030)")))
dev.off()


png(height = 4, width = 4, units = "in", res = 300, "outputs/stable_isotopes/d13_cor_suess_vs_ppm_by_site_v2.png")
ggplot(deltas[!deltas$site %in% "UNI", ], aes(x = ppm, y = Cor.d13C.suess, color = site))+geom_point()+theme_bw()+stat_smooth(method = "gam" )+scale_color_manual(values = c("red", "blue", "forestgreen", "orange"))+theme_black()+ylab(expression(paste(delta^{13}, "C corrected (\u2030)")))
dev.off()

png(height = 4, width = 4, units = "in", res = 300, "outputs/stable_isotopes/d13_cor_vs_ppm_by_site_v2.png")
ggplot(deltas[!deltas$site %in% "UNI", ], aes(x = ppm, y = d13C_12C_corr, color = site))+geom_point()+theme_bw()+stat_smooth(method = "gam" )+scale_color_manual(values = c("red", "blue", "forestgreen", "orange"))+theme_black()+ylab(expression(paste(delta^{13}, "C (\u2030)")))
dev.off()


ggplot(deltas[!deltas$site %in% "UNI", ], aes(x = year, y = iWUE, color = site))+geom_point()+theme_bw()+geom_line(alpha = 0.5)+scale_color_manual(values = c("red", "blue", "forestgreen", "orange"))

wue.avgs <- aggregate(iWUE ~ year + site, data=deltas, FUN=mean, na.rm = T) 
wue.sds <- aggregate(iWUE ~ year + site, data=deltas, FUN=sd, na.rm = T) 
colnames(wue.sds) <- c("year", "site", "sd")
wue.avgs <- merge(wue.avgs, wue.sds, by = c("year", "site"))

ggplot(wue.avgs[!wue.avgs$site %in% "UNI", ], aes(x = year, y = iWUE, color = site))+geom_point() +theme_bw()+stat_smooth(method = "gam" )+scale_color_manual(values = c("red", "blue", "forestgreen", "orange"))
ggplot(wue.avgs[!wue.avgs$site %in% "UNI", ], aes(x = year, y = iWUE, color = site))+geom_point()+theme_bw()+geom_line(alpha = 0.5)+scale_color_manual(values = c("red", "blue", "forestgreen", "orange"))

# plot with errorbars
ggplot(wue.avgs[!wue.avgs$site %in% "UNI", ], aes(x = year, y = iWUE, color = site))+geom_point()+geom_errorbar(aes(ymin=iWUE - sd, ymax = iWUE + sd), size = 0.2, width = 0.9)+theme_bw()+geom_line(alpha = 0.5)+scale_color_manual(values = c("red", "blue", "forestgreen", "orange"))

# plot only the years where we have multiple sample estimates
ggplot(wue.avgs[!wue.avgs$site %in% "UNI" & ! is.na(wue.avgs$sd), ], aes(x = year, y = iWUE, color = site))+geom_point()+geom_errorbar(aes(ymin=iWUE - sd, ymax = iWUE + sd), size = 0.2, width = 0.9)+theme_bw()+geom_line(alpha = 0.5)+scale_color_manual(values = c("red", "blue", "forestgreen", "orange"))


ggplot(deltas, aes(x = year, y = iWUE, color = ID))+geom_point()+theme_bw()+facet_wrap(~site)

# write deltas to a csv:
write.csv(deltas, "outputs/stable_isotopes/deltas_13C_WUE_data.csv", row.names = FALSE)

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
#deltas[deltas$site %in% "BON" & deltas$year %in% old.yrs.bon & !deltas$ID %in% young.trees.bon,]$class <- "Past"
#deltas[deltas$site %in% "BON" & deltas$year %in% young.yrs.bon & deltas$ID %in% young.trees.bon,]$class <-  "Modern"
deltas[deltas$site %in% "BON" & deltas$year %in% old.yrs.bon & !deltas$ID %in% young.trees.bon ,]$class <- "Past"
deltas[deltas$site %in% "BON" & deltas$year %in% young.yrs.bon & deltas$ID %in% young.trees.bon,]$class <-  "Modern"


deltas[deltas$site %in% "GLL" & deltas$year %in% old.yrs.gll,]$class <-  "Past"
deltas[deltas$site %in% "GLL" & deltas$year %in% young.yrs.gll,]$class <- "Modern"
deltas[deltas$site %in% "MOU" & deltas$year %in% old.yrs.mou,]$class <-  "Past"
deltas[deltas$site %in% "MOU" & deltas$year %in% young.yrs.mou,]$class <- "Modern"

deltas[deltas$site %in% "GLA" & deltas$year %in% old.yrs.gla,]$class <-  "Past"
deltas[deltas$site %in% "GLA" & deltas$year %in% young.yrs.gla,]$class <- "Modern"
# plot based on groups:

# get colors that match tree growth plots
ageColors <- c( "#009E73", "#D55E00")
#ageColors <- c( "blue", "#D55E00")
deltas$class <- factor(deltas$class, levels = c("Past", "Modern", "NA"))
names(ageColors) <- levels(deltas$class)[1:2]

ggplot(na.omit(deltas), aes(x = ppm, y = iWUE, color = ID))+geom_point()+theme_bw()+facet_wrap(~site + class, scales = "free_x")+ylim(90, 200)+xlim(290, 410)
ggplot(na.omit(deltas), aes(x = class, y = Cor.d13C.suess, color = class))+geom_jitter()+theme_bw()+facet_wrap(~site) #, scales = "free_x")+ylim(90, 200)+xlim(290, 410)

ggplot(na.omit(deltas[!deltas$ID %in% c("BON7", "BON6"),]), aes(x = class, y = Cor.d13C.suess, color = class))+geom_jitter()+theme_bw()+facet_wrap(~site) #, scales = "free_x")+ylim(90, 200)+xlim(290, 410)

ggplot(na.omit(deltas[!deltas$ID %in% c("BON13", "BON9"),]), aes(x = class, y = Cor.d13C.suess, color = class))+geom_jitter()+theme_bw()#+facet_wrap(~site) #, scales = "free_x")+ylim(90, 200)+xlim(290, 410)
ggplot(na.omit(deltas), aes(x = class, y = iWUE, color = class))+geom_jitter()+geom_boxplot()+scale_color_manual(values = ageColors)+facet_wrap(~site)+theme_black(base_size = 20)#, scales = "free_x")+ylim(90, 200)+xlim(290, 410)

deltas$site <- factor(deltas$site, levels = c("GLL", "GLA", "MOU", "BON", "UNI"))
png(width = 8, height = 4, units = "in", res = 300, "outputs/stable_isotopes/d13C_cor_by_age_class_sites_v2.png")
ggplot(na.omit(deltas), aes(x = class, y = Cor.d13C.suess, fill = class))+geom_boxplot( color = "white")+scale_fill_manual(values = ageColors)+facet_wrap(~site)+theme_black(base_size = 20)+ylab(expression(paste(delta^{13}, "C corrected (\u2030)")))+xlab(" ")
dev.off()

delts.df <- deltas[!deltas$site %in% "BON",]
head(delts.df)
delts.df


ggplot(na.omit(deltas[!deltas$site %in% "BON",]), aes(x = class, y = Cor.d13C.suess))+geom_jitter()
ggplot(na.omit(deltas), aes(x = class, y = Cor.d13C.suess, color = ID))+geom_boxplot(fill = NA, color = "white")+geom_jitter()+facet_wrap(~site)+theme_black(base_size = 20)+ylab(expression(paste(delta^{13}, "C corrected (\u2030)")))+xlab(" ")


png(width = 7, height = 4, units = "in", res = 300, "outputs/stable_isotopes/iWUE_by_age_class_sites_v2.png")
ggplot(na.omit(deltas[!deltas$site %in% "BON",]), aes(x = class, y = iWUE, fill = class))+geom_boxplot( color = "white")+scale_fill_manual(values = ageColors)+facet_wrap(~site)+theme_black(base_size = 20)+ylab("iWUE")+xlab(" ")
dev.off()

ggplot(na.omit(deltas), aes(x = ppm, y = Cor.d13C.suess, color = class))+geom_point()+scale_color_manual(values = ageColors)+facet_wrap(~site)+theme_black(base_size = 20)+ylab("d13C corrected")+xlab(" ")



png(width = 6, height = 4, units = "in", res = 300, "outputs/stable_isotopes/d13C_cor_by_age_class_sites_v2.png")
ggplot(na.omit(deltas[deltas$site %in% c("BON", "GLL"),]), aes(x = class, y = Cor.d13C.suess, color = class))+geom_boxplot(fill = NA, color = "white")+geom_jitter()+scale_color_manual(values = ageColors)+theme_black(base_size = 20)+ylab(expression(paste(delta^{13}, "C corrected (\u2030)")))+xlab(" ")
dev.off()

png(width = 6, height = 4, units = "in", res = 300, "outputs/stable_isotopes/iWUE_by_age_class_sites_v2.png")
ggplot(na.omit(deltas), aes(x = class, y = iWUE, color = class))+geom_boxplot(fill = NA, color = "white")+geom_jitter()+scale_color_manual(values = ageColors)+facet_wrap(~site)+theme_black(base_size = 20)+ylab("iWUE")+xlab(" ")
dev.off()


t.test(na.omit(deltas[deltas$site %in% "BON" & deltas$year %in% old.yrs.bon,]$Cor.d13C.suess), na.omit(deltas[deltas$site %in% "BON" & deltas$year %in% young.yrs.bon,]$Cor.d13C.suess))

t.test(na.omit(deltas[deltas$site %in% "BON" & deltas$year %in% old.yrs.bon,]$iWUE), na.omit(deltas[deltas$site %in% "BON" & deltas$year %in% young.yrs.bon,]$iWUE))

t.test(na.omit(deltas[deltas$site %in% "GLL" & deltas$year %in% old.yrs.gll,]$Cor.d13C.suess), na.omit(deltas[deltas$site %in% "GLL" & deltas$year %in% young.yrs.gll,]$Cor.d13C.suess))

t.test(na.omit(deltas[deltas$site %in% "GLL" & deltas$year %in% old.yrs.gll,]$iWUE), na.omit(deltas[deltas$site %in% "GLL" & deltas$year %in% young.yrs.gll,]$iWUE))


t.test(na.omit(deltas[deltas$site %in% "GLA" & deltas$year %in% old.yrs.gla,]$Cor.d13C.suess), na.omit(deltas[deltas$site %in% "GLA" & deltas$year %in% young.yrs.gla,]$Cor.d13C.suess))

t.test(na.omit(deltas[deltas$site %in% "GLA" & deltas$year %in% old.yrs.gla,]$iWUE), na.omit(deltas[deltas$site %in% "GLA" & deltas$year %in% young.yrs.gla,]$iWUE))


# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> How Much does VPD affect delta C 13? >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# read in the tree ring data and climate from these sites and make correlations:

BON.clim <- read.csv("data/climate/PRISM/BONfull.clim.csv")
GLL2.clim <- read.csv("data/climate/PRISM/GLL2full.clim.csv")

BON.clim$site <- "BON"
GLL2.clim$site <- "GLL"

climates <- rbind(BON.clim, GLL2.clim)
colnames(climates)[2] <- "year"
deltas <- merge(deltas, climates, by = c("year", "site"))


png(height = 4, width = 4, units = "in", res = 300, "outputs/stable_isotopes/d13_cor_vs_JJAVPDmax_by_site.png")
ggplot(deltas[!deltas$site %in% "UNI", ], aes(x = jja.VPDmax, y = Cor.d13C.suess, color = site))+geom_point()+theme_bw()+stat_smooth(method = "gam" )+scale_color_manual(values = c("red", "blue"))+theme_black()+ylab(expression(paste(delta^{13}, "C corrected (\u2030)")))+xlab("Mean JJA VPDmax")
dev.off()

png(height = 4, width = 4, units = "in", res = 300, "outputs/stable_isotopes/d13_cor_vs_JulVPDmax_by_site.png")
ggplot(deltas[!deltas$site %in% "UNI", ], aes(x = jul.VPDmax, y = Cor.d13C.suess, color = site))+geom_point()+theme_bw()+stat_smooth(method = "gam" )+scale_color_manual(values = c("red", "blue"))+theme_black()+ylab(expression(paste(delta^{13}, "C corrected (\u2030)")))+xlab("Mean JJA VPDmax")
dev.off()

png(height = 4, width = 4, units = "in", res = 300, "outputs/stable_isotopes/d13_cor_vs_junTmax_by_site.png")
ggplot(deltas[!deltas$site %in% "UNI", ], aes(x = JUNTmax, y = Cor.d13C.suess, color = site))+geom_point()+theme_bw()+stat_smooth(method = "gam" )+scale_color_manual(values = c("red", "blue"))+theme_black()+ylab(expression(paste(delta^{13}, "C corrected (\u2030)")))+xlab("Mean JJA VPDmax")
dev.off()

ggplot(deltas[!deltas$site %in% "UNI", ], aes(x = jul.BAL, y = Cor.d13C.suess, color = site))+geom_point()+theme_bw()+stat_smooth(method = "gam" )+scale_color_manual(values = c("red", "blue"))+theme_black()+ylab(expression(paste(delta^{13}, "C corrected (\u2030)")))
ggplot(deltas[!deltas$site %in% "UNI", ], aes(x = jul.VPDmax, y = Cor.d13C.suess, color = site))+geom_point()+theme_bw()+stat_smooth(method = "gam" )+scale_color_manual(values = c("red", "blue"))+theme_black()+ylab(expression(paste(delta^{13}, "C corrected (\u2030)")))
ggplot(deltas[!deltas$site %in% "UNI", ], aes(x = jul.VPDmax, y = Cor.d13C.suess, color = site))+geom_point()+theme_bw()+stat_smooth(method = "gam" )+scale_color_manual(values = c("red", "blue"))+theme_black()+ylab(expression(paste(delta^{13}, "C corrected (\u2030)")))
ggplot(deltas[!deltas$site %in% "UNI", ], aes(x = JUNTmax, y = Cor.d13C.suess, color = site))+geom_point()+theme_bw()+stat_smooth(method = "gam" )+scale_color_manual(values = c("red", "blue"))+theme_black()+ylab(expression(paste(delta^{13}, "C corrected (\u2030)")))


png(height = 4, width = 4, units = "in", res = 300, "outputs/stable_isotopes/iWUE_vs_JJAVPDmax_by_site.png")
ggplot(deltas[!deltas$site %in% "UNI", ], aes(x = jja.VPDmax, y = iWUE, color = site))+geom_point()+theme_bw()+stat_smooth(method = "gam" )+scale_color_manual(values = c("red", "blue"))+theme_black()+ylab("iWUE")+xlab("Mean JJA VPDmax")
dev.off()

png(height = 4, width = 4, units = "in", res = 300, "outputs/stable_isotopes/iWUE_cor_vs_JulVPDmax_by_class.png")
ggplot(deltas[!deltas$site %in% "UNI" & ! is.na(deltas$class), ], aes(x = jja.VPDmax, y = iWUE, color = class))+geom_point()+theme_bw()+stat_smooth(method = "gam" )+scale_color_manual(values = c("red", "blue"))+theme_black()+ylab("iWUE")+xlab("Mean JJA VPDmax")
dev.off()

png(height = 4, width = 4, units = "in", res = 300, "outputs/stable_isotopes/Cor_d13_suess_cor_vs_JulVPDmax_by_class.png")
ggplot(deltas[!deltas$site %in% "UNI" & ! is.na(deltas$class), ], aes(x = jja.VPDmax, y = Cor.d13C.suess, color = class))+geom_point()+theme_bw()+stat_smooth(method = "gam" )+scale_color_manual(values = c("red", "blue"))+theme_black()+ylab(expression(paste(delta^{13}, "C corrected (\u2030)")))+xlab("Mean JJA VPDmax")
dev.off()

png(height = 4, width = 4, units = "in", res = 300, "outputs/stable_isotopes/iWUE_cor_vs_JulVPDmax_by_site.png")
ggplot(deltas[!deltas$site %in% "UNI", ], aes(x = jul.VPDmax, y = iWUE, color = site))+geom_point()+theme_bw()+stat_smooth(method = "gam" )+scale_color_manual(values = c("red", "blue"))+theme_black()+ylab("iWUE")+xlab("Mean JJA VPDmax")
dev.off()

png(height = 4, width = 4, units = "in", res = 300, "outputs/stable_isotopes/iWUE_cor_vs_junTmax_by_site.png")
ggplot(deltas[!deltas$site %in% "UNI", ], aes(x = JUNTmax, y = iWUE, color = site))+geom_point()+theme_bw()+stat_smooth(method = "gam" )+scale_color_manual(values = c("red", "blue"))+theme_black()+ylab("iWUE")+xlab("Mean JJA VPDmax")
dev.off()

ggplot(deltas[!deltas$site %in% "UNI", ], aes(x = jja.VPDmax, y = iWUE, color = site))+geom_point()+theme_bw()+stat_smooth(method = "gam" )+scale_color_manual(values = c("red", "blue"))+theme_black()+ylab(expression(paste(delta^{13}, "C corrected (\u2030)")))
ggplot(deltas[!deltas$site %in% "UNI", ], aes(x = jul.BAL, y = iWUE, color = site))+geom_point()+theme_bw()+stat_smooth(method = "gam" )+scale_color_manual(values = c("red", "blue"))+theme_black()+ylab(expression(paste(delta^{13}, "C corrected (\u2030)")))
ggplot(deltas[!deltas$site %in% "UNI", ], aes(x = jul.VPDmax, y = iWUE, color = site))+geom_point()+theme_bw()+stat_smooth(method = "gam" )+scale_color_manual(values = c("red", "blue"))+theme_black()+ylab(expression(paste(delta^{13}, "C corrected (\u2030)")))
ggplot(deltas[!deltas$site %in% "UNI", ], aes(x = JJA.p, y = iWUE, color = site))+geom_point()+theme_bw()+stat_smooth(method = "gam" )+scale_color_manual(values = c("red", "blue"))+theme_black()+ylab(expression(paste(delta^{13}, "C corrected (\u2030)")))
ggplot(deltas[!deltas$site %in% "UNI", ], aes(x = JUNTmax, y = iWUE, color = site))+geom_point()+theme_bw()+stat_smooth(method = "gam" )+scale_color_manual(values = c("red", "blue"))+theme_black()+ylab(expression(paste(delta^{13}, "C corrected (\u2030)")))

ggplot(deltas[!deltas$site %in% "UNI", ], aes(x = ppm, y = jja.VPDmax, color = site))+geom_point()+theme_bw()+stat_smooth(method = "gam" )+scale_color_manual(values = c("red", "blue"))+theme_black()+ylab(expression(paste(delta^{13}, "C corrected (\u2030)")))

# basic linear models for climate and isotope data
summary(glm(Cor.d13C.suess ~ jja.VPDmax, data = deltas[deltas$site %in% "GLL",]))
summary(glm(Cor.d13C.suess ~ jul.BAL, data = deltas[deltas$site %in% "GLL",]))
summary(glm(Cor.d13C.suess ~ ppm, data = deltas[deltas$site %in% "GLL",]))
summary(glm(Cor.d13C.suess ~ JJA.p, data = deltas[deltas$site %in% "GLL",]))
summary(glm(Cor.d13C.suess ~ JUNTmax, data = deltas[deltas$site %in% "GLL",]))

summary(glm(Cor.d13C.suess ~ jja.VPDmax, data = deltas[deltas$site %in% "BON",]))
summary(glm(Cor.d13C.suess ~ jul.BAL, data = deltas[deltas$site %in% "BON",]))
summary(glm(Cor.d13C.suess ~ ppm, data = deltas[deltas$site %in% "BON",]))
summary(glm(Cor.d13C.suess ~ JJA.p, data = deltas[deltas$site %in% "BON",]))
summary(glm(Cor.d13C.suess ~ JUNTmax, data = deltas[deltas$site %in% "BON",]))

summary(gam(jja.VPDmax ~ ppm, data = deltas[deltas$site %in% "GLL",]))


# basic linear models for climate and iWUE data:
summary(glm(iWUE ~ jja.VPDmax, data = deltas[deltas$site %in% "GLL",]))
summary(glm(iWUE ~ jul.BAL, data = deltas[deltas$site %in% "GLL",]))
summary(glm(iWUE ~ ppm, data = deltas[deltas$site %in% "GLL",]))
summary(glm(iWUE ~ JJA.p, data = deltas[deltas$site %in% "GLL",]))
summary(glm(iWUE ~ JUNTmax, data = deltas[deltas$site %in% "GLL",]))

summary(glm(iWUE ~ jja.VPDmax, data = deltas[deltas$site %in% "BON",]))
summary(glm(iWUE ~ jul.BAL, data = deltas[deltas$site %in% "BON",]))
summary(glm(iWUE ~ ppm, data = deltas[deltas$site %in% "BON",]))
summary(glm(iWUE ~ JJA.p, data = deltas[deltas$site %in% "BON",]))
summary(glm(iWUE ~ JUNTmax, data = deltas[deltas$site %in% "BON",]))

summary(gam(iWUE ~ ppm + jja.VPDmax, data = deltas[deltas$site %in% "BON",]))
summary(glm(Cor.d13C.suess ~ ppm + jja.VPDmax, data = deltas[deltas$site %in% "BON",]))



# make initial plots of the data
png(height = 4, width = 4, units = 'in', res=300, "outputs/stable_isotopes/Bon_iWUE_time.png")
ggplot(deltas[deltas$Wood %in% "LW" & deltas$year >= 1990,], aes(x = year, y = iWUE, color = ID))+geom_point()+geom_line(data = deltas[deltas$Wood %in% "LW" & deltas$year >= 1990,], aes(x = year, y = iWUE, color = ID))+theme_bw()
dev.off()

png(height = 4, width = 4, units = 'in', res=300, "outputs/stable_isotopes/Bon_iWUE_ppm.png")
ggplot(deltas[deltas$Wood %in% "LW" & deltas$year >= 1990,], aes(x = ppm, y = iWUE,color = ID))+geom_point()+geom_line(data = deltas[deltas$Wood %in% "LW" & deltas$year >= 1990,], aes(x = ppm, y = iWUE,color = ID))+theme_bw()
dev.off()

png(height = 4, width = 4, units = 'in', res=300, "outputs/stable_isotopes/Bon_delta13C_ppm.png")
ggplot(deltas[deltas$Wood %in% "LW"& deltas$year >= 1990,], aes(x = ppm, y = Corr.d13C, color = ID))+geom_point()+geom_line(data = deltas[deltas$Wood %in% "LW"& deltas$year >= 1990,], aes(x = ppm, y = Corr.d13C,color = ID))+theme_bw()
dev.off()

#ggplot(deltas, aes(x = ppm, y = Corr.d13C, color = ID))+geom_point()+geom_line(data= deltas, aes(x = ppm, y = Corr.d13C, color = ID))+theme_bw()


# >>>>>>>>>>>>>>>>>>>>>> Is WUE or delta 13C correlated with ID ring growth? <<<<<<<<<<<<<<<<<<<<<<<
d13 <- read.csv("outputs/stable_isotopes/deltas_13C_WUE_data.csv")
head(d13)
d13$site <- ifelse(d13$site %in% "GLL", "GLL2", as.character(d13$site))
# need to match "ID" with the Tellervo IDS
# hardcoding this for now, but need to come up with a conversion table for this:


growth <- read.csv("outputs/data/rwi_age_dbh_ghcn.df")


d13.growth <- merge(d13, growth, by = c("site", "year", "ID"))


# Since many of the points at BON were of "Past" trees during the 2nd half of the 20th century, lets relabel:
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
d13.growth$class <- NA
#d13.growth[d13.growth$site %in% "BON" & d13.growth$year %in% old.yrs.bon & !d13.growth$ID %in% young.trees.bon,]$class <- "Past"
#d13.growth[d13.growth$site %in% "BON" & d13.growth$year %in% young.yrs.bon & d13.growth$ID %in% young.trees.bon,]$class <-  "Modern"
d13.growth[d13.growth$site %in% "BON" & d13.growth$year %in% old.yrs.bon  & !d13.growth$ID %in% young.trees.bon,]$class <- "Past"
d13.growth[d13.growth$site %in% "BON" & d13.growth$year %in% young.yrs.bon & d13.growth$ID %in% young.trees.bon,]$class <-  "Modern"


d13.growth[d13.growth$site %in% "GLL2" & d13.growth$year %in% old.yrs.gll,]$class <-  "Past"
d13.growth[d13.growth$site %in% "GLL2" & d13.growth$year %in% young.yrs.gll,]$class <- "Modern"
d13.growth[d13.growth$site %in% "MOU" & d13.growth$year %in% old.yrs.mou,]$class <-  "Past"
d13.growth[d13.growth$site %in% "MOU" & d13.growth$year %in% young.yrs.mou,]$class <- "Modern"

d13.growth[d13.growth$site %in% "GLA" & d13.growth$year %in% old.yrs.gla,]$class <-  "Past"
d13.growth[d13.growth$site %in% "GLA" & d13.growth$year %in% young.yrs.gla,]$class <- "Modern"


# >>>>>>>>lets make plots of the relationships between iWUE and RWI overall:<<<<<<<<<<<
ggplot(d13.growth, aes(Cor.d13C.suess, RWI))+geom_point()+stat_smooth(method = "lm")
ggplot(d13.growth, aes(iWUE, RWI))+geom_point()+stat_smooth(method = "lm")

# overall, stomatal closure occurs with summer drought and iWUE is higher in non-drought conditions
ggplot(d13.growth, aes(JJA.pdsi, Cor.d13C.suess))+geom_point()+stat_smooth(method = "lm")
ggplot(d13.growth, aes(JJA.pdsi, iWUE))+geom_point()+stat_smooth(method = "lm")

# similar trends but not as obvious as with PDSI:
ggplot(d13.growth, aes(JUNTmin, Cor.d13C.suess))+geom_point()+stat_smooth(method = "lm")
ggplot(d13.growth, aes(JUNTmin, iWUE))+geom_point()+stat_smooth(method = "lm")

ggplot(d13.growth, aes(PCP, Cor.d13C.suess))+geom_point()+stat_smooth(method = "lm")
ggplot(d13.growth, aes(PCP, iWUE))+geom_point()+stat_smooth(method = "lm")

# plot growth overall agains iwue:
ggplot(d13.growth, aes(RWI, iWUE, color = ageclass))+geom_point()+stat_smooth(method = "lm")
ggplot(d13.growth, aes(RWI, Cor.d13C.suess, color = ageclass))+geom_point()+stat_smooth(method = "lm")


# break it down by site:
ggplot(d13.growth[d13.growth$class %in% c("Past", "Modern"),], aes(iWUE, RWI))+geom_point()+stat_smooth(method = "lm")+facet_wrap(~site)
ggplot(d13.growth, aes(iWUE, RWI))+geom_point()+stat_smooth(method = "lm")+facet_wrap(~site)
ggplot(d13.growth[d13.growth$class %in% c("Past", "Modern"),], aes(iWUE, DBH))+geom_point()+stat_smooth(method = "lm")+facet_wrap(~site)
ggplot(d13.growth[d13.growth$class %in% c("Past", "Modern"),], aes(Cor.d13C.suess, DBH))+geom_point()+stat_smooth(method = "lm")+facet_wrap(~site)
ggplot(d13.growth[d13.growth$class %in% c("Past", "Modern"),], aes(Cor.d13C.suess, Age))+geom_point()+stat_smooth(method = "lm")+facet_wrap(~site)

ggplot(d13.growth, aes( JJA.pdsi, Cor.d13C.suess, color = class))+geom_point()+stat_smooth(method = "lm")
ggplot(d13.growth, aes( JJA.pdsi, iWUE, color = class))+geom_point()+stat_smooth(method = "lm")


# break it down by site and ageclass
ggplot(d13.growth, aes(iWUE, Age, color = class))+geom_point()+stat_smooth(method = "lm")+facet_wrap(~site)
ggplot(d13.growth, aes(iWUE, DBH, color = class))+geom_point()+stat_smooth(method = "lm")+facet_wrap(~site)


ggplot(d13.growth, aes( PCP, Cor.d13C.suess, color = class))+geom_point()+stat_smooth(method = "lm")+facet_wrap(~site)
ggplot(d13.growth, aes( JJA.pdsi, Cor.d13C.suess, color = class))+geom_point()+stat_smooth(method = "lm")+facet_wrap(~site)
ggplot(d13.growth, aes( JUNTmin, Cor.d13C.suess, color = class))+geom_point()+stat_smooth(method = "lm")+facet_wrap(~site)


# relationship between iWUE and climate has different intecepts (and maybe slopes)
ggplot(d13.growth, aes( PCP, iWUE, color = class))+geom_point()+stat_smooth(method = "lm")+facet_wrap(~site)
ggplot(d13.growth, aes( JJA.pdsi, iWUE, color = class))+geom_point()+stat_smooth(method = "lm")+facet_wrap(~site)
ggplot(d13.growth, aes( JUNTmin, iWUE, color = class))+geom_point()+stat_smooth(method = "lm")+facet_wrap(~site)

# relationship between d13C and tree ring growth has different intecepts (and maybe slopes?)
# for the same RWI, WUE is higher in modern than in past, and generally higher RWI is slightly correlated with higher iWUE
ggplot(d13.growth, aes( RWI, iWUE, color = class))+geom_point()+stat_smooth(method = "lm")+facet_wrap(~site)

# for the same RWI, past and modern have different d13C values, though the direction of differences may not be consistant across sites:
ggplot(d13.growth, aes( RWI, Cor.d13C.suess, color = class))+geom_point()+stat_smooth(method = "lm")+facet_wrap(~site)

# lets plot time series of growth and isotopes together:

ggplot(d13.growth, aes( year, Cor.d13C.suess, color = ID))+geom_point()+facet_wrap(~site)+theme(legend.position = "none")
ggplot(d13.growth, aes( year, iWUE, color = ID))+geom_point()+facet_wrap(~site)+theme(legend.position = "none")
ggplot(d13.growth, aes( year, RWI, color = ID))+geom_point()+geom_line()+facet_wrap(~site)+theme(legend.position = "none")

ggplot(d13.growth, aes( Cor.d13C.suess, RWI, color = ID))+geom_point()+stat_smooth(method = "lm", se = FALSE)

ggplot(d13.growth, aes( year, Cor.d13C.suess, color = SpecCode))+geom_point()+facet_wrap(~site)+theme(legend.position = "none")
ggplot(d13.growth, aes( year, iWUE, color = SpecCode))+geom_point()+facet_wrap(~site)+theme(legend.position = "none")
ggplot(d13.growth, aes( year, RWI, color = SpecCode))+geom_point()+geom_line()+facet_wrap(~site)+theme(legend.position = "none")

ggplot(d13.growth, aes( Cor.d13C.suess, RWI, color = ID))+geom_point()+stat_smooth(method = "lm", se = FALSE)

ggplot(d13.growth, aes( JJA.pdsi,RWI, color = ageclass))+geom_point()+stat_smooth(method = "lm", se = FALSE)+facet_wrap(~site)


ggplot(d13.growth, aes( JJA.pdsi, RWI, color = Cor.d13C.suess))+geom_point()+stat_smooth(method = "lm", se = FALSE)+facet_wrap(~site + ageclass)

ggplot(d13.growth, aes( Age, Cor.d13C.suess, color = dbhclass))+geom_point()+stat_smooth(method = "lm", se = FALSE)+facet_wrap(~site + ageclass)

# get an idea if there is any relationship between variables
summary(lm(Cor.d13C.suess ~ RWI , data = d13.growth[!d13.growth %in% "BON",]))
summary(lm(iWUE ~ RWI , data = d13.growth[!d13.growth %in% "BON",]))

summary(lm(Cor.d13C.suess ~ RWI + JJA.pdsi , data = d13.growth[!d13.growth %in% "MOU",]))
summary(lm(iWUE ~ RWI + JJA.pdsi , data = d13.growth[!d13.growth %in% "MOU",]))

summary(lm(Cor.d13C.suess ~ RWI + JJA.pdsi + class , data = d13.growth))
summary(lm(iWUE ~ RWI + JJA.pdsi + class, data = d13.growth))
summary(lm(RWI ~ JJA.pdsi*class*site + class+site, data = d13.growth))

summary(lm(Cor.d13C.suess ~ RWI*site + JJA.pdsi , data = d13.growth))
summary(lm(iWUE ~ RWI*site + JJA.pdsi , data = d13.growth))

summary(lm(Cor.d13C.suess ~ class + site + JJA.pdsi , data = d13.growth))
summary(lm(iWUE ~ class + site + JJA.pdsi , data = d13.growth))


write.csv(d13.growth, "outputs/stable_isotopes/merged_d13_growth.csv")
