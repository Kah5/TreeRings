library(ggplot2)

# this script will calculate WUE from delta13C tree ring isotope measurements
# we need: the delta ATM:
# note that these delta values need to be checked---i used mccarroll and loader until 2003 and filled in to 2011 with data from https://www.nature.com/ngeo/journal/v8/n1/extref/ngeo2313-s1.pdf
# then assume that delta hasnot changed since 2011--to get the code running

deltaATM <- read.csv("data/stable_isotopes/Mccarrol_loader_deltaC_atm.csv") # data from mccarroll and loader patched with recent ppm an dneed to check the delta13atm values
deltaTR<- read.csv("data/stable_isotopes/BON_7a_1996_2011.csv")
deltaTR2 <- read.csv("data/stable_isotopes/BON_9a_13a_1996_2015.csv")

deltaTR <- rbind(deltaTR, deltaTR2)

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
