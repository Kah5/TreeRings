library(ggplot2)

# this script will calculate WUE from delta13C tree ring isotope measurements
# we need: the delta ATM:
# note that these delta values need to be checked---i used mccarroll and loader until 2003 and filled in to 2011 with data from https://www.nature.com/ngeo/journal/v8/n1/extref/ngeo2313-s1.pdf
# then assume that delta hasnot changed since 2011--to get the code running

deltaATM <- read.csv("data/stable_isotopes/Mccarrol_loader_deltaC_atm.csv")
deltaTR<- read.csv("data/stable_isotopes/BON_7a_1996_2011.csv")

deltas <- merge(deltaTR, deltaATM, by = "Year")

# now calculate the WUE:
Ca*(1-(Corr.d13C-delta13C + a))/(b-a)*0.625