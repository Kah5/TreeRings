#Correlating ring widths to precip in Illinois
library(dplR)


#read in ring width file
HIC.test <- read.tucson("./HIC/Tellervo export project-a(1).rwl", header = T)

#read in Illinois precipitation file
ILp <- read.csv("IL_monthly_PT.csv")

ILp.Marengo <- ILp[ILp$STATION_NAME == "MARENGO IL US",]

ILp.Marengo 
keeps <- c("STATION_NAME", "YEAR", "Month",  "TPCP")
ILp.df <- ILp.Marengo[,keeps]
ILp.df[ILp.df == -9999]<- NA

library(plyr)
total.p <- aggregate(TPCP~YEAR, data=ILp.df, FUN=sum, na.rm = T) 
HIC.test
