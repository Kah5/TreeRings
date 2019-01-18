# read in peak flow
pkflow <- read.table("peak_flow_algonquin.txt", header = TRUE)

#split date string into 3 columns
library("data.table")
pkflow <- data.table(pkflow)

# split peak_dt into new columns
pkflow$Year = as.character(lapply(strsplit(as.character(pkflow$peak_dt), split="-"), "[", 1))
pkflow$Month = as.character(lapply(strsplit(as.character(pkflow$peak_dt), split="-"), "[", 2))
pkflow$Day = as.character(lapply(strsplit(as.character(pkflow$peak_dt), split="-"), "[", 3))

pkflow <- data.frame(pkflow)

#read in false rings
false <- read.csv("HIC_false_rings.csv")
full <- merge(false, pkflow, by = "Year")

#extended flood stage would be a better estimate, but one thing to know i
plot(full$Month, full$Total)
plot(full$Total, full$Year)
plot(full$Year, full$Total)
plot(full$Total, full$peak_va)


#read in daily flow
cfs <- read.table("daily_algonquin_streamflow.txt", header = TRUE)
plot(cfs$discharge, type = "l")

#from this we should get the 90th percentile of streamflow, and see which years this occurs in
#additionally looking at the period of hight discharge events

