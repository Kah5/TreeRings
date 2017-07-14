# Author: Kelly Heilman
# This Script goes over the basics of detrending using the dplr package
# clear environment before using:
rm(list=ls())

#load packages:
library(dplR)
library(reshape2)
library(ggplot2)
library(plyr)

#use the "read.tucson" function to to read in the rwl
treeringfile <- read.tucson("./cofecha/BONww.rwl", header = TRUE)


#change site --need to run this script for each site. It will output correlation coeffeiencts and save them in csv
site <- treeringfile # assigns site as the same object as bonanaza rwl file
site.code <- "BON" # for naming purposes

###########################################
# Basic Plotting Functions in dplR package#
###########################################

#plots barplots of record length per core
plot (site)

#plots "spagetti plots of the individual cores
plot(site, plot.type="spag")


# first we will look at the series from just one core:
series <- site[, "BON13a"] # extract just one series
names(series) <- rownames(site) # give it years as rownames

# here we plot the series using the base package R plotting
plot(series, type = 'l', 
     ylab = "Year", 
     xlab = "Raw Tree Ring Width", 
     main = "Raw Tree Ring Width")

# we can also look at descriptive stats for Bonanaza using rwl.stats()
rwl.stats(site[,1:10])

# notice that this record has very high growth at the beginning and a year of very high growth in the middle
# The Growth at the beginning of the record is often thought of as age related fast growth
# The growth in the middle may be in response to disturbances or climate
# We want to see how well the trees respond to annual variations in climate, so we want to remove the age trends
# Removing thes long term trends to yield annual variations is called detrending.


# There are different methods of detrending & sometimes they produce different results
X11(width = 12) # open a new X11 window to view plots

# detrend.series() is a function that will detrend an individual series using all possible methods
# setting verbose = TRUE in this function will print the parameters estimated for each model
# it will also plot the raw series with the lines from each fit
# It prints the resulting detrended ring width series for each method
series.rwi <- detrend.series(y = series, y.name = "BON13a", verbose=TRUE)



# we can then detrend an entire site using the same method using the detrend function()
# this function uses detrend.series on all the series in the rwl file
site.code.rwi <- detrend(rwl = site, method = "Friedman")

# in reality, we should make sure that the method we use to detrend is apppriate,
# but for an intro, we will just assume a Friedman is appropriate. 

# once we have all of the series in the site detrended, we can create a chronology
site.code.crn <- chron(site.code.rwi, prefix = paste(site.code))
# a chronology can be thought of as an average of the detrended growth for each year 

# the site.code.crn object now has two columns, BONstd (the value for chronology) and samp.depth (the )
head(site.code.crn)

# We can then plot the detrended chronology. This plot also has sample depth on it
crn.plot(site.code.crn, add.spline = TRUE)

# for refrence, this is what the plot would look like if we did not detrend:
Trended.crn <- chron(site, prefix = paste(site.code))
crn.plot(Trended.crn, add.spline = TRUE)

# create a column that for the years
site.code.crn$Year <- rownames(site.code.crn)
#write the detrend chronology to text: This allows us to use it later
write.csv(site.code.crn, paste0('data/crn/',site.code, "-crn.csv"))
write.csv(site.code.rwi, paste0('data/crn/',site.code, "-rwi.csv"))
