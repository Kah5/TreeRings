# This Script goes over the basics of detrending using the dplr package
# clear environment before using:
rm(list=ls())

#load packages:
library(dplR)
library(reshape2)
library(ggplot2)
library(plyr)

#use the "read.tucson" function to to read in the rwl
Bonanza <- read.tucson("./cofecha/BONww.rwl", header = TRUE)


#change site --need to run this script for each site. It will output correlation coeffeiencts and save them in csv
site <- Bonanza # assigns site as the same object as bonanaza rwl file
site.code <- "BON" # for naming purposes

###########################################
# Basic Plotting Functions in dplR package#
###########################################

#plots barplots of record length per core
plot (site)

#plots "spagetti plots of the individual cores
plot(site, plot.type="spag")

#there are different methods of detrending:
X11(width = 12) # open a new X11 window to view plots
site.code.rwi <- detrend(rwl = site, method = "Spline")
plot(site.code.rwi, plot.type = "spag")
