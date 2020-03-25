library(reshape)
library(ggplot2)
library(sp)
library(rgdal)
library(fields)
library(maptools)
library(neotoma)
require(grid)
require(plyr)
require(maps)
library(gridExtra)
gpclibPermit()
require(maptools)
require(ggplot2)


mip_lon = c(-72.18,-68.73,-89.53,-94.58,-95.17,-82.83)
mip_lat = c(42.54,45.25,46.22,46.28,47.17,43.61)
mip_names = c("PHA","PHO","PUN","PBL","PDL","PMB")

quartz()
map('state', xlim=range(mip_lon)+c(-2, 2), ylim=range(mip_lat)+c(-1, 1))
points(mip_lon, mip_lat, pch=19, cex=1)
text(mip_lon, mip_lat+.5,labels=mip_names)
title(main="PalEON MIP Sites")

quartz()
precip = rnorm(1000,40,15)
hist(precip)
seedling = rgamma(length(precip),300/(1+exp(-.5*(precip-40)))*.1,.05)
seedling1 = rnorm(length(precip),200/(1+exp(-.25*(precip-40)))+200,50)
hist(seedling1)
plot(precip,seedling,pch = 19, cex = .5,
     xlab = "Reconstructed Average Yearly Precipitation (cm)",
     ylab = "Estimated Median Seedling Produced per Tree per Year",
     xlim=c(0,80),ylim=c(0,800))
points(precip,seedling1,col="red",pch = 19, cex = .5)
legend("bottomright",c("Long Lived Species","Short Lived Species"),col=c("black","red"),pch=c(19,19))

conif = sort(rbeta(1000,5,5))
decomp = rnorm(length(conif),20*exp(-5*conif),.5)
hist(decomp)
quartz()
plot(conif,decomp,xlab = "Percent Coniferous",
     ylab = "Litter Decompostion Rate",pch=19,
     cex=.5,col=c(rep("lightgray",100),rep("darkgray",200),
                  rep("black",100),rep("darkgray",300),
                  rep("lightgray",300)))
legend("topright",c("greatest elasticity",
                    "medium elasticity",
                    "small elasticity"),
       pch = rep(19,3),col=c("black","darkgray","lightgray"))




