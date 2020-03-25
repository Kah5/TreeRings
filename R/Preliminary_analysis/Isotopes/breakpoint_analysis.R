# Breakpoint analysis to determine when savanna trees growth rate changed
# Kelly Heilman
# Created: August 27th, 2016
library(segmented)
library(strucchange)


#need to work on this more...
#We have identified that pre- and post 1950 growth is different in savannas (post-1950 is greater than pre), 
# but we want to use this analysis to determine where exactly growth shifted, and if this is consistant across all savannas
Hickory <- read.tucson ("./cofecha/HICww.rwl")
HIC.stats <- rwi.stats(Hickory)

#create basal area increment dataset from hickory
Hickory.bai <- data.frame(bai.out(Hickory))
Hickory.bai <- cbind(Year = as.numeric(rownames(Hickory.bai)), Hickory.bai)

Hickory.rwi <- detrend(rwl = Hickory, method = "ModNegExp")
Hickory <- chron(Hickory.rwi)            
#plot(Hickory)

HiC<- as.list(Hickory.bai)

data('Nile')
plot(Nile)

trees<- colnames(Hickory.bai)
bkpts1 <- as.matrix(0, 37,1)

#find the breakpoint of HICa698--this only works for records that start at 1850--using Na.trim we can rempnve this
pdf('outputs/break_points_hic.pdf')
for(i in 2:37){
#x <- as.ts(Hickory.bai[i], Hickory.bai$Year)
x <- na.trim(as.ts(Hickory.bai[i], Hickory.bai$Year, start=c(1850, 1)))


plot(x, main = trees[i])
#id breakpoints using strucchange
bp.hic <- breakpoints(x~1)
ci.hic <- confint(bp.hic, breaks = 2)
lines(ci.hic)

summary(bp.hic)


## fit and visualize segmented and unsegmented model
#fm0 <- lm(x ~ 1)
fm1 <- lm(x ~ breakfactor(bp.hic, breaks = 2))
lines(fitted(fm0), col = 3)
lines(fitted(fm1), col = 4)
lines(bp.hic, breaks = 2)
bkpts1[i] <- breakdates(bp.hic) # add all breakpts to a vector
}
dev.off()

#now plot a histogram of breakpts:
hist(bkpts1, breaks = 10)


