# This script finds potential releases within a tree:
library(TRADER)


# read in tree ring files of interest:
files <- list.files("/Users/kah/Documents/TreeRings/cleanrwl/",pattern = ".rwl")

# read each rwl file, find the mean for each tree, and name the robject XXXww.rwl 
for (i in seq_along(files)) {
  temp <- read.rwl(paste0("/Users/kah/Documents/TreeRings/cleanrwl/",files[i])) # read all rwl files
  temp <- treeMean(temp, autoread.ids(temp), na.rm=TRUE) # need to combine cores from the same tree:
  assign(paste(files[i]), temp) # assign each series to a name defined by its site
  
}


# use the growthAverageALL function to find potential releases in trees
# this uses the Growth averageing method (Nowacki and Abrams 2007):
#
#%GC = [(M2 − M1 )/M1 ] × 100. The minimum thresholds applied is a 50% growth change and >75% for a major release

# set dir to wd that we want to save all the files in:
setwd("/Users/kah/Documents/TreeRings/outputs/release/")
 
dfs <- c(BONww.rwl, TOWww.rwl, STCww.rwl, PVCww.rwl)


growthAveragingALL(BONww.rwl , releases=NULL, m1=10, m2=10, buffer=10, drawing=TRUE, criteria=0.5, criteria2=0.75, prefix=paste(df[i]), gfun=mean, length=5)
growthAveragingALL(TOWww.rwl , releases=NULL, m1=10, m2=10, buffer=10, drawing=TRUE, criteria=0.5, criteria2=0.75, prefix='TOW', gfun=mean, length=5)
growthAveragingALL(STCww.rwl , releases=NULL, m1=10, m2=10, buffer=10, drawing=TRUE, criteria=0.5, criteria2=0.75, prefix='STC', gfun=mean, length=5)
growthAveragingALL(PVCww.rwl , releases=NULL, m1=10, m2=10, buffer=10, drawing=TRUE, criteria=0.5, criteria2=0.75, prefix='PVC', gfun=mean, length=5)
growthAveragingALL(HICww.rwl , releases=NULL, m1=10, m2=10, buffer=10, drawing=TRUE, criteria=0.5, criteria2=0.75, prefix='HIC', gfun=mean, length=5)
growthAveragingALL(PVCww.rwl , releases=NULL, m1=10, m2=10, buffer=10, drawing=TRUE, criteria=0.5, criteria2=0.75, prefix='GLA', gfun=mean, length=5)
growthAveragingALL(PVCww.rwl , releases=NULL, m1=10, m2=10, buffer=10, drawing=TRUE, criteria=0.5, criteria2=0.75, prefix='GLL1', gfun=mean, length=5)
growthAveragingALL(PVCww.rwl , releases=NULL, m1=10, m2=10, buffer=10, drawing=TRUE, criteria=0.5, criteria2=0.75, prefix='GLL2', gfun=mean, length=5)
