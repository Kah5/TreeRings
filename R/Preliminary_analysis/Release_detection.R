# This script finds potential releases within a tree using the TRADER package:
# Altman J, Fibich P, Dolezal J & Aakala T (2014). TRADER: a package for Tree Ring Analysis of Disturbance Events in R. Dendrochonologia 32: 107-112, URL http://www.sciencedirect.com/science/article/pii/S1125786514000058 .
# This script also uses dplR package

library(TRADER)
library(ggplot2)
library(dplR)

# change this to the folder with your RWL files:
cleaned.rwl.file.folder <- "/Users/kah/Documents/TreeRings/cleanrwl/"

# read in tree ring files of interest:
files <- list.files(cleaned.rwl.file.folder, pattern = ".rwl")

# read each rwl file, find the mean for each tree, and name the robject XXXww.rwl 
for (i in seq_along(files)) {
  
  temp <- read.rwl(paste0(cleaned.rwl.file.folder, files[i])) # read all rwl files
  temp <- treeMean(temp, autoread.ids(temp), na.rm=TRUE) # need to combine cores from the same tree:
  assign(paste(files[i]), temp) # assign each series to a name defined by its site
  
}


# use the growthAverageALL function to find potential releases in trees
# this uses the Growth averageing method (see Nowacki and Abrams 2007):
#
#%GC = [(M2 − M1 )/M1 ] × 100. The minimum thresholds applied is a 50% growth change and >75% for a major release

# set dir to wd that we want to save all the files in:
setwd("/Users/kah/Documents/TreeRings/outputs/release/")
 
# with this function, m1 = # period before release, m2 = # years in period before release, and buffer = # years to average growth over
# criteria = the porportional increase in growth needed to be classified as a minor release
# criteria2 = the proportional increase in growth needed to be classifed as a major release.
# note that Nowacki and Abrams growth averaging method may actually overestimate the releases detected because of the low criteria estimates



# use the growthAveragenALL function to find releases based on Nowacki and Abrams growth averageing
growthAveragingALL(BONww.rwl , releases=NULL, m1=10, m2=10, buffer=10, drawing=TRUE, criteria=0.25, criteria2=0.5, prefix="BON", gfun=mean, length=5)
growthAveragingALL(TOWww.rwl , releases=NULL, m1=10, m2=10, buffer=10, drawing=TRUE, criteria=0.25, criteria2=0.5, prefix='TOW', gfun=mean, length=5)
growthAveragingALL(STCww.rwl , releases=NULL, m1=10, m2=10, buffer=10, drawing=TRUE, criteria=0.25, criteria2=0.5, prefix='STC', gfun=mean, length=5)
growthAveragingALL(PVCww.rwl , releases=NULL, m1=10, m2=10, buffer=10, drawing=TRUE, criteria=0.25, criteria2=0.5, prefix='PVC', gfun=mean, length=5)
growthAveragingALL(HICww.rwl , releases=NULL, m1=10, m2=10, buffer=10, drawing=TRUE, criteria=0.25, criteria2=0.5, prefix='HIC', gfun=mean, length=5)
growthAveragingALL(CORww.rwl , releases=NULL, m1=10, m2=10, buffer=10, drawing=TRUE, criteria=0.25, criteria2=0.5, prefix='COR', gfun=mean, length=5)
growthAveragingALL(ENGww.rwl , releases=NULL, m1=10, m2=10, buffer=10, drawing=TRUE, criteria=0.25, criteria2=0.5, prefix='ENG', gfun=mean, length=5)
growthAveragingALL(GLAww.rwl , releases=NULL, m1=10, m2=10, buffer=10, drawing=TRUE, criteria=0.25, criteria2=0.5, prefix='GLA', gfun=mean, length=5)
growthAveragingALL(GLL1ww.rwl , releases=NULL, m1=10, m2=10, buffer=10, drawing=TRUE, criteria=0.25, criteria2=0.5, prefix='GLL1', gfun=mean, length=5)
growthAveragingALL(GLL2ww.rwl , releases=NULL, m1=10, m2=10, buffer=10, drawing=TRUE, criteria=0.25, criteria2=0.5, prefix='GLL2', gfun=mean, length=5)
growthAveragingALL(GLL3ww.rwl , releases=NULL, m1=10, m2=10, buffer=10, drawing=TRUE, criteria=0.25, criteria2=0.5, prefix='GLL3', gfun=mean, length=5)
growthAveragingALL(GLL4ww.rwl , releases=NULL, m1=10, m2=10, buffer=10, drawing=TRUE, criteria=0.25, criteria2=0.5, prefix='GLL4', gfun=mean, length=5)
growthAveragingALL(HICww.rwl , releases=NULL, m1=10, m2=10, buffer=10, drawing=TRUE, criteria=0.25, criteria2=0.5, prefix='HIC', gfun=mean, length=5)
growthAveragingALL(MOUww.rwl , releases=NULL, m1=10, m2=10, buffer=10, drawing=TRUE, criteria=0.25, criteria2=0.5, prefix='MOU', gfun=mean, length=5)
growthAveragingALL(PLEww.rwl , releases=NULL, m1=10, m2=10, buffer=10, drawing=TRUE, criteria=0.25, criteria2=0.5, prefix='PLE', gfun=mean, length=5)
growthAveragingALL(UNCww.rwl , releases=NULL, m1=10, m2=10, buffer=10, drawing=TRUE, criteria=0.25, criteria2=0.5, prefix='UNC', gfun=mean, length=5)


#-------------------- quantifying stand level vs. individual releases:
# the above methods give a value for a release in each tree for each year. 
# since releases might occur within a single tree, or might be representative of a stand level release, we can explore this here
setwd("/Users/kah/Documents/TreeRings/")

files <- list.files("/Users/kah/Documents/TreeRings/outputs/release/",pattern = "releases_years_total.csv")

df.rel <- list()

# define a # of trees in the plot that need to have a release in a given year for that year to be classifed as a "stand level release"
standreleasecriteria <- 3 # here I call it 3, but it could be anything ecologically meaningful

# create a for loop that extracts all of the release summaries, finds any releases that occur in more than 2 trees, and creates a list of all sites
for (i in seq_along(files)) {
  
  temp <- read.csv(paste0("/Users/kah/Documents/TreeRings/outputs/release/",files[i])) # read all release summary files
  temp$standrelease <- ifelse(temp$NumberOfAllReleses > standreleasecriteria, "Yes", "No" )
  temp$site <- strsplit(files[i], "_")[[1]][1] # take the site name from the file name
  
  df.rel[[i]] <- temp # add each site to a single df
  
}
# make df.rel into a dataframe
df.rel <- do.call(rbind, df.rel)

# plot the release years in each site:
png(height = 7, width = 9, units =  "in",res=300,"outputs/release/full_site_releases_barplot.png")
ggplot(data = df.rel, aes(AllReleasesYear, NumberOfAllReleses, fill = standrelease))+geom_bar(stat = "identity")+scale_fill_manual(values=c("blue", "red"))+facet_wrap(~site)+theme_bw()
dev.off()
# save df.rel 
write.csv(df.rel, "outputs/release/full_site_releases.csv")


# based on conversations with Neil Pederson & discussion @ world dendro, need to characterize disturbances using the lorimer method:
#minor release = 50 - 99% increase in growth over 15 years compared to the previous 15 years
# major releast = > 100% increase in growth over 15 years compared to the previous 15 years

setwd("/Users/kah/Documents/TreeRings/outputs/release/lorimer")
# use the growthAveragenALL function to find releases based on Nowacki and Abrams growth averageing
growthAveragingALL(BONww.rwl , releases=NULL, m1=15, m2=15, buffer=1, drawing=TRUE, criteria=0.5, criteria2=1, prefix="BON", gfun=mean, length=5)
growthAveragingALL(TOWww.rwl , releases=NULL, m1=15, m2=15, buffer=1, drawing=TRUE,  criteria=0.5, criteria2=1, prefix='TOW', gfun=mean, length=5)
growthAveragingALL(STCww.rwl , releases=NULL, m1=15, m2=15, buffer=1, drawing=TRUE,  criteria=0.5, criteria2=1, prefix='STC', gfun=mean, length=5)
growthAveragingALL(PVCww.rwl , releases=NULL, m1=15, m2=15, buffer=1, drawing=TRUE,  criteria=0.5, criteria2=1, prefix='PVC', gfun=mean, length=5)
growthAveragingALL(HICww.rwl , releases=NULL, m1=15, m2=15, buffer=1, drawing=TRUE,  criteria=0.5, criteria2=1, prefix='HIC', gfun=mean, length=5)
growthAveragingALL(CORww.rwl , releases=NULL, m1=15, m2=15, buffer=1, drawing=TRUE,  criteria=0.5, criteria2=1, prefix='COR', gfun=mean, length=5)
growthAveragingALL(ENGww.rwl , releases=NULL, m1=15, m2=15, buffer=1, drawing=TRUE,  criteria=0.5, criteria2=1, prefix='ENG', gfun=mean, length=5)
growthAveragingALL(GLAww.rwl , releases=NULL, m1=15, m2=15, buffer=1, drawing=TRUE,  criteria=0.5, criteria2=1, prefix='GLA', gfun=mean, length=5)
growthAveragingALL(GLL1ww.rwl , releases=NULL,m1=15, m2=15, buffer=1, drawing=TRUE,  criteria=0.5, criteria2=1, prefix='GLL1', gfun=mean, length=5)
growthAveragingALL(GLL2ww.rwl , releases=NULL, m1=15, m2=15, buffer=1, drawing=TRUE,  criteria=0.5, criteria2=1, prefix='GLL2', gfun=mean, length=5)
growthAveragingALL(GLL3ww.rwl , releases=NULL, m1=15, m2=15, buffer=1, drawing=TRUE,  criteria=0.5, criteria2=1, prefix='GLL3', gfun=mean, length=5)
growthAveragingALL(GLL4ww.rwl , releases=NULL, m1=15, m2=15, buffer=1, drawing=TRUE,  criteria=0.5, criteria2=1, prefix='GLL4', gfun=mean, length=5)
growthAveragingALL(HICww.rwl , releases=NULL, m1=15, m2=15, buffer=1, drawing=TRUE,  criteria=0.5, criteria2=1, prefix='HIC', gfun=mean, length=5)
growthAveragingALL(MOUww.rwl , releases=NULL, m1=15, m2=15, buffer=1, drawing=TRUE,  criteria=0.5, criteria2=1, prefix='MOU', gfun=mean, length=5)
growthAveragingALL(PLEww.rwl , releases=NULL, m1=15, m2=15, buffer=1, drawing=TRUE,  criteria=0.5, criteria2=1, prefix='PLE', gfun=mean, length=5)
growthAveragingALL(UNCww.rwl , releases=NULL, m1=15, m2=15, buffer=1, drawing=TRUE,  criteria=0.5, criteria2=1, prefix='UNC', gfun=mean, length=5)




setwd("/Users/kah/Documents/TreeRings/")

files <- list.files("/Users/kah/Documents/TreeRings/outputs/release/lorimer/",pattern = "releases_years_total.csv")

df.rel <- list()
# create a for loop that extracts all of the release summaries, finds any releases that occur in more than 2 trees, and creates a list of all sites
for (i in seq_along(files)) {
  
  temp <- read.csv(paste0("/Users/kah/Documents/TreeRings/outputs/release/lorimer/",files[i])) # read all release summary files
  temp$standrelease <- ifelse(temp$NumberOfAllReleses > 2, "Yes", "No" )
  temp$site <- strsplit(files[i], "_")[[1]][1] # take the site name from the file name
  
  df.rel[[i]] <- temp # add each site to a single df
  
}
# make df.rel into a dataframe
df.rel <- do.call(rbind, df.rel)

# plot the release years in each site:
png(height = 7, width = 9, units =  "in",res=300,"outputs/release/lorimer/full_site_releases_barplot_lorimer_method.png")
ggplot(data = df.rel, aes(AllReleasesYear, NumberOfAllReleses, fill = standrelease))+geom_bar(stat = "identity")+scale_fill_manual(values=c("blue", "red"))+facet_wrap(~site)+theme_bw()
dev.off()

# save df.rel 
write.csv(df.rel, "outputs/release/lorimer/full_site_releases_lorimer.csv", row.names = FALSE)




