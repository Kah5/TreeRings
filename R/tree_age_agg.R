# Author: kheilman
library(lme4)
library(dplR)
library(reshape2)
library(ggplot2)



# this function calculates tree each for each tree in each year
# then outputs the melted df with year, tree id, age, and RWI
# note, you need to add years as a column for the rwiorbai first
# this saves as a csv but also outputs a df
tree_age_agg <- function(rwiorbai, site.code, age1950, type){

  
  Hic <- as.data.frame(rwiorbai)
  Hic$year <- row.names(Hic)
  sampleyear <- as.numeric(max(Hic$year))
  Hic <- Hic[,!is.na(colnames(Hic)) ]
  
  # calculate record age
  treedata <- data.frame(ID = colnames(Hic),
                         sampleyr = sampleyear)
  
 
  # Find tree age for each tree at the time of sampling
  for(i in unique(colnames(Hic[,1:(length(Hic)-2)]))){
    treedata[treedata$ID==i, "age"] <- treedata[treedata$ID == i, c("sampleyr")] -  as.numeric( min(Hic[!is.na(Hic[,i]), "year"], na.rm=T))
  }
  summary(treedata)  
  
  
  
  Hic.age <- data.frame(Hic)
  
  # code for calculating tree age in each year
  
  for(i in unique(colnames(Hic[,1:(length(Hic)-2)]))){
    
    firstyr <- as.numeric( min(Hic[!is.na(Hic[,i]), "year"], na.rm=T))
    #Hic.age[Hic.age$year == firstyr,i] <- 1
    yridx <- firstyr:sampleyear
    for (yr in firstyr:sampleyear){
      Hic.age[as.numeric(Hic.age$year) == yr,i] <- yr-firstyr
    }
  }
  
  Hic.age$year <- rownames(Hic)
  
  # plot rwi vs. tree age:
  Age.m <- melt(Hic.age)
  colnames(Age.m) <- c("year","site", "ID", "Age")
  
  RWI.m <- melt(Hic)
  colnames(RWI.m) <- c("year", "site","ID", "RWI")
  
  site.m <- merge(Age.m, RWI.m, by = c('year', "site","ID"))
  
  ggplot(site.m, aes(x = Age, y = RWI, color = ID)) + geom_line()
  
  site.m$year <- as.numeric(site.m$year)
  site.m$ID <- as.character(site.m$ID)
  site.m$ageclass <- "Modern"
  site.code <- unique(site.m$site)
  # need to assign old trees then and old trees now
    for (i in unique(site.m$ID)){
      ifelse(site.m[site.m$ID %in% i & site.m$year == 1950,]$Age <= age1950 , site.m[site.m$ID %in% i, ]$ageclass <-  "Modern",  
             ifelse(site.m[site.m$ID %in% i & site.m$year == 1950,]$Age > age1950 ,site.m[site.m$ID %in% i, ]$ageclass<- "Past", site.m[site.m$ID %in% i, ]$ageclass <-  "Modern"))
                    
    }
  write.csv(site.m, paste0( "data/tree_growth_age/", site.code, "-",type, ".csv"))
  site.m
}


