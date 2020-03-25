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
  
  # get previous years growth
  Hic.prev <- Hic[1:(length(Hic$year)-1),]
  Hic.prev$year <- Hic[2:(length(Hic$year)),]$year # assign previous year growth 
  
  RWI.prev <- melt(Hic.prev)
  colnames(RWI.prev) <- c("year", "site","ID", "RWI_1")
  
  
  # while we are at it, get the year - 2 years growth:
  Hic.prev2 <- Hic[1:(length(Hic$year)-2),]
  Hic.prev2$year <- Hic[3:(length(Hic$year)),]$year # assign previous year growth 
  
  RWI.prev2 <- melt(Hic.prev2)
  colnames(RWI.prev2) <- c("year", "site","ID", "RWI_2")
  
  # while we are at it, get the year - 3 years growth:
  Hic.prev3 <- Hic[1:(length(Hic$year)-3),]
  Hic.prev3$year <- Hic[4:(length(Hic$year)),]$year # assign previous year growth 
  
  RWI.prev3 <- melt(Hic.prev3)
  colnames(RWI.prev3) <- c("year", "site","ID", "RWI_3")
  
  # now merge at and RWI together
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
  
  # add on the previous years growth:
  site.m2<- merge(site.m, RWI.prev, by = c("year", "site", "ID"), all.x = TRUE)
  site.m2<- merge(site.m2, RWI.prev2, by = c("year", "site", "ID"), all.x = TRUE)
  site.m<- merge(site.m2, RWI.prev3, by = c("year", "site", "ID"), all.x = TRUE)
  
  write.csv(site.m, paste0( "data/tree_growth_age/", site.code, "-",type, ".csv"))
  site.m
}


