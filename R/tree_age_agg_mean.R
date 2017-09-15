# Author: kheilman
library(lme4)
library(dplR)
library(reshape2)
library(ggplot2)



# this function calculates tree each for each tree in each year
# then outputs the melted df with year, tree id, age, and RWI
# note, you need to add years as a column for the rwiorbai first
# this saves as a csv but also outputs a df
tree_age_agg_mean <- function(rwiorbai, sampleyear, site.code, age1950,type){

  # calculate record age
  treedata <- data.frame(ID = colnames(rwiorbai),
                         sampleyr = sampleyear)
  
  
  Hic <- data.frame(rwiorbai)
  Hic$year <- row.names(Hic)
  
  
  # Find tree age for each tree at the time of sampling
  for(i in unique(colnames(Hic))){
    treedata[treedata$ID==i, "age"] <- treedata[treedata$ID == i, c("sampleyr")] -  as.numeric( min(Hic[!is.na(Hic[,i]), "year"], na.rm=T))
  }
  summary(treedata)  
  
  
  
  Hic.age <- data.frame(Hic)
  
  # code for calculating tree age in each year
  
  for(i in unique(colnames(Hic))){
    
    firstyr <- as.numeric( min(Hic[!is.na(Hic[,i]), "year"], na.rm=T))
    #Hic.age[Hic.age$year == firstyr,i] <- 1
    yridx <- firstyr:sampleyear
    for (yr in firstyr:sampleyear){
      Hic.age[as.numeric(Hic.age$year)== yr,i] <- yr-firstyr
    }
  }
  
  Hic.age$year <- rownames(Hic)
  
  # plot rwi vs. tree age:
  Age.m <- melt(Hic.age)
  colnames(Age.m) <- c("year", "ID", "Age")
  
  RWI.m <- melt(Hic)
  colnames(RWI.m) <- c("year", "ID", "RWI")
  
  site.m <- merge(Age.m, RWI.m, by = c('year', "ID"))
  
  ggplot(site.m, aes(x = Age, y = RWI, color = ID)) + geom_line()
  
  site.m$year <- as.numeric(site.m$year)
  site.m$ID <- as.character(site.m$ID)
  site.m$ageclass <- "young"
  
  # need to assign old trees then and old trees now
    for (i in unique(site.m$ID)){
      ifelse(site.m[site.m$ID %in% i & site.m$year == 1950,]$Age <= age1950 , site.m[site.m$ID %in% i, ]$ageclass <-  "young",  
             ifelse(site.m[site.m$ID %in% i & site.m$year == 1950,]$Age > age1950 ,site.m[site.m$ID %in% i, ]$ageclass<- "old", site.m[site.m$ID %in% i, ]$ageclass <-  "young"))
                    
    }
  
  
  # calculate mean for each tree age:
  site.mean <- aggregate(RWI ~ Age, data = site.m, mean)
  site.std <- aggregate(RWI ~ Age, data = site.m, sd)
  site.m <- merge(site.mean, site.std, by = "Age")
  colnames(site.m) <- c("Age", "Mean", "Std")
  write.csv(site.m, paste0( "data/tree_growth_age/", site.code, "-raw-meanRWI-age",type, ".csv"))
  site.m
}


# find the mean growth, but average the tree ages based on tree age class--
# trees established before 1920 are "Old" and those established after 1920 are "young"

tree_age_agg_mean_class <- function(rwiorbai, sampleyear, site.code, age1950,type){
  
  # calculate record age
  treedata <- data.frame(ID = colnames(rwiorbai),
                         sampleyr = sampleyear)
  
  
  Hic <- data.frame(rwiorbai)
  Hic$year <- row.names(Hic)
  
  
  # Find tree age for each tree at the time of sampling
  for(i in unique(colnames(Hic))){
    treedata[treedata$ID==i, "age"] <- treedata[treedata$ID == i, c("sampleyr")] -  as.numeric( min(Hic[!is.na(Hic[,i]), "year"], na.rm=T))
  }
  summary(treedata)  
  
  
  
  Hic.age <- data.frame(Hic)
  
  # code for calculating tree age in each year
  
  for(i in unique(colnames(Hic))){
    
    firstyr <- as.numeric( min(Hic[!is.na(Hic[,i]), "year"], na.rm=T))
    #Hic.age[Hic.age$year == firstyr,i] <- 1
    yridx <- firstyr:sampleyear
    for (yr in firstyr:sampleyear){
      Hic.age[as.numeric(Hic.age$year)== yr,i] <- yr-firstyr
    }
  }
  
  Hic.age$year <- rownames(Hic)
  
  # plot rwi vs. tree age:
  Age.m <- melt(Hic.age)
  colnames(Age.m) <- c("year", "ID", "Age")
  
  RWI.m <- melt(Hic)
  colnames(RWI.m) <- c("year", "ID", "RWI")
  
  site.m <- merge(Age.m, RWI.m, by = c('year', "ID"))
  
  ggplot(site.m, aes(x = Age, y = RWI, color = ID)) + geom_line()
  
  site.m$year <- as.numeric(site.m$year)
  site.m$ID <- as.character(site.m$ID)
  site.m$ageclass <- "young"
  
  # need to assign old trees then and old trees now
  for (i in unique(site.m$ID)){
    ifelse(site.m[site.m$ID %in% i & site.m$year == 1950,]$Age <= age1950 , site.m[site.m$ID %in% i, ]$ageclass <-  "young",  
           ifelse(site.m[site.m$ID %in% i & site.m$year == 1950,]$Age > age1950 ,site.m[site.m$ID %in% i, ]$ageclass<- "old", site.m[site.m$ID %in% i, ]$ageclass <-  "young"))
    
  }
  
  
  # calculate mean for each tree age with each ageclasse:
  site.mean <- aggregate(RWI ~ Age + ageclass, data = site.m, mean)
  site.std <- aggregate(RWI ~  Age + ageclass, data = site.m, sd)
  site.m <- merge(site.mean, site.std, by = c("Age", "ageclass"))
  colnames(site.m) <- c("Age", "Ageclass", "Mean", "Std")
  
  # write to a csv
  write.csv(site.m, paste0( "data/tree_growth_age/", site.code, "-raw-meanRWI-byageclass",type, ".csv"))
  site.m
}

# claculate mean growth by pith date
tree_pith_agg_mean <- function(rwiorbai, sampleyear, site.code, age1950,type){
  
  # calculate record age
  treedata <- data.frame(ID = colnames(rwiorbai),
                         sampleyr = sampleyear)
  
  
  Hic <- data.frame(rwiorbai)
  Hic$year <- row.names(Hic)
  
  
  # Find tree age and pith date for each tree at the time of sampling
  for(i in unique(colnames(Hic))){
    treedata[treedata$ID==i, "age"] <- treedata[treedata$ID == i, c("sampleyr")] -  as.numeric( min(Hic[!is.na(Hic[,i]), "year"], na.rm=T))
    treedata[treedata$ID==i, "pith"] <- as.numeric( min(Hic[!is.na(Hic[,i]), "year"], na.rm=T))
  }
  
  summary(treedata)  
  
  
  
  Hic.age <- data.frame(Hic)
  
  # code for calculating tree age in each year
  
  for(i in unique(colnames(Hic))){
    
    firstyr <- as.numeric( min(Hic[!is.na(Hic[,i]), "year"], na.rm=T))
    #Hic.age[Hic.age$year == firstyr,i] <- 1
    yridx <- firstyr:sampleyear
    for (yr in firstyr:sampleyear){
      Hic.age[as.numeric(Hic.age$year)== yr,i] <- yr-firstyr
    }
  }
  
  Hic.age$year <- rownames(Hic)
  
  # plot rwi vs. tree age:
  Age.m <- melt(Hic.age)
  colnames(Age.m) <- c("year", "ID", "Age") 
  Age.m2<- merge(Age.m, treedata[,c("ID", "pith")], by = "ID")
  colnames(Age.m2) <- c( "ID", "year","Age", "Pith")
  
  RWI.m <- melt(Hic)
  colnames(RWI.m) <- c("year", "ID", "RWI")
  
  site.m <- merge(Age.m2, RWI.m, by = c('year', "ID"))
  
  ggplot(site.m, aes(x = Pith, y = RWI, color = ID)) + geom_boxplot()
  
  site.m$year <- as.numeric(site.m$year)
  site.m$ID <- as.character(site.m$ID)
  site.m$ageclass <- "young"
  
  # need to assign old trees then and old trees now
  for (i in unique(site.m$ID)){
    ifelse(site.m[site.m$ID %in% i & site.m$year == 1950,]$Age <= age1950 , site.m[site.m$ID %in% i, ]$ageclass <-  "young",  
           ifelse(site.m[site.m$ID %in% i & site.m$year == 1950,]$Age > age1950 ,site.m[site.m$ID %in% i, ]$ageclass<- "old", site.m[site.m$ID %in% i, ]$ageclass <-  "young"))
    
  }
  
  
  # calculate mean for each tree age:
  site.mean <- aggregate(RWI ~ Pith, data = site.m, mean)
  site.std <- aggregate(RWI ~ Pith, data = site.m, sd)
  site.m <- merge(site.mean, site.std, by = "Pith")
  colnames(site.m) <- c("Pith", "Mean", "Std")
  write.csv(site.m, paste0( "data/tree_growth_age/", site.code, "-raw-meanRWI-pith",type, ".csv"))
  site.m
  
}

