# R script for mapping out moisture availability at the sites (mean) and over time

# Moisture Indices that might be useful to look at, and their data sources:
# 1. VPD--From PRISM data
# 2. PDSI--From gridded GHCN data
# 3. SPI (standardized precipitation index)
#   -1, -12, -24 month integrated values
#   - Note these values are extracted from the datset in climate_growth_reg_chron.R and saved in outpus/data
# 4. Monthly climatic water balance: P-ET (calculate from Prism data)
# 5. SPEI (from Monthly climate water balance)


# 3. SPI (Standardized precipitation idex) -----------------------------------------------------------------------
# read in the moisture data from outputs/data and merge into same df

setwd("data/PRISM/")

file_list <- list.files()

for (file in file_list){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- read.csv(file, header=TRUE, skip = 10) # csv has 10 line header
    dataset$site <- unlist(strsplit(file, "_"))[4] # pull out the site name
    # reformat Date into Month and year columns
    
    
    #  We know the dataframe should have 4 columns m <- matrix( elems , ncol = 4 , byrow = TRUE )
    elems <- unlist(strsplit(as.character(dataset$Date), "-"))
    m <- data.frame(matrix( elems , ncol = 2, byrow = TRUE ))
    colnames(m) <- c("Year", "Month")
    dataset <- data.frame(m, dataset)
    
    
  }
  
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    temp_dataset <-read.csv(file, header=TRUE, skip =10)
    temp_dataset$site <- unlist(strsplit(file, "_"))[4] # pull out the site name
    # reformat Date into Month and year columns
    elems <- unlist(strsplit(as.character(temp_dataset$Date), "-"))
    m <- data.frame(matrix( elems , ncol = 2, byrow = TRUE ))
    colnames(m) <- c("Year", "Month")
    temp_dataset <- data.frame(m, temp_dataset)
    
    SP.dataset<-rbind(dataset, temp_dataset)
    rm(temp_dataset)
  }
  
}


# for AVO:
test <- dataset[dataset$site %in% "AVO",]
mydata <- test[test$Month == '07' ,4:7]
mydata2 <- test[test$Month == '07' ,]

# calculate sum of squares to make the elbow plots:
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,sd))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)

#png("outputs/cluster/ncluster_within_ssq.png")
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Optimal Number of Clusters with the Elbow Method",
     pch=20, cex=2)
#dev.off()

set.seed(7)
km2 = kmeans(mydata, 2, nstart=100)
km5 = kmeans(mydata, 5, nstart = 100)
km6 = kmeans(mydata, 6, nstart = 100)
km3 = kmeans(mydata, 3, nstart = 100)
km4 = kmeans(mydata, 4, nstart = 100)
km7 = kmeans(mydata, 7, nstart = 100)
km8 = kmeans(mydata, 8, nstart = 100)

# Examine the result of the clustering algorithm
km2

# add the cluster values to the "newdf"
mydata2$cluster2 <- km2$cluster
mydata2$cluster3 <- km3$cluster
mydata2$cluster4 <- km4$cluster
mydata2$cluster5 <- km5$cluster
mydata2$cluster6 <- km6$cluster
mydata2$cluster7 <- km7$cluster
mydata2$cluster8 <- km8$cluster

# plot out clusters by year:
ggplot(mydata2, aes(x=as.numeric(Year), y=cluster2, color = as.character(cluster2)))+geom_point()
ggplot(test, aes(x=as.numeric(Year), y=as.numeric(Month), color = as.character(cluster3)))+geom_point()
ggplot(test, aes(x=as.numeric(Year), y=as.numeric(Month), color = as.character(cluster4)))+geom_point()
ggplot(test, aes(x=as.numeric(Year), y=as.numeric(Month), color = as.character(cluster5)))+geom_point()




# setwd back:
setwd("/Users/kah/Documents/TreeRings")

df.01 <- data.frame(Year = SP.dataset$Year, Month = SP.dataset$Month,SP.dataset %>% dplyr:: select(grep("01", names(SP.dataset)), grep("01", names(SP.dataset))))
df.02 <- SP.dataset %>% dplyr:: select(grep("02", names(SP.dataset)), grep("02", names(SP.dataset)))
df.06 <- SP.dataset %>% dplyr:: select(grep("06", names(SP.dataset)), grep("06", names(SP.dataset)))
df.09 <- SP.dataset %>% dplyr:: select(grep("09", names(SP.dataset)), grep("09", names(SP.dataset)))
df.12 <- SP.dataset %>% dplyr:: select(grep("12", names(SP.dataset)), grep("12", names(SP.dataset)))
df.24 <- SP.dataset %>% dplyr:: select(grep("24", names(SP.dataset)), grep("24", names(SP.dataset)))

# melt df to better plot in ggplot:
sp01.m <- melt(df.01, id.vars = c("Year", "Month"))

ggplot(sp01.m, aes(Month, value, color = variable))+geom_point()

summary(df.01)
test <- sp01.m[sp01.m$variable %in% "UNC.SP01.y",]
test$timeperiod <- ifelse(test$Year %in% 1895:1950, "pre-1950", "post-1950")

# what we should do is get all the climate data together, then do a cluster analysis
