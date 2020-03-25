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
    
    dataset<-rbind(dataset, temp_dataset)
    rm(temp_dataset)
  }
  
}

ggplot(dataset[dataset$Month %in% '07', ], aes(as.numeric(Year), ppt..inches.))+geom_point()+stat_smooth(method= "lm")

ggplot(dataset[dataset$Month %in% '07', ], aes(as.numeric(Year), vpdmax..hPa.))+geom_point()+stat_smooth(method= "lm")

# --------------------------finding the most similar climate years for each site------------------------:

setwd("/Users/kah/Documents/TreeRings")
sites <- unique(dataset$site)
library(reshape2)

# this loop does a pca for each site's climate time series, 
# finds the euclidian distance bewteen PC1 and PC2 for each year,
# then finds the years before and after 1950 that have the smallest distance bewteen them
# outputs them into outputs/data/Isotope_climate

for(p in 1:length(sites)){
      dataset.t <- dataset[dataset$site %in% sites[p],]

      # using PRSIM data:
      # need a df with monthly climate as columns and site year as the row identifier for each site
      dataset.mo <- dataset.t[dataset.t$Month %in% c("06","07","08"),]
      
      data.mo <- melt(dataset.mo, id.vars= c("Year", "Month", "site", "Date"))
      
      # get JJA means:
      JJA.means <- dcast(data.mo, Year ~ variable, mean)
      JJA.means <- data.frame(JJA.means[2:nrow(JJA.means),], JJA.means[1:121,2:7])
      colnames(JJA.means) <- c('Year',"PPT" ,"tmin" ,"tmean" ,"tmax" ,"tdmean",  
                                "vpdmin","vpdmax" , "ppt_prev",      
                                "tmin_prev",   "tmean_prev" , "tmax_prev",  
                               "tdmean_prev" ,"vpdmin_prev"   )
      
      JJA.pca<- princomp(scale(JJA.means[,2:14]))
      
      plot(JJA.pca)
      biplot(JJA.pca)
      scores <- JJA.pca$scores
      
      # add scores to pls.full:
      JJA.means$pc1 <- scores[,1]
      JJA.means$pc2 <- scores[,2]
      JJA.means$Year <- as.numeric(as.character(JJA.means$Year))
      JJA.means$class <- ifelse(JJA.means$Year >= 1950, "post-1950", "pre-1950")
      
      # plot out the climate spaces:
      ggplot(JJA.means, aes(pc1, pc2, color = class)) + geom_point()
      dists <- dist(JJA.means[,15:16]) #, by.rows  = FALSE)
      
      # need to find years that are close to each other:
      library(sp)
      df <- spDists(as.matrix(JJA.means[,15:16]))
      i <- apply(df,2, function (x) which(x %in% min(x[x != 0])))
      dist <- apply(df,2, function (x) min(x[x != 0]))
      closest <- data.frame(Year = JJA.means$Year, distance = dist, closest = i)
      
      # print out the closest year into a column:
      closest$nearestyear <- closest[closest$closest,]$Year 
      
      # get the class of each year in the closest pairings
      closest$class_1 <- ifelse(closest$Year > 1950, "post-1950", "pre-1950")
      closest$class_2 <- ifelse(closest$nearestyear > 1950, "post-1950", "pre-1950")
      post.pre <- closest[closest$class_1 %in% "post-1950" & closest$class_2 %in% "pre-1950",]
      pre.post <- closest[closest$class_1 %in% "pre-1950" & closest$class_2 %in% "post-1950",]
      
      
      # order the dataframe and take the top 10 pairs of years 
      ordered <- post.pre[order(post.pre$distance),]
      post_1950 <- JJA.means[JJA.means$Year %in% ordered$Year,]
      pre_1950 <- JJA.means[JJA.means$Year %in% closest$nearestyear,]
      years.to.do <- ordered[1:15,]
      
      write.csv(years.to.do, file =  paste0("outputs/data/Isotope_climate/", sites[p],"_most_similar_years.csv"))
}      

# clustering:
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
      ggplot(mydata2, aes(x=ppt..inches., y=tmean..degrees.F., color = as.character(cluster2)))+geom_point()
      ggplot(mydata2, aes(x=ppt..inches., y=tmean..degrees.F., color = as.character(cluster3)))+geom_point()
      ggplot(mydata2, aes(x=ppt..inches., y=tmean..degrees.F., color = as.character(cluster4)))+geom_point()
      ggplot(mydata2, aes(x=ppt..inches., y=tmean..degrees.F., color = as.character(cluster5)))+geom_point()
      
      
      #four clusters is what the elbow method would distinguish:
      ggplot(mydata2, aes(x=ppt..inches., y=tmean..degrees.F., color = as.character(cluster4)))+geom_point()
      ggplot(mydata2, aes(x=ppt..inches., y=vpdmin..hPa., color = as.character(cluster4)))+geom_point()
      ggplot(mydata2, aes(x=ppt..inches., y=vpdmax..hPa., color = as.character(cluster4)))+geom_point()
      ggplot(mydata2, aes(x=ppt..inches., y=tmax..degrees.F., color = as.character(cluster4)))+geom_point()
      ggplot(mydata2, aes(x=ppt..inches., y=tmin..degrees.F., color = as.character(cluster4)))+geom_point()
      
      ggplot(mydata2, aes(x=vpdmin..hPa., y=tmean..degrees.F., color = as.character(cluster4)))+geom_point()
      #ggplot(mydata2, aes(x=vpdmin..hPa., y=vpdmin..hPa., color = as.character(cluster4)))+geom_point()
      ggplot(mydata2, aes(x=vpdmin..hPa., y=vpdmax..hPa., color = as.character(cluster4)))+geom_point()
      ggplot(mydata2, aes(x=vpdmin..hPa., y=tmax..degrees.F., color = as.character(cluster4)))+geom_point()
      ggplot(mydata2, aes(x=vpdmin..hPa., y=tmin..degrees.F., color = as.character(cluster4)))+geom_point()
      
      # clusters are distinguished by tmean, 
      summary(mydata2[mydata2$cluster4 %in% "1", 1:10])
      summary(mydata2[mydata2$cluster4 %in% "2", 1:10])
      summary(mydata2[mydata2$cluster4 %in% "3", 1:10])
      summary(mydata2[mydata2$cluster4 %in% "4", 1:10])
      
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
      
#---------------------------- calculate ET-------------:

# Thornthwaite:
# Ep = 16*(L/30)*(N/30)*(10*Ta/I)^a

# L = daylenght(hrs)
# N = # of days in the month
# Ta is mean monthy air tmeperature (degC)
# a = 6.75 E-7 *I^3 - 7.71E-5*I^2 +1.79E-2*I +0.49
# I = sum(Ta/5)^1.514 (for months Ta > 0 deg C)

L <- 14 # daylength
N <- 30
Taf <- 59.19
Ta <- (Taf - 32)*5/9
I <- sum(Ta/5)^1.514 #(for months Ta > 0 deg C)
a <- 6.75E-7 *I^3 - 7.71E-5*I^2 +1.79E-2*I +0.49
  
Ep <- 16*(L/30)*(N/30)*(10*Ta/I)^a

# Get P-ET for the months of June, July, August

# what we should do is get all the climate data together, then do a cluster analysis
