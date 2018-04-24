# file for reading in corrected stable isotope files:
# outline:

# 1. read in all files in the stable isotope data dir
# 2. [not now, but eventually correct for standards within the script]
# 3. Visual check on replicates: plot mean with analytical replicates as error bars for each site
# 4. Combine data from the same run or different runs by year, tree, site to get df:
#    colnames() <- c("Year", "Site", "Tree", "Wood", "Rep1", "Rep2", "Rep3")
# 5. Get mean values for each year and plot
library(ggplot2)


# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Read in Data >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# read all files ending in "deltaC_data.csv" from the deltaC folder:
setwd("/Users/kah/Documents/TreeRings/data/stable_isotopes/deltaC/")

corrected.isotope.files <- list.files(pattern = "data.csv$") # get all the data files
iso.data <- lapply(corrected.isotope.files,FUN = read.csv) 
iso.data.df <- do.call(rbind, iso.data) # converte from list of df to df
  
unique(iso.data.df$Identifier.2)
setwd("/Users/kah/Documents/TreeRings")


# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Data Cleaning >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# some of the nomenclature for the "Identifier.2" or the tree name is inconsistant
# for example, the tree "Bon 9" might be named Bon9, BON9, BON9a, BON9c ....etc
# so far these are only in the "BON" site
# since we only care about the tree number, lets correct these:


iso.data.df$Identifier.2 <- toupper(iso.data.df$Identifier.2) # converts to all uppercase

#Correct BON:

iso.data.df[iso.data.df$Identifier.2 %in% c("BON13A", "BON13B", "BON13C"),]$Identifier.2 <- "BON13" 
iso.data.df[iso.data.df$Identifier.2 %in% c("BON9A", "BON9B", "BON9C"),]$Identifier.2 <- "BON9" 
iso.data.df[iso.data.df$Identifier.2 %in% c("BON8A", "BON8B", "BON8C"),]$Identifier.2 <- "BON8" 
iso.data.df[iso.data.df$Identifier.2 %in% c("BON7A", "BON7B", "BON7C"),]$Identifier.2 <- "BON7" 

unique(iso.data.df$Identifier.2) # check that this took care of all the data:

# change to more intutitive column names:
colnames(iso.data.df) <- c("Year", "Tree", "Peak.Nr", "d.15N.14N", "d.13C.12C","Time.Code", "d13C_12C_corr")

# some samples are analytical replicates, denote replicate # here:
iso.data.df$samplenum <- substr(iso.data.df$Year, 5, 6)
iso.data.df$Year <-  as.numeric(substr(iso.data.df$Year, 1, 4)) # just get year value and convert to numeric

# add the site to the df:
iso.data.df$Site <- substr(iso.data.df$Tree, 1, 3)

# now keep only the useful columns:
iso.df <- iso.data.df[,c("Year", "Tree","Site","samplenum", "d.13C.12C", "d13C_12C_corr")]




# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< correct for seuss effect: >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# 13C corrected = 13C plant + (13C atm + 6.4)
deltaATM <- read.csv("data/stable_isotopes/Mccarrol_loader_deltaC_atm.csv") # data from mccarroll and loader patched with recent ppm an dneed to check the delta13atm values
head(deltaATM)

full.df <- merge(iso.df, deltaATM[,c("Year", "d13atm", "ppm")], by = "Year")
full.df$Cor.d13C.suess <- full.df$d13C_12C_corr + (full.df$d13atm + 6.4)


ggplot(full.df, aes(Year, Cor.d13C.suess, color = Tree))+geom_point()

ggplot(full.df, aes(Year, Cor.d13C.suess, color = Site))+geom_point()+facet_wrap(~Site)

ggplot(full.df, aes(Year, Cor.d13C.suess, color = Tree))+geom_point()+geom_line()+facet_wrap(~Site)

ggplot(full.df[!full.df$Year == 1979, ], aes(ppm, Cor.d13C.suess, color = Tree))+geom_point()+facet_wrap(~Site)

# write to a csv
write.csv(full.df, "outputs/stable_isotopes/full_std_suess_corrected_d13C.csv")

