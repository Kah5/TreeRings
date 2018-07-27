# file for reading in corrected stable isotope files:
# outline:

# 1. read in all files in the stable isotope data dir
# 2. [not now, but eventually correct for standards within the script]
# 3. Visual check on replicates: plot mean with analytical replicates as error bars for each site
# 4. Combine data from the same run or different runs by year, tree, site to get df:
#    colnames() <- c("Year", "Site", "Tree", "Wood", "Rep1", "Rep2", "Rep3")
# 5. Get mean values for each year and plot
library(ggplot2)
library(data.table)


# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Read in Data >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# read all files ending in "deltaC_data.csv" from the deltaC folder:
setwd("/Users/kah/Documents/TreeRings/data/stable_isotopes/deltaC/")

corrected.isotope.files <- list.files(pattern = "data.csv$") # get all the data files
iso.data <- lapply(corrected.isotope.files, FUN = read.csv) 
iso.data.df <- do.call(rbind, iso.data) # converte from list of df to df
  
unique(iso.data.df$Identifier.2)
setwd("/Users/kah/Documents/TreeRings")



iso.data.df[iso.data.df$d13C_12C_corr < -28.9,]
iso.data.df[iso.data.df$Time.Code %like% "2/19/18",]
iso.data.df[iso.data.df$Time.Code %like% "2018/02/23",]

#iso.data.df <- iso.data.df[!iso.data.df$Time.Code %like% "2018/02/23" & !is.na(iso.data.df$Time.Code) & ! iso.data.df$Time.Code %like% "2/19/18" ,]
iso.data.df <- iso.data.df[!iso.data.df$Time.Code %like% "2018/02/23" & !is.na(iso.data.df$Time.Code) ,]

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Data Cleaning >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# some of the nomenclature for the "Identifier.2" or the tree name is inconsistant
# for example, the tree "Bon 9" might be named Bon9, BON9, BON9a, BON9c ....etc
# so far these are only in the "BON" site
# since we only care about the tree number, lets correct these:


iso.data.df$Identifier.2 <- toupper(iso.data.df$Identifier.2) # converts to all uppercase

iso.data.df <- iso.data.df[!iso.data.df$Identifier.2 %in% "BLANK",]
#Correct BON:

iso.data.df[iso.data.df$Identifier.2 %in% c("BON13A", "BON13B", "BON13C"),]$Identifier.2 <- "BON13" 
iso.data.df[iso.data.df$Identifier.2 %in% c("BON9A", "BON9B", "BON9C"),]$Identifier.2 <- "BON9" 
iso.data.df[iso.data.df$Identifier.2 %in% c("BON8A", "BON8B", "BON8C"),]$Identifier.2 <- "BON8" 
iso.data.df[iso.data.df$Identifier.2 %in% c("BON7A", "BON7B", "BON7C"),]$Identifier.2 <- "BON7" 

# rename for GLL2:
iso.data.df[iso.data.df$Identifier.2 %in% c("GLL2-20"),]$Identifier.2 <- "GLL220" 
iso.data.df[iso.data.df$Identifier.2 %in% c("GLL2-16"),]$Identifier.2 <- "GLL216"
iso.data.df[iso.data.df$Identifier.2 %in% c("GLL2-7"),]$Identifier.2 <- "GLL27" 
iso.data.df[iso.data.df$Identifier.2 %in% c("GLL2-12"),]$Identifier.2 <- "GLL212" 
iso.data.df[iso.data.df$Identifier.2 %in% c("GLL2-19"),]$Identifier.2 <- "GLL219" 

# rename for MOU:
iso.data.df[iso.data.df$Identifier.2 %in% c("MOU-1"),]$Identifier.2 <- "MOU1" 
iso.data.df[iso.data.df$Identifier.2 %in% c("MOU-2"),]$Identifier.2 <- "MOU2"
iso.data.df[iso.data.df$Identifier.2 %in% c("MOU-3"),]$Identifier.2 <- "MOU3" 
iso.data.df[iso.data.df$Identifier.2 %in% c("MOU4"),]$Identifier.2 <- "MOU4"
iso.data.df[iso.data.df$Identifier.2 %in% c("MOU-5"),]$Identifier.2 <- "MOU5" 
iso.data.df[iso.data.df$Identifier.2 %in% c("MOU6"),]$Identifier.2 <- "MOU6"


# rename for GLA:
iso.data.df[iso.data.df$Identifier.2 %in% c("GLA-1883"),]$Identifier.2 <- "GLA1883" 
iso.data.df[iso.data.df$Identifier.2 %in% c("GLA-1179"),]$Identifier.2 <- "GLA1179"
iso.data.df[iso.data.df$Identifier.2 %in% c("GLA-867"),]$Identifier.2 <- "GLA0867" 
iso.data.df[iso.data.df$Identifier.2 %in% c("GLA-1700"),]$Identifier.2 <- "GLA1700"
iso.data.df[iso.data.df$Identifier.2 %in% c("GLA867"),]$Identifier.2 <- "GLA0867" 
iso.data.df[iso.data.df$Identifier.2 %in% c("GLA1700"),]$Identifier.2 <- "GLA1700"
iso.data.df[iso.data.df$Identifier.2 %in% c("GLA-870"),]$Identifier.2 <- "GLA0870"
iso.data.df[iso.data.df$Identifier.2 %in% c("GLA-872"),]$Identifier.2 <- "GLA0872"
iso.data.df[iso.data.df$Identifier.2 %in% c("GLA-1884"),]$Identifier.2 <- "GLA1884"


iso.data.df[iso.data.df$Identifier.2 %in% c("UNI5B"),]$Identifier.2 <- "UNI5" 
unique(iso.data.df$Identifier.2) # check that this took care of all the data:

# change to more intutitive column names:
colnames(iso.data.df) <- c("year", "ID", "Peak.Nr", "d.15N.14N", "d.13C.12C","Time.Code", "d13C_12C_corr")

# some of the sample years are labeled with EW and LW:
iso.EW <- iso.data.df[iso.data.df$Year %like% "EW",]

# get all the non-EW data:
iso.data.LW <- iso.data.df[!iso.data.df$year %like% "EW",]

# if the year has an LW after it, remove it here, else, keep all the characters
iso.data.LW[iso.data.LW$year %like% "LW",]$year <- substr(iso.data.LW[iso.data.LW$year %like% "LW",]$year , 1, 4)
iso.data.df <- iso.data.LW 

# some samples are analytical replicates, denote replicate # here:
iso.data.df$samplenum <- substr(iso.data.df$year, 5, 6)
iso.data.df$year <-  as.numeric(substr(iso.data.df$year, 1, 4)) # just get year value and convert to numeric

# add the site to the df:
iso.data.df$site <- substr(iso.data.df$ID, 1, 3)

# now keep only the useful columns:
iso.df <- iso.data.df[,c("year", "ID","site","samplenum", "d.13C.12C", "d13C_12C_corr")]




# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< correct for seuss effect: >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# 13C corrected = 13C plant + (13C atm + 6.4)
# note, check the deltaC_atm
deltaATM <- read.csv("data/stable_isotopes/Mccarrol_loader_deltaC_atm.csv") # data from mccarroll and loader patched with recent ppm an dneed to check the delta13atm values
head(deltaATM)

full.df <- merge(iso.df, deltaATM[,c("Year", "d13atm", "ppm")], by.x = "year", by.y = "Year")
full.df$Cor.d13C.suess <- full.df$d13C_12C_corr - (full.df$d13atm + 6.4)

# there are a few points around -20 per mil which are points that may need to be checked:
full.df[full.df$d13C_12C_corr > -22,] # the 1934 + 1933 may be strange b/c of the high drought in that year--see what the rest of the values say
full.df <- full.df[!full.df$d13C_12C_corr > -21.55,]

full.df[full.df$d13C_12C_corr < -28,] # need to check the values of BON9 below -28
full.df[full.df$ID %in% "BON9",] 

ggplot(full.df, aes(year, Cor.d13C.suess, color = ID))+geom_point()

ggplot(full.df, aes(year, Cor.d13C.suess, color = site))+geom_point()+facet_wrap(~site)

ggplot(full.df, aes(year, Cor.d13C.suess, color = ID))+geom_point()+geom_line()+facet_wrap(~site)

ggplot(full.df[!full.df$year == 1979, ], aes(ppm, Cor.d13C.suess, color = ID))+geom_point()+facet_wrap(~site)

# write to a csv
write.csv(full.df, "outputs/stable_isotopes/full_std_suess_corrected_d13C.csv")

