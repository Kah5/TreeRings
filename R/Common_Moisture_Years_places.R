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

setwd("outputs/data/")

file_list <- list.files()

for (file in file_list){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- read.csv(file, header=TRUE,)
  }
  
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    temp_dataset <-read.csv(file, header=TRUE)
    SP.dataset<-merge(dataset, temp_dataset, by = c("Year", "Month"))
    rm(temp_dataset)
  }
  
}

# setwd back:
setwd("/Users/kah/Documents/TreeRings")

df.01 <- data.frame(Year = SP.dataset$Year, Month = SP.dataset$Year,SP.dataset %>% dplyr:: select(grep("01", names(SP.dataset)), grep("01", names(SP.dataset))))
df.02 <- SP.dataset %>% dplyr:: select(grep("02", names(SP.dataset)), grep("02", names(SP.dataset)))
df.06 <- SP.dataset %>% dplyr:: select(grep("06", names(SP.dataset)), grep("06", names(SP.dataset)))
df.09 <- SP.dataset %>% dplyr:: select(grep("09", names(SP.dataset)), grep("09", names(SP.dataset)))
df.12 <- SP.dataset %>% dplyr:: select(grep("12", names(SP.dataset)), grep("12", names(SP.dataset)))
df.24 <- SP.dataset %>% dplyr:: select(grep("24", names(SP.dataset)), grep("24", names(SP.dataset)))

# melt df to better plot in ggplot:
sp01.m <- melt(df.01, id.vars = c("Year", "Month"))

ggplot()