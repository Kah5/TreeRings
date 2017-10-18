# This is a script that runs the bootstrapped climate correlations over all sites & all woood types:



# ------------------------------load all the files needed:
wood <- "WW"
#read in whole ring width file for site
#Bonanza <L- read.tucson("./cofecha/BON_out/BONall.rwl", header = T)
#for EW
if(wood == "EW"){
  Bonanza <- read.tucson("./cofecha/BONew.rwl", header = T)
  Hickory <- read.tucson("./cofecha/HICew.rwl", header = F)
  #Glacial <- read.tucson("./cofecha/GLAew.rwl")
  Townsend <- read.tucson('./cofecha/tow/TOWew.rwl', header = T)
  Pleasant <- read.tucson('./cofecha/PLEew.rwl', header = T)
}else{if(wood == "LW"){
  Bonanza <- read.tucson("./cofecha/BONlw.rwl")
  Hickory <- read.tucson("./cofecha/HIClw.rwl", header = F)
  #Glacial <- read.tucson("./cofecha/GLAlw.rwl")
  Townsend <- read.tucson('./cofecha/tow/TOWlw.rwl', header = T)
  Pleasant <- read.tucson('./cofecha/PLElw.rwl', header = T)
}else{
  Bonanza <- read.tucson("./cofecha/BONww.rwl", header = TRUE)
  Hickory <- read.tucson ("./cofecha/HICww.rwl", header = FALSE)
  PleasantWolf <- read.tucson('data/wi006.rwl') #Pleasant prairie in southeast WI, from ITRDB
  StCroix <- read.tucson("./cofecha/STCww.rwl") #saint croix savanna, MN
  Sand <- read.tucson("data/il001.rwl", header = TRUE) #Sandwich, il. Cook tree rings from the 1980's
  #Pulaski <- read.tucson("./in001.rwl", header = TRUE)
  Townsend <- read.tucson('./cofecha/tow/TOWww.rwl', header = TRUE)#townsedn woods
  YellowRiver <- read.tucson('data/ia029.rwl', header = TRUE) # had to fix a wrong year
  Pleasant <- read.tucson('./cofecha/PLEww.rwl', header = TRUE) #Pleasant valley conservency
  Desouix <- read.tucson('data/mn029.rwl', header = TRUE) #close to BONanza
  Coral <- read.tucson('C:/Users/JMac/Documents/Kelly/crossdating/data/cofecha/COR.rwl')
  Uncas <- read.tucson("C:/Users/JMac/Documents/Kelly/crossdating/data/cofecha/UNC.rwl")
  Glacial <- read.tucson("C:/Users/JMac/Documents/Kelly/crossdating/data/cofecha/GLA.rwl")
  Englund <- read.tucson("C:/Users/JMac/Documents/Kelly/crossdating/data/cofecha/ENG.rwl")
  Mound <- read.tucson("C:/Users/JMac/Documents/Kelly/crossdating/data/cofecha/MOU.rwl")
  GLL1 <- read.tucson("cleanrwl/GLL1ww.rwl")
  GLL2 <- read.tucson("cleanrwl/GLL1ww.rwl")
  GLL3 <- read.tucson("cleanrwl/GLL2ww.rwl")
  GLL4 <- read.tucson("cleanrwl/GLL3ww.rwl")
  PVC <- read.tucson("cleanrwl/GLL4ww.rwl")
}}

# create a list of the tree ring growth sites rwls
sites <- list(Townsend, Hickory)
# create a list of codes for site names
site.codes <- c("TOW", "HIC")

# run the R/corr_P.R script over all of the sites:
source("R/corr_P.R")
for(s in 1:length(sites)){
  site <- sites[[s]]
  site.code <- site.codes[s]
  clim.corrs(site, site.code)
}
