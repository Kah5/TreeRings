# This is a script that runs the bootstrapped climate correlations over all sites & all woood types:



# ------------------------------load all the files needed:
wood <- "WW"
#read in whole ring width file for site
#Bonanza <L- read.tucson("./cofecha/BON_out/BONall.rwl", header = T)
#for EW
if(wood == "EW"){
  Bonanza <- read.tucson("cleanrwl/BONew.rwl", header = TRUE)
  Hickory <- read.tucson ("cleanrwl/HICew.rwl", header = FALSE)
  #PleasantWolf <- read.tucson('data/wi006.rwl') #Pleasant prairie in southeast WI, from ITRDB
  StCroix <- read.tucson("cleanrwl/STCew.rwl") #saint croix savanna, MN
  #Sand <- read.tucson("data/il001.rwl", header = TRUE) #Sandwich, il. Cook tree rings from the 1980's
  #Pulaski <- read.tucson("./in001.rwl", header = TRUE)
  Townsend <- read.tucson('cleanrwl/TOeww.rwl', header = TRUE)#townsedn woods
  #YellowRiver <- read.tucson('data/ia029.rwl', header = TRUE) # had to fix a wrong year
  #Pleasant <- read.tucson('./cofecha/PLEew.rwl', header = TRUE) #Pleasant valley conservency
  #Desouix <- read.tucson('data/mn029.rwl', header = TRUE) #close to BONanza, in ITRDB
  Coral <- read.tucson('cleanrwl/CORew.rwl')
  Uncas <- read.tucson("cleanrwl/UNCew.rwl")
  Glacial <- read.tucson("cleanrwl/GLAew.rwl")
  Englund <- read.tucson("cleanrwl/ENGew.rwl")
  Mound <- read.tucson("cleanrwl/MOUew.rwl")
  GLL1 <- read.tucson("cleanrwl/GLL1ew.rwl")
  GLL2 <- read.tucson("cleanrwl/GLL1ew.rwl")
  GLL3 <- read.tucson("cleanrwl/GLL2ew.rwl")
  GLL4 <- read.tucson("cleanrwl/GLL3ew.rwl")
  PVC <- read.tucson("cleanrwl/PVCew.rwl")
  
}else{if(wood == "LW"){
  Bonanza <- read.tucson("cleanrwl/BONlw.rwl", header = TRUE)
  Hickory <- read.tucson ("cleanrwl/HIClw.rwl", header = FALSE)
  #PleasantWolf <- read.tucson('data/wi006.rwl') #Pleasant prairie in southeast WI, from ITRDB
  StCroix <- read.tucson("cleanrwl/STClw.rwl") #saint croix savanna, MN
  #Sand <- read.tucson("data/il001.rwl", header = TRUE) #Sandwich, il. Cook tree rings from the 1980's
  #Pulaski <- read.tucson("./in001.rwl", header = TRUE)
  Townsend <- read.tucson('cleanrwl/TOWlw.rwl', header = TRUE)#townsedn woods
  #YellowRiver <- read.tucson('data/ia029.rwl', header = TRUE) # had to fix a wrong year
  #Pleasant <- read.tucson('./cofecha/PLElw.rwl', header = TRUE) #Pleasant valley conservency
  #Desouix <- read.tucson('data/mn029.rwl', header = TRUE) #close to BONanza, in ITRDB
  Coral <- read.tucson('cleanrwl/CORlw.rwl')
  Uncas <- read.tucson("cleanrwl/UNClw.rwl")
  Glacial <- read.tucson("cleanrwl/GLAlw.rwl")
  Englund <- read.tucson("cleanrwl/ENGlw.rwl")
  Mound <- read.tucson("cleanrwl/MOUlw.rwl")
  GLL1 <- read.tucson("cleanrwl/GLL1lw.rwl")
  GLL2 <- read.tucson("cleanrwl/GLL1lw.rwl")
  GLL3 <- read.tucson("cleanrwl/GLL2lw.rwl")
  GLL4 <- read.tucson("cleanrwl/GLL3lw.rwl")
  PVC <- read.tucson("cleanrwl/PVClw.rwl")}
  else{
  Bonanza <- read.tucson("cleanrwl/BONww.rwl", header = TRUE)
  Hickory <- read.tucson ("cleanrwl/HICww.rwl", header = FALSE)
  #PleasantWolf <- read.tucson('data/wi006.rwl') #Pleasant prairie in southeast WI, from ITRDB
  StCroix <- read.tucson("cleanrwl/STCww.rwl") #saint croix savanna, MN
  #Sand <- read.tucson("data/il001.rwl", header = TRUE) #Sandwich, il. Cook tree rings from the 1980's
  #Pulaski <- read.tucson("./in001.rwl", header = TRUE)
  Townsend <- read.tucson('cleanrwl/TOWww.rwl', header = TRUE)#townsedn woods
  #YellowRiver <- read.tucson('data/ia029.rwl', header = TRUE) # had to fix a wrong year
  #Pleasant <- read.tucson('./cofecha/PLEww.rwl', header = TRUE) #Pleasant valley conservency
  #Desouix <- read.tucson('data/mn029.rwl', header = TRUE) #close to BONanza, in ITRDB
  Coral <- read.tucson('cleanrwl/CORww.rwl')
  Uncas <- read.tucson("cleanrwl/UNCww.rwl")
  Glacial <- read.tucson("cleanrwl/GLAww.rwl")
  Englund <- read.tucson("cleanrwl/ENGww.rwl")
  Mound <- read.tucson("cleanrwl/MOUww.rwl")
  GLL1 <- read.tucson("cleanrwl/GLL1ww.rwl")
  GLL2 <- read.tucson("cleanrwl/GLL1ww.rwl")
  GLL3 <- read.tucson("cleanrwl/GLL2ww.rwl")
  GLL4 <- read.tucson("cleanrwl/GLL3ww.rwl")
  PVC <- read.tucson("cleanrwl/PVCww.rwl")
}}

# create a list of the tree ring growth sites rwls


# run the R/corr_P.R script over all of the sites:
source("R/corr_P.R")
woods <- c( "EW", "LW")

for(w in 1:length(woods)){
  wood <- woods[w] # run script over all wood types
  
  if(wood == "EW"){
    Bonanza <- read.tucson("cleanrwl/BONew.rwl", header = TRUE)
    Hickory <- read.tucson ("cleanrwl/HICew.rwl", header = FALSE)
    #PleasantWolf <- read.tucson('data/wi006.rwl') #Pleasant prairie in southeast WI, from ITRDB
    StCroix <- read.tucson("cleanrwl/STCew.rwl") #saint croix savanna, MN
    #Sand <- read.tucson("data/il001.rwl", header = TRUE) #Sandwich, il. Cook tree rings from the 1980's
    #Pulaski <- read.tucson("./in001.rwl", header = TRUE)
    Townsend <- read.tucson('cleanrwl/TOWew.rwl', header = TRUE)#townsedn woods
    #YellowRiver <- read.tucson('data/ia029.rwl', header = TRUE) # had to fix a wrong year
    #Pleasant <- read.tucson('./cofecha/PLEew.rwl', header = TRUE) #Pleasant valley conservency
    #Desouix <- read.tucson('data/mn029.rwl', header = TRUE) #close to BONanza, in ITRDB
    Coral <- read.tucson('cleanrwl/CORew.rwl')
    Uncas <- read.tucson("cleanrwl/UNCew.rwl")
    Glacial <- read.tucson("cleanrwl/GLAew.rwl")
    Englund <- read.tucson("cleanrwl/ENGew.rwl")
    Mound <- read.tucson("cleanrwl/MOUew.rwl")
    GLL1 <- read.tucson("cleanrwl/GLL1ew.rwl")
    GLL2 <- read.tucson("cleanrwl/GLL1ew.rwl")
    GLL3 <- read.tucson("cleanrwl/GLL2ew.rwl")
    GLL4 <- read.tucson("cleanrwl/GLL3ew.rwl")
    PVC <- read.tucson("cleanrwl/PVCew.rwl")
    
  }else{if(wood == "LW"){
    Bonanza <- read.tucson("cleanrwl/BONlw.rwl", header = TRUE)
    Hickory <- read.tucson ("cleanrwl/HIClw.rwl", header = FALSE)
    #PleasantWolf <- read.tucson('data/wi006.rwl') #Pleasant prairie in southeast WI, from ITRDB
    StCroix <- read.tucson("cleanrwl/STClw.rwl") #saint croix savanna, MN
    #Sand <- read.tucson("data/il001.rwl", header = TRUE) #Sandwich, il. Cook tree rings from the 1980's
    #Pulaski <- read.tucson("./in001.rwl", header = TRUE)
    Townsend <- read.tucson('cleanrwl/TOWlw.rwl', header = TRUE)#townsedn woods
    #YellowRiver <- read.tucson('data/ia029.rwl', header = TRUE) # had to fix a wrong year
    #Pleasant <- read.tucson('./cofecha/PLElw.rwl', header = TRUE) #Pleasant valley conservency
    #Desouix <- read.tucson('data/mn029.rwl', header = TRUE) #close to BONanza, in ITRDB
    Coral <- read.tucson('cleanrwl/CORlw.rwl')
    Uncas <- read.tucson("cleanrwl/UNClw.rwl")
    Glacial <- read.tucson("cleanrwl/GLAlw.rwl")
    Englund <- read.tucson("cleanrwl/ENGlw.rwl")
    Mound <- read.tucson("cleanrwl/MOUlw.rwl")
    GLL1 <- read.tucson("cleanrwl/GLL1lw.rwl")
    GLL2 <- read.tucson("cleanrwl/GLL1lw.rwl")
    GLL3 <- read.tucson("cleanrwl/GLL2lw.rwl")
    GLL4 <- read.tucson("cleanrwl/GLL3lw.rwl")
    PVC <- read.tucson("cleanrwl/PVClw.rwl")}
    else{
      Bonanza <- read.tucson("cleanrwl/BONww.rwl", header = TRUE)
      Hickory <- read.tucson ("cleanrwl/HICww.rwl", header = FALSE)
      #PleasantWolf <- read.tucson('data/wi006.rwl') #Pleasant prairie in southeast WI, from ITRDB
      StCroix <- read.tucson("cleanrwl/STCww.rwl") #saint croix savanna, MN
      #Sand <- read.tucson("data/il001.rwl", header = TRUE) #Sandwich, il. Cook tree rings from the 1980's
      #Pulaski <- read.tucson("./in001.rwl", header = TRUE)
      Townsend <- read.tucson('cleanrwl/TOWww.rwl', header = TRUE)#townsedn woods
      #YellowRiver <- read.tucson('data/ia029.rwl', header = TRUE) # had to fix a wrong year
      #Pleasant <- read.tucson('./cofecha/PLEww.rwl', header = TRUE) #Pleasant valley conservency
      #Desouix <- read.tucson('data/mn029.rwl', header = TRUE) #close to BONanza, in ITRDB
      Coral <- read.tucson('cleanrwl/CORww.rwl')
      Uncas <- read.tucson("cleanrwl/UNCww.rwl")
      Glacial <- read.tucson("cleanrwl/GLAww.rwl")
      Englund <- read.tucson("cleanrwl/ENGww.rwl")
      Mound <- read.tucson("cleanrwl/MOUww.rwl")
      GLL1 <- read.tucson("cleanrwl/GLL1ww.rwl")
      GLL2 <- read.tucson("cleanrwl/GLL1ww.rwl")
      GLL3 <- read.tucson("cleanrwl/GLL2ww.rwl")
      GLL4 <- read.tucson("cleanrwl/GLL3ww.rwl")
      PVC <- read.tucson("cleanrwl/PVCww.rwl")
    }}
  
  
  sites <- list(Townsend, Hickory, Bonanza, StCroix, Coral, Uncas, Glacial, Englund, Mound, GLL1, GLL2, GLL3, GLL4, PVC )
  
  # create a list of codes for site names
  site.codes <- c("TOW", "HIC", "BON", "STC","COR", "UNC", "ENG", "MOU", "GL1", "GL2", "GL3", "GL4", "PVC")
  
  
  
        for(s in 1:length(sites)){
          site <- sites[[s]]
          site.code <- site.codes[s]
          clim.corrs(site, site.code)
        }
}
