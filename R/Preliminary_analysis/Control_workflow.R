# This is a script that runs the bootstrapped climate correlations over all sites & all woood types:

library(dplR)

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
  Townsend <- read.tucson('cleanrwl/TOWww.rwl', header = TRUE)#townsedn woods
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
woods <- c("WW" ,"EW", "LW")

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
  site.codes <- c("TOW", "HIC", "BON", "STC","COR", "UNC", "GLA","ENG", "MOU", "GL1", "GL2", "GL3", "GL4", "PVC")
  
  
  
        for(s in 1:length(sites)){
          site <- sites[[s]]
          site.code <- site.codes[s]
          clim.corrs(site, site.code)
        }
}


# now run the correlations for all sites and wood types on PRISM data:

source("R/corr_PRISM_data.R")
woods <- c("WW" ,"EW", "LW")
wood <- "WW"

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
      GLL2 <- read.tucson("cleanrwl/GLL2ww.rwl")
      GLL3 <- read.tucson("cleanrwl/GLL3ww.rwl")
      GLL4 <- read.tucson("cleanrwl/GLL4ww.rwl")
      PVC <- read.tucson("cleanrwl/PVCww.rwl")
      Avon <-read.tucson("cleanrwl/AVOww.rwl")
    }}
  
  
  sites <- list(Townsend, Hickory, Bonanza, StCroix, Coral, Uncas, Glacial, Englund, Mound, GLL1, GLL2, GLL3, GLL4,  Avon)
  
  # create a list of codes for site names
  site.codes <- c("TOW", "HIC", "BON", "STC","COR", "UNC","GLA", "ENG", "MOU", "GL1", "GL2", "GL3", "GL4", "AVO")
  
  
  
  
  for(s in 11:length(sites)){
    site <- sites[[s]]
    site.code <- site.codes[s]
    clim.PRISM.corrs(site, site.code)
  }
}



# pseudo code:
# read in all the precip correlatinos  for all sites
# add a site column and name
# join all together
# make ggplot with monthy Precip correlations + water year, with different colors as 

# now make one big figure to Plot all the monthly correlations at each site + total precipitation


site.codes <- c("TOW", "HIC", "BON", "STC","COR", "UNC","GLA", "ENG", "MOU", "GL1", "GL2", "GL3", "GL4", "AVO")


# all precip.plots: 

vpdmaxcors<- read.csv(paste0("data/BootCors/PRISM/",site.code, "-", "WW", "VPDmaxcor.csv"))

precipcors <- read.csv(paste0("data/BootCors/PRISM/", "COR", "-", "LW", "Precipcor.csv"))

read.precip.cors <- function(x){
 
  precipcors <- read.csv(paste0("data/BootCors/PRISM/", x, "-", "WW", "Precipcor.csv"))
  precipcors$site <- x 
  precipcors
}

cor.list <- list()
for(i in 1:length(site.codes)){
cor.list[[i]] <- read.precip.cors(site.codes[i])
}


all.cors <- do.call(rbind, cor.list)
all.cors.sub <- all.cors[all.cors$site %in% c("AVO", "BON","ENG", "GLA", "GL1", "GL2", "GL3", "MOU", "UNC"), ]
all.cors.sub$site <- factor(all.cors.sub$site, levels = c("BON", "GL1", "GL2", "GL3", "ENG", "UNC", "AVO", "MOU", "GLA"))


month.df <- data.frame(month = 1:25, 
                       mo.clim = c("pJan", "pFeb", "pMar", "pApr", "pMay", "pJun", "pJul", "pAug", "pSep", "pOct", "pNov", "pDec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "wateryear"))

all.cors.sub<- merge(all.cors.sub, month.df, by = "month")
all.cors.sub$mo.clim <- factor(all.cors.sub$mo.clim, levels = c("pJan", "pFeb", "pMar", "pApr", "pMay", "pJun", "pJul", "pAug", "pSep", "pOct", "pNov", "pDec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "wateryear"))

precipitation.cors <- ggplot(data = all.cors.sub, 
       aes(x=mo.clim,
           y= cor, 
           ymin=ci.min, 
           ymax=ci.max,       
           fill=site)) +
  geom_bar(position="dodge", stat = "identity") + 
  geom_errorbar( position = position_dodge(), colour="grey")+
  scale_fill_manual(values = c(`BON`= "#d73027",
    `GL1`="#f46d43",
    `GL2`="#fdae61",
    `GL3`= "#fee090",
    `ENG`="#ffffbf",
    `UNC`="#e0f3f8",
    `AVO`="#abd9e9",
    `MOU`="#74add1",
    `GLA`="#4575b4"))+ theme_bw()+theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+ylab("Correlation Coefficient")+xlab("Precipiation")

png(height = 4, width = 12, units = "in", res = 300, "outputs/growth_model/paper_figures/full_PRISM_wateryr_all_sites_correlation_bootci.png")
precipitation.cors 
dev.off()

precipitation.cors.noboot <- ggplot(data = all.cors.sub, 
                             aes(x=mo.clim,
                                 y= cor, 
                                 #ymin=ci.min, 
                                 #ymax=ci.max,       
                                 fill=site)) +
  geom_bar(position="dodge", stat = "identity") + 
  #geom_errorbar( position = position_dodge(), colour="grey")+
  scale_fill_manual(values = c(`BON`= "#d73027",
                               `GL1`="#f46d43",
                               `GL2`="#fdae61",
                               `GL3`= "#fee090",
                               `ENG`="#ffffbf",
                               `UNC`="#e0f3f8",
                               `AVO`="#abd9e9",
                               `MOU`="#74add1",
                               `GLA`="#4575b4"))+ theme_bw()+theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+ylab("Correlation Coefficient")+xlab("Precipiation")

png(height = 4, width = 12, units = "in", res = 300, "outputs/growth_model/paper_figures/full_PRISM_wateryr_all_sites_correlation.png")
precipitation.cors.noboot
dev.off()



# now do the same thing for TMAX and VPDMAX:

read.tmax.cors <- function(x){
  
  tmaxcors <- read.csv(paste0("data/BootCors/PRISM/", x, "-", "WW", "tmaxcor.csv"))
  tmaxcors$site <- x 
  tmaxcors
}

cor.list <- list()
for(i in 1:length(site.codes)){
  cor.list[[i]] <- read.tmax.cors(site.codes[i])
}


all.cors <- do.call(rbind, cor.list)
all.cors.sub <- all.cors[all.cors$site %in% c("AVO", "BON","ENG", "GLA", "GL1", "GL2", "GL3", "MOU", "UNC"), ]
all.cors.sub$site <- factor(all.cors.sub$site, levels = c("BON", "GL1", "GL2", "GL3", "ENG", "UNC", "AVO", "MOU", "GLA"))


month.df <- data.frame(month = 1:24, 
                       mo.clim = c("pJan", "pFeb", "pMar", "pApr", "pMay", "pJun", "pJul", "pAug", "pSep", "pOct", "pNov", "pDec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

all.cors.sub<- merge(all.cors.sub, month.df, by = "month")
all.cors.sub$mo.clim <- factor(all.cors.sub$mo.clim, levels = c("pJan", "pFeb", "pMar", "pApr", "pMay", "pJun", "pJul", "pAug", "pSep", "pOct", "pNov", "pDec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

tmax.cors <- ggplot(data = all.cors.sub, 
                             aes(x=mo.clim,
                                 y= cor, 
                                 ymin=ci.min, 
                                 ymax=ci.max,       
                                 fill=site)) +
  geom_bar(position="dodge", stat = "identity") + 
  geom_errorbar( position = position_dodge(), colour="grey")+
  scale_fill_manual(values = c(`BON`= "#d73027",
                               `GL1`="#f46d43",
                               `GL2`="#fdae61",
                               `GL3`= "#fee090",
                               `ENG`="#ffffbf",
                               `UNC`="#e0f3f8",
                               `AVO`="#abd9e9",
                               `MOU`="#74add1",
                               `GLA`="#4575b4"))+ theme_bw(base_size = 12)+theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+ylab("Correlation Coefficient")+xlab("Maximum Temperature")

png(height = 4, width = 12, units = "in", res = 300, "outputs/growth_model/paper_figures/full_PRISM_tmax_all_sites_correlation_bootci.png")
tmax.cors 
dev.off()

tmax.cors.noboot <- ggplot(data = all.cors.sub, 
                                    aes(x=mo.clim,
                                        y= cor, 
                                        #ymin=ci.min, 
                                        #ymax=ci.max,       
                                        fill=site)) +
  geom_bar(position="dodge", stat = "identity") + 
  #geom_errorbar( position = position_dodge(), colour="grey")+
  scale_fill_manual(values = c(`BON`= "#d73027",
                               `GL1`="#f46d43",
                               `GL2`="#fdae61",
                               `GL3`= "#fee090",
                               `ENG`="#ffffbf",
                               `UNC`="#e0f3f8",
                               `AVO`="#abd9e9",
                               `MOU`="#74add1",
                               `GLA`="#4575b4"))+ theme_bw(base_size = 12)+theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+ylab("Correlation Coefficient")+xlab("Maximum Temperature")

png(height = 4, width = 12, units = "in", res = 300, "outputs/growth_model/paper_figures/full_PRISM_tmax_all_sites_correlation.png")
tmax.cors.noboot
dev.off()


# for VPD max:
read.VPDmax.cors <- function(x){
  
  VPDmaxcors <- read.csv(paste0("data/BootCors/PRISM/", x, "-", "WW", "VPDmaxcor.csv"))
  VPDmaxcors$site <- x 
  VPDmaxcors
}

cor.list <- list()
for(i in 1:length(site.codes)){
  cor.list[[i]] <- read.VPDmax.cors(site.codes[i])
}


all.cors <- do.call(rbind, cor.list)
all.cors.sub <- all.cors[all.cors$site %in% c("AVO", "BON","ENG", "GLA", "GL1", "GL2", "GL3", "MOU", "UNC"), ]
all.cors.sub$site <- factor(all.cors.sub$site, levels = c("BON", "GL1", "GL2", "GL3", "ENG", "UNC", "AVO", "MOU", "GLA"))


month.df <- data.frame(month = 1:24, 
                       mo.clim = c("pJan", "pFeb", "pMar", "pApr", "pMay", "pJun", "pJul", "pAug", "pSep", "pOct", "pNov", "pDec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

all.cors.sub<- merge(all.cors.sub, month.df, by = "month")
all.cors.sub$mo.clim <- factor(all.cors.sub$mo.clim, levels = c("pJan", "pFeb", "pMar", "pApr", "pMay", "pJun", "pJul", "pAug", "pSep", "pOct", "pNov", "pDec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

VPDmax.cors <- ggplot(data = all.cors.sub, 
                    aes(x=mo.clim,
                        y= cor, 
                        ymin=ci.min, 
                        ymax=ci.max,       
                        fill=site)) +
  geom_bar(position="dodge", stat = "identity") + 
  geom_errorbar( position = position_dodge(), colour="grey")+
  scale_fill_manual(values = c(`BON`= "#d73027",
                               `GL1`="#f46d43",
                               `GL2`="#fdae61",
                               `GL3`= "#fee090",
                               `ENG`="#ffffbf",
                               `UNC`="#e0f3f8",
                               `AVO`="#abd9e9",
                               `MOU`="#74add1",
                               `GLA`="#4575b4"))+ theme_bw(base_size = 12)+theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+ylab("Correlation Coefficient")+xlab("Maximum VPD")

png(height = 4, width = 12, units = "in", res = 300, "outputs/growth_model/paper_figures/full_PRISM_VPDmax_all_sites_correlation_bootci.png")
VPDmax.cors 
dev.off()

VPDmax.cors.noboot <- ggplot(data = all.cors.sub, 
                           aes(x=mo.clim,
                               y= cor, 
                               #ymin=ci.min, 
                               #ymax=ci.max,       
                               fill=site)) +
  geom_bar(position="dodge", stat = "identity") + 
  #geom_errorbar( position = position_dodge(), colour="grey")+
  scale_fill_manual(values = c(`BON`= "#d73027",
                               `GL1`="#f46d43",
                               `GL2`="#fdae61",
                               `GL3`= "#fee090",
                               `ENG`="#ffffbf",
                               `UNC`="#e0f3f8",
                               `AVO`="#abd9e9",
                               `MOU`="#74add1",
                               `GLA`="#4575b4"))+ theme_bw(base_size = 12)+theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+ylab("Correlation Coefficient")+xlab("Maximum VPD")

png(height = 4, width = 12, units = "in", res = 300, "outputs/growth_model/paper_figures/full_PRISM_VPDmax_all_sites_correlation.png")
VPDmax.cors.noboot
dev.off()

site.legend <- get_legend(precipitation.cors.noboot)

# make a big plot with tmax, precip, and vpdmax:
png(height = 12, width = 15, units = "in", res = 300, "outputs/growth_model/paper_figures/full_PRISM_3clim_all_sites_correlation_bootci.png")
plot_grid(
plot_grid(precipitation.cors, 
          tmax.cors, 
          VPDmax.cors, ncol = 1, labels = "AUTO"),
site.legend, ncol = 2, rel_widths = c(1,0.05))
dev.off()


# make a big plot with tmax, precip, and vpdmax, but no confidence intervals
png(height = 12, width = 15, units = "in", res = 300, "outputs/growth_model/paper_figures/full_PRISM_3clim_all_sites_correlation.png")
plot_grid(
plot_grid(precipitation.cors.noboot+theme(legend.position = "none"), 
          tmax.cors.noboot+theme(legend.position = "none"), 
          VPDmax.cors.noboot+theme(legend.position = "none"), ncol = 1, labels = "AUTO"), 
site.legend, ncol = 2, rel_widths = c(1,0.05))
dev.off()



# for VPD max:
read.BAL.cors <- function(x){
  
  BALcors <- read.csv(paste0("data/BootCors/PRISM/", x, "-", "WW", "BALcor.csv"))
  BALcors$site <- x 
  BALcors
}

cor.list <- list()
for(i in 1:length(site.codes)){
  cor.list[[i]] <- read.BAL.cors(site.codes[i])
}


all.cors <- do.call(rbind, cor.list)
all.cors.sub <- all.cors[all.cors$site %in% c("AVO", "BON","ENG", "GLA", "GL1", "GL2", "GL3", "MOU", "UNC"), ]
all.cors.sub$site <- factor(all.cors.sub$site, levels = c("BON", "GL1", "GL2", "GL3", "ENG", "UNC", "AVO", "MOU", "GLA"))


month.df <- data.frame(month = 1:24, 
                       mo.clim = c("pJan", "pFeb", "pMar", "pApr", "pMay", "pJun", "pJul", "pAug", "pSep", "pOct", "pNov", "pDec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

all.cors.sub<- merge(all.cors.sub, month.df, by = "month")
all.cors.sub$mo.clim <- factor(all.cors.sub$mo.clim, levels = c("pJan", "pFeb", "pMar", "pApr", "pMay", "pJun", "pJul", "pAug", "pSep", "pOct", "pNov", "pDec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

BAL.cors <- ggplot(data = all.cors.sub, 
                      aes(x=mo.clim,
                          y= cor, 
                          ymin=ci.min, 
                          ymax=ci.max,       
                          fill=site)) +
  geom_bar(position="dodge", stat = "identity") + 
  geom_errorbar( position = position_dodge(), colour="grey")+
  scale_fill_manual(values = c(`BON`= "#d73027",
                               `GL1`="#f46d43",
                               `GL2`="#fdae61",
                               `GL3`= "#fee090",
                               `ENG`="#ffffbf",
                               `UNC`="#e0f3f8",
                               `AVO`="#abd9e9",
                               `MOU`="#74add1",
                               `GLA`="#4575b4"))+ theme_bw(base_size = 12)+theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+ylab("Correlation Coefficient")+xlab("Precipitation - Potential Evapotranspiration")

png(height = 4, width = 12, units = "in", res = 300, "outputs/growth_model/paper_figures/full_PRISM_BAL_all_sites_correlation_bootci.png")
BAL.cors 
dev.off()

BAL.cors.noboot <- ggplot(data = all.cors.sub, 
                             aes(x=mo.clim,
                                 y= cor, 
                                 #ymin=ci.min, 
                                 #ymax=ci.max,       
                                 fill=site)) +
  geom_bar(position="dodge", stat = "identity") + 
  #geom_errorbar( position = position_dodge(), colour="grey")+
  scale_fill_manual(values = c(`BON`= "#d73027",
                               `GL1`="#f46d43",
                               `GL2`="#fdae61",
                               `GL3`= "#fee090",
                               `ENG`="#ffffbf",
                               `UNC`="#e0f3f8",
                               `AVO`="#abd9e9",
                               `MOU`="#74add1",
                               `GLA`="#4575b4"))+ theme_bw(base_size = 12)+theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+ylab("Correlation Coefficient")+xlab("Precipitation - Potential Evapotranspiration")

png(height = 4, width = 12, units = "in", res = 300, "outputs/growth_model/paper_figures/full_PRISM_BAL_all_sites_correlation.png")
BAL.cors.noboot
dev.off()

site.legend <- get_legend(precipitation.cors.noboot)

# make a big plot with tmax, precip, and BAL:
png(height = 16, width = 15, units = "in", res = 300, "outputs/growth_model/paper_figures/full_PRISM_3clim_all_sites_correlation_bootci.png")
plot_grid(
  plot_grid(precipitation.cors, 
            tmax.cors, 
            BAL.cors, 
            VPDmax.cors, ncol = 1, labels = "AUTO"),
  site.legend, ncol = 2, rel_widths = c(1,0.05))
dev.off()


# make a big plot with tmax, precip, and BAL, but no confidence intervals
png(height = 16, width = 15, units = "in", res = 300, "outputs/growth_model/paper_figures/full_PRISM_3clim_all_sites_correlation.png")
plot_grid(
  plot_grid(precipitation.cors.noboot+theme(legend.position = "none"), 
            tmax.cors.noboot+theme(legend.position = "none"), 
            BAL.cors.noboot+theme(legend.position = "none"),
            VPDmax.cors.noboot + theme(legend.position = "none"), ncol = 1, labels = "AUTO"), 
  site.legend, ncol = 2, rel_widths = c(1,0.05))
dev.off()
