# this script does some basic cleaning of the Tree ring rwl files
# this renames the id's so that dplR can recognize cores from the same tree
# the rwl file sare now in the "cleanrwl" folder

library(dplR)

# This function renames many of the early tree ring records recorded (we had inconsistant naming structure at the beginning)

clean_rwl <- function(site, rwl.file, wood){
  if(site == "HIC"){
    newseries <- read.rwl(rwl.file)
    new <- colnames(newseries)
    
      for(i in 1:length(colnames(newseries))){
        core<- substring(colnames(newseries)[i], first = 4, 4)
        tree<- substring(colnames(newseries)[i], first = 5, 7)
        site <- substring(colnames(newseries)[i], first = 1, 3)
        new[i] <- paste0(site, tree, core)
      }
    colnames(newseries) <- new
    #read.ids(newseries,stc = c(3,3,3))
    write.rwl(newseries, fname = paste0("./cleanrwl/",site, wood,".rwl") , format="tucson")
  }else{
    if(site == "STC"){
      newseries <- read.rwl(rwl.file)
      
      # choose only well crossdated cores for STC:
      newseries <- newseries[,c("STC211c",  "STC2c11",  "STC2c13",  "STC411c", 
                                "STC4a11",  "STC511a" , "STC5b11",  "STC5c11",  "STC611a",  "STC6a1a", 
                                "STC6b11" , "STC711b",  "STC7c11" , "STC822a" , "STC8b11",  "STC8c11", 
                                "STC911b" ,  "STC9a11",  "STC9c11" )]
      
      
      
      colnames(newseries) <- c("STC2c11", "STC2b11", "STC2a11", "STC4b11", "STC4c11", "STC5a11", 
                              "STC5b11", "STC5c11", "STC6a11", "STC6c11", "STC6b11", "STC7b11",
                              "STC7c11", "STC8a11", "STC8b11", "STC8c11", "STC9a11", "STC9c11", "STC9b11")
      write.rwl(newseries, fname = paste0("./cleanrwl/",site, wood,".rwl") , format="tucson")
      
    }else{
      if(site == "BON"){
        newseries <- read.rwl(rwl.file)
        colnames(newseries) <- c("BON1a11" , "BON1b11" , "BON1c11",  "BON4a11",  "BON5a11",  "BON6a11","BON7a11",  "BON7b11",  "BON7c11",  "BON8a11", 
                               "BON8b11",  "BON8c11",  "BON9b11",  "BON9a11",  "BON9c11",  "BON10a11",   "BON10b11" ,  "BON10c11",   "BON11a11",   "BON12a11",  
                               "BON13b11",   "BON13a11","BON13c11"  , "BON14a11",   "BON14b11",   "BON14c11",   "BON15a11")
        
      
        read.ids(newseries, "auto")
        write.rwl(newseries, fname = paste0("./cleanrwl/",site, wood,".rwl") , format="tucson")
        
      }else{
        if(site == "MOU"){
          newseries <- read.rwl(rwl.file)
          # check these for MOU:
          colnames(newseries) <- c("MOU1a1", "MOU2a1", "MOU2b1" ,"MOU2c1", "MOU311", "MOU3a1", "MOU3b1", "MOU3c1", "MOU440", "MOU441", "MOU4b1", "MOU511",
                                    "MOU5a1", "MOU611", "MOU6a1", "MOU6b1", "MOU6c1")
            write.rwl(newseries, fname = paste0("./cleanrwl/",site, wood,".rwl") , format="tucson")
          
        }else{
          if(site == "GLA"){
          newseries <- read.rwl(rwl.file)
          colnames(newseries) <-  c("GLA1791a", "GLA1423a", "GLA1700a", "GLA1883a", "GLA1883b" ,"GLA1883c", "GLA1884a", "GLA1884b", "GLA1884c", "GLA0468a",
                                    "GLA609a1", "GLA0609b", "GLA609c", "GLA0867a", "GLA0869a", "GLA0869b", "GLA0869c" ,"GLA0870c", "GLA0870a", "GLA0870b",
                                    "GLA870c1", "GLA0871a", "GLA0871b", "GLA0871c1", "GLA0872b", "GLA0872c", "GLA0873c" ,"GLA0873a", "GLA999a")
          #read.ids(newseries, "auto")
          write.tucson(newseries, fname = paste0("./cleanrwl/",site, wood,".rwl") , format="tucson", long.names=TRUE)
          
          
          }else{
            if(site %in% c("UNC", "TOW", "ENG", "PLE","GLL1", "GLL2", "GLL3", "GLL4", "PVC")){
            newseries <- read.rwl(rwl.file)
            write.tucson(newseries, fname = paste0("./cleanrwl/",site, wood,".rwl") , format="tucson", long.names=TRUE)
            
            }else{
              if(site == "COR"){
                newseries <- read.rwl(rwl.file)
                new <- colnames(newseries)
                
                # rename the cores so that they have letters, not numbers to distinguish cores
                for(i in 1:length(colnames(newseries))){
                  core <- ifelse(substring(colnames(newseries)[i], first = 8, 8)==0, 'a',
                                 ifelse(substring(colnames(newseries)[i], first = 8, 8)==1, 'b','c'))
                  tree <- substring(colnames(newseries)[i], first = 4, 7)
                  site <- substring(colnames(newseries)[i], first = 1, 3)
                  new[i] <- paste0(site, tree, core)
                }
                colnames(newseries) <- new
                
              }else{cat("NA")}
              write.tucson(newseries, fname = paste0("./cleanrwl/",site, wood,".rwl") , format="tucson", long.names=TRUE)
              
            }}
          
          }
          }
      }
    }
}


# run the function on all the sites we have measured:
# for Whole Ring widths:
clean_rwl(site = "HIC", rwl.file = "./cofecha/HICww.rwl", "ww")
clean_rwl(site = "BON", rwl.file = "./cofecha/BONww.rwl", "ww")
clean_rwl(site = "STC", rwl.file = "/Users/kah/Documents/crossdating/data/cofecha/STC.rwl", "ww")
clean_rwl(site = "TOW", rwl.file = './cofecha/tow/TOWww.rwl', "ww")
clean_rwl(site = "MOU", rwl.file = "/Users/kah/Documents/crossdating/data/cofecha/MOU.rwl", "ww")
clean_rwl(site = "ENG", rwl.file = "/Users/kah/Documents/crossdating/data/cofecha/ENG.rwl", "ww")
clean_rwl(site = "UNC", rwl.file = "/Users/kah/Documents/crossdating/data/cofecha/UNC.rwl", "ww")
clean_rwl(site = "GLA", rwl.file = "/Users/kah/Documents/crossdating/data/cofecha/GLA.rwl", "ww")
clean_rwl(site = "COR", rwl.file = "/Users/kah/Documents/crossdating/data/cofecha/COR.rwl", "ww")
clean_rwl(site = "GLL1", rwl.file = "/Users/kah/Documents/crossdating/data/cofecha/GLL1.rwl", "ww")
clean_rwl(site = "GLL2", rwl.file = "/Users/kah/Documents/crossdating/data/cofecha/GLL2.rwl", "ww")
clean_rwl(site = "GLL3", rwl.file = "/Users/kah/Documents/crossdating/data/cofecha/GLL3.rwl", "ww")
clean_rwl(site = "GLL4", rwl.file = "/Users/kah/Documents/crossdating/data/cofecha/GLL4.rwl", "ww")
clean_rwl(site = "PLE", rwl.file = './cofecha/PLEww.rwl', "ww")
clean_rwl(site = "PVC", rwl.file = "/Users/kah/Documents/crossdating/data/cofecha/PVC.rwl", "ww")

# for Earlywood
clean_rwl(site = "HIC", rwl.file = "./cofecha/HICew.rwl", "ew")
clean_rwl(site = "BON", rwl.file = "./cofecha/BONew.rwl", "ew")
clean_rwl(site = "STC", rwl.file = "/Users/kah/Documents/crossdating/data/cofecha/STCearlywood width.rwl", "ew")
clean_rwl(site = "TOW", rwl.file = './cofecha/tow/TOWew.rwl', "ew")
clean_rwl(site = "MOU", rwl.file = "/Users/kah/Documents/crossdating/data/cofecha/MOUearlywood width.rwl", "ew")
clean_rwl(site = "ENG", rwl.file = "/Users/kah/Documents/crossdating/data/cofecha/ENGearlywood width.rwl", "ew")
clean_rwl(site = "UNC", rwl.file = "/Users/kah/Documents/crossdating/data/cofecha/UNCearlywood width.rwl", "ew")
clean_rwl(site = "GLA", rwl.file = "/Users/kah/Documents/crossdating/data/cofecha/GLAearlywood width.rwl", "ew")
clean_rwl(site = "COR", rwl.file = "/Users/kah/Documents/crossdating/data/cofecha/CORearlywood width.rwl", "ew")
clean_rwl(site = "GLL1", rwl.file = "/Users/kah/Documents/crossdating/data/cofecha/GLL1earlywood width.rwl", "ew")
clean_rwl(site = "GLL2", rwl.file = "/Users/kah/Documents/crossdating/data/cofecha/GLL2earlywood width.rwl", "ew")
clean_rwl(site = "GLL3", rwl.file = "/Users/kah/Documents/crossdating/data/cofecha/GLL3earlywood width.rwl", "ew")
clean_rwl(site = "GLL4", rwl.file = "/Users/kah/Documents/crossdating/data/cofecha/GLL4earlywood width.rwl", "ew")
clean_rwl(site = "PLE", rwl.file = './cofecha/PLEew.rwl', "ew")
clean_rwl(site = "PVC", rwl.file = "/Users/kah/Documents/crossdating/data/cofecha/PVCearlywood width.rwl", "ew")


# for latewood:
clean_rwl(site = "HIC", rwl.file = "./cofecha/HIClw.rwl", "lw")
clean_rwl(site = "BON", rwl.file = "./cofecha/BONlw.rwl", "lw")
clean_rwl(site = "STC", rwl.file = "/Users/kah/Documents/crossdating/data/cofecha/STClatewood width.rwl", "lw")
clean_rwl(site = "TOW", rwl.file = './cofecha/tow/TOWlw.rwl', "lw")
clean_rwl(site = "MOU", rwl.file = "/Users/kah/Documents/crossdating/data/cofecha/MOUlatewood width.rwl", "lw")
clean_rwl(site = "ENG", rwl.file = "/Users/kah/Documents/crossdating/data/cofecha/ENGlatewood width.rwl", "lw")
clean_rwl(site = "UNC", rwl.file = "/Users/kah/Documents/crossdating/data/cofecha/UNClatewood width.rwl", "lw")
clean_rwl(site = "GLA", rwl.file = "/Users/kah/Documents/crossdating/data/cofecha/GLAlatewood width.rwl", "lw")
clean_rwl(site = "COR", rwl.file = "/Users/kah/Documents/crossdating/data/cofecha/CORlatewood width.rwl", "lw")
clean_rwl(site = "GLL1", rwl.file = "/Users/kah/Documents/crossdating/data/cofecha/GLL1latewood width.rwl", "lw")
clean_rwl(site = "GLL2", rwl.file = "/Users/kah/Documents/crossdating/data/cofecha/GLL2latewood width.rwl", "lw")
clean_rwl(site = "GLL3", rwl.file = "/Users/kah/Documents/crossdating/data/cofecha/GLL3latewood width.rwl", "lw")
clean_rwl(site = "GLL4", rwl.file = "/Users/kah/Documents/crossdating/data/cofecha/GLL4latewood width.rwl", "lw")
clean_rwl(site = "PLE", rwl.file = './cofecha/PLEew.rwl', "lw")
clean_rwl(site = "PVC", rwl.file = "/Users/kah/Documents/crossdating/data/cofecha/PVClatewood width.rwl", "lw")


