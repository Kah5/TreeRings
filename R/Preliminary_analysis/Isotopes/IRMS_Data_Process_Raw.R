# IRMS_Data_Process_Raw.R
# This script reads in raw data from the CEST IRMS (Delta V), pulls out the d13C peaks, creates std curve
# after correcting raw values, export data to one large csv with raw data and corrected data. 
# updated from Jones lab program
# input data format: data should be as exported using the Heilman.wke from the Isodat Software on the Delta Plus IRMS at CEST. To ease importing into R, covert .xlsx to .csv before importing


library(dplyr) 
library(ggplot2) 
date <- ("08_09_19") # enter date of sample run


# -------------------------------Load raw isotope data in csv format---------------------
filename <- file.choose()
data.raw <- read.csv(filename)
#data.raw  <- read.csv("data/stable_isotopes/raw_irms/Export_180321.csv") # need to change this filename

head(data.raw)

# ------------------------------------------QA/QC blank peaks ---------------------------
#Check the Blanks for any blanks with more than 4 peaks
data.blanks <- filter(data.raw, Identifier.2=="blank") 

#check if there are any N or CO2 blank peaks - in blanks 
#if there are more than 4 peaks in blank it will tell you where
if (max(data.blanks$Peak.Nr)>6){
  message("there are blank peaks in these samples: ")
  print(data.blanks[data.blanks$Peak.Nr>4,1:4])
  readline("press enter to continue")
} 

# just check that these are only N peaks--

#-----------------------------------Grab sample and standard data-----------------------------------

if("Time" %in% colnames(data.raw)){
  data.raw$Time.Code <- paste(data.raw$Time, data.raw$Date)
}
#get the all samples excluding the blanks

data.refs.samples <- filter(data.raw, !Identifier.2=="blank",!Identifier.2=="trash")%>%dplyr::select(Identifier.1, Identifier.2, Peak.Nr, d.15N.14N, d.13C.12C, Time.Code)
head(data.refs.samples)

# a couple of special cases where the data was formattted differently

# a special case where the data contains several samples that did not properly drop, so there are several flags on data to check:
if(filename %in% c("/Users/kah/Documents/TreeRings/data/stable_isotopes/raw_irms/BON13a_1969_1954_171115.csv")){
  data.raw$d.15N.14N <- NA
  data.raw$Peak.Nr <- ifelse(data.raw$Identifier.2 %in% "std", 5, 4)
  data.raw <- data.raw[data.raw$Notes %in% c("g", "std"),]
  data.refs.samples <- filter(data.raw, !Identifier.2=="blank",!Identifier.2=="trash")%>%select(c("Identifier.1", "Identifier.2","Peak.Nr", "d.15N.14N","d.13C.12C", "Time.Code"))
  
  head(data.refs.samples)
}

# a special case where we have different columns in the data (these are outputs from early runs)
if(filename %in% c("/Users/kah/Documents/TreeRings/data/stable_isotopes/raw_irms/170420_Heilman.csv", 
                   "/Users/kah/Documents/TreeRings/data/stable_isotopes/raw_irms/170511_Heilman_full.csv", 
                   "/Users/kah/Documents/TreeRings/data/stable_isotopes/raw_irms/Export_171115.csv")){
  data.refs.samples <- filter(data.raw, !Identifier.2=="blank",!Identifier.2=="trash")%>%select(c("Identifier.1", "Identifier.2","Peak.Nr",  "d.15N.14N", "d.13C.12C", "Time.Code"))
  head(data.refs.samples)
}


# since I have no N peaks for the samples, but I do in my standards, we need to grab a different peak number depending on whether the Identifier.2 == std or not:

data.N.std <- filter(data.refs.samples, Peak.Nr==4, Identifier.2 == "std") %>% dplyr::select(-c(5,6)) # grab all N peaks for std
data.C.std <- filter(data.refs.samples, Peak.Nr==5, Identifier.2 == "std") %>% dplyr::select(-c(4,6)) # grab all C peaks for sts

data.C.std  <- data.C.std [data.C.std$d.13C.12C < 0,] # if there are any strnage peaks, get rid of them here
# if protein is off (as in run on 02/19/18), then get rid of this value:
data.C.std <- data.C.std [data.C.std$d.13C.12C > -30,]

data.C <- filter(data.refs.samples, Peak.Nr==4, !Identifier.2 == "std") # grab all the C peaks for the data (kh cellulose does not have any N peaks)
data.C<- data.C[data.C$d.13C.12C < 0,]
# this is for the special case where we prescreened the N peaks out
if(length(data.N.std$Identifier.1) > 0){
std.NC <- merge( data.C.std, data.N.std, by = c("Identifier.1", "Identifier.2"))# combine N and C data
}else{
  std.NC <- data.C.std
  std.NC$Peak_NrN <- NA
  std.NC$d15N_14N <- NA
  std.NC <- std.NC[,c("Identifier.1", "Identifier.2", "Peak.Nr", "d.13C.12C", "Peak_NrN", "d15N_14N")]
}

head(std.NC)
colnames <- c("Sample","Type", "Peak_NrC","d13C_12C","Peak_NrN", "d15N_14N")
names(std.NC) <- colnames

# -------------------------------- create standard curve -----------------------------------------------

# select sorghum standard data-- in the old code these values are averaged, then regressed, but I am going to keep them for now
sorghum <- filter(std.NC, Sample %in% c("sourgumstd", "sourgumstd2","Sorhgum1std", "Sorhgum2std", "Sorhgum3std", "Sorghum4std", "Sorghum1std", "Sorghum2std", "Sorghum3std", "Sorghum4std"))#%>%summarise(d13C_12C=mean(d13C_12C))        

# select peach leaves standard data
peach <- filter(std.NC, Sample %in% c("peachstd", "peachstd2","Peach1std", "Peach2std", "Peach3std", "Peach4std"))#%>%summarise(d13C_12C=mean(d13C_12C))

# select protein standard data
protein <- filter(std.NC, Sample %in% c("proteinstd", "proteinstd2","Protein1std", "Protein2std", "Protein3std", "Protein4std"))#%>%summarise(d13C_12C=mean(d13C_12C))

protein <- protein[protein$d13C_12C < -20,] # if the samples didnt drop, then peak 5 will be the background CO2 peak, which we need to remove
peach <- peach[peach$d13C_12C < -20,]



# set up vectors for observed and expected delta 13 C values
if(date %in% "04_26_19"){ # standards were too large for this day, so removing one of them:
  obs_13C <- c(sorghum[1:2,]$d13C_12C, peach[1:2,]$d13C_12C, protein[1:2,]$d13C_12C) # get vector of observed standards
  exp_13C <- c(rep(-13.68, length(sorghum[1:2,]$d13C_12C)), rep(-25.95, length(peach[1:2,]$d13C_12C)), rep(-26.98, length(protein[1:2,]$d13C_12C))) # since we might have variable # of standards for each run, use "rep"
  
}else{
  obs_13C <- c(sorghum$d13C_12C, peach$d13C_12C, protein$d13C_12C) # get vector of observed standards
  exp_13C <- c(rep(-13.68, length(sorghum$d13C_12C)), rep(-25.95, length(peach$d13C_12C)), rep(-26.98, length(protein$d13C_12C))) # since we might have variable # of standards for each run, use "rep"
}
# run a linear regression on expected vs. observed values
modelC <- lm( exp_13C ~ obs_13C )
summary(modelC)# check p value + Rsquared



ccoeff <- coefficients(modelC)

# correct delta 13C values based on standard calibration equation
data.C$d13C_12C_corr <- ccoeff[[1]] + ccoeff[[2]] * data.C$d.13C.12C

#Quick Summary Plots
ggplot(data.C, aes(Identifier.1, d13C_12C_corr))+geom_point()
ggplot(data.C, aes(Identifier.2, d13C_12C_corr))+geom_point()

#Plot of Standards:
plot(obs_13C[1:9], exp_13C[1:9])
abline(a = 0, b = 1)

stds.corrected <- ccoeff[[1]] + ccoeff[[2]] *obs_13C[1:9] 

summary(stds.corrected - exp_13C[1:9])


#save data - save raw, processed, and database prep data into a both individual csvs:
#setwd("/Users/kah/Documents/TreeRings/data/stable_isotopes/deltaC/")
setwd("/Users/kah/Documents/TreeRings")
  savefilename<-paste0('data/stable_isotopes/deltaC/',date,'_deltaC_')
  write.csv(std.NC, paste0(savefilename,'stds.csv'), row.names=F)
  write.csv(data.C, paste0(savefilename,'data.csv'),row.names=F)


# add processed samples to existing master CSV sample (if it exists) 
  # note that you can only run this once per raw data file, otherwise it will keep adding repeated data

if(exists(paste0("data/stable_isotopes/deltaC/cellulose_processed_samps.csv"))){
  samps <- read.csv('data/stable_isotopes/deltaC/cellulose_processed_samps.csv', header=T)
  samps <- rbind(samps, data.C) #add refs to file
  View(samps)
  write.csv(samps,'data/stable_isotopes/deltaC/cellulose_processed_samps.csv') 
 
   }else{
  write.csv(data.C, "data/stable_isotopes/deltaC/cellulose_processed_samps.csv")
}
  


 