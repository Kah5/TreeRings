library(tidyr)
library(dplyr)


# calculate the machine precision for these runs:
setwd("/Users/kah/Documents/TreeRings")
savefilename <- paste0('data/stable_isotopes/deltaC/',date,'_deltaC_')
#write.csv(std.NC, paste0(savefilename,'stds.csv'), row.names=F)

all.standards <- paste0("/Users/kah/Documents/TreeRings/data/stable_isotopes/deltaC/", 
                        list.files('data/stable_isotopes/deltaC/', pattern = "stds.csv"))

stds <- lapply(all.standards[1:26], read.csv)

stds.new  <- lapply(stds, function(x){
  x[c("Sample", "Type", "d13C_12C")] # get only the d13C for all the values
})


merge.stds <- function(x){
  
  x$standard <- ifelse(x$Sample %in% c("Peach1std", "Peach2std", "Peach3std", "peachstd", "peachstd2", "Peach4std"), "Peach",
                             ifelse(x$Sample %in% c("Protein1std", "Protein2std", "Protein3std", "Protein4std", "proteinstd", "proteinstd2"), "Protein",
                                    ifelse(x$Sample %in% c("Sorhgum1std", "Sorhgum2std", "Sorhgum3std", "Sorhgum4std", "sourgumstd", "sourgumstd2", "sorhgumstd2"), "Sorhgum", "blank")))
  
  standard.exp <- data.frame(standard = c("Peach", "Protein", "Sorhgum"),
                             actual = c(-25.95,-26.98, -13.68))
  new.x <- merge(x, standard.exp, by = "standard")
  new.x
}


merged.stds <- lapply(stds.new, merge.stds)


generate.corrected.d13 <- function(x){
  # generate a relationship between all actual values & the d13C instrument values:
  x$diff <- x$d13C_12C-x$actual
  x <- x[x$diff <=3, ]
  
  modelC <- lm(x$actual ~ x$d13C_12C)
  model.summary <- summary(modelC)# check p value + Rsquared
  
  x$r.squared <- model.summary$r.squared
  
  ccoeff <- coefficients(modelC)
  
  # correct delta 13C values based on standard calibration equation
  x$d13C_12C_corr <- ccoeff[[1]] + ccoeff[[2]] * x$d13C_12C
  x
}


corr.stds <- lapply(merged.stds, generate.corrected.d13)

# make into a dataframe
stds.df <- do.call(rbind, corr.stds )
# add an id for each run:
stds.df$runid <- rep(1:26, sapply(corr.stds , nrow)) # add the site names



std.diffs <- stds.df %>% group_by(runid, standard) %>% summarise(mean= mean(d13C_12C_corr),
                                                                   sd = sd(d13C_12C_corr),
                                                                 r.squared = mean(r.squared))#,
                                                                   #avg.diff = mean(diff), 
                                                                   #sd.diff = sd(diff))

stds.df%>% filter(r.squared >= 0.99)

stds.df %>% filter(r.squared >= 0.99) %>% group_by(standard) %>% summarise(mean= mean(d13C_12C_corr),
                                               sd = sd(d13C_12C_corr),
                                               r.squared = mean(r.squared))

stds.df$cor.diff <- stds.df$d13C_12C_corr-stds.df$actual

stds.df %>% filter(r.squared >= 0.99) %>% filter(abs(cor.diff) < 0.7) %>% group_by(standard) %>% summarise(mean= mean(d13C_12C_corr),
                                                                           sd = sd(d13C_12C_corr),
                                                                           r.squared = mean(r.squared), 
                                                                           mean.diff = mean(cor.diff),
                                                                           sd.diff = sd(cor.diff), 
                                                                           actual.mean = mean(actual))

stds.df %>% filter(r.squared >= 0.99) %>% group_by(standard) %>% summarise(mean= mean(d13C_12C_corr),
                                                                                                           sd = sd(d13C_12C_corr),
                                                                                                           r.squared = mean(r.squared), 
                                                                                                           mean.diff = mean(cor.diff),
                                                                                                           sd.diff = sd(cor.diff), 
                                                                                                           actual.mean = mean(actual))


 # make into a dataframe
# stds.df <- do.call(rbind, stds.new)
# # add an id for each run:
# stds.df$runid <- rep(1:26, sapply(stds.new, nrow)) # add the site names



# stds.df$standard <- ifelse(stds.df$Sample %in% c("Peach1std", "Peach2std", "Peach3std", "peachstd", "peachstd2", "Peach4std"), "Peach",
#                            ifelse(stds.df$Sample %in% c("Protein1std", "Protein2std", "Protein3std", "Protein4std", "proteinstd", "proteinstd2"), "Protein",
#                                   ifelse(stds.df$Sample %in% c("Sorhgum1std", "Sorhgum2std", "Sorhgum3std", "Sorhgum4std", "sourgumstd", "sourgumstd2", "sorhgumstd2"), "Sorhgum", "blank")))
# 
# standard.exp <- data.frame(standard = c("Peach", "Protein", "Sorhgum"),
#            actual = c(-25.95,-26.98, -13.68))

# stds.full <- merge(stds.df, standard.exp, by = "standard")
# stds.full$diff <- stds.full$actual - stds.full$d13C_12C
# 

stds.full <- stds.full[stds.full$diff < 5,]
std.diffs <- stds.full %>% group_by(runid, standard) %>% summarise(mean= mean(d13C_12C),
                                                                   sd = sd(d13C_12C),
                                                                   avg.diff = mean(diff), 
                                            sd.diff = sd(diff))


stds.full %>% group_by(standard) %>% summarise(mean= mean(d13C_12C),
                                                      sd = sd(d13C_12C),
                                                      avg.diff = mean(diff), 
                                                      sd.diff = sd(diff))


#stds.clean <- stds.full[stds.full$diff <=3, ]
hist(stds.clean$diff)



# finding repeated measurements of alpha cellulose to quantify precision:
full.df <- read.csv( "outputs/stable_isotopes/full_std_suess_corrected_d13C_v2.csv")


duplicated.df <- full.df[duplicated(full.df[c("year", "ID", "site")]),]
summary.dups <- duplicated.df %>% group_by(year, ID, site) %>% summarise(d13 = mean(d13C_12C_corr), sd = sd(d13C_12C_corr), n = n())

