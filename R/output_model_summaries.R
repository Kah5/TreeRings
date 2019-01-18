# model comparison
# read in output model summaries and compile
# output full model summary comparison

setwd("/Users/kah/Documents/TreeRings/outputs/growth_model/model_summary/")

#-----------------read in model summaries-----------------------
summary.files <- list.files(pattern = ".csv$") # get all the data files
summary.data <- lapply(summary.files, FUN = read.csv) 

# rename because some have model.structure listed as model summary
colnames.ls <- c("model", "model.structure", "MSE", "BIAS", "Rsq")

summary.data <- lapply(summary.data, setNames, colnames.ls)
summary.data.df <- do.call(rbind, summary.data) # convert from list of df to df




#--------------------- get DIC estimates from each model--------

summary.files.DIC <- list.files(pattern = "DIC.rds$") # get all the data files
summary.dic.DATA <- lapply(summary.files.DIC, FUN = readRDS)

# get names of models: (in theory these should be the same as "model" in summary.data.df)
model.names <- stringr::str_remove(summary.files.DIC, "_DIC.rds")
names(summary.dic.DATA) <- model.names


# summarise DIC values using sums--need to do this & unlist the values:

deviances <- lapply(summary.dic.DATA, FUN = function(x){sum(x$deviance)})
penalties <- lapply(summary.dic.DATA, FUN = function(x){sum(x$penalty)})

deviance.df <- data.frame(penalties = (unlist(penalties)), 
           deviances = (unlist(deviances)))
deviance.df$model <- rownames(deviance.df)

# add together to get penalized deviances:
deviance.df$penalized_deviance <- deviance.df$penalties + deviance.df$deviances


length(deviance.df$penalties) # only have for 18 models, need to run for the rest:

# merge with the summary.data.df dataframe:
summary.data.df.dev <- merge(summary.data.df, deviance.df, by = "model", all.x = TRUE)

# list any models with missing dic samples
summary.data.df[is.na(summary.data.df.dev$deviances),]$model
summary.data.df.dev<- summary.data.df.dev %>% arrange(Rsq,penalized_deviance) # order by rsq and dev

setwd("/Users/kah/Documents/TreeRings")

write.csv(summary.data.df.dev ,"/Users/kah/Documents/TreeRings/outputs/growth_model/model_summary_full.csv", row.names = FALSE)





