# model comparison
# read in output model summaries and compile
# output full model summary comparison

setwd("/Users/kah/Documents/TreeRings/outputs/growth_model/model_summary/")

summary.files <- list.files(pattern = ".csv$") # get all the data files
summary.data <- lapply(summary.files, FUN = read.csv) 
summary.data.df <- do.call(rbind, summary.data) # converte from list of df to df

# get DIC estimates:

summary.files.DIC <- list.files(pattern = "DIC.rds$") # get all the data files
summary.dic.DATA <- lapply(summary.files, FUN = readRDS)

# get names of models:
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


summary.data.df.dev <- merge(summary.data.df, deviance.df, by = "model", all.x = TRUE)

# merge with the summary.data.df dataframe:

write.csv(summary.data.df.dev ,"/Users/kah/Documents/TreeRings/outputs/growth_model/model_summary_full.csv", row.names = FALSE)
