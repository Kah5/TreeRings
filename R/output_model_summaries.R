# model comparison
# read in output model summaries and compile
# output full model summary comparison

setwd("/Users/kah/Documents/TreeRings/outputs/growth_model/model_summary/")

summary.files <- list.files(pattern = ".csv$") # get all the data files
summary.data <- lapply(summary.files, FUN = read.csv) 
summary.data.df <- do.call(rbind, summary.data) # converte from list of df to df

write.csv(summary.data.df ,"/Users/kah/Documents/TreeRings/outputs/growth_model/model_summary_full.csv", row.names = FALSE)
