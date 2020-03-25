# TreeRings
Tree ring analyses after cofecha crossdating

This repository houses scripts used to processing tree ring widths, growth sensitivities to climate, and site level differences in climate sensitivity.

# Contents:
### R/Preliminary_analyses:
Rscript for detrending tree ring widths, and conducting preliminary correlations with climate for each site.
Rscript for plotting correlations with climate


### R/manuscript_code:
Contains R code to run jags bayesian models of tree ring growth  and bayesian models of water use efficiency and delta 13C. Analysis covers 9 sites across the upper midwest US, spanning the savanna-forest boundary.

#### Code used to run bayesian models and generate figures in the manuscript
- Cleaning of rwl names, back-calculating tree diameter, and some prelimnary analysis occurs in **clean_rwl_names.R**
- Aggregation of tree ring rwls into a single dataframe, with associated cliamte data
- Tree age estimates are is conducted in **tree_age_agg.R**
- Separation of data into testing and training datasets conducted in **clean_separate_data.R**
- All bayesian models (including the selected model) explored in this analysis are detailed in **RWI_models.Rmd** 
- Selected tree ring growth model is run in **RWI_bayes_model.R**
- Chosen WUE and d13C bayesian heiarichical models are in **WUE_bayes_model**
- Paper figures for the manuscript are generated in **Paper_figures.R**
- Summary tables are generated in **Summary_tables.Rmd**
- WUE tables are generated in **WUE_d13C_model_tables.Rmd**

