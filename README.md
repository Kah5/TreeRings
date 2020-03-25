# TreeRings
Tree ring analyses after cofecha crossdating

This repository houses scripts used to processing tree ring widths, growth sensitivities to climate, and site level differences in climate sensitivity.

# Contents:

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

### R/Preliminary_analyses:
Contains R code for detrending tree ring widths, and conducting preliminary correlations with climate for each site. 

### UNDERC_training folder
Contains rcode used to train and demonstrate basic climate correlations and stand mapping for tree ring analyses

-Basic detrending options in dplr are in **dplR_detrend.R**
-Some code to generate stand maps from plot coordinates is in **UNDERC_plot_maps.R**
-A basic guide to generating tree ring growth - climate correlations/responses is in **Climate_response_UNDERC.R**

### Isotopes folder
Contains the code used to read in IRMS data, do standard corrections,

-R code used to read in IRMS raw data and correct for standards **IRMS_Data_Process_Raw.R**
-R code used to identify potential years with similar climate to do sampling on is in **Picking_isotope_years.R**
-R code used to read in all the $delta$ 13 C values, correct for the suess effect and do some  preliminary plotting and cleaning of the data in **read_plot_deltaC.R**
-R code used to calucluate intrinsic water use efficiency from the stable isotpe values is **calc_WUE.R**