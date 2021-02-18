# TreeRings
Tree ring analyses after cofecha crossdating & bayesian models of tree ring growth, Water Use Efficiency, and &delta;<sup>13</sup>C .

This repository holds the code used to quantify the effects of climate, tree size, stand strucutre, and CO2 on tree ring growth across the savanna-forest boundary region in the Uppermidwest US. Specifically, we develop bayesian heiarchical models to understand the joint effects of multiple envirmental factors on tree growth across 9 sites in the upper midwest. 

![Map of sites included in this analysis](images/site_map_general.png)

It also houses a variety of general scripts used to process tree ring widths, growth sensitivities to climate, and site level differences in climate sensitivity. 

# Contents:

# Data used in this manuscript are archived:


### R/manuscript_code:
Contains R code to run jags bayesian models of tree ring growth  and bayesian models of water use efficiency and &delta;<sup>13</sup>C . Analysis covers 9 sites across the upper midwest US, spanning the savanna-forest boundary.

#### Code used to run bayesian models and generate figures in the manuscript

- Aggregation of tree ring rwls into a single dataframe, with associated climate data, and back-calculated tree diameter are is conducted in **RW_trends.R** (calls code from **tree_age_agg.R**)
- Separation of ring width and iWUE data into testing and training datasets conducted in **clean_separate_data.R** (Requires outputs from **RW_trends.R** & **calc_WUE.R**)
- All bayesian models (including the selected model) explored in this analysis are detailed in **RWI_models.Rmd** 
- Selected tree ring growth model is run in **RWI_bayes_model.R** (run with globally scaled covarate data)
- Selected tree ring growth model after doing cohort/structure covariate scaling is in **RWI_bayes_model_scaled_struct_cohort.R**
- Chosen WUE and &delta;<sup>13</sup>C bayesian heiarichical models are in **WUE_bayes_model**
- Paper figures for the manuscript are generated in **Paper_figures.R** (global scaling) & **Paper_figures_cohort_struct_scaled.R**
- Summary tables are generated in **Summary_tables.Rmd**
- WUE tables are generated in **WUE_d13C_model_tables.Rmd**

#### Code used to clean up and calculate WUE from &delta;<sup>13</sup>C  values
- R code used to read in all the &delta;<sup>13</sup>C  values, correct for the suess effect and do some  preliminary plotting and cleaning of the data in **read_plot_deltaC.R**
- R code used to calucluate intrinsic water use efficiency from the stable isotope values output from **read_plot_deltaC.R** is **calc_WUE.R**



### R/Preliminary_analyses:
Contains R code for detrending tree ring widths, and conducting preliminary correlations with climate for each site. 

### UNDERC_training folder
Contains rcode used to train and demonstrate basic climate correlations and stand mapping for tree ring analyses

- Basic detrending options in dplr are in **dplR_detrend.R**
- Some code to generate stand maps from plot coordinates is in **UNDERC_plot_maps.R**
- A basic guide to generating tree ring growth - climate correlations/responses is in **Climate_response_UNDERC.R**

### R/Preliminary_analyses/Isotopes
Contains the code used to read in IRMS data and do standard corrections.

- R code used to read in IRMS raw data and correct for standards **IRMS_Data_Process_Raw.R**
- R code used to identify potential years with similar climate to do sampling on is in **Picking_isotope_years.R**
