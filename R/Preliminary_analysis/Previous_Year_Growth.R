# script to plot the compouning effects of tree growth declines & temperature
# adding in t-1 rwi, instead of assuming "Average tree growth"

cohort.struct.params <- readRDS("outputs/growth_model/lag2_reg_struct_x_cohort_re_t_pr_dry_yrs_site_rs_inter/samps.rds")
train.dry.pair <- readRDS("outputs/growth_model/lag2_reg_struct_x_cohort_re_t_pr_dry_yrs_site_rs_inter/train.rds")
test.dry.pair <- readRDS("outputs/growth_model/lag2_reg_struct_x_cohort_re_t_pr_dry_yrs_site_rs_inter/test.rds")

samps.cs <- as.data.frame(cohort.struct.params)


alpha.samps.cs  <- samps.cs[,1:9]# one alpha for each of 4 cohort-strcuture groups
beta2.samps.cs <- samps.cs[,10:13]
beta3.samps.cs <- samps.cs[,14:17]
beta4.samps.cs <- samps.cs[,18:21]
beta5.samps.cs <- samps.cs[,22:25]
beta6.samps.cs <- samps.cs[,26:29]
beta7.samps.cs <- samps.cs[,30:33]
mu_beta.samps.cs <- samps.cs[,34:40]
sigma.samps.cs <- samps.cs[,41]



# get the different degree scenarios:

mod.mean.clim <-
  test.dry.pair %>% filter(ageclass %in% "Modern" & year > 1950) %>%
  #group_by(site) %>%
  dplyr::summarize(
    mod.mean.Tmax = mean(JUNTmax, na.rm = TRUE),
    one = mean(JUNTmax, na.rm = TRUE) + 1,
    two = mean(JUNTmax, na.rm = TRUE) + 2,
    three = mean(JUNTmax, na.rm = TRUE) + 3,
    four = mean(JUNTmax, na.rm = TRUE) + 4,
    five = mean(JUNTmax, na.rm = TRUE) + 5,
    minusone = mean(JUNTmax, na.rm = TRUE) - 1,
    minustwo =  mean(JUNTmax, na.rm = TRUE) - 2,
    minusthree =  mean(JUNTmax, na.rm = TRUE) - 3,
    minusfour = mean(JUNTmax, na.rm = TRUE) - 4,
    minusfive =  mean(JUNTmax, na.rm = TRUE) - 5,
    
    mod.mean.MAP = mean(MAP.prism, na.rm = TRUE),
    MAPminus100 = mean(MAP.prism, na.rm = TRUE) -
      100,
    MAPminus50 = mean(MAP.prism, na.rm = TRUE) -
      50,
    MAPplus50 = mean(MAP.prism, na.rm = TRUE) +
      50,
    
    MAP750 = 750,
    
    MAP515 = 516.814,
    MAP850 = 850,
    MAP950 = 945.942,
    MAPplus100 = mean(MAP.prism, na.rm = TRUE) +
      100
  )

MAP.scenarios <- mod.mean.clim %>% dplyr::select(mod.mean.MAP:MAPplus100) %>% gather(MAP.Scenario, MAP, mod.mean.MAP:MAPplus100)

temp.scenarios <- mod.mean.clim %>% dplyr::select( mod.mean.Tmax:minusfive)  %>% gather(Temp.Scenario,Tmax, mod.mean.Tmax:minusfive)
site.scenarios <- expand.grid(Temp.Scenario = temp.scenarios$Temp.Scenario, MAP.Scenario = MAP.scenarios$MAP.Scenario, site = as.character(unique(test.dry.pair$site)))
test.MAP.scenarios <- merge(site.scenarios, MAP.scenarios, by = c("MAP.Scenario"))
all.scen <- merge(test.MAP.scenarios, temp.scenarios, by = "Temp.Scenario", all = TRUE)

# get the scaled values for MAP and TMAX to run through parameters
all.scen$MAP.scaled <- as.numeric(scale(all.scen$MAP, attr(MAP.scaled, "scaled:center"), attr(MAP.scaled, "scaled:scale")))

all.scen$T.scaled <- as.numeric(scale(all.scen$Tmax, attr(T.scaled, "scaled:center"), attr(T.scaled, "scaled:scale")))

# expand grid to add structure + cohort class - add 1 diameter, add 1 lag, add 2 lag, then add probe to the model....will need to re-run model
extras <- expand.grid(RWI_1 = 0.103, 
                      RWI_2 = 0.103, 
                      site = unique(all.scen$site), 
                      DBH.scaled = mean(test.dry.pair$DBH.scaled))

sites.unique <- unique(train.dry.pair[, c("site", "struct.cohort.code", "structure", "ageclass")])
extras <- merge(extras, sites.unique[, c("site", "struct.cohort.code")], by = "site")

degree.scenario <- merge(all.scen, extras, by = "site", all = TRUE)
site.num.df <- data.frame(site = as.character(unique(train.dry.pair$site)), 
                          site.num = 1:length(as.character(unique(train.dry.pair$site))))
degree.scenario <- merge(degree.scenario, site.num.df, by = "site")


meanMAP.sim <- degree.scenario %>% filter( MAP.Scenario == "mod.mean.MAP")
meanMAP.sim <- degree.scenario
meanMAP.sim <- meanMAP.sim[meanMAP.sim$site.num %in% "2", ]

sim.data.constant.MAP <- data.frame(site.num = rep(2,100), # BON
                      struct.cohort.code = rep(3,100), # modern savannas 
                      MAP = rep(750, 100), 
                      TMAX = rep(29.2, 100),
                      T.scaled = rep(1.906151, 100),
                      MAP.scaled = rep(0.06720535, 100),
                      RWI_1 = rep(0, 100),
                      RWI_2 = rep(0, 100), 
                      DBH.scaled = rep(-0.6562796, 100)) # in theory we should also be changning dbh each time too
meanMAP.sim <- sim.data.constant.MAP



int.mcmc <- as.mcmc(samps.cs)
int.mcmc.mat <- as.matrix(int.mcmc)
int.mcmc.dat <- data.frame(int.mcmc.mat)

int.1 <- matrix(rep(NA, nrow(int.mcmc.dat)*length(meanMAP.sim$T.scaled)), nrow = nrow(int.mcmc.dat))



# use betas to generate pp given a value for site, structure, dbh, rwi1, rwi2, and varying T and MAP:
for(i in 1:length(meanMAP.sim$T.scaled)){
  
  if(i <= 2){
    int.1[,i] <- int.mcmc.dat[,paste0("beta1.", meanMAP.sim[i,"site.num"], ".")]+
      int.mcmc.dat[,paste0("beta2.", meanMAP.sim[i,"struct.cohort.code"], ".")]*meanMAP.sim[i,]$MAP.scaled+    
      int.mcmc.dat[,paste0("beta3.", meanMAP.sim[i,"struct.cohort.code"], ".")]*meanMAP.sim[i,"DBH.scaled"] + 
      int.mcmc.dat[,paste0("beta4.", meanMAP.sim[i,"struct.cohort.code"], ".")]*meanMAP.sim[i,"RWI_1"]  + # for RWI 1, use default average if we have not made predictions yet
      int.mcmc.dat[,paste0("beta5.", meanMAP.sim[i,"struct.cohort.code"], ".")]*meanMAP.sim[i,"RWI_2"]+ # for RWI2,  
      int.mcmc.dat[,paste0("beta6.", meanMAP.sim[i,"struct.cohort.code"], ".")]*meanMAP.sim[i,"T.scaled"] + 
      int.mcmc.dat[,paste0("beta7.", meanMAP.sim[i,"struct.cohort.code"], ".")] * (meanMAP.sim[i,"MAP.scaled"] *meanMAP.sim[i,"T.scaled"])
    
  }else{
  int.1[,i] <- int.mcmc.dat[,paste0("beta1.", meanMAP.sim[i,"site.num"], ".")]+
    int.mcmc.dat[,paste0("beta2.", meanMAP.sim[i,"struct.cohort.code"], ".")]*meanMAP.sim[i,]$MAP.scaled+    
    int.mcmc.dat[,paste0("beta3.", meanMAP.sim[i,"struct.cohort.code"], ".")]*meanMAP.sim[i,"DBH.scaled"] + 
    int.mcmc.dat[,paste0("beta4.", meanMAP.sim[i,"struct.cohort.code"], ".")]*int.1[,i-1]  + # for RWI 1
    int.mcmc.dat[,paste0("beta5.", meanMAP.sim[i,"struct.cohort.code"], ".")]*int.1[,i-2] + # for RWI2,  
    int.mcmc.dat[,paste0("beta6.", meanMAP.sim[i,"struct.cohort.code"], ".")]*meanMAP.sim[i,"T.scaled"] + 
    int.mcmc.dat[,paste0("beta7.", meanMAP.sim[i,"struct.cohort.code"], ".")] * (meanMAP.sim[i,"MAP.scaled"] *meanMAP.sim[i,"T.scaled"])
  }
  
}
