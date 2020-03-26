# predict mean growth + 95% CI as time series for a given tree ID & see how well it matches the yearly data
# use the full model to predcit individual years for each site

library(ggplot2)
library(tidyr)
library(dplyr)
library(coda)
library(ggnewscale)

# read in the full dataset that we want to use to assess time series predictions:

ghcn.clean <- readRDS("outputs/growth_model/ghcn.clean.full.data.rds")



# --------read in estimates + 95% CI for the full model fit on all climatic years--------------------------
samps <- readRDS("/Users/kah/Documents/TreeRings/outputs/growth_model/lag2_reg_struct_x_cohort_re_t_pr_dry_yrs_site_rs_inter_full_dataset/samps.rds")
test.dry <- readRDS("/Users/kah/Documents/TreeRings/outputs/growth_model/lag2_reg_struct_x_cohort_re_t_pr_dry_yrs_site_rs_inter_full_dataset/test.rds")
train.dry <- readRDS("/Users/kah/Documents/TreeRings/outputs/growth_model/lag2_reg_struct_x_cohort_re_t_pr_dry_yrs_site_rs_inter_full_dataset/train.rds")


# add site number to ghcn clean dataframe if id does not already exist
site.num.df <- data.frame(site = as.character(unique(train.dry$site)), 
                          site.num = 1:length(as.character(unique(train.dry$site))))

if(!"site.num" %in% colnames(ghcn.clean)){
 ghcn.clean <-  merge(site.num.df, ghcn.clean, by = "site")
}


meanMAP.sim <- ghcn.clean

int.mcmc <- as.mcmc(samps)
int.mcmc.mat <- as.matrix(int.mcmc)
int.mcmc.dat <- data.frame(int.mcmc.mat)

int.1 <- matrix(rep(NA, nrow(int.mcmc.dat)*length(meanMAP.sim$T.scaled)), nrow = nrow(int.mcmc.dat))


# use betas to generate pp given a value for site, structure, dbh, rwi1, rwi2, and varying T and MAP:
for(i in 1:length(meanMAP.sim$T.scaled)){
  # for struct.cohort == 1
  int.1[,i] <- int.mcmc.dat[,paste0("beta1.", meanMAP.sim[i,"site.num"], ".")]+
    int.mcmc.dat[,paste0("beta2.", meanMAP.sim[i,"struct.cohort.code"], ".")]*meanMAP.sim[i,]$MAP.scaled+    
    int.mcmc.dat[,paste0("beta3.", meanMAP.sim[i,"struct.cohort.code"], ".")]*meanMAP.sim[i,"DBH.scaled"] + 
    int.mcmc.dat[,paste0("beta4.", meanMAP.sim[i,"struct.cohort.code"], ".")]*meanMAP.sim[i,"RWI_1"]  + 
    int.mcmc.dat[,paste0("beta5.", meanMAP.sim[i,"struct.cohort.code"], ".")]*meanMAP.sim[i,"RWI_2"] +
    int.mcmc.dat[,paste0("beta6.", meanMAP.sim[i,"struct.cohort.code"], ".")]*meanMAP.sim[i,"T.scaled"] + 
    int.mcmc.dat[,paste0("beta7.", meanMAP.sim[i,"struct.cohort.code"], ".")] * (meanMAP.sim[i,"MAP.scaled"] *meanMAP.sim[i,"T.scaled"])
  
  
}


# columns are the different degree-site scenario combinations
meanMAP.sim$idval <- 1:length(meanMAP.sim$site)
# rows are the mcmc values
colnames(int.1) <- 1:length(meanMAP.sim$site)
test.m <- melt(int.1)
colnames(test.m) <- c("MCMC", "idval", "Ypred")
full.pred <- left_join(test.m, meanMAP.sim, by = "idval")
full.pred$RWI.pred <- full.pred$Ypred

indiv.summary <- full.pred %>% group_by(site, year, ID) %>% dplyr::summarise(mean.pred = mean(RWI.pred, na.rm =TRUE),
                                                                    ci.low.pred=quantile(RWI.pred, 0.025, na.rm =TRUE),
                                                                    ci.high.pred=quantile(RWI.pred, 0.975, na.rm =TRUE),
                                                                    mean.obs = mean(RWI, na.rm =TRUE), 
                                                                    ci.low.obs=quantile(RWI, 0.025, na.rm =TRUE),
                                                                    ci.high.obs=quantile(RWI, 0.975, na.rm =TRUE))

ggplot(indiv.summary , aes(mean.obs, mean.pred, color = ID))+geom_point()+geom_abline(intercept = 0, 1, color = "red")+ylim(0,10)+xlim(0,7)+theme(legend.position = "none")+facet_wrap(~site)

ggplot(indiv.summary , aes(year, mean.pred, color = ID))+geom_line()+geom_ribbon(aes(ymin = ci.low.pred, ymax = ci.high.pred, fill = ID), alpha = 0.25, linetype = "dashed", colour = NA)+
  geom_point(data = indiv.summary , aes(year, mean.obs), color = "black", size = 0.05)+theme(legend.position = "none")+
  facet_wrap(~site, scales = "free_y")


indiv.summary <- full.pred %>% group_by(site, year) %>% dplyr::summarise(mean.pred = mean(RWI.pred, na.rm =TRUE),
                                                                             ci.low.pred=quantile(RWI.pred, 0.025, na.rm =TRUE),
                                                                             ci.high.pred=quantile(RWI.pred, 0.975, na.rm =TRUE),
                                                                             mean.obs = mean(RWI, na.rm =TRUE), 
                                                                             ci.low.obs=quantile(RWI, 0.025, na.rm =TRUE),
                                                                             ci.high.obs=quantile(RWI, 0.975, na.rm =TRUE))

ggplot(indiv.summary , aes(mean.obs, mean.pred))+geom_point()+geom_abline(intercept = 0, 1, color = "red")+ylim(0,10)+xlim(0,7)+theme(legend.position = "none")+facet_wrap(~site)

pred.obs.time <- ggplot(indiv.summary , aes(year, mean.pred))+geom_line()+geom_ribbon(aes(ymin = ci.low.pred, ymax = ci.high.pred, fill = site), alpha = 0.25, linetype = "dashed", colour = NA)+
  geom_point(data = indiv.summary , aes(year, mean.obs), color = "black", size = 0.05)+geom_line(data = indiv.summary , aes(year, mean.obs), color = "red", linetype = "dashed")+theme(legend.position = "none")+
  facet_wrap(~site, scales = "free_y")+ylab("Predicted Tree Growth (mm)")+xlab("Year")

png(height = 6, width = 8, units = "in", res = 300, "outputs/growth_model/lag2_reg_struct_x_cohort_re_t_pr_dry_yrs_site_rs_inter_full_dataset/pred_observed_time_series.png")
pred.obs.time
dev.off()



indiv.summary.age <- full.pred %>% group_by(site, year, ageclass) %>% dplyr::summarise(mean.pred = mean(RWI.pred, na.rm =TRUE),
                                                                         ci.low.pred=quantile(RWI.pred, 0.025, na.rm =TRUE),
                                                                         ci.high.pred=quantile(RWI.pred, 0.975, na.rm =TRUE),
                                                                         mean.obs = mean(RWI, na.rm =TRUE), 
                                                                         ci.low.obs=quantile(RWI, 0.025, na.rm =TRUE),
                                                                         ci.high.obs=quantile(RWI, 0.975, na.rm =TRUE))

ggplot(indiv.summary.age , aes(mean.obs, mean.pred))+geom_point()+geom_abline(intercept = 0, 1, color = "red")+ylim(0,10)+xlim(0,7)+theme(legend.position = "none")+facet_wrap(~site)


library(ggnewscale)

indiv.summary.age$Observed <- indiv.summary.age$ageclass
indiv.summary.age$Predicted <- indiv.summary.age$ageclass


pred.obs.time.age <- ggplot()+geom_line(data = indiv.summary.age , aes(x= year, y= mean.obs, color = Observed), linetype = "dashed")+geom_ribbon(data = indiv.summary.age, aes(x = year,ymin = ci.low.obs, ymax = ci.high.obs, fill = Observed), alpha = 0.75, linetype = "dashed", colour = NA)+
  scale_fill_manual(values = c("Modern"="#ffffb2", "Past" = "#bdbdbd"))+scale_color_manual(values = c("Modern"="#ffffb2", "Past" = "#bdbdbd"))+
  new_scale("color") + new_scale("fill")+
  geom_line(data= indiv.summary.age , aes(year, mean.pred, color = Predicted))+theme()+geom_ribbon(data = indiv.summary.age, aes(x = year,ymin = ci.low.pred, ymax = ci.high.pred, fill = Predicted), alpha = 0.45, linetype = "dashed", colour = NA)+scale_fill_manual(values = c("Modern"="#ca0020", "Past" = "#0571b0"))+scale_color_manual(values = c("Modern"="#fd8d3c", "Past" = "#0571b0"))+
  
  facet_wrap(~site, scales = "free_y")+ylab("Predicted Tree Growth (mm)")+xlab("Year")

png(height = 6, width = 8, units = "in", res = 300, "outputs/growth_model/lag2_reg_struct_x_cohort_re_t_pr_dry_yrs_site_rs_inter_full_dataset/pred_observed_time_series_by_ageclass.png")
pred.obs.time.age
dev.off()


#-----------------------Prediction plots from model fit just with dry years--------------------------------

samps <- readRDS("/Users/kah/Documents/TreeRings/outputs/growth_model/lag2_reg_struct_x_cohort_re_t_pr_dry_yrs_site_rs_inter/samps.rds")
test.dry <- readRDS("/Users/kah/Documents/TreeRings/outputs/growth_model/lag2_reg_struct_x_cohort_re_t_pr_dry_yrs_site_rs_inter/test.rds")
train.dry <- readRDS("/Users/kah/Documents/TreeRings/outputs/growth_model/lag2_reg_struct_x_cohort_re_t_pr_dry_yrs_site_rs_inter/train.rds")


# 
# alpha.samps  <- samps[,1:9]# one alpha for each of 4 cohort-strcuture groups
# beta2.samps <- samps[,17:20]
# beta3.samps <- samps[,21:24]
# beta4.samps <- samps[,25:28]
# beta5.samps <- samps[,29:32]
# beta6.samps <- samps[,33:36]
# beta7.samps <- samps[,37:40]
# sigma.samps <- samps[,48]
# sigma_betas <- samps[,41:47]
# 

# get the data on tree ids:
# read in ghcn.clean

# add site number to ghcn clean dataframe
site.num.df <- data.frame(site = as.character(unique(train.dry$site)), 
                          site.num = 1:length(as.character(unique(train.dry$site))))

if(!"site.num" %in% colnames(ghcn.clean)){
  ghcn.clean <-  merge(site.num.df, ghcn.clean, by = "site")
}

meanMAP.sim <- ghcn.clean  %>% filter( ID == "UNC10a11")
meanMAP.sim <- ghcn.clean

int.mcmc <- as.mcmc(samps)
int.mcmc.mat <- as.matrix(int.mcmc)
int.mcmc.dat <- data.frame(int.mcmc.mat)

int.1 <- matrix(rep(NA, nrow(int.mcmc.dat)*length(meanMAP.sim$T.scaled)), nrow = nrow(int.mcmc.dat))


# use betas to generate pp given a value for site, structure, dbh, rwi1, rwi2, and varying T and MAP:
for(i in 1:length(meanMAP.sim$T.scaled)){
  # for struct.cohort == 1
  int.1[,i] <- int.mcmc.dat[,paste0("beta1.", meanMAP.sim[i,"site.num"], ".")]+
    int.mcmc.dat[,paste0("beta2.", meanMAP.sim[i,"struct.cohort.code"], ".")]*meanMAP.sim[i,]$MAP.scaled+    
    int.mcmc.dat[,paste0("beta3.", meanMAP.sim[i,"struct.cohort.code"], ".")]*meanMAP.sim[i,"DBH.scaled"] + 
    int.mcmc.dat[,paste0("beta4.", meanMAP.sim[i,"struct.cohort.code"], ".")]*meanMAP.sim[i,"RWI_1"]  + 
    int.mcmc.dat[,paste0("beta5.", meanMAP.sim[i,"struct.cohort.code"], ".")]*meanMAP.sim[i,"RWI_2"] +
    int.mcmc.dat[,paste0("beta6.", meanMAP.sim[i,"struct.cohort.code"], ".")]*meanMAP.sim[i,"T.scaled"] + 
    int.mcmc.dat[,paste0("beta7.", meanMAP.sim[i,"struct.cohort.code"], ".")] * (meanMAP.sim[i,"MAP.scaled"] *meanMAP.sim[i,"T.scaled"])
  
  
}


# columns are the different degree-site scenario combinations
meanMAP.sim$idval <- 1:length(meanMAP.sim$site)
# rows are the mcmc values
colnames(int.1) <- 1:length(meanMAP.sim$site)
test.m <- melt(int.1)
colnames(test.m) <- c("MCMC", "idval", "Ypred")
full.pred <- left_join(test.m, meanMAP.sim, by = "idval")
full.pred$RWI.pred <- full.pred$Ypred

indiv.summary <- full.pred %>% group_by(site, year, ID) %>% dplyr::summarise(mean.pred = mean(RWI.pred, na.rm =TRUE),
                                                                             ci.low.pred=quantile(RWI.pred, 0.025, na.rm =TRUE),
                                                                             ci.high.pred=quantile(RWI.pred, 0.975, na.rm =TRUE),
                                                                             mean.obs = mean(RWI, na.rm =TRUE), 
                                                                             ci.low.obs=quantile(RWI, 0.025, na.rm =TRUE),
                                                                             ci.high.obs=quantile(RWI, 0.975, na.rm =TRUE))

ggplot(indiv.summary , aes(mean.obs, mean.pred, color = ID))+geom_point()+geom_abline(intercept = 0, 1, color = "red")+ylim(0,10)+xlim(0,7)+theme(legend.position = "none")+facet_wrap(~site)

ggplot(indiv.summary , aes(year, mean.pred, color = ID))+geom_line()+geom_ribbon(aes(ymin = ci.low.pred, ymax = ci.high.pred, fill = ID), alpha = 0.25, linetype = "dashed", colour = NA)+
  geom_point(data = indiv.summary , aes(year, mean.obs), color = "black", size = 0.05)+theme(legend.position = "none")+
  facet_wrap(~site, scales = "free_y")


indiv.summary <- full.pred %>% group_by(site, year) %>% dplyr::summarise(mean.pred = mean(RWI.pred, na.rm =TRUE),
                                                                         ci.low.pred=quantile(RWI.pred, 0.025, na.rm =TRUE),
                                                                         ci.high.pred=quantile(RWI.pred, 0.975, na.rm =TRUE),
                                                                         mean.obs = mean(RWI, na.rm =TRUE), 
                                                                         ci.low.obs=quantile(RWI, 0.025, na.rm =TRUE),
                                                                         ci.high.obs=quantile(RWI, 0.975, na.rm =TRUE))

ggplot(indiv.summary , aes(mean.obs, mean.pred))+geom_point()+geom_abline(intercept = 0, 1, color = "red")+ylim(0,10)+xlim(0,7)+theme(legend.position = "none")+facet_wrap(~site)

pred.obs.time <- ggplot(indiv.summary , aes(year, mean.pred))+geom_line()+geom_ribbon(aes(ymin = ci.low.pred, ymax = ci.high.pred, fill = site), alpha = 0.25, linetype = "dashed", colour = NA)+
  geom_point(data = indiv.summary , aes(year, mean.obs), color = "black", size = 0.05)+geom_line(data = indiv.summary , aes(year, mean.obs), color = "red", linetype = "dashed")+theme(legend.position = "none")+
  facet_wrap(~site, scales = "free_y")+ylab("Predicted Tree Growth (mm)")+xlab("Year")

png(height = 6, width = 8, units = "in", res = 300, "outputs/growth_model/lag2_reg_struct_x_cohort_re_t_pr_dry_yrs_site_rs_inter/pred_observed_time_series.png")
pred.obs.time
dev.off()



indiv.summary.age <- full.pred %>% group_by(site, year, ageclass) %>% dplyr::summarise(mean.pred = mean(RWI.pred, na.rm =TRUE),
                                                                                       ci.low.pred=quantile(RWI.pred, 0.025, na.rm =TRUE),
                                                                                       ci.high.pred=quantile(RWI.pred, 0.975, na.rm =TRUE),
                                                                                       mean.obs = mean(RWI, na.rm =TRUE), 
                                                                                       ci.low.obs=quantile(RWI, 0.025, na.rm =TRUE),
                                                                                       ci.high.obs=quantile(RWI, 0.975, na.rm =TRUE))

ggplot(indiv.summary.age , aes(mean.obs, mean.pred))+geom_point()+geom_abline(intercept = 0, 1, color = "red")+ylim(0,10)+xlim(0,7)+theme(legend.position = "none")+facet_wrap(~site)


indiv.summary.age$Observed <- indiv.summary.age$ageclass
indiv.summary.age$Predicted <- indiv.summary.age$ageclass

pred.obs.time.age <- ggplot()+geom_line(data = indiv.summary.age , aes(x= year, y= mean.obs, color = Observed), linetype = "dashed")+geom_ribbon(data = indiv.summary.age, aes(x = year,ymin = ci.low.obs, ymax = ci.high.obs, fill = Observed), alpha = 0.75, linetype = "dashed", colour = NA)+
  scale_fill_manual(values = c("Modern"="#ffffb2", "Past" = "#bdbdbd"))+scale_color_manual(values = c("Modern"="#ffffb2", "Past" = "#bdbdbd"))+
  new_scale("color") + new_scale("fill")+
  geom_line(data= indiv.summary.age , aes(year, mean.pred, color = Predicted))+theme()+geom_ribbon(data = indiv.summary.age, aes(x = year,ymin = ci.low.pred, ymax = ci.high.pred, fill = Predicted), alpha = 0.45, linetype = "dashed", colour = NA)+scale_fill_manual(values = c("Modern"="#ca0020", "Past" = "#0571b0"))+scale_color_manual(values = c("Modern"="#fd8d3c", "Past" = "#0571b0"))+
  
  facet_wrap(~site, scales = "free_y")+ylab("Predicted Tree Growth (mm)")+xlab("Year")

png(height = 6, width = 8, units = "in", res = 300, "outputs/growth_model/lag2_reg_struct_x_cohort_re_t_pr_dry_yrs_site_rs_inter/pred_observed_time_series_by_ageclass.png")
pred.obs.time.age
dev.off()


summary(lm(mean.obs ~ mean.pred, data = indiv.summary.age))
