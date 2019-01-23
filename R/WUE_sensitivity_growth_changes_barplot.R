# this script gets the estimated change in tree growth, change in tree climate sensitivity & change in WUE to make one figure:

# get differences in WUE:

wue.samps <- readRDS("outputs/growth_model/iWUE_MAP_TMAX_dbh_cohort/alpha_intercept_samps.rds")

wue.pct.diff <- data.frame(
  change = "WUE",
  pct.change = mean(alpha.diff),
  Ci.low = quantile(alpha.diff, 0.025), 
  Ci.high = quantile(alpha.diff, 0.975), 
  median = median(alpha.diff), 
  hdi.low = hdi(alpha.diff)[1], 
  hdi.high = hdi(alpha.diff)[2])


beta_diffsm <- readRDS("outputs/growth_model/lag2_reg_cohort_only_re_t_pr_dry_yrs_site_rs_inter/beta_differences_cohorts.rds")
beta_diffs <- readRDS("outputs/growth_model/lag2_reg_cohort_only_re_t_pr_dry_yrs_site_rs_inter/beta_diffs_cohorts.rds")

beta_diffsm <- beta_diffs %>% gather(beta, difference) %>% group_by(beta) %>% 
  summarise(mean = mean(difference),
            Ci.low = quantile(difference, 0.025),
            Ci.high = quantile(difference, 0.975),
            hdi.low = hdi(difference)[1],
            hdi.high = hdi(difference)[2]) 

# hard coding this, but need to make it better
beta_diffsm$significant <- c("NS", "NS", "Significant", "Significant", "Significant", "NS")


MAP.beta.diff <- data.frame(
  change = "MAP",
  pct.change = mean(beta_diffs[,"MAP"]),
  Ci.low = quantile(beta_diffs[,"MAP"], 0.025), 
  Ci.high = quantile(beta_diffs[,"MAP"], 0.975), 
  median = median(beta_diffs[,"MAP"]), 
  hdi.low = hdi(beta_diffs[,"MAP"])[1], 
  hdi.high = hdi(beta_diffs[,"MAP"])[2])


Lag2.beta.diff <- data.frame(
  change = "lag_2",
  pct.change = mean(beta_diffs[,"lag_2"]),
  Ci.low = quantile(beta_diffs[,"lag_2"], 0.025), 
  Ci.high = quantile(beta_diffs[,"lag_2"], 0.975), 
  median = median(beta_diffs[,"lag_2"]), 
  hdi.low = hdi(beta_diffs[,"lag_2"])[1], 
  hdi.high = hdi(beta_diffs[,"lag_2"])[2])


# also read in the change in predicted tree growth:
Yp.full <- readRDS( "outputs/growth_model/lag2_reg_cohort_only_re_t_pr_dry_yrs_site_rs_inter/predicted_growth_YP.rds")

growth.cohort  <- Yp.full %>% group_by(ageclass) %>% dplyr::summarise(Predicted = mean(exp(value)),
                                                                          ci.hi = quantile(exp(value),0.975),
                                                                          ci.lo = quantile(exp(value),0.025), 
                                                                          Observed = mean(RWI))

# plot overall differences in WUE:
wue.diff.bar <- ggplot(wue.pct.diff, aes(change, pct.change))+geom_bar(stat = "identity")+
  geom_errorbar(data = wue.pct.diff, aes(ymin=hdi.low, ymax=hdi.high), color = "grey", alpha = 0.8, size = 0.5, width = 0.2)+ylab("percent change in iWUE")+theme_bw(base_size = 16)+theme(panel.grid = element_blank(), axis.title.x = element_blank())

drought.bar <- ggplot(MAP.beta.diff, aes(change, pct.change))+geom_bar(stat = "identity")+
  geom_errorbar(data = MAP.beta.diff, aes(ymin=hdi.low, ymax=hdi.high), color = "grey", alpha = 0.8, size = 0.5, width = 0.2)+ylab("change in drought sensitivity")+theme_bw(base_size = 16)+theme(panel.grid = element_blank(), axis.title.x = element_blank())

lag1.bar <- ggplot(Lag2.beta.diff, aes(change, pct.change))+geom_bar(stat = "identity")+
  geom_errorbar(data = Lag2.beta.diff, aes(ymin=hdi.low, ymax=hdi.high), color = "grey", alpha = 0.8, size = 0.5, width = 0.2)+ylab("change in lag (-2) parameter")+theme_bw(base_size = 16)+theme(panel.grid = element_blank(), axis.title.x = element_blank())


mean.pred.growth <- ggplot(Yp.summary.cohort, aes(ageclass,Predicted, fill = ageclass))+geom_bar(stat = "identity")+geom_errorbar(data = Yp.summary.cohort,aes(ymin=ci.lo, ymax=ci.hi), color = "grey", alpha = 0.8, size = 0.5, width = 0.2)+ylim(0, 6) + ylab("Mean predicted tree growth (mm)")+scale_fill_manual(values = c("Past"='#2166ac', 'Modern' = "#b2182b"))+theme_bw(base_size = 16)+theme(panel.grid = element_blank(), axis.title.x = element_blank(), legend.position = "none")

png(height = 4, width = 10, units = "in", res = 300, "outputs/growth_model/summary_differences_by_cohort.png")
plot_grid(wue.diff.bar, drought.bar, lag1.bar,mean.pred.growth, ncol = 4, align = 'h', rel_widths = c(0.5,0.5,0.5, 1))
dev.off()


png(height = 4, width = 10, units = "in", res = 300, "outputs/growth_model/summary_differences_by_cohort.png")
plot_grid(wue.diff.bar, drought.bar, lag1.bar,mean.pred.growth, ncol = 4, align = 'h', rel_widths = c(0.5,0.5,0.5, 1))
dev.off()

# a better figure:

# A  iWUE estimates side by side
Yp.summary.cohort  <- Yp.full %>% group_by(ageclass) %>% dplyr::summarise(Predicted = mean(exp(value)),
                                                                          ci.hi = quantile(exp(value),0.975),
                                                                          ci.lo = quantile(exp(value),0.025), 
                                                                          Observed = mean(RWI))
Yp.summary.cohort$ageclass <- factor(Yp.summary.cohort$ageclass, levels = c("Past", "Modern"))



mean.pred.growth <- ggplot(Yp.summary.cohort, aes(ageclass,Predicted, fill = ageclass))+geom_bar(stat = "identity", width = 0.5)+geom_errorbar(data = Yp.summary.cohort,aes(ymin=ci.lo, ymax=ci.hi), color = "grey29", alpha = 0.8, size = 0.5, width = 0.1)+ylim(0, 6) + ylab("Mean predicted \n tree growth (mm)")+scale_fill_manual(values = c("Past"='#2166ac', 'Modern' = "#b2182b"))+theme_bw(base_size = 16)+theme(panel.grid = element_blank(), axis.title.x = element_blank(), legend.position = "none")

# B  mean estimates side by site
# C next to it change in beta params
samps.wue <- readRDS("outputs/growth_model/iWUE_MAP_TMAX_dbh_cohort/samps.iWUE_nobon.rds")
#Yp.samps <- samps[,1:660] # one alpha for each of 16 sites
#iWUEpred <- samps
alpha.samps  <- data.frame(samps.wue[,1:2])
colnames(alpha.samps) <- c("Modern", "Past")
alpha.samps.m <- melt(alpha.samps)
colnames(alpha.samps.m) <- c("ageclass", "value")
wue.cohort  <- alpha.samps.m  %>% group_by(ageclass) %>% dplyr::summarise(Predicted = mean(value),
                                                                          ci.hi = quantile(value,0.975),
                                                                          ci.lo = quantile(value,0.025) )


wue.cohort$ageclass <- factor(wue.cohort$ageclass, levels = c("Past", "Modern"))
mean.pred.wue <- ggplot(wue.cohort, aes(ageclass, Predicted, fill = ageclass))+geom_bar(stat = "identity", width = 0.5)+
  geom_errorbar(data = wue.cohort, aes(ymin=ci.lo, ymax=ci.hi), color = "grey29", alpha = 0.8, size = 0.5, width = 0.1) + ylab("Mean estimated iWUE")+scale_fill_manual(values = c("Past"='#2166ac', 'Modern' = "#b2182b"))+theme_bw(base_size = 16)+theme(panel.grid = element_blank(), axis.title.x = element_blank(), legend.position = "none")+ylim(0,170)

beta_diffsm$beta <- factor(beta_diffsm$beta, levels = c("MAP", "Tmax", "MAPxTmax","DBH", "lag_1", "lag_2"))

beta.diffs.plot.mod.past <- ggplot(beta_diffsm, aes(beta, mean, fill = significant))+geom_bar(stat = "identity", width = 0.8)+geom_errorbar(data = beta_diffsm,aes(ymin=hdi.low, ymax=hdi.high), color = "grey29", alpha = 0.8, size = 0.5, width = 0.1)+
  ylab("Difference in beta parameter \n (Modern - Past)")+xlab("model parameters")+theme_bw(base_size = 16)+theme(panel.grid = element_blank(), legend.position = "none")+scale_fill_manual(values = c("Significant" = "black", "NS" = "lightgrey"))

png(height = 6, width = 12, units = "in", res = 300, "outputs/growth_model/summary_of_differences_cohort.png")
plot_grid(
plot_grid(mean.pred.wue, mean.pred.growth, ncol = 1, align = "hv", labels = c("", "B")),
beta.diffs.plot.mod.past, ncol = 2, labels = c("A", "C"), rel_widths = c(0.75, 1))
dev.off()

