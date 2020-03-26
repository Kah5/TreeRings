# this script gets the estimated change in tree growth, change in tree climate sensitivity & change in WUE to make one figure:
library(HDInterval)
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
  dplyr::summarise(mean = mean(difference),
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
mean.pred.growth.dot <- ggplot(Yp.summary.cohort, aes(ageclass,Predicted, color = ageclass))+geom_point(size = 4)+geom_errorbar(data = Yp.summary.cohort,aes(ymin=ci.lo, ymax=ci.hi, color = ageclass), size = 2, width = 0)+ylim(0, 6) + ylab("Mean predicted \n tree growth (mm)")+scale_color_manual(values = c("Past"='#2166ac', 'Modern' = "#b2182b"))+theme_bw(base_size = 16)+theme(panel.grid = element_blank(), axis.title.x = element_blank(), legend.position = "none")

# B  mean estimates side by site
# C next to it change in beta params
samps.wue <- readRDS("outputs/growth_model/iWUE_MAP_TMAX_dbh_cohort/samps.iWUE_nobon.rds")
samps.wue.full <- readRDS("outputs/growth_model/iWUE_MAP_TMAX_dbh/samps.iWUE_nobon.rds")
samps.wue.params <- readRDS("outputs/growth_model/iWUE_MAP_TMAX_dbh_cohort/params.iWUE_nobon_ageclass.rds")
samps.d13.samps <- readRDS("outputs/growth_model/d13_MAP_TMAX_dbh/samps.d13_nobon_age.rds")
samps.d13.params <- samps.d13.samps[,1:8]

samps.growth.params <- readRDS("outputs/growth_model/lag2_reg_cohort_only_re_t_pr_dry_yrs_site_rs_inter/samps.rds")
alpha.samps  <- samps.growth.params [,1:9]# one alpha for each of 4 cohort-strcuture groups
beta2.samps <- samps.growth.params [,10:11]
beta3.samps <- samps.growth.params [,12:13]
beta4.samps <- samps.growth.params [,14:15]
beta5.samps <- samps.growth.params [,16:17]
beta6.samps <- samps.growth.params [,18:19]
beta7.samps <- samps.growth.params [,20:21]



# plot marginal distributions of cohort + structure specific parameters:
a <- data.frame(alpha.samps)
colnames(a) <- unique(train.dry.pair$site)#[order(unique(train.dry.pair[,c("site", "site.num")])[,2])])
a$num <- rownames(a)
a.m <- melt(a, id.vars=c("num"))

b2 <- data.frame(beta2.samps)
colnames(b2) <- c("Modern" ,"Past")
#colnames(b2) <- c(paste0(c(unique(train.dry.pair$struct.cohort))))
b2$num <- rownames(b2)
b2.m <- melt(b2, id.vars=c("num"))
b2.mplots <- ggplot(b2.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Precipitation Index slope")

b3 <- data.frame(beta3.samps)
colnames(b3) <- c("Modern" ,"Past")
b3$num <- rownames(b3)
b3.m <- melt(b3, id.vars=c("num"))
b3.mplots <- ggplot(b3.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("DBH Index slope")


b4 <- data.frame(beta4.samps)
colnames(b4) <- c("Modern" ,"Past")
b4$num <- rownames(b4)
b4.m <- melt(b4, id.vars=c("num"))
b4.mplots <- ggplot(b4.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Log_RWI-1 Index slope")

b5 <- data.frame(beta5.samps)
colnames(b5) <- c("Modern" ,"Past")
b5$num <- rownames(b5)
b5.m <- melt(b5, id.vars=c("num"))
b5.mplots <- ggplot(b5.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Log_RWI-2 Index slope")

b6 <- data.frame(beta6.samps)
colnames(b6) <- c("Modern" ,"Past")
b6$num <- rownames(b6)
b6.m <- melt(b6, id.vars=c("num"))
b6.mplots <- ggplot(b6.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Summer Temperature slope")

b7 <- data.frame(beta7.samps)
colnames(b7) <- c("Modern" ,"Past")
b7$num <- rownames(b7)
b7.m <- melt(b7, id.vars=c("num"))
b7.mplots <- ggplot(b7.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Precip*Temp slope")



# -------------------- Dotplots by modern vs. past only ---------------------
# make dotplots for all the factors in the model--
# get summaries by age class from the melted samples:

a1.sum <- a.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                          Ci.low = quantile(value, 0.025), 
                                                          Ci.high = quantile(value, 0.975),
                                                          hdi.low = hdi(value)[1], 
                                                          hdi.high = hdi(value)[2])
a1.sum$variable <- unique(train.dry.pair$site)

df.site.struct <- unique(train.dry.pair[,c("site", "structure")])
a1.sum <- merge(a1.sum, df.site.struct, by.x = "variable", by.y = "site")
a1.sum$structure <- factor(a1.sum$structure, levels = c( "Forest", "Savanna"))

b2.sum <- b2.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975),
                                                           hdi.low = hdi(value)[1], 
                                                           hdi.high = hdi(value)[2] )

b2.sum$variable <- factor(b2.sum$variable, levels = c( "Past","Modern"))
colnames(b2.sum) <- c("cohort", "mean.val", "Ci.low", "Ci.high", "hdi.low", "hdi.high")

b3.sum <- b3.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975),
                                                           hdi.low = hdi(value)[1], 
                                                           hdi.high = hdi(value)[2] )
b3.sum$variable <- factor(b3.sum$variable, levels = c( "Past","Modern"))
colnames(b3.sum) <- c("cohort", "mean.val", "Ci.low", "Ci.high", "hdi.low", "hdi.high")

b4.sum <- b4.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975),
                                                           hdi.low = hdi(value)[1], 
                                                           hdi.high = hdi(value)[2] )


b4.sum$variable <- factor(b4.sum$variable, levels = c( "Past","Modern"))
colnames(b4.sum) <- c("cohort", "mean.val", "Ci.low", "Ci.high", "hdi.low", "hdi.high")

b5.sum <- b5.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975),
                                                           hdi.low = hdi(value)[1], 
                                                           hdi.high = hdi(value)[2] )
b5.sum$variable <- factor(b5.sum$variable, levels = c( "Past","Modern"))
colnames(b5.sum) <- c("cohort", "mean.val", "Ci.low", "Ci.high", "hdi.low", "hdi.high")

b6.sum <- b6.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975),
                                                           hdi.low = hdi(value)[1], 
                                                           hdi.high = hdi(value)[2] )
b6.sum$variable <- factor(b6.sum$variable, levels = c( "Past","Modern"))
colnames(b6.sum) <- c("cohort", "mean.val", "Ci.low", "Ci.high", "hdi.low", "hdi.high")


b7.sum <- b7.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975),
                                                           hdi.low = hdi(value)[1], 
                                                           hdi.high = hdi(value)[2] )
b7.sum$variable <- factor(b7.sum$variable, levels = c( "Past","Modern"))
colnames(b7.sum) <- c("cohort", "mean.val", "Ci.low", "Ci.high", "hdi.low", "hdi.high")



# write out all the dotplots with 95% ci
int.dot.age <- ggplot(data.frame(a1.sum), aes(x = mean.val, y = variable, color = structure, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+geom_point()+scale_color_manual(values = c("Savanna"='sienna4', 'Forest' = "forestgreen"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated Intercept (alpha)")+xlim(-0.3, 0.8) + geom_vline(xintercept = 0, linetype = "dashed")

b2.dot.age <- ggplot(data.frame(b2.sum), aes(x = mean.val, y = cohort, color = cohort, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+geom_point()+xlim(-0.1, 0.25)+scale_color_manual(values = c("Past"='#2166ac', 'Modern' = "#b2182b"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated Drought Sensitivity (Beta2)")+xlim(-0.3, 0.8)+ geom_vline(xintercept = 0, linetype = "dashed")

b2.dot.age <- ggplot(data.frame(b2.sum), aes(x =cohort,  y =  mean.val, color = cohort), soze = 2)+geom_errorbar( aes(ymin = Ci.low, ymax = Ci.high), width = 0, size = 1)+geom_point(size = 2)+
  scale_color_manual(values = c("Past"='#2166ac', 'Modern' = "#b2182b"))+theme(legend.position = "none", axis.title.x = element_blank())+ylab("Effect of Precip on growth")+
  ylim(-0.15, 0.18)+ geom_hline(yintercept = 0, linetype = "dashed")

b2.bar.age <- ggplot(data.frame(b2.sum), aes(cohort, mean.val, fill = cohort))+geom_bar(stat = "identity", width = 0.5)+
  geom_errorbar(data = data.frame(b2.sum), aes(ymin=Ci.low, ymax=Ci.high), color = "grey29", alpha = 0.8, size = 0.5, width = 0.1) + 
  ylim(-0.15, 0.18)+geom_hline(yintercept = 0, color = "grey", linetype = "dashed")+
  ylab("Effect of Precip. on growth")+scale_fill_manual(values = c("Past"='#2166ac', 'Modern' = "#b2182b"))+theme_bw(base_size = 16)+theme(panel.grid = element_blank(), axis.title.x = element_blank(), legend.position = "none")


b3.dot.age <- ggplot(data.frame(b3.sum), aes(x =cohort,  y =  mean.val, color = cohort), soze = 2)+geom_errorbar( aes(ymin = Ci.low, ymax = Ci.high), width = 0, size = 1)+geom_point(size = 2)+
  scale_color_manual(values = c("Past"='#2166ac', 'Modern' = "#b2182b"))+theme(legend.position = "none", axis.title.x = element_blank())+ylab("Effect of DBH on growth")+
  ylim(-0.15, 0.18)+ geom_hline(yintercept = 0, linetype = "dashed")


b3.bar.age <- ggplot(data.frame(b3.sum), aes(cohort, mean.val, fill = cohort))+geom_bar(stat = "identity", width = 0.5)+
  geom_errorbar(data = data.frame(b3.sum), aes(ymin=Ci.low, ymax=Ci.high), color = "grey29", alpha = 0.8, size = 0.5, width = 0.1) + 
  ylim(-0.15, 0.18)+geom_hline(yintercept = 0, color = "grey", linetype = "dashed")+
  ylab("Effect of DBH on growth")+scale_fill_manual(values = c("Past"='#2166ac', 'Modern' = "#b2182b"))+theme_bw(base_size = 16)+theme(panel.grid = element_blank(), axis.title.x = element_blank(), legend.position = "none")


b4.dot.age <- ggplot(data.frame(b4.sum), aes(x =cohort,  y =  mean.val, color = cohort), soze = 2)+geom_errorbar( aes(ymin = Ci.low, ymax = Ci.high), width = 0, size = 1)+geom_point(size = 2)+
  scale_color_manual(values = c("Past"='#2166ac', 'Modern' = "#b2182b"))+theme(legend.position = "none", axis.title.x = element_blank())+ylab("Effect of prev(-1) on growth")+
  ylim(-0.15, 0.18)+ geom_hline(yintercept = 0, linetype = "dashed")

b4.bar.age <- ggplot(data.frame(b4.sum), aes(cohort, mean.val, fill = cohort))+geom_bar(stat = "identity", width = 0.5)+
  geom_errorbar(data = data.frame(b4.sum), aes(ymin=Ci.low, ymax=Ci.high), color = "grey29", alpha = 0.8, size = 0.5, width = 0.1) + 
  ylim(-0.15, 0.18)+geom_hline(yintercept = 0, color = "grey", linetype = "dashed")+
  ylab("Effect of prev(-1) on growth")+scale_fill_manual(values = c("Past"='#2166ac', 'Modern' = "#b2182b"))+theme_bw(base_size = 16)+theme(panel.grid = element_blank(), axis.title.x = element_blank(), legend.position = "none")


b5.dot.age <- ggplot(data.frame(b5.sum), aes(x =cohort,  y =  mean.val, color = cohort), soze = 2)+geom_errorbar( aes(ymin = Ci.low, ymax = Ci.high), width = 0, size = 1)+geom_point(size = 2)+
  scale_color_manual(values = c("Past"='#2166ac', 'Modern' = "#b2182b"))+theme(legend.position = "none", axis.title.x = element_blank())+ylab("Effect of prev(-2) on growth")+
  ylim(-0.15, 0.18)+ geom_hline(yintercept = 0, linetype = "dashed")

b5.bar.age <- ggplot(data.frame(b5.sum), aes(cohort, mean.val, fill = cohort))+geom_bar(stat = "identity", width = 0.5)+
  geom_errorbar(data = data.frame(b5.sum), aes(ymin=Ci.low, ymax=Ci.high), color = "grey29", alpha = 0.8, size = 0.5, width = 0.1) + 
  ylim(-0.15, 0.18)+geom_hline(yintercept = 0, color = "grey", linetype = "dashed")+
  ylab("Effect of prev(-2) on growth")+scale_fill_manual(values = c("Past"='#2166ac', 'Modern' = "#b2182b"))+theme_bw(base_size = 16)+theme(panel.grid = element_blank(), axis.title.x = element_blank(), legend.position = "none")


b6.dot.age <- ggplot(data.frame(b6.sum), aes(x =cohort,  y =  mean.val, color = cohort), soze = 2)+geom_errorbar( aes(ymin = Ci.low, ymax = Ci.high), width = 0, size = 1)+geom_point(size = 2)+
  scale_color_manual(values = c("Past"='#2166ac', 'Modern' = "#b2182b"))+theme(legend.position = "none", axis.title.x = element_blank())+ylab("Effect of Tmax on growth")+
  ylim(-0.15, 0.18)+ geom_hline(yintercept = 0, linetype = "dashed")

b6.bar.age <- ggplot(data.frame(b6.sum), aes(cohort, mean.val, fill = cohort))+geom_bar(stat = "identity", width = 0.5)+
  geom_errorbar(data = data.frame(b6.sum), aes(ymin=Ci.low, ymax=Ci.high), color = "grey29", alpha = 0.8, size = 0.5, width = 0.1) + 
  ylim(-0.15, 0.18)+geom_hline(yintercept = 0, color = "grey", linetype = "dashed")+
  ylab("Effect of Tmax on growth")+scale_fill_manual(values = c("Past"='#2166ac', 'Modern' = "#b2182b"))+theme_bw(base_size = 16)+theme(panel.grid = element_blank(), axis.title.x = element_blank(), legend.position = "none")


b7.dot.age <- ggplot(data.frame(b7.sum), aes(x =cohort,  y =  mean.val, color = cohort), soze = 2)+geom_errorbar( aes(ymin = Ci.low, ymax = Ci.high), width = 0, size = 1)+geom_point(size = 2)+
  scale_color_manual(values = c("Past"='#2166ac', 'Modern' = "#b2182b"))+theme(legend.position = "none", axis.title.x = element_blank())+ylab("Effect of Tmax*Precip on growth")+
  ylim(-0.15, 0.18)+ geom_hline(yintercept = 0, linetype = "dashed")


b7.bar.age <- ggplot(data.frame(b7.sum), aes(cohort, mean.val, fill = cohort))+geom_bar(stat = "identity", width = 0.5)+
  geom_errorbar(data = data.frame(b7.sum), aes(ymin=Ci.low, ymax=Ci.high), color = "grey29", alpha = 0.8, size = 0.5, width = 0.1) + 
  ylim(-0.15, 0.18)+geom_hline(yintercept = 0, color = "grey", linetype = "dashed")+
  ylab("Effect of Tmax Interaction")+scale_fill_manual(values = c("Past"='#2166ac', 'Modern' = "#b2182b"))+theme_bw(base_size = 16)+theme(panel.grid = element_blank(), axis.title.x = element_blank(), legend.position = "none")


png(height = 3, width = 12, units = "in", res = 300, "outputs/growth_model/summary_of_growth_mod.png")
plot_grid(mean.pred.growth.dot, b2.dot.age, b7.dot.age, b6.dot.age, ncol = 4)
dev.off()

#------------------Plot WUE responses
alpha.samps  <- data.frame(samps.wue.params[,1:2])
colnames(alpha.samps) <- c( "Past", "Modern") # note this may need to be changed
alphawue.samps.diffs <-alpha.samps$Modern -alpha.samps$Past

alpha.samps.m <- melt(alpha.samps)
colnames(alpha.samps.m) <- c("ageclass", "value")
wue.cohort  <- alpha.samps.m  %>% group_by(ageclass) %>% dplyr::summarise(Predicted = mean(value),
                                                                          ci.hi = quantile(value,0.975),
                                                                          ci.lo = quantile(value,0.025) )


wue.cohort$ageclass <- factor(wue.cohort$ageclass, levels = c("Past", "Modern"))

mean.pred.wue <- ggplot(wue.cohort, aes(ageclass, Predicted, fill = ageclass))+geom_bar(stat = "identity", width = 0.5)+
  geom_errorbar(data = wue.cohort, aes(ymin=ci.lo, ymax=ci.hi), color = "grey29", alpha = 0.8, size = 0.5, width = 0.1) + ylab("Mean estimated iWUE")+scale_fill_manual(values = c("Past"='#2166ac', 'Modern' = "#b2182b"))+theme_bw(base_size = 16)+theme(panel.grid = element_blank(), axis.title.x = element_blank(), legend.position = "none")+ylim(0,170)


mean.pred.wue.dot <- ggplot(data = wue.cohort, aes(x = Predicted, y = ageclass, color = ageclass, size = 4))+
  geom_errorbarh(data = wue.cohort, aes(xmin = ci.lo, xmax = ci.hi, size = 2,height = 0))+geom_point()+
  theme_bw(base_size = 18)+xlim(120, 160)+scale_color_manual(values = c("Past"='blue',"Modern"='red'))+coord_flip()+theme(legend.position = "none", axis.title.x = element_blank(), panel.grid = element_blank())+ xlab("Baseline iWUE")


# get the parameters for the WUE model:

beta1wue.samps  <- data.frame(samps.wue.params[,3:4])
colnames(beta1wue.samps) <- c( "Past", "Modern")
beta1wue.samps.diffs <- beta1wue.samps$Modern - beta1wue.samps$Past

beta1wue.samps.m <- melt(beta1wue.samps)
colnames(beta1wue.samps.m) <- c("ageclass", "value")
beta1wue.cohort  <- beta1wue.samps.m  %>% group_by(ageclass) %>% dplyr::summarise(Predicted = mean(value),
                                                                          ci.hi = quantile(value,0.975),
                                                                          ci.lo = quantile(value,0.025) )

beta1wue.cohort$ageclass <- factor(beta1wue.cohort$ageclass, levels = c("Past", "Modern"))

mean.pred.beta1 <- ggplot(beta1wue.cohort, aes(ageclass, Predicted, fill = ageclass))+geom_bar(stat = "identity", width = 0.5)+
  geom_errorbar(data = beta1wue.cohort, aes(ymin=ci.lo, ymax=ci.hi), color = "grey29", alpha = 0.8, size = 0.5, width = 0.1) + 
  ylim(-7, 5)+geom_hline(yintercept = 0, color = "grey", linetype = "dashed")+
  ylab("Effect of Precip. on iWUE")+scale_fill_manual(values = c("Past"='#2166ac', 'Modern' = "#b2182b"))+theme_bw(base_size = 16)+theme(panel.grid = element_blank(), axis.title.x = element_blank(), legend.position = "none")


beta2wue.samps  <- data.frame(samps.wue.params[,5:6])
colnames(beta2wue.samps) <- c("Past", "Modern")
beta2wue.samps.diffs <- beta2wue.samps$Modern - beta2wue.samps$Past
beta2wue.samps.m <- melt(beta2wue.samps)
colnames(beta2wue.samps.m) <- c("ageclass", "value")
beta2wue.cohort  <- beta2wue.samps.m  %>% group_by(ageclass) %>% dplyr::summarise(Predicted = mean(value),
                                                                                  ci.hi = quantile(value,0.975),
                                                                                  ci.lo = quantile(value,0.025) )



beta2wue.cohort$ageclass <- factor(beta2wue.cohort$ageclass, levels = c("Past", "Modern"))

mean.pred.beta2 <- ggplot(beta2wue.cohort, aes(ageclass, Predicted, fill = ageclass))+geom_bar(stat = "identity", width = 0.5)+
  geom_errorbar(data = beta2wue.cohort, aes(ymin=ci.lo, ymax=ci.hi), color = "grey29", alpha = 0.8, size = 0.5, width = 0.1) + ylab("Effect of Tmax on iWUE")+
  ylim(-7, 5)+geom_hline(yintercept = 0, color = "grey", linetype = "dashed")+scale_fill_manual(values = c("Past"='#2166ac', 'Modern' = "#b2182b"))+theme_bw(base_size = 16)+theme(panel.grid = element_blank(), axis.title.x = element_blank(), legend.position = "none")

# for Diameter
beta3wue.samps  <- data.frame(samps.wue.params[,7:8])
colnames(beta3wue.samps) <- c("Past", "Modern")
beta3wue.samps.diffs <- beta3wue.samps$Modern - beta3wue.samps$Past


beta3wue.samps.m <- melt(beta3wue.samps)
colnames(beta3wue.samps.m) <- c("ageclass", "value")
beta3wue.cohort  <- beta3wue.samps.m  %>% group_by(ageclass) %>% dplyr::summarise(Predicted = mean(value),
                                                                                  ci.hi = quantile(value,0.975),
                                                                                  ci.lo = quantile(value,0.025) )



beta3wue.cohort$ageclass <- factor(beta3wue.cohort$ageclass, levels = c("Past", "Modern"))

mean.pred.beta3 <- ggplot(beta3wue.cohort, aes(ageclass, Predicted, fill = ageclass))+geom_bar(stat = "identity", width = 0.5)+
  geom_errorbar(data = beta3wue.cohort, aes(ymin=ci.lo, ymax=ci.hi), color = "grey29", alpha = 0.8, size = 0.5, width = 0.1) + ylab("Effect of DBH on iWUE")+
ylim(-10, 12)+geom_hline(yintercept = 0, color = "grey", linetype = "dashed")+scale_fill_manual(values = c("Past"='#2166ac', 'Modern' = "#b2182b"))+theme_bw(base_size = 16)+theme(panel.grid = element_blank(), axis.title.x = element_blank(), legend.position = "none")



# plot out WUE model responses:
png(height = 3, width = 12, units = "in", res = 300, "outputs/growth_model/summary_of_WUE_mod.png")
plot_grid(mean.pred.wue.dot, mean.pred.beta1, mean.pred.beta2, mean.pred.beta3, ncol = 4)
dev.off()


# ----------get the parameters for the d13 model:
alpha.samps  <- data.frame(samps.d13.params[,1:2])
colnames(alpha.samps) <- c( "Modern", "Past") # note this may need to be changed
alphad13.samps.diffs <-alpha.samps$Modern -alpha.samps$Past

alpha.samps.m <- melt(alpha.samps)
colnames(alpha.samps.m) <- c("ageclass", "value")
d13.cohort  <- alpha.samps.m  %>% group_by(ageclass) %>% dplyr::summarise(Predicted = mean(value),
                                                                          ci.hi = quantile(value,0.975),
                                                                          ci.lo = quantile(value,0.025) )


d13.cohort$ageclass <- factor(d13.cohort$ageclass, levels = c("Past", "Modern"))

mean.pred.d13 <- ggplot(d13.cohort, aes(ageclass, Predicted, fill = ageclass))+geom_bar(stat = "identity", width = 0.5)+
  geom_errorbar(data = d13.cohort, aes(ymin=ci.lo, ymax=ci.hi), color = "grey29", alpha = 0.8, size = 0.5, width = 0.1) + ylab(expression(paste("Baseline " ,delta^{13}, "C (\u2030)")))+scale_fill_manual(values = c("Past"='#2166ac', 'Modern' = "#b2182b"))+theme_bw(base_size = 16)+theme(panel.grid = element_blank(), axis.title.x = element_blank(), legend.position = "none")
# make dot plots:

mean.pred.d13.dot <- ggplot(data = d13.cohort, aes(x = Predicted, y = ageclass, color = ageclass, size = 4))+
  geom_errorbarh(data = d13.cohort, aes(xmin = ci.lo, xmax = ci.hi, size = 2,height = 0))+geom_point()+
  theme_bw(base_size = 18)+xlim(-25.5, -23)+scale_color_manual(values = c("Past"='blue',"Modern"='red'))+coord_flip()+theme(legend.position = "none", axis.title.x = element_blank(), panel.grid = element_blank())+ xlab(expression(paste("Baseline " ,delta^{13}, "C (\u2030)")))


beta1d13.samps  <- data.frame(samps.d13.params[,3:4])
colnames(beta1d13.samps) <- c("Modern", "Past")
beta1d13.samps.diffs <- beta1d13.samps$Modern - beta1d13.samps$Past

beta1d13.samps.m <- melt(beta1d13.samps)
colnames(beta1d13.samps.m) <- c("ageclass", "value")
beta1d13.cohort  <- beta1d13.samps.m  %>% group_by(ageclass) %>% dplyr::summarise(Predicted = mean(value),
                                                                                  ci.hi = quantile(value,0.975),
                                                                                  ci.lo = quantile(value,0.025) )

beta1d13.cohort$ageclass <- factor(beta1d13.cohort$ageclass, levels = c("Past", "Modern"))

mean.pred.beta1.d13 <- ggplot(beta1d13.cohort, aes(ageclass, Predicted, fill = ageclass))+geom_bar(stat = "identity", width = 0.5)+
  geom_errorbar(data = beta1d13.cohort, aes(ymin=ci.lo, ymax=ci.hi), color = "grey29", alpha = 0.8, size = 0.5, width = 0.1)+ 
ylim(-0.7, 0.6)+geom_hline(yintercept = 0, color = "grey", linetype = "dashed")+ ylab(expression(paste("Precip. effect on " ,delta^{13}, "C (\u2030)")))+scale_fill_manual(values = c("Past"='#2166ac', 'Modern' = "#b2182b"))+theme_bw(base_size = 16)+theme(panel.grid = element_blank(), axis.title.x = element_blank(), legend.position = "none")


beta2d13.samps  <- data.frame(samps.d13.params[,5:6])
colnames(beta2d13.samps) <- c("Modern", "Past")
beta2d13.samps.diffs <- beta2d13.samps$Modern - beta2d13.samps$Past
beta2d13.samps.m <- melt(beta2d13.samps)
colnames(beta2d13.samps.m) <- c("ageclass", "value")
beta2d13.cohort  <- beta2d13.samps.m  %>% group_by(ageclass) %>% dplyr::summarise(Predicted = mean(value),
                                                                                  ci.hi = quantile(value,0.975),
                                                                                  ci.lo = quantile(value,0.025) )



beta2d13.cohort$ageclass <- factor(beta2d13.cohort$ageclass, levels = c("Past", "Modern"))

mean.pred.beta2.d13 <- ggplot(beta2d13.cohort, aes(ageclass, Predicted, fill = ageclass))+geom_bar(stat = "identity", width = 0.5)+
  geom_errorbar(data = beta2d13.cohort, aes(ymin=ci.lo, ymax=ci.hi), color = "grey29", alpha = 0.8, size = 0.5, width = 0.1) +
  ylim(-0.7, 0.6)+geom_hline(yintercept = 0, color = "grey", linetype = "dashed")+ylab(expression(paste("Tmax effect on " ,delta^{13}, "C (\u2030)")))+scale_fill_manual(values = c("Past"='#2166ac', 'Modern' = "#b2182b"))+theme_bw(base_size = 16)+theme(panel.grid = element_blank(), axis.title.x = element_blank(), legend.position = "none")

# for Diameter
beta3d13.samps  <- data.frame(samps.d13.params[,7:8])
colnames(beta3d13.samps) <- c("Modern", "Past")
beta3d13.samps.diffs <- beta3d13.samps$Modern - beta3d13.samps$Past


beta3d13.samps.m <- melt(beta3d13.samps)
colnames(beta3d13.samps.m) <- c("ageclass", "value")
beta3d13.cohort  <- beta3d13.samps.m  %>% group_by(ageclass) %>% dplyr::summarise(Predicted = mean(value),
                                                                                  ci.hi = quantile(value,0.975),
                                                                                  ci.lo = quantile(value,0.025) )



beta3d13.cohort$ageclass <- factor(beta3d13.cohort$ageclass, levels = c("Past", "Modern"))

mean.pred.beta3.d13 <- ggplot(beta3d13.cohort, aes(ageclass, Predicted, fill = ageclass))+geom_bar(stat = "identity", width = 0.5)+
  geom_errorbar(data = beta3d13.cohort, aes(ymin=ci.lo, ymax=ci.hi), color = "grey29", alpha = 0.8, size = 0.5, width = 0.1) +
  ylim(-0.7, 0.6)+geom_hline(yintercept = 0, color = "grey", linetype = "dashed")+ylab(expression(paste("DBH effect on " ,delta^{13}, "C (\u2030)")))+scale_fill_manual(values = c("Past"='#2166ac', 'Modern' = "#b2182b"))+theme_bw(base_size = 16)+theme(panel.grid = element_blank(), axis.title.x = element_blank(), legend.position = "none")



# plot out d13 model responses:
png(height = 3, width = 12, units = "in", res = 300, "outputs/growth_model/summary_of_d13_mod.png")
plot_grid(mean.pred.d13.dot, mean.pred.beta1.d13, mean.pred.beta2.d13, mean.pred.beta3.d13, ncol = 4)
dev.off()

# plot overall
png(height = 6, width = 12, units = "in", res = 300, "outputs/growth_model/summary_of_d13_iwue_mod.png")
plot_grid(mean.pred.d13.dot, mean.pred.beta1.d13, mean.pred.beta2.d13, mean.pred.beta3.d13,
          mean.pred.wue.dot, mean.pred.beta1, mean.pred.beta2, mean.pred.beta3, ncol = 4, align = "v")
dev.off()


png(height = 9, width = 12, units = "in", res = 300, "outputs/growth_model/summary_of_d13_iwue_growth_mod.png")
plot_grid(mean.pred.d13.dot, mean.pred.beta1.d13, mean.pred.beta2.d13, mean.pred.beta3.d13,
          mean.pred.wue.dot, mean.pred.beta1, mean.pred.beta2, mean.pred.beta3, 
          mean.pred.growth.dot, b2.bar.age, b7.bar.age, b3.bar.age,ncol = 4, align = "v")
dev.off()

# beta diffs for tree ring growth cohorts:

beta_diffsm$beta <- factor(beta_diffsm$beta, levels = c("MAP", "Tmax", "MAPxTmax","DBH", "lag_1", "lag_2"))

beta.diffs.plot.mod.past <- ggplot(beta_diffsm, aes(beta, mean, fill = significant))+geom_bar(stat = "identity", width = 0.8)+geom_errorbar(data = beta_diffsm,aes(ymin=hdi.low, ymax=hdi.high), color = "grey29", alpha = 0.8, size = 0.5, width = 0.1)+
  ylab("Difference in beta parameter \n (Modern - Past)")+xlab("model parameters")+theme_bw(base_size = 16)+theme(panel.grid = element_blank(), legend.position = "none")+scale_fill_manual(values = c("Significant" = "black", "NS" = "lightgrey"))

png(height = 6, width = 12, units = "in", res = 300, "outputs/growth_model/summary_of_differences_cohort.png")
plot_grid(
plot_grid(mean.pred.wue, mean.pred.growth, ncol = 1, align = "hv", labels = c("", "B")),
beta.diffs.plot.mod.past, ncol = 2, labels = c("A", "C"), rel_widths = c(0.75, 1))
dev.off()

