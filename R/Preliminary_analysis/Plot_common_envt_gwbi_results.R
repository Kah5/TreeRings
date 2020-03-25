#------------------------Bring in all the coefficient estimates and put in one big graph-----------



samp.rwi.period <- readRDS( "outputs/gwbi_model/Lag4_cohort_re_clim/rwi_parameter_samps.rds")
samp.GUESS.period <- readRDS( "outputs/gwbi_model/Lag4_cohort_re_clim/Climate_match/GUESS_parameter_samps.rds")
samp.ED.period <- readRDS( "outputs/gwbi_model/Lag4_cohort_re_clim/Climate_match/ED_parameter_samps.rds")

test.RWI <- readRDS( "outputs/gwbi_model/Lag4_cohort_re_clim/rwi_testdata.rds")
test.GUESS <- readRDS( "outputs/gwbi_model/Lag4_cohort_re_clim/Climate_match/GUESS_testdata.rds")
test.ED <- readRDS( "outputs/gwbi_model/Lag4_cohort_re_clim/Climate_match/ED_testdata.rds")

train.RWI <- readRDS( "outputs/gwbi_model/Lag4_cohort_re_clim/Climate_match/rwi_traindata.rds")
train.GUESS <- readRDS( "outputs/gwbi_model/Lag4_cohort_re_clim/Climate_match/GUESS_traindata.rds")
train.ED <- readRDS( "outputs/gwbi_model/Lag4_cohort_re_clim/Climate_match/ED_traindata.rds")

samp.rwi.period <- samp.rwi.period[[1]]
samp.GUESS.period <- samp.GUESS.period[[1]]
samp.ED.period <- samp.ED.period[[1]]

# get parameter estimates from TR model:
alpha.samps <- samp.rwi.period[,1:length(unique(test.RWI$site))]
beta1.samps  <- samp.rwi.period[,(length(unique(test.RWI$site))+1):(length(unique(test.RWI$site))+2)]
beta2.samps  <- samp.rwi.period[,(length(unique(test.RWI$site))+3):(length(unique(test.RWI$site))+4)]
beta3.samps  <- samp.rwi.period[,(length(unique(test.RWI$site))+5):(length(unique(test.RWI$site))+6)]
beta4.samps  <- samp.rwi.period[,(length(unique(test.RWI$site))+7):(length(unique(test.RWI$site))+8)]
beta5.samps  <- samp.rwi.period[,(length(unique(test.RWI$site))+9):(length(unique(test.RWI$site))+10)]
beta6.samps  <- samp.rwi.period[,(length(unique(test.RWI$site))+11):(length(unique(test.RWI$site))+12)]

a <- data.frame(alpha.samps)
colnames(a) <- unique(train.RWI$site)
a$num <- rownames(a)
a.m.TR <- melt(a, id.vars=c("num"))
alpha.mplots <- ggplot(a.m.TR, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Random intercepts")+theme(legend.position = "none")

b1 <- data.frame(beta1.samps)
colnames(b1) <- unique(levels(train.RWI$ageclass)) # past = 2, modern = 1 here
b1$num <- rownames(b1)
b1.m.TR <- melt(b1, id.vars=c("num"))
b1.m.TR$model <- "Tree Rings"

b2 <- data.frame(beta2.samps)
colnames(b2) <-unique(levels(train.RWI$ageclass)) 
b2$num <- rownames(b2)
b2.m.TR <- melt(b2, id.vars=c("num"))
b2.m.TR$model <- "Tree Rings"

b3 <- data.frame(beta3.samps)
colnames(b3) <-unique(levels(train.RWI$ageclass)) 
b3$num <- rownames(b3)
b3.m.TR<- melt(b3, id.vars=c("num"))
b3.m.TR$model <- "Tree Rings"

b4 <- data.frame(beta4.samps)
colnames(b4) <-unique(levels(train.RWI$ageclass)) 
b4$num <- rownames(b4)
b4.m.TR <- melt(b4, id.vars=c("num"))
b4.m.TR$model <- "Tree Rings"

b5 <- data.frame(beta5.samps)
colnames(b5) <-unique(levels(train.RWI$ageclass)) 
b5$num <- rownames(b5)
b5.m.TR <- melt(b5, id.vars=c("num"))
b5.m.TR$model <- "Tree Rings"

b6 <- data.frame(beta6.samps)
colnames(b6) <-unique(levels(train.RWI$ageclass)) 
b6$num <- rownames(b6)
b6.m.TR <- melt(b6, id.vars=c("num"))
b6.m.TR$model <- "Tree Rings"

# get parameter estimates from ED model:
alpha.sampsED <- samp.ED.period[,1:length(unique(test.ED$site_num))]
beta1.sampsED  <- samp.ED.period[,(length(unique(test.ED$site_num))+1):(length(unique(test.ED$site_num))+2)]
beta2.sampsED  <- samp.ED.period[,(length(unique(test.ED$site_num))+3):(length(unique(test.ED$site_num))+4)]
beta3.sampsED  <- samp.ED.period[,(length(unique(test.ED$site_num))+5):(length(unique(test.ED$site_num))+6)]
beta4.sampsED  <- samp.ED.period[,(length(unique(test.ED$site_num))+7):(length(unique(test.ED$site_num))+8)]
beta5.sampsED  <- samp.ED.period[,(length(unique(test.ED$site_num))+9):(length(unique(test.ED$site_num))+10)]
beta6.sampsED  <- samp.ED.period[,(length(unique(test.ED$site_num))+11):(length(unique(test.ED$site_num))+12)]

a <- data.frame(alpha.sampsED)
colnames(a) <- unique(train.ED$site_num)
a$num <- rownames(a)
a.m.ED <- melt(a, id.vars=c("num"))
alpha.mplots <- ggplot(a.m.ED, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Random intercepts")+theme(legend.position = "none")
a.m.ED$model <- "ED2"

b1 <- data.frame(beta1.sampsED)
colnames(b1) <- c("Modern", "Past") # past = 2, modern = 1 here
b1$num <- rownames(b1)
b1.m.ED <- melt(b1, id.vars=c("num"))
b1.m.ED$model <- "ED2"

b2 <- data.frame(beta2.sampsED)
colnames(b2) <-c("Modern", "Past")
b2$num <- rownames(b2)
b2.m.ED <- melt(b2, id.vars=c("num"))
b2.m.ED$model <- "ED2"

b3 <- data.frame(beta3.sampsED)
colnames(b3) <-c("Modern", "Past")
b3$num <- rownames(b3)
b3.m.ED<- melt(b3, id.vars=c("num"))
b3.m.ED$model <- "ED2"

b4 <- data.frame(beta4.sampsED)
colnames(b4) <-c("Modern", "Past")
b4$num <- rownames(b4)
b4.m.ED <- melt(b4, id.vars=c("num"))
b4.m.ED$model <- "ED2"

b5 <- data.frame(beta5.sampsED)
colnames(b5) <-c("Modern", "Past")
b5$num <- rownames(b5)
b5.m.ED <- melt(b5, id.vars=c("num"))
b5.m.ED$model <- "ED2"

b6 <- data.frame(beta6.sampsED)
colnames(b6) <-c("Modern", "Past") 
b6$num <- rownames(b6)
b6.m.ED <- melt(b6, id.vars=c("num"))
b6.m.ED$model <- "ED2"


# get parameter estimates from LPJ-GUESS model:
alpha.sampsGUESS <- samp.GUESS.period[,1:length(unique(test.GUESS$site_num))]
beta1.sampsGUESS  <- samp.GUESS.period[,(length(unique(test.GUESS$site_num))+1):(length(unique(test.GUESS$site_num))+2)]
beta2.sampsGUESS  <- samp.GUESS.period[,(length(unique(test.GUESS$site_num))+3):(length(unique(test.GUESS$site_num))+4)]
beta3.sampsGUESS  <- samp.GUESS.period[,(length(unique(test.GUESS$site_num))+5):(length(unique(test.GUESS$site_num))+6)]
beta4.sampsGUESS  <- samp.GUESS.period[,(length(unique(test.GUESS$site_num))+7):(length(unique(test.GUESS$site_num))+8)]
beta5.sampsGUESS  <- samp.GUESS.period[,(length(unique(test.GUESS$site_num))+9):(length(unique(test.GUESS$site_num))+10)]
beta6.sampsGUESS  <- samp.GUESS.period[,(length(unique(test.GUESS$site_num))+11):(length(unique(test.GUESS$site_num))+12)]

a <- data.frame(alpha.sampsGUESS)
colnames(a) <- unique(train.GUESS$site_num)
a$num <- rownames(a)
a.m.GUESS <- melt(a, id.vars=c("num"))
alpha.mplots <- ggplot(a.m.GUESS, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Random intercepts")+theme(legend.position = "none")
a.m.GUESS$model <- "LPJ-GUESS"

b1 <- data.frame(beta1.sampsGUESS)
colnames(b1) <- c("Modern", "Past") # past = 2, modern = 1 here
b1$num <- rownames(b1)
b1.m.GUESS <- melt(b1, id.vars=c("num"))
b1.m.GUESS$model <- "LPJ-GUESS"

b2 <- data.frame(beta2.sampsGUESS)
colnames(b2) <-c("Modern", "Past")
b2$num <- rownames(b2)
b2.m.GUESS <- melt(b2, id.vars=c("num"))
b2.m.GUESS$model <- "LPJ-GUESS"

b3 <- data.frame(beta3.sampsGUESS)
colnames(b3) <-c("Modern", "Past")
b3$num <- rownames(b3)
b3.m.GUESS<- melt(b3, id.vars=c("num"))
b3.m.GUESS$model <- "LPJ-GUESS"

b4 <- data.frame(beta4.sampsGUESS)
colnames(b4) <-c("Modern", "Past")
b4$num <- rownames(b4)
b4.m.GUESS <- melt(b4, id.vars=c("num"))
b4.m.GUESS$model <- "LPJ-GUESS"

b5 <- data.frame(beta5.sampsGUESS)
colnames(b5) <-c("Modern", "Past")
b5$num <- rownames(b5)
b5.m.GUESS <- melt(b5, id.vars=c("num"))
b5.m.GUESS$model <- "LPJ-GUESS"

b6 <- data.frame(beta6.sampsGUESS)
colnames(b6) <-c("Modern", "Past")
b6$num <- rownames(b6)
b6.m.GUESS <- melt(b6, id.vars=c("num"))
b6.m.GUESS$model <- "LPJ-GUESS"


# now combine all the beta1s together and make a modern past dotplot:
a.m.TR$model <- "Tree Rings"
a1.m <- bind_rows(a.m.TR, a.m.ED, a.m.GUESS)
b1.m <- bind_rows(b1.m.TR, b1.m.ED, b1.m.GUESS)
b2.m <- bind_rows(b2.m.TR, b2.m.ED, b2.m.GUESS)
b3.m <- bind_rows(b3.m.TR, b3.m.ED, b3.m.GUESS)
b4.m <- bind_rows(b4.m.TR, b4.m.ED, b4.m.GUESS)
b5.m <- bind_rows(b5.m.TR, b5.m.ED, b5.m.GUESS)
b6.m <- bind_rows(b6.m.TR, b6.m.ED, b6.m.GUESS)


b1.sum <- b1.m %>% group_by(variable, model) %>% dplyr::summarise(mean.val = mean(value),
                                                                  Ci.low = quantile(value, 0.025), 
                                                                  Ci.high = quantile(value, 0.975))

b1.sum$variable <- factor(b1.sum$variable, levels = c( "Past",  "Modern"))

b2.sum <- b2.m %>% group_by(variable, model) %>% dplyr::summarise(mean.val = mean(value),
                                                                  Ci.low = quantile(value, 0.025), 
                                                                  Ci.high = quantile(value, 0.975))
b2.sum$variable <- factor(b2.sum$variable, levels = c( "Past",  "Modern"))


b3.sum <- b3.m %>% group_by(variable, model) %>% dplyr::summarise(mean.val = mean(value),
                                                                  Ci.low = quantile(value, 0.025), 
                                                                  Ci.high = quantile(value, 0.975))
b3.sum$variable <- factor(b3.sum$variable, levels = c( "Past",  "Modern"))


b4.sum <- b4.m %>% group_by(variable, model) %>% dplyr::summarise(mean.val = mean(value),
                                                                  Ci.low = quantile(value, 0.025), 
                                                                  Ci.high = quantile(value, 0.975))
b4.sum$variable <- factor(b4.sum$variable, levels = c( "Past",  "Modern"))

b5.sum <- b5.m %>% group_by(variable, model) %>% dplyr::summarise(mean.val = mean(value),
                                                                  Ci.low = quantile(value, 0.025), 
                                                                  Ci.high = quantile(value, 0.975))
b5.sum$variable <- factor(b5.sum$variable, levels = c( "Past",  "Modern"))

b6.sum <- b6.m %>% group_by(variable, model) %>% dplyr::summarise(mean.val = mean(value),
                                                                  Ci.low = quantile(value, 0.025), 
                                                                  Ci.high = quantile(value, 0.975))

b6.sum$variable <- factor(b6.sum$variable, levels = c( "Past",  "Modern"))


# now plot dotplots:

b1.dot <- ggplot(data.frame(b1.sum), aes(x = model, y = mean.val, color = variable), size = 5)+geom_errorbar( aes(ymin = Ci.low, ymax = Ci.high, width = 0.5, alpha = 0.8), size = 5, position = position_dodge(width=0.5))+
  geom_point(position=position_dodge(width=0.5), size = 5)+geom_abline(aes(intercept = 0, slope = 0), linetype = "dashed")+scale_color_manual(values = c("Past" = "blue","Modern" = "red"), name = " ")+ylab("Precipitation \n sensitivity") + geom_vline(xintercept = 0, linetype = "dashed")+theme_bw(base_size = 25)+theme( axis.title.x = element_blank(), panel.grid = element_blank())+ylim(0,0.25)

b1.dot <- ggplot(data.frame(b1.sum), aes(x = model, y = mean.val, color = variable), size =  5)+geom_errorbar( aes(ymin = Ci.low, ymax = Ci.high, width = 0), size =  5, position = position_dodge(width=0.5))+
  geom_point(position=position_dodge(width=0.5), size =  10)+geom_abline(aes(intercept = 0, slope = 0), linetype = "dashed")+scale_color_manual(values = c("Past" = "blue","Modern" = "red"), name = " ")+ylab("Temperature \n sensitivity") + geom_vline(xintercept = 0, linetype = "dashed")+theme_bw(base_size = 25)+theme( axis.title.x = element_blank(), panel.grid = element_blank())+ylim(0,0.25)

b1.dot <- ggplot(data.frame(b1.sum), aes(x = model, y = mean.val, color = variable), size =  5)+geom_point(position=position_dodge(width=0.5), size =  10)+geom_errorbar( aes(ymin = Ci.low, ymax = Ci.high, width = 0), size =  5, position = position_dodge(width=0.5))+
  geom_abline(aes(intercept = 0, slope = 0), linetype = "dashed")+scale_color_manual(values = c("Past" = "blue","Modern" = "red"), name = " ")+ylab("Precipitation \n sensitivity") + geom_vline(xintercept = 0, linetype = "dashed")+theme_bw(base_size = 25)+theme( axis.title.x = element_blank(), panel.grid = element_blank())+ylim(0,0.25)

b2.dot <- ggplot(data.frame(b2.sum), aes(x = model, y = mean.val, color = variable), size =  5)+geom_errorbar( aes(ymin = Ci.low, ymax = Ci.high, width = 0), size =  5, position = position_dodge(width=0.5))+
  geom_point(position=position_dodge(width=0.5), size =  10)+geom_abline(aes(intercept = 0, slope = 0), linetype = "dashed")+scale_color_manual(values = c("Past" = "blue","Modern" = "red"), name = " ")+ylab("Temperature \n sensitivity") + geom_vline(xintercept = 0, linetype = "dashed")+theme_bw(base_size = 25)+theme( axis.title.x = element_blank(), panel.grid = element_blank())+ylim(-0.155,0)

b3.dot <- ggplot(data.frame(b3.sum), aes(x = model, y = mean.val, color = variable), size = 5)+geom_errorbar( aes(ymin = Ci.low, ymax = Ci.high, width = 0), size =  5, position = position_dodge(width=0.5))+
  geom_point(position=position_dodge(width=0.5), size =  10)+geom_abline(aes(intercept = 0, slope = 0), linetype = "dashed")+scale_color_manual(values = c("Past" = "blue","Modern" = "red"), name = " ")+ylab("Lag -1 effect") + geom_vline(xintercept = 0, linetype = "dashed")+theme_bw(base_size = 25)+theme( axis.title.x = element_blank(), panel.grid = element_blank())#+ylim(-0.150,0)

b4.dot <- ggplot(data.frame(b4.sum), aes(x = model, y = mean.val, color = variable), size =  5)+geom_errorbar( aes(ymin = Ci.low, ymax = Ci.high, width = 0), size =  5, position = position_dodge(width=0.5))+
  geom_point(position=position_dodge(width=0.5), size =  10)+geom_abline(aes(intercept = 0, slope = 0), linetype = "dashed")+scale_color_manual(values = c("Past" = "blue","Modern" = "red"), name = " ")+ylab("Lag -2 effect") + geom_vline(xintercept = 0, linetype = "dashed")+theme_bw(base_size = 25)+theme( axis.title.x = element_blank(), panel.grid = element_blank())#+ylim(-0.150,0)

b5.dot <- ggplot(data.frame(b5.sum), aes(x = model, y = mean.val, color = variable), size =  5)+geom_errorbar( aes(ymin = Ci.low, ymax = Ci.high, width = 0), size =  5, position = position_dodge(width=0.5))+
  geom_point(position=position_dodge(width=0.5), size =  10)+geom_abline(aes(intercept = 0, slope = 0), linetype = "dashed")+scale_color_manual(values = c("Past" = "blue","Modern" = "red"), name = " ")+ylab("Lag -3 effect") + geom_vline(xintercept = 0, linetype = "dashed")+theme_bw(base_size = 25)+theme( axis.title.x = element_blank(), panel.grid = element_blank())#+ylim(-0.150,0)

b6.dot <- ggplot(data.frame(b6.sum), aes(x = model, y = mean.val, color = variable), size = 5)+geom_errorbar( aes(ymin = Ci.low, ymax = Ci.high, width = 0), size =  5, position = position_dodge(width=0.5))+
  geom_point(position=position_dodge(width=0.5), size =  10)+geom_abline(aes(intercept = 0, slope = 0), linetype = "dashed")+scale_color_manual(values = c("Past" = "blue","Modern" = "red"), name = " ")+ylab("Lag -4 effect") + geom_vline(xintercept = 0, linetype = "dashed")+theme_bw(base_size = 25)+theme( axis.title.x = element_blank(), panel.grid = element_blank())#+ylim(-0.150,0)

legend <- get_legend(b1.dot)

png(height = 14, width = 15, units = "in", res = 500, "outputs/gwbi_model/Lag4_cohort_re_clim/Climate_match/all_params_dotplot.png")
plot_grid(plot_grid( b1.dot+theme(legend.position = "none"), 
           b2.dot+theme(legend.position = "none"), 
           b3.dot+theme(legend.position = "none"), 
           b4.dot+theme(legend.position = "none"), 
           b5.dot+theme(legend.position = "none"),
           b6.dot+theme(legend.position = "none"), ncol = 2, align = "hv", rel_heights = c(1,1,1,1,1,1)),
          legend, ncol = 2, rel_widths = c(1,0.15))
dev.off()

png(height = 20, width = 8, units = "in", res = 500, "outputs/gwbi_model/Lag4_cohort_re_clim/Climate_match/all_params_dotplot_vertical.png")
plot_grid(plot_grid( b1.dot+theme(legend.position = "none", plot.margin = unit(c(-1, 0, 0, 0), "cm")), 
           b2.dot+theme(legend.position = "none",plot.margin = unit(c(0, 0, 0, 0), "cm")), 
           b3.dot+theme(legend.position = "none",plot.margin = unit(c(0, 0, 0, 0), "cm")), 
           b4.dot+theme(legend.position = "none",plot.margin = unit(c(0, 0, 0, 0), "cm")), 
           b5.dot+theme(legend.position = "none",plot.margin = unit(c(0, 0, 0, 0), "cm")),
           b6.dot+theme(legend.position = "none"), ncol = 1, align = "hv",axis = "tb", rel_heights = c(1,1,1,1,1,1)),
        legend, ncol = 2, rel_widths = c(1,0.15))
dev.off()

png(height = 8, width = 20, units = "in", res = 500, "outputs/gwbi_model/Lag4_cohort_re_clim/Climate_match/all_params_dotplot_horizontal.png")
plot_grid(plot_grid( b1.dot+theme(legend.position = "none", plot.margin = unit(c(-1, 0, 0, 0), "cm")), 
           b2.dot+theme(legend.position = "none",plot.margin = unit(c(0, 0, 0, 0), "cm")), 
           b3.dot+theme(legend.position = "none",plot.margin = unit(c(0, 0, 0, 0), "cm")), 
           b4.dot+theme(legend.position = "none",plot.margin = unit(c(0, 0, 0, 0), "cm")), 
           b5.dot+theme(legend.position = "none",plot.margin = unit(c(0, 0, 0, 0), "cm")),
           b6.dot+theme(legend.position = "none"), ncol = 3, align = "hv",axis = "tb", rel_heights = c(1,1,1,1,1,1)),
          legend, ncol = 2, rel_widths = c(1,0.15))
dev.off()


# calculate mean differences between overall predicted tree growth under the same climate:
precip.range <- c(-2.4571, 0.1661, 3.014)
tmax.range <- c(-2.4571, 0.1661, 3.014)
rel_gwbi_1<- c(0.05)
rel_gwbi_2<- c(0.05)
rel_gwbi_3<- c(0.05)
rel_gwbi_4<- c(0.05)
period_cd <- unique(test.ED$period_cd)
model <- c("ED2", "LPJ-GUESS", "Tree Rings")
meanMAP.sim.all <- expand.grid(precip.range, tmax.range, rel_gwbi_1, rel_gwbi_2, rel_gwbi_3, rel_gwbi_4, period_cd, model)
colnames(meanMAP.sim.all) <- c("Precip", "tmax", "gwbi_1", "gwbi_2", "gwbi_3", "gwbi_4", "period", "model")

head(samp.ED.period)
head(samp.GUESS.period)
head(samp.rwi.period)

int.mcmc <- as.mcmc(samp.ED.period)
int.mcmc.mat <- as.matrix(int.mcmc)
int.mcmc.dat <- data.frame(int.mcmc.mat)

meanMAP.sim <- meanMAP.sim.all[meanMAP.sim.all$model %in% "ED2",]
meanMAP.sim<- expand.grid(precip.range, tmax.range, rel_gwbi_1, rel_gwbi_2, rel_gwbi_3, rel_gwbi_4, period_cd, model = "ED2", site.num = 1:15)
colnames(meanMAP.sim) <- c("Precip", "tmax", "gwbi_1", "gwbi_2", "gwbi_3", "gwbi_4", "period", "model", "site.num")
meanMAP.sim$site.num <- as.character(meanMAP.sim$site.num)

int.1 <- matrix(rep(NA, nrow(int.mcmc.dat)*length(meanMAP.sim$tmax)), nrow = nrow(int.mcmc.dat))
#meanMAP.sim.all <- expand.grid(precip.range, tmax.range, rel_gwbi_1, rel_gwbi_2, rel_gwbi_3, rel_gwbi_4, period_cd, model)
#colnames(meanMAP.sim.all) <- c("Precip", "tmax", "gwbi_1", "gwbi_2", "gwbi_3", "gwbi_4", "period", "model")

#test <- a1.m %>% group_by(num, variable) %>% spread(value, model)

# use betas to generate pp given a value for site, structure, dbh, rwi1, rwi2, and varying T and MAP:

for(i in 1:length(meanMAP.sim$tmax)){
  # for struct.cohort == 1
  int.1[,i] <- int.mcmc.dat[,paste0("alpha.", meanMAP.sim[i,"site.num"], ".")]+
    int.mcmc.dat[,paste0("beta1.", meanMAP.sim[i,"period"], ".")]*meanMAP.sim[i,]$Precip+    
    int.mcmc.dat[,paste0("beta2.", meanMAP.sim[i,"period"], ".")]*meanMAP.sim[i,"tmax"] + 
    int.mcmc.dat[,paste0("beta3.", meanMAP.sim[i,"period"], ".")]*meanMAP.sim[i,"gwbi_1"]  + 
    int.mcmc.dat[,paste0("beta4.", meanMAP.sim[i,"period"], ".")]*meanMAP.sim[i,"gwbi_2"] +
    int.mcmc.dat[,paste0("beta5.", meanMAP.sim[i,"period"], ".")]*meanMAP.sim[i,"gwbi_3"] + 
    int.mcmc.dat[,paste0("beta6.", meanMAP.sim[i,"period"], ".")] * (meanMAP.sim[i,"gwbi_4"])
  
  
}


# columns are the different degree-site scenario combinations
meanMAP.sim$idval <- 1:length(meanMAP.sim$site)
# rows are the mcmc values
colnames(int.1) <- 1:length(meanMAP.sim$site)
test.m <- melt(int.1)
colnames(test.m) <- c("MCMC", "idval", "Ypred")
full.pred <- left_join(test.m, meanMAP.sim, by = "idval")
full.pred$RWI.pred <- full.pred$Ypred

full.pred.ED <- full.pred

# make predictions for GUESS:

int.mcmc <- as.mcmc(samp.GUESS.period)
int.mcmc.mat <- as.matrix(int.mcmc)
int.mcmc.dat <- data.frame(int.mcmc.mat)

meanMAP.sim <- meanMAP.sim.all[meanMAP.sim.all$model %in% "LPJ-GUESS",]
meanMAP.sim <- expand.grid(precip.range, tmax.range, rel_gwbi_1, rel_gwbi_2, rel_gwbi_3, rel_gwbi_4, period_cd, model = "LPJ-GUESS", site.num = 1:15)
colnames(meanMAP.sim) <- c("Precip", "tmax", "gwbi_1", "gwbi_2", "gwbi_3", "gwbi_4", "period", "model", "site.num")
meanMAP.sim$site.num <- as.character(meanMAP.sim$site.num)

int.1 <- matrix(rep(NA, nrow(int.mcmc.dat)*length(meanMAP.sim$tmax)), nrow = nrow(int.mcmc.dat))

# use betas to generate pp given a value for site, structure, dbh, rwi1, rwi2, and varying T and MAP:

for(i in 1:length(meanMAP.sim$tmax)){
  # for struct.cohort == 1
  int.1[,i] <- int.mcmc.dat[,paste0("alpha.", meanMAP.sim[i,"site.num"], ".")]+
    int.mcmc.dat[,paste0("beta1.", meanMAP.sim[i,"period"], ".")]*meanMAP.sim[i,]$Precip+    
    int.mcmc.dat[,paste0("beta2.", meanMAP.sim[i,"period"], ".")]*meanMAP.sim[i,"tmax"] + 
    int.mcmc.dat[,paste0("beta3.", meanMAP.sim[i,"period"], ".")]*meanMAP.sim[i,"gwbi_1"]  + 
    int.mcmc.dat[,paste0("beta4.", meanMAP.sim[i,"period"], ".")]*meanMAP.sim[i,"gwbi_2"] +
    int.mcmc.dat[,paste0("beta5.", meanMAP.sim[i,"period"], ".")]*meanMAP.sim[i,"gwbi_3"] + 
    int.mcmc.dat[,paste0("beta6.", meanMAP.sim[i,"period"], ".")] * (meanMAP.sim[i,"gwbi_4"])
  
  
}


# columns are the different degree-site scenario combinations
meanMAP.sim$idval <- 1:length(meanMAP.sim$site)
# rows are the mcmc values
colnames(int.1) <- 1:length(meanMAP.sim$site)
test.m <- melt(int.1)
colnames(test.m) <- c("MCMC", "idval", "Ypred")
full.pred <- left_join(test.m, meanMAP.sim, by = "idval")
full.pred$RWI.pred <- full.pred$Ypred

full.pred.GUESS <- full.pred


# make predictions for RWI:


int.mcmc <- as.mcmc(samp.rwi.period)
int.mcmc.mat <- as.matrix(int.mcmc)
int.mcmc.dat <- data.frame(int.mcmc.mat)

meanMAP.sim <- meanMAP.sim.all[meanMAP.sim.all$model %in% "Tree Rings",]
meanMAP.sim<- expand.grid(precip.range, tmax.range, rel_gwbi_1, rel_gwbi_2, rel_gwbi_3, rel_gwbi_4, period_cd, model = "Tree Rings", site.num = 1:16)
colnames(meanMAP.sim) <- c("Precip", "tmax", "gwbi_1", "gwbi_2", "gwbi_3", "gwbi_4", "period", "model", "site.num")
meanMAP.sim$site.num <- as.character(meanMAP.sim$site.num)

int.1 <- matrix(rep(NA, nrow(int.mcmc.dat)*length(meanMAP.sim$tmax)), nrow = nrow(int.mcmc.dat))

# use betas to generate pp given a value for site, structure, dbh, rwi1, rwi2, and varying T and MAP:

for(i in 1:length(meanMAP.sim$tmax)){
  # for struct.cohort == 1
  int.1[,i] <- int.mcmc.dat[,paste0("alpha.", meanMAP.sim[i,"site.num"], ".")]+
    int.mcmc.dat[,paste0("beta1.", meanMAP.sim[i,"period"], ".")]*meanMAP.sim[i,]$Precip+    
    int.mcmc.dat[,paste0("beta2.", meanMAP.sim[i,"period"], ".")]*meanMAP.sim[i,"tmax"] + 
    int.mcmc.dat[,paste0("beta3.", meanMAP.sim[i,"period"], ".")]*meanMAP.sim[i,"gwbi_1"]  + 
    int.mcmc.dat[,paste0("beta4.", meanMAP.sim[i,"period"], ".")]*meanMAP.sim[i,"gwbi_2"] +
    int.mcmc.dat[,paste0("beta5.", meanMAP.sim[i,"period"], ".")]*meanMAP.sim[i,"gwbi_3"] + 
    int.mcmc.dat[,paste0("beta6.", meanMAP.sim[i,"period"], ".")] * (meanMAP.sim[i,"gwbi_4"])
  
  
}


# columns are the different degree-site scenario combinations
meanMAP.sim$idval <- 1:length(meanMAP.sim$site)
# rows are the mcmc values
colnames(int.1) <- 1:length(meanMAP.sim$site)
test.m <- melt(int.1)
colnames(test.m) <- c("MCMC", "idval", "Ypred")
full.pred <- left_join(test.m, meanMAP.sim, by = "idval")
full.pred$RWI.pred <- full.pred$Ypred

full.pred.TR <- full.pred

full.pred.all <- rbind(full.pred.ED, full.pred.GUESS, full.pred.TR)


allgrowth.summary <- full.pred.all %>% group_by(model,period) %>% dplyr::summarise(mean.pred = mean(exp(RWI.pred), na.rm =TRUE),
                                                                                   ci.low.pred=quantile(exp(RWI.pred), 0.025, na.rm =TRUE),
                                                                                   ci.high.pred=quantile(exp(RWI.pred), 0.975, na.rm =TRUE))


ggplot(allgrowth.summary, aes(x=model,y= mean.pred, fill = period))+geom_bar(stat = "identity", position = "dodge")+
  geom_errorbar(aes(ymin = ci.low.pred, ymax = ci.high.pred), position = "dodge")

allgrowth.summary.byclim <- full.pred.all %>% group_by(model,period, tmax, Precip) %>% dplyr::summarise(mean.pred = mean(exp(RWI.pred), na.rm =TRUE),
                                                                                                        ci.low.pred=quantile(exp(RWI.pred), 0.025, na.rm =TRUE),
                                                                                                        ci.high.pred=quantile(exp(RWI.pred), 0.975, na.rm =TRUE))



allgrowth.summary.byclim
lowprecip <- allgrowth.summary.byclim[allgrowth.summary.byclim$Precip <= 0,]

ggplot(lowprecip, aes(x=model,y= mean.pred, fill = period))+geom_bar(stat = "identity", position = "dodge")+
  geom_errorbar(aes(ymin = ci.low.pred, ymax = ci.high.pred), position = "dodge")+facet_wrap(~tmax)

allgrowth.summary.byclim$time<- ifelse(allgrowth.summary.byclim$period == 2, "1950-2011", "1895-1950")


png(height = 5, width = 7, units = "in", res = 300, "outputs/gwbi_model/Lag4_cohort_re_clim/Climate_match/growth_model_comparison_all_models.png")
ggplot(allgrowth.summary.byclim[allgrowth.summary.byclim$Precip == 0.1661 & allgrowth.summary.byclim$tmax == 0.1661,], aes(x=model,y= mean.pred, fill = time))+geom_bar(stat = "identity", position = "dodge")+
  geom_errorbar(aes(ymin = ci.low.pred, ymax = ci.high.pred), position = position_dodge(width = 0.9), width = 0.3)+theme_bw(base_size = 20)+theme(panel.grid = element_blank())+ylab("Mean predicted Growth")+xlab("")+scale_fill_manual(values = c("blue", "red"), name = "Time Period")
dev.off()

# calculate mean differences between parameters:

b1.class <- b1.m %>% group_by(num, model) %>% spread( key = variable, value = value)
b1.class$pct_change <- ((b1.class$Modern - b1.class$Past))

beta1.diff <- b1.class %>% group_by(model) %>% summarise(mean = mean(pct_change),
                                                         ci.low = quantile(pct_change, 0.025), 
                                                         ci.high = quantile(pct_change, 0.975))

# plot average change in drought sensitivity
pct.drought.change <- ggplot(beta1.diff, aes( x=model, y = mean, fill = model))+geom_bar(stat="identity")+geom_errorbar( aes(ymin = ci.low, ymax = ci.high, width = 0.25), size = 1.5, position = position_dodge(width=0.5))+scale_fill_manual(values = c('#1b9e77',
                                                                                                                                                                                                                                                          '#d95f02',
                                                                                                                                                                                                                                                          '#7570b3'))+theme_bw(base_size = 35)+theme(legend.position = "none", axis.title.x = element_blank(), panel.grid = element_blank())+ylab("drought sensitvity change \n between Modern and Past")

png(height = 8, width = 10, units = "in", res = 300, "outputs/gwbi_model/Lag4_cohort_re_clim/Climate_match/pct_change_drought_senstivity_updated.png")
pct.drought.change
dev.off()


# calculate pct temperature change
b2.class <- b2.m %>% group_by(num, model) %>% spread( key = variable, value = value)
b2.class$pct_change <- ((abs(b2.class$Modern) - abs(b2.class$Past)))

beta2.diff <- b2.class %>% group_by(model) %>% summarise(mean = mean(pct_change),
                                                         ci.low = quantile(pct_change, 0.025), 
                                                         ci.high = quantile(pct_change, 0.975))

# plot average change in drought sensitivity
pct.temp.change <- ggplot(beta2.diff, aes( x=model, y = mean, fill = model))+geom_bar(stat="identity")+geom_errorbar( aes(ymin = ci.low, ymax = ci.high, width = 0.25), size = 1.5, position = position_dodge(width=0.5))+scale_fill_manual(values = c('#1b9e77',
                                                                                                                                                                                                                                                       '#d95f02',
                                                                                                                                                                                                                                                       '#7570b3'))+theme_bw(base_size = 35)+theme(legend.position = "none", axis.title.x = element_blank(), panel.grid = element_blank())+ylab("temperature sensitvity change \n between Modern and Past")

png(height = 8, width = 10, units = "in", res = 300, "outputs/gwbi_model/Lag4_cohort_re_clim/Climate_match/pct_change_tmax_senstivity_updated.png")
pct.temp.change
dev.off()
