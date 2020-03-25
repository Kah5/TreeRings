# Make plots of average percent change in grwoth from regional average growth due to tempearut:
# 1. get range of modern Tmax:
# 2. Find mean growth response for each site at mean Tmax overall 26.34, with all else constant
# 3. generate PP response to range of temperatures 21.94 to 31.94
# 4. Calculate % change between mean growth and 

mod.mean.clim <- test.dry.pair %>% filter(ageclass %in% "Modern" & year > 1950) %>% 
  #group_by(site) %>% 
  dplyr::summarize(mod.mean.Tmax = mean(JUNTmax, na.rm = TRUE), one = mean(JUNTmax, na.rm = TRUE) + 1,
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
                                                                                                                        MAPminus100 = mean(MAP.prism, na.rm = TRUE)-100,
                                                                                                                        MAPminus50 = mean(MAP.prism, na.rm = TRUE)-50,
                                                                                                                        MAPplus50 = mean(MAP.prism, na.rm = TRUE)+50,
                   
                                                                                                                         MAP750 = 750,
                                                                                                                         MAP515 = 515,
                                                                                                                         MAP850 = 850,
                                                                                                                         MAP950 = 950,
                                                                                                                        MAPplus100 = mean(MAP.prism, na.rm = TRUE)+100
)




# get summaries for MAP and Temperature scenarios & join
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
degree.scenario <- merge(degree.scenario, site.num.df, by = "site")

# -------------use estimates of betas to generate estimates for predicted growth:
meanMAP.sim <- degree.scenario %>% filter( MAP.Scenario == "mod.mean.MAP")
meanMAP.sim <- degree.scenario

int.mcmc <- as.mcmc(samp.structure.cohort.re[[1]])
int.mcmc.mat <- as.matrix(int.mcmc)
int.mcmc.dat <- data.frame(int.mcmc.mat)

int.1 <- matrix(rep(NA, nrow(int.mcmc.dat)*length(meanMAP.sim$T.scaled)), nrow = nrow(int.mcmc.dat))

# need to clean this up but this is the basic idea:


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
meanMAP.sim$idval <- 1:length(meanMAP.sim$Tmax)
# rows are the mcmc values
colnames(int.1) <- 1:length(meanMAP.sim$Tmax)
test.m <- melt(int.1)
colnames(test.m) <- c("MCMC", "idval", "Ypred")
full.pred <- left_join(test.m, meanMAP.sim, by = "idval")
full.pred$RWI <- exp(full.pred$Ypred)



#  create dfs with RWI values for each map and temp scernario:

past.Temp <- full.pred %>% dplyr::select(site, MAP.Scenario, struct.cohort.code, MCMC, RWI, Temp.Scenario, Tmax, MAP) %>%
  filter(Temp.Scenario %in% "mod.mean.Tmax") %>% dplyr::rename(zero = RWI) %>% dplyr::select( -Temp.Scenario) %>% arrange(site, MAP.Scenario, struct.cohort.code, MCMC,zero)

one <- full.pred %>% dplyr::select(site, MAP.Scenario, struct.cohort.code, MCMC,RWI, Temp.Scenario, Tmax, MAP) %>% filter(Temp.Scenario %in% "one" ) %>% dplyr::rename(one = RWI) %>% dplyr::select( -Temp.Scenario)%>% arrange(site, MAP.Scenario, struct.cohort.code, MCMC,one)

two <- full.pred %>%  dplyr::select(site, MAP.Scenario, struct.cohort.code,MCMC, RWI, Temp.Scenario, Tmax, MAP) %>%filter(Temp.Scenario %in% "two" ) %>% dplyr::rename(two = RWI) %>% dplyr::select( -Temp.Scenario) %>% arrange(site, MAP.Scenario, struct.cohort.code,MCMC, two)

three <- full.pred %>% dplyr::select(site, MAP.Scenario, struct.cohort.code, MCMC,RWI, Temp.Scenario, Tmax, MAP) %>% filter(Temp.Scenario %in% "three" ) %>% dplyr::rename(three = RWI) %>% dplyr::select( -Temp.Scenario) %>% arrange(site, MAP.Scenario, struct.cohort.code,MCMC, three)

four <- full.pred %>%  dplyr::select(site, MAP.Scenario, struct.cohort.code, MCMC,RWI, Temp.Scenario, Tmax, MAP) %>%filter(Temp.Scenario %in% "four" ) %>% dplyr::rename(four = RWI) %>% dplyr::select( -Temp.Scenario) %>% arrange(site, MAP.Scenario, struct.cohort.code, MCMC,four)

five <-  full.pred %>% dplyr::select(site, MAP.Scenario, struct.cohort.code, MCMC,RWI, Temp.Scenario, Tmax, MAP) %>% filter(Temp.Scenario %in% "five" ) %>% dplyr::rename(five = RWI) %>% dplyr::select( -Temp.Scenario) %>% arrange(site, MAP.Scenario, struct.cohort.code,MCMC, five)

minusone <-  full.pred %>%  dplyr::select(site, MAP.Scenario, struct.cohort.code, MCMC,RWI, Temp.Scenario, Tmax, MAP) %>%filter(Temp.Scenario %in% "minusone" ) %>% dplyr::rename(minusone = RWI) %>% dplyr::select( -Temp.Scenario) %>% arrange(site, MAP.Scenario, struct.cohort.code,MCMC, minusone)

minustwo <- full.pred %>% dplyr::select(site, MAP.Scenario, struct.cohort.code, MCMC,RWI, Temp.Scenario, Tmax, MAP) %>% filter(Temp.Scenario %in% "minustwo" ) %>% dplyr::rename(minustwo = RWI) %>% dplyr::select( -Temp.Scenario) %>% arrange(site, MAP.Scenario, struct.cohort.code,MCMC, minustwo)

minusthree<-  full.pred %>% dplyr::select(site, MAP.Scenario, struct.cohort.code, MCMC,RWI, Temp.Scenario, Tmax, MAP) %>% filter(Temp.Scenario %in% "minusthree" ) %>% dplyr::rename(minusthree = RWI) %>% dplyr::select( -Temp.Scenario) %>% arrange(site, MAP.Scenario, struct.cohort.code, MCMC, minusthree)

minusfour <- full.pred %>% dplyr::select(site, MAP.Scenario, struct.cohort.code, MCMC,RWI, Temp.Scenario, Tmax, MAP) %>% filter(Temp.Scenario %in% "minusfour" ) %>% dplyr::rename(minusfour = RWI) %>% dplyr::select( -Temp.Scenario) %>% arrange(site, MAP.Scenario, struct.cohort.code,MCMC, minusfour)

minusfive <-  full.pred %>% dplyr::select(site, MAP.Scenario, struct.cohort.code, MCMC,RWI, Temp.Scenario, Tmax, MAP) %>% filter(Temp.Scenario %in% "minusfive" ) %>% dplyr::rename(minusfive = RWI) %>% dplyr::select( -Temp.Scenario) %>% arrange(site, MAP.Scenario, struct.cohort.code, MCMC, minusfive)


newdf <- cbind( past.Temp, one[,c("one")], two[,c("two")], three[,c("three")], four[,c("four")], five[, c("five")], minusone[, c("minusone")], minustwo[, c("minustwo")], minusthree[, c("minusthree")], minusfour[, c("minusfour")], minusfive[, c("minusfive")])       
colnames(newdf) <- c("site", "MAP.Scenario", "struct.cohort.code","MCMC", "base","Tmax", "MAP", "one", "two", "three", "four", "five", "minusone", "minustwo", "minusthree", "minusfour", "minusfive")


# get means and CI for each temperature and MAP scenario:

pct.change.temp <- newdf %>% group_by(site, struct.cohort.code, MAP.Scenario, Tmax, MAP)  %>%
  dplyr::summarise(zero = mean(((base-base)/base)*100, na.rm = TRUE),
                   one = mean(((one-base)/base)*100, na.rm = TRUE),
                   two = mean(((two-base)/base)*100, na.rm = TRUE),
                   three = mean(((three-base)/base)*100, na.rm = TRUE),
                   four = mean(((four-base)/base)*100, na.rm = TRUE),
                   five = mean(((five-base)/base)*100, na.rm = TRUE),
                   minusone = mean(((minusone-base)/base)*100, na.rm = TRUE),
                   minustwo = mean(((minustwo-base)/base)*100, na.rm = TRUE),
                   minusthree = mean(((minusthree-base)/base)*100, na.rm = TRUE),
                   minusfour = mean(((minusfour-base)/base)*100, na.rm = TRUE),
                   minusfive = mean(((minusfive-base)/base)*100, na.rm = TRUE)
  ) %>% 
  gather(key = incT, pct_change, zero:minusfive)


pct.change.hi.ci <- newdf %>% group_by(site, struct.cohort.code, MAP.Scenario, Tmax, MAP)  %>%
  dplyr::summarise(zero = quantile(((base-base)/base)*100, 0.975 ,na.rm = TRUE),
                   one = quantile(((one-base)/base)*100, 0.975 ,na.rm = TRUE),
                   two = quantile(((two-base)/base)*100, 0.975 ,na.rm = TRUE),
                   three = quantile(((three-base)/base)*100, 0.975 ,na.rm = TRUE),
                   four = quantile(((four-base)/base)*100,0.975 , na.rm = TRUE),
                   five = quantile(((five-base)/base)*100,0.975 , na.rm = TRUE),
                   minusone = quantile((( minusone-base)/base)*100,0.975 , na.rm = TRUE),
                   minustwo = quantile((( minustwo-base)/base)*100,0.975 , na.rm = TRUE),
                   minusthree = quantile((( minusthree-base)/base)*100,0.975 , na.rm = TRUE),
                   minusfour = quantile((( minusfour-base)/base)*100,0.975 , na.rm = TRUE),
                   minusfive = quantile((( minusfive-base)/base)*100,0.975 , na.rm = TRUE)
  ) %>% 
  gather(key = incT, ci.hi, zero:minusfive)



pct.change.low.ci <- newdf %>% group_by(site, struct.cohort.code, MAP.Scenario, Tmax, MAP)  %>%
  dplyr::summarise(zero = quantile(((base-base)/base)*100, 0.025 ,na.rm = TRUE),
                   one = quantile(((one-base)/base)*100, 0.025 ,na.rm = TRUE),
                   two = quantile(((two-base)/base)*100, 0.025 ,na.rm = TRUE),
                   three = quantile(((three-base)/base)*100, 0.025 ,na.rm = TRUE),
                   four = quantile(((four-base)/base)*100, 0.025 , na.rm = TRUE),
                   five = quantile(((five-base)/base)*100, 0.025 , na.rm = TRUE),
                   minusone = quantile((( minusone-base)/base)*100,0.025 , na.rm = TRUE),
                   minustwo = quantile((( minustwo-base)/base)*100,0.025 , na.rm = TRUE),
                   minusthree = quantile((( minusthree-base)/base)*100,0.025 , na.rm = TRUE),
                   minusfour = quantile((( minusfour-base)/base)*100,0.025 , na.rm = TRUE),
                   minusfive = quantile((( minusfive-base)/base)*100,0.025 , na.rm = TRUE)
  ) %>% 
  gather(key = incT, ci.low, zero:minusfive)



cis <- merge(pct.change.hi.ci, pct.change.low.ci, by = c("site", "incT", "struct.cohort.code", "MAP.Scenario", "Tmax", "MAP") )
pct.change.temp <- merge(pct.change.temp, cis, by = c("site","incT", "struct.cohort.code", "MAP.Scenario", "Tmax", "MAP")) 

pct.change.temp <- merge(pct.change.temp , sites.unique, by = c("site", "struct.cohort.code"))


Tdf <- data.frame(incT = c("minusfive","minusfour", "minusthree", "minustwo", "minusone","zero", "one", "two", "three", "four", "five"), 
                  deltaT = -5:5)

pct.change.temp <- merge(pct.change.temp, Tdf, by = "incT")
pct.change.temp$struct.cohort.code <- as.character(pct.change.temp$struct.cohort.code)

locs.sites <- locs %>% filter(code %in% unique(pct.change.temp$site)) %>% arrange(pr30yr)
pct.change.temp$site <-factor(pct.change.temp$site, levels = c("BON", "GLL1", "GLL2", "GLL3", "ENG", "AVO", "UNC","MOU", "GLA"))

plus100 <- ggplot(pct.change.temp[pct.change.temp$MAP.Scenario %in% "MAPplus100",], aes(deltaT, pct_change, color = ageclass))+geom_point()+geom_line() + geom_ribbon(data = pct.change.temp[pct.change.temp$MAP.Scenario %in% "MAPplus100",], aes(x = deltaT, ymin = ci.low, ymax = ci.hi, fill = ageclass), alpha = 0.25, colour = NA)+facet_wrap(~site, ncol = 9)+geom_hline(yintercept = 0, linetype = "dashed")+ylab("+100mm ANNUAL PRECIPITATION \n % change in growth")+xlab("increase in Tmax (degC)")



# get regional responses:


pct.change.temp.reg <- newdf %>% group_by(struct.cohort.code, MAP.Scenario, Tmax, MAP)  %>%
  dplyr::summarise(zero = mean(((base-base)/base)*100, na.rm = TRUE),
                   one = mean(((one-base)/base)*100, na.rm = TRUE),
                   two = mean(((two-base)/base)*100, na.rm = TRUE),
                   three = mean(((three-base)/base)*100, na.rm = TRUE),
                   four = mean(((four-base)/base)*100, na.rm = TRUE),
                   five = mean(((five-base)/base)*100, na.rm = TRUE),
                   minusone = mean(((minusone-base)/base)*100, na.rm = TRUE),
                   minustwo = mean(((minustwo-base)/base)*100, na.rm = TRUE),
                   minusthree = mean(((minusthree-base)/base)*100, na.rm = TRUE),
                   minusfour = mean(((minusfour-base)/base)*100, na.rm = TRUE),
                   minusfive = mean(((minusfive-base)/base)*100, na.rm = TRUE)
  ) %>% 
  gather(key = incT, pct_change, zero:minusfive)


pct.change.hi.ci.reg <- newdf %>% group_by( struct.cohort.code, MAP.Scenario, Tmax, MAP)  %>%
  dplyr::summarise(zero = quantile(((base-base)/base)*100, 0.975 ,na.rm = TRUE),
                   one = quantile(((one-base)/base)*100, 0.975 ,na.rm = TRUE),
                   two = quantile(((two-base)/base)*100, 0.975 ,na.rm = TRUE),
                   three = quantile(((three-base)/base)*100, 0.975 ,na.rm = TRUE),
                   four = quantile(((four-base)/base)*100,0.975 , na.rm = TRUE),
                   five = quantile(((five-base)/base)*100,0.975 , na.rm = TRUE),
                   minusone = quantile((( minusone-base)/base)*100,0.975 , na.rm = TRUE),
                   minustwo = quantile((( minustwo-base)/base)*100,0.975 , na.rm = TRUE),
                   minusthree = quantile((( minusthree-base)/base)*100,0.975 , na.rm = TRUE),
                   minusfour = quantile((( minusfour-base)/base)*100,0.975 , na.rm = TRUE),
                   minusfive = quantile((( minusfive-base)/base)*100,0.975 , na.rm = TRUE)
  ) %>% 
  gather(key = incT, ci.hi, zero:minusfive)



pct.change.low.ci.reg <- newdf %>% group_by( struct.cohort.code, MAP.Scenario, Tmax, MAP)  %>%
  dplyr::summarise(zero = quantile(((base-base)/base)*100, 0.025 ,na.rm = TRUE),
                   one = quantile(((one-base)/base)*100, 0.025 ,na.rm = TRUE),
                   two = quantile(((two-base)/base)*100, 0.025 ,na.rm = TRUE),
                   three = quantile(((three-base)/base)*100, 0.025 ,na.rm = TRUE),
                   four = quantile(((four-base)/base)*100, 0.025 , na.rm = TRUE),
                   five = quantile(((five-base)/base)*100, 0.025 , na.rm = TRUE),
                   minusone = quantile((( minusone-base)/base)*100,0.025 , na.rm = TRUE),
                   minustwo = quantile((( minustwo-base)/base)*100,0.025 , na.rm = TRUE),
                   minusthree = quantile((( minusthree-base)/base)*100,0.025 , na.rm = TRUE),
                   minusfour = quantile((( minusfour-base)/base)*100,0.025 , na.rm = TRUE),
                   minusfive = quantile((( minusfive-base)/base)*100,0.025 , na.rm = TRUE)
  ) %>% 
  gather(key = incT, ci.low, zero:minusfive)


cis <- merge(pct.change.hi.ci.reg, pct.change.low.ci.reg, by = c("incT", "struct.cohort.code", "MAP.Scenario", "Tmax", "MAP") )
pct.change.temp.reg <- merge(pct.change.temp.reg, cis, by = c("incT", "struct.cohort.code", "MAP.Scenario", "Tmax", "MAP")) 

pct.change.temp.reg <- merge(pct.change.temp.reg , unique(sites.unique[,c("struct.cohort.code", "structure", "ageclass")]), by = c( "struct.cohort.code"))


Tdf <- data.frame(incT = c("minusfive","minusfour", "minusthree", "minustwo", "minusone","zero", "one", "two", "three", "four", "five"), 
                  deltaT = -5:5)
Tdf$Temperature <- Tdf$deltaT + 26.2

pct.change.temp.reg <- merge(pct.change.temp.reg, Tdf, by = "incT")
pct.change.temp.reg$struct.cohort.code <- as.character(pct.change.temp.reg$struct.cohort.code)


# plot out pct change in growth estimated:
ggplot(pct.change.temp.reg[pct.change.temp.reg$ageclass %in% "Modern",], aes(deltaT, pct_change, color = MAP.Scenario))+geom_point()+
  geom_line() + geom_ribbon(data = pct.change.temp.reg[pct.change.temp.reg$ageclass %in% "Modern",], aes(x = deltaT, ymin = ci.low, ymax = ci.hi, fill = MAP.Scenario), alpha = 0.25, colour = NA)+
  geom_hline(yintercept = 0, linetype = "dashed")+ylab("% change in growth")+xlab("change in mean Tmax (degC)")+facet_wrap(~structure, ncol = 1)


ggplot(pct.change.temp.reg[pct.change.temp.reg$ageclass %in% "Modern" & pct.change.temp.reg$MAP.Scenario %in% c("MAP950", "MAP515", "mod.mean.MAP"),], aes(Temperature, pct_change, color = MAP.Scenario))+
  geom_line() + geom_ribbon(data = pct.change.temp.reg[pct.change.temp.reg$ageclass %in% "Modern" & pct.change.temp.reg$MAP.Scenario %in% c("MAP950", "MAP515", "mod.mean.MAP"),], aes(x = Temperature, ymin = ci.low, ymax = ci.hi, fill = MAP.Scenario), alpha = 0.25, colour = NA)+
  geom_hline(yintercept = 0, linetype = "dashed")+ylab("% change in tree growth")+xlab("Tmax (degC)")+facet_wrap(~structure, ncol = 2)+scale_color_manual(values = c("MAP950"='#008837', 'MAP515' = "#7b3294", "mod.mean.MAP" = "grey"), name = "MAP scenario")+scale_fill_manual(values = c("MAP950"='#008837', 'MAP515' = "#7b3294", "mod.mean.MAP" = "grey"), name = "MAP scenario")
