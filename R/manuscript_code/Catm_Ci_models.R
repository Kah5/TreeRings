# This script plots iWUE assuming different theoretical Ci Ca models & compares to my data

# read in the data that has Catm & data iWUE:
d13 <- read.csv("outputs/stable_isotopes/merged_d13_growth.csv")


iwue_summary_by_year <- d13 %>% group_by(year, ppm)%>% summarise(iWUE = mean(iWUE))

#------------ constant Ci -------------------
Catm <- unique(d13$ppm)

# to reproduce initial value of Ci
iWUEinit <-  iwue_summary_by_year[iwue_summary_by_year$ppm %in% min(d13$ppm),]$iWUE
Catminit <- min(d13$ppm)
Ciinitil = -1*(Catminit*((iWUEinit/(Catminit*0.625)) - 1))

# use initial value of Ci as the constant value of Ci
Ci <- Ciinitil
iWUE_conCi <- Catm *(1 - (Ci/Catm))*0.625
constant_ci<- data.frame(Catm = Catm, 
                         Ci = Ci,
           iWUE = iWUE_conCi, 
           group = "constant_ci")

ggplot(constant_ci, aes(Catm, iWUE))+geom_point()


#------------ constant Ci/Ca -------------------
# for example, it might be that Ci/Catm = 0.8
Ci <- 0.25*Catm

# or we can paramertize the ratio using the inital ratio between ci + ca
# to reproduce initial value of Ci
iWUEinit <-  iwue_summary_by_year[iwue_summary_by_year$ppm %in% min(d13$ppm),]$iWUE
Catminit <- min(d13$ppm)
Ci.Ca.ratio = -1*(((iWUEinit/(Catminit*0.625)) - 1))

Ci <- Ci.Ca.ratio*Catm

iWUE_con_ratio <- Catm *(1 - (Ci/Catm))*0.625
constant_ratio<- data.frame(Catm = Catm, 
                            Ci = Ci,
                            iWUE = iWUE_con_ratio, 
                            group = "constant_ratio")

ggplot(constant_ratio, aes(Catm, iWUE))+geom_point()


#------------ constant Ci-Ca -------------------
# for example, it might be that Ca-Ci = 100, and this is constant across time

# to reproduce initial value of Ci
iWUEinit <-  iwue_summary_by_year[iwue_summary_by_year$ppm %in% min(d13$ppm),]$iWUE
Catminit <- min(d13$ppm)
Ciinitil = -1*(Catminit*((iWUEinit/(Catminit*0.625)) - 1))

abs(Ciinitil - Catminit)
Ci <-  Catm - abs(Ciinitil - Catminit) # get constant Ci from Catm 

iWUE_con_diff <- Catm *(1 - (Ci/Catm))*0.625
constant_diff <- data.frame(Catm = Catm, 
                            Ci = Ci,
                            iWUE = iWUE_con_diff, 
                            group = "constant_diff")

ggplot(constant_diff, aes(Catm, iWUE))+geom_point()

#----- Plot all strategies together--------
strategies <- rbind(constant_diff, constant_ratio, constant_ci)

ggplot(strategies, aes(Catm, iWUE, color = group))+geom_line()+theme_bw(base_size = 14)+
  geom_point(data = d13, aes(ppm, iWUE, color = site))



# ---------------------------model iWUE change strategies by site-----------
iwue_summary_by_year <- d13 %>% group_by(year, ppm, site)%>% summarise(iWUE = mean(iWUE))

ggplot(iwue_summary_by_year, aes(ppm, iWUE, color = site))+geom_point()+geom_line()


#------------ constant Ci by site -------------------
Catm <- unique(d13$ppm)

# to reproduce initial value of Ci

ppminit <-  iwue_summary_by_year %>% group_by(site) %>% summarize(ppm = min(ppm))
iWUEinit <- merge(data.frame(iwue_summary_by_year), ppminit, by = c('site', "ppm"))
Catminit <- min(iWUEinit$ppm)
Ci.func <- function(df) {-1*(df$ppm*((df$iWUE/(df$ppm*0.625)) - 1))}

iWUEinit$Ciinit <- Ci.func( iWUEinit )


# use initial value of Ci as the constant value of Ci
Ci.vals <- iWUEinit$Ciinit
iWUE_conCi <- matrix(nrow = length(Catm), ncol = length(Ci.vals))

for(i in 1:length(Ci.vals)){
iWUE_conCi[,i] <- Catm *(1 - (Ci.vals[i]/Catm))*0.625
}
colnames(iWUE_conCi) <- iWUEinit$site
constant_ci<- data.frame(Catm = Catm, 
                         #Ci = Ci,
                         group = "constant_ci", 
                         iWUE_conCi)

constant_ci.m <- melt(constant_ci, id.vars = c("Catm", "group"))
ggplot(constant_ci.m, aes(Catm, value, color = variable))+geom_point()



#------------ constant Ci/Ca by site -------------------
Catm <- unique(d13$ppm)

# to reproduce initial value of Ci

ppminit <-  iwue_summary_by_year %>% group_by(site) %>% summarize(ppm = min(ppm))
iWUEinit <- merge(data.frame(iwue_summary_by_year), ppminit, by = c('site', "ppm"))
Catminit <- min(iWUEinit$ppm)
#Ci.ca.ratio.func <- function(df) {-1*(df$ppm*((df$iWUE/(df$ppm*0.625)) - 1))}


iWUEinit$Ciinit <- Ci.func( iWUEinit )


# use initial value of Ci as the constant value of Ci
Ci.vals <- iWUEinit$Ciinit
iWUE_conrat <- matrix(nrow = length(Catm), ncol = length(Ci.vals))
Ci <- matrix(nrow = length(Catm), ncol = length(Ci.vals))

# calculate Ci.Ca ratio from initial Cppm and iWUE
Ci.Ca.ratio = -1*(((iWUEinit$iWUE/(iWUEinit$ppm*0.625)) - 1))



for(i in 1:length(Ci.Ca.ratio)){
  
  Ci[,i] <- (Ci.Ca.ratio[i]*Catm) # estimate Ci @ each time from constant Ci/Ca assumption
  
  iWUE_conrat[,i] <- Catm *(1 - Ci.Ca.ratio[i])*0.625
}
colnames(iWUE_conrat) <- iWUEinit$site
constant_ratio<- data.frame(Catm = Catm, 
                         #Ci = Ci,
                         group = "constant_ratio", 
                         iWUE_conrat)

constant_ratio.m <- melt(constant_ratio, id.vars = c("Catm", "group"))
ggplot(constant_ratio.m, aes(Catm, value, color = variable))+geom_point()



#------------ constant Ci - Ca by site -------------------

Catm <- unique(d13$ppm)

# to reproduce initial value of Ci

ppminit <-  iwue_summary_by_year %>% group_by(site) %>% summarize(ppm = min(ppm))
iWUEinit <- merge(data.frame(iwue_summary_by_year), ppminit, by = c('site', "ppm"))
Catminit <- min(iWUEinit$ppm)
#Ci.ca.ratio.func <- function(df) {-1*(df$ppm*((df$iWUE/(df$ppm*0.625)) - 1))}


iWUEinit$Ciinit <- Ci.func( iWUEinit )


Ci.vals <- iWUEinit$Ciinit
iWUE_condiff <- matrix(nrow = length(Catm), ncol = length(Ci.vals))
Ci <- matrix(nrow = length(Catm), ncol = length(Ci.vals))


for(i in 1:length(Ci.vals)){
  Ci[,i] <- Catm - (abs(iWUEinit[i,]$Ciinit - iWUEinit[i,]$ppm)) # estimate Ci @ each time from constant Ci-Ca assumption
  
  iWUE_condiff[,i] <- Catm *(1 - Ci[,i]/Catm)*0.625
}

colnames(iWUE_condiff) <- iWUEinit$site
constant_diff <- data.frame(Catm = Catm,
                            #Ci = Ci,
                            group = "constant_diff", 
                            iWUE_condiff)


constant_diff.m <- melt(constant_diff, id.vars = c("Catm", "group"))
ggplot(constant_diff.m, aes(Catm, value, color = variable))+geom_point()

#------------------ Join all strategies and plot with data -----------------

strategies <- rbind(constant_diff.m, constant_ratio.m, constant_ci.m)
colnames(strategies) <- c("ppm", "strategy", "site", "iWUE")

strat.yr <- merge(strategies, d13[,c("ppm", "year")], by = "ppm")
# get mean values for each year of iWUE:
iWUE.summary <- d13 %>% group_by(site, year) %>% summarize(ppm = mean(ppm, na.rm = TRUE), 
                                           iWUE = mean(iWUE, na.rm = TRUE), 
                                           iWUEsd.low = mean(iWUE, na.rm = TRUE) - sd(iWUE, na.rm = TRUE), 
                                           iWUEsd.high = mean(iWUE, na.rm = TRUE) - sd(iWUE, na.rm = TRUE))

# plot ratios and mean iWUE by ppm and site
png(height = 5, width = 8, units = "in", res = 250, "outputs/stable_isotopes/stomatal_strategies_by_ppm.png")
ggplot(strat.yr, aes(ppm, iWUE, color = strategy))+geom_line()+theme_bw(base_size = 14)+ geom_point(data = iWUE.summary, aes(ppm, iWUE), color = "black", size = 0.5)+
  facet_wrap(~ site) 
dev.off()

# plot ratios and mean iWUE by year and site
png(height = 5, width = 8, units = "in", res = 250, "outputs/stable_isotopes/stomatal_strategies_by_year.png")
ggplot(strat.yr, aes(year, iWUE, color = strategy))+geom_line()+theme_bw(base_size = 14)+ geom_point(data = iWUE.summary, aes(year, iWUE), color = "black", size = 0.5)+
  facet_wrap(~ site) 
dev.off()
