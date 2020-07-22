library(lme4)
library(boot)
clean.full.spline.df <- readRDS("outputs/growth_model/ghcn.clean.full.spline.det.data_v3.rds")
sites2 <- c("UNC", "MOU", "GLL2", "GLL1", "GLA", "ENG", "BON", "AVO")
clean.full.spline <- clean.full.spline.df[clean.full.spline.df$site %in% sites2,]
# time only class
clean.full.spline$year.class <- ifelse(clean.full.spline$year >= 1950, "post-1950", "pre-1950")


ggplot(clean.full.spline, aes(MAP.scaled, RWI, color = ageclass))+geom_point(size = 0.1)+
  stat_smooth(method = "lm")+facet_wrap(~site, scales = "free_y")+theme_bw()

png(height = 5, width = 6, units = "in", res = 300, "outputs/spline_detrended_pre_post_precipiation.png")
ggplot(clean.full.spline, aes(MAP.scaled, RWI, color = year.class))+geom_point(size = 0.1)+
  stat_smooth(method = "lm")+facet_wrap(~site, scales = "free_y")+theme_bw()+scale_color_manual(values = c("red", "blue"))+ylab("Detrended Ring Width Index")+xlab("Scaled Total Annual Precipitation")
dev.off()


png(height = 5, width = 6, units = "in", res = 300, "outputs/spline_detrended_ageclass_precipiation.png")
ggplot(clean.full.spline, aes(MAP.scaled, RWI, color = ageclass))+geom_point(size = 0.1)+
  stat_smooth(method = "lm")+facet_wrap(~site, scales = "free_y")+theme_bw()+scale_color_manual(values = c("red", "blue"))+ylab("Detrended Ring Width Index")+xlab("Scaled Total Annual Precipitation")
dev.off()

mod.mod<- clean.full.spline %>% filter(ageclass %in% "Modern", year.class %in% "post-1950")
past.past<- clean.full.spline %>% filter(ageclass %in% "Past", year.class %in% "pre-1950")
all.subset <- rbind(mod.mod, past.past)

png(height = 5, width = 6, units = "in", res = 300, "outputs/spline_detrended_pre.post.ageclass_precipiation.png")
ggplot(all.subset, aes(MAP.scaled, RWI, color = ageclass))+geom_point(size = 0.1)+
  stat_smooth(method = "lm")+facet_wrap(~site, scales = "free_y")+theme_bw()+scale_color_manual(values = c("red", "blue"))+ylab("Detrended Ring Width Index")+xlab("Scaled Total Annual Precipitation")
dev.off()

# histogram of tree ages:
ggplot(clean.full.spline, aes(Age, fill = ageclass), alpha = 0.5)+geom_histogram(position = "dodge")
clean.full.spline$est.date<- clean.full.spline$year - clean.full.spline$Age

ggplot(all.subset, aes(est.date, fill = ageclass), alpha = 0.5)+geom_histogram(position = "dodge")

clean.full.spline %>% group_by(ageclass) %>% summarise(min.age = min(est.date), 
                                                      max.age = max(est.date), 
                                                      mean.age = mean(est.date))

ggplot(clean.full.spline, aes(MAP.scaled, RWI, color = ageclass))+
  stat_smooth(method = "lm")+facet_wrap(~site)

ggplot(clean.full.spline, aes(MAP.scaled, RWI, color = ageclass))+
  stat_smooth(method = "lm")+facet_wrap(~site)



ggplot(clean.full.spline, aes(year, RWI, color = ageclass))+geom_point()+facet_wrap(~site)
  #stat_smooth(method = "lm")+facet_wrap(~site)




sites <- unique(clean.full.spline$site)
lm.stats <- list()
for(i in 1:length(unique(sites))){
  sites[i]
  dat <- clean.full.spline[clean.full.spline$site %in% sites[i],]
  
lm.stats[[i]]  <- lm(RWI~MAP.scaled, data = dat)
}



library(lme4)
lm.time.stats <- list()
for(i in 1:length(unique(sites))){
  sites[i]
  dat <- clean.full.spline[clean.full.spline$site %in% sites[i],]
  lm.time.stats[[i]]  <-  summary(lmer(RWI ~  MAP.scaled + (MAP.scaled|year.class), data=dat))
}




# by cohort class
clean.full.spline$year.class <- ifelse(clean.full.spline$year >= 1950, 1, 0)

library(lme4)
lm.ageclass.stats <- list()
for(i in 1:length(unique(sites))){
  sites[i]
  dat <- clean.full.spline[clean.full.spline$site %in% sites[i],]
  lm.ageclass.stats[[i]]  <-  summary(lmer(RWI ~  MAP.scaled + (MAP.scaled|ageclass), data=dat))
}


# for ageclass only
lm.ageclass <- list()
for(i in 1:length(unique(sites))){
  sites[i]
  dat <- clean.full.spline[clean.full.spline$site %in% sites[i],]
  lm.ageclass.stats[[i]]  <-  summary(lmer(RWI ~  MAP.scaled + (MAP.scaled|ageclass), data=dat))
}

# for ageclass within their respective years
lm.ageclass.time.stats <- list()
# time only class
clean.full.spline$year.class <- ifelse(clean.full.spline$year >= 1950, 1, 0)
for(i in 1:length(unique(sites))){
  sites[i]
  dat <- clean.full.spline[clean.full.spline$site %in% sites[i],]
  dat.pre <- dat%>% filter(ageclass %in% "Past" & year.class %in% 0)
  dat.post <- dat %>% filter(ageclass %in% "Modern" & year.class %in% 1)
  
  dat.new  <- rbind(dat.pre, dat.post)
  if(length(na.omit(unique(dat.new$ageclass))) == 2){
    
    summary(lm(RWI ~  MAP.scaled,  data=dat.pre))
    summary(lm(RWI ~  MAP.scaled,  data=dat.post))
    lm.ageclass.time.stats[[i]]  <-  summary(lmer(RWI ~  MAP.scaled + (MAP.scaled|ageclass), data=dat.new))
}else{
    lm.ageclass.time.stats[[i]]  <- summary(lm(RWI ~  MAP.scaled , data=dat.new))
  }
}
########################################
# correlations
###################################3
sites <- unique(clean.full.spline$site)
cor.stats <- list()
for(i in 1:length(unique(sites))){
  sites[i]
  dat <- clean.full.spline[clean.full.spline$site %in% sites[i],]
  
 
  cor.stats[[i]]  <- cor(dat$RWI, dat$MAP.scaled)
}


# time only class
clean.full.spline$year.class <- ifelse(clean.full.spline$year >= 1950, 1, 0)

library(lme4)
cor.time.stats <- list()
for(i in 1:length(unique(sites))){
  sites[i]
  dat <- clean.full.spline[clean.full.spline$site %in% sites[i],]
  dat.pre <- dat[clean.full.spline$year.class == 0, ]
  dat.post <- dat[clean.full.spline$year.class==1, ]
  pre.cor <- cor.test(dat.pre$RWI, dat.pre$MAP.scaled, na.rm =TRUE)
  post.cor <- cor.test(dat.post$RWI, dat.post$MAP.scaled, na.rm =TRUE)
  cor.time.stats[[i]]  <-  list(pre.cor, post.cor)

}



# note that ageclass only includes Past trees in the modern time
# by cohort class
clean.full.spline$year.class <- ifelse(clean.full.spline$year >= 1950, 1, 0)

library(lme4)
cor.ageclass.stats <- list()
for(i in 1:length(unique(sites))){
  sites[i]
  dat <- clean.full.spline[clean.full.spline$site %in% sites[i],]
  dat.pre <- dat[clean.full.spline$ageclass == "Past", ]
  dat.post <- dat[clean.full.spline$ageclass=="Modern", ]
  pre.cor <- cor.test(dat.pre$RWI, dat.pre$MAP.scaled, na.rm =TRUE)
  post.cor <- cor.test(dat.post$RWI, dat.post$MAP.scaled, na.rm =TRUE)
  cor.ageclass.stats[[i]]  <-  list(pre.cor, post.cor)
  }


library(lme4)
cor.ageclass.time.stats <- list()
for(i in 1:length(unique(sites))){
  sites[i]
  dat <- clean.full.spline[clean.full.spline$site %in% sites[i],]
  dat.pre <- dat[clean.full.spline$ageclass %in% "Past" & clean.full.spline$year.class == 0, ]
  dat.post <- dat[clean.full.spline$ageclass %in%"Modern" & clean.full.spline$year.class == 1, ]
  pre.cor <- cor.test(dat.pre$RWI, dat.pre$MAP.scaled, na.rm =TRUE)
  post.cor <- cor.test(dat.post$RWI, dat.post$MAP.scaled, na.rm =TRUE)
  cor.ageclass.time.stats[[i]]  <-  list(pre.cor, post.cor)
}


library(lme4)
lm.ageclass <- list()
for(i in 1:length(unique(sites))){
  sites[i]
  dat <- clean.full.spline[clean.full.spline$site %in% sites[i],]
  lm.ageclass.stats[[i]]  <-  summary(lmer(RWI ~  MAP.scaled + (MAP.scaled|ageclass), data=dat))
}


boot.results <- boot(d = dat, colno=mo , boot.cor, R= 500)

cis <- boot.ci(boot.out = boot.results, type = c("norm", "basic", "perc", "bca"))
ci.mo <- cis$normal[2:3]
t <- boot.results$t0
tcors[mo,1] <- t
tcors[mo,2] <- mo
tcors[mo,3]<- ci.mo[1]
tcors[mo,4]<- ci.mo[2]
tcors <-  data.frame(tcors)
colnames(tcors) <- c("cor", "month","ci.min", "ci.max")
