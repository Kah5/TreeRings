library(lme4)
library(dplR)
library(ggplot2)
library(treeclim)
library(sp)  # classes for spatial data
library(raster)  # grids, rasters
library(rasterVis)  # raster visualisation
library(maptools)
library(rgeos)
library(ggplot2)
library(rgdal)
library(ggrepel)
library(reshape2)
library(RColorBrewer)
library(grid)
library(gridExtra)

#read in growth crns and make individual barplot correlations for each site

cor.barplot <- function(site.code){
tavg <- read.csv(paste0(site.code, '-WWtavgcor.csv'))
tmin <- read.csv(paste0(site.code, '-WWtmincor.csv'))
tmax <- read.csv(paste0(site.code, '-WWtmaxcor.csv'))
precip <- read.csv(paste0(site.code, '-WWPrecipcor.csv'))
PDSI <- read.csv(paste0(site.code, '-WWPDSIcor.csv'))

months <- c("pJan", "pFeb", "pMar", "pApr", "pMay", "pJun", "pJul",
            "pAug", "pSep", "pOct", "pNov", "pDec",
            "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
            "Aug", "Sep", "Oct", "Nov", "Dec")

tavg$months <- months
colnames(tavg) <- c('mono', 'tavg', 'months')
full <- tavg
full$tmin <- tmin$V1
full$tmax <- tmax$V1
full$precip <- precip$V1
full$PDSI <- PDSI$V1
cors.melt <- melt(full, id.vars = c('months', 'mono'))
cors.melt$months <- factor(cors.melt$months, levels=full$months)
cors.melt[order(cors.melt$months),]
output<- ggplot(data = cors.melt, aes(months, value, fill = variable))+
  geom_bar(stat = 'identity', position = position_dodge()) + 
  facet_grid(variable~.)+theme_bw()+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1)) + ggtitle(paste0(site.code, " Correlations"))
output
}

site.cd <- c("COR", "STC", "BON", "HIC", "TOW", "GLA", "ENG", "UNC", "MOU", "GL1", "GL2", "GL3", "GL4", "PVC")
#run for loop and save plots to outputs/barplots

for(i in 1:length(site.cd)){
  mypath <- file.path("C:/Users/JMac/Documents/Kelly/TreeRings/outputs/barplots",paste("barplots_", site.cd[i], ".png", sep = ""))
  cor.barplot(site.cd[i]) 
  ggsave(filename=mypath)
}

#also can run them by themselves
cor.barplot("COR")
cor.barplot('STC')
cor.barplot('BON')
cor.barplot('HIC')
cor.barplot('TOW')
cor.barplot('GLA')
cor.barplot('ENG')
cor.barplot('UNC')
cor.barplot('MOU')
cor.barplot('GL1')
cor.barplot('GL2')
cor.barplot('GL3')
cor.barplot('GL4')
cor.barplot('PVC')
#now make a barplot for each climate factors with the sites on it using the sites.barplot funciton

#This is a set up tocolor code sites by their total mean annual precip
sites <- c("COR", "STC", "BON", "HIC", "TOW", "GLA", "ENG", "UNC", "MOU", "GL1", "GL2", "GL3", "GL4", "PVC")

precip <- matrix(NA ,nrow = length(sites), ncol = 2)
for (i in 1:length(sites)){
  precip[i,1] <- sites[i]
  a <- read.csv(paste0(sites[i], "-annualP.csv"))
  precip[i,2] <- mean(a$PCP)
}
precip <- precip[order(as.numeric(precip[,2])),]
site.order <- rev(precip[,1])

tmax <- matrix(NA ,nrow = length(sites), ncol = 2)
for (i in 1:length(sites)){
  tmax[i,1] <- sites[i]
  a <- read.csv(paste0(sites[i], "-annualtmax.csv"))
  tmax[i,2] <- mean(a$TMAX)
}
tmax <- tmax[order(as.numeric(tmax[,2])),]
site.order <- rev(tmax[,1])

tmin <- matrix(NA ,nrow = length(sites), ncol = 2)
for (i in 1:length(sites)){
  tmin[i,1] <- sites[i]
  a <- read.csv(paste0(sites[i], "-annualtmin.csv"))
  tmin[i,2] <- mean(a$TMIN)
}
tmin <- tmin[order(as.numeric(tmin[,2])),]
site.order <- rev(tmin[,1])

PDSI <- matrix(NA ,nrow = length(sites), ncol = 2)
for (i in 1:length(sites)){
  PDSI[i,1] <- sites[i]
  a <- read.csv(paste0(sites[i], "-annualPDSI.csv"))
  PDSI[i,2] <- mean(a$PDSI)
}
PDSI <- PDSI[order(as.numeric(PDSI[,2])),]
site.order <- rev(PDSI[,1])

#this function plots all the sites on the same barplot and color codes from driest to wettest
sites.barplot <- function(clim) {
COR <- read.csv(paste0('COR-WW', clim, 'cor.csv'))
HIC <- read.csv(paste0('HIC-WW', clim, 'cor.csv'))
GLA <- read.csv(paste0('GLA-WW', clim, 'cor.csv'))
STC <- read.csv(paste0('STC-WW', clim, 'cor.csv'))
TOW <- read.csv(paste0('TOW-WW', clim, 'cor.csv'))
ENG <- read.csv(paste0('ENG-WW', clim, 'cor.csv'))
UNC <- read.csv(paste0('UNC-WW', clim, 'cor.csv'))
BON <- read.csv(paste0('BON-WW', clim, 'cor.csv'))
MOU <- read.csv(paste0('MOU-WW', clim, 'cor.csv'))
GL1 <- read.csv(paste0('GL1-WW', clim, 'cor.csv'))
GL2 <- read.csv(paste0('GL2-WW', clim, 'cor.csv'))
GL3 <- read.csv(paste0('GL3-WW', clim, 'cor.csv'))
GL4 <- read.csv(paste0('GL4-WW', clim, 'cor.csv'))
PVC <- read.csv(paste0('PVC-WW', clim, 'cor.csv'))

months <- c("pJan", "pFeb", "pMar", "pApr", "pMay", "pJun", "pJul",
            "pAug", "pSep", "pOct", "pNov", "pDec",
            "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
            "Aug", "Sep", "Oct", "Nov", "Dec")

COR$months <- months
colnames(COR) <- c('mono', 'COR', 'months')
full <- COR
full$HIC <- HIC$V1
full$GLA <- GLA$V1
full$STC <- STC$V1
full$TOW <- TOW$V1
full$ENG <- ENG$V1
full$UNC <- UNC$V1
full$BON <- BON$V1
full$MOU <- MOU$V1
full$GL1 <- GL1$V1
full$GL2 <- GL2$V1
full$GL3 <- GL3$V1
full$GL4 <- GL4$V1
full$PVC <- PVC$V1

half <- full[13:24,]

cors.melt <- melt(half, id.vars = c('months', 'mono'))
cors.melt$months <- factor(cors.melt$months, levels=full$months)
cors.melt$variable <- factor(cors.melt$variable, levels = site.order)
sitesnames <- data.frame(variable = c("COR", "HIC", "GLA", "STC", "UNC", "ENG", "MOU",
                                 "TOW", "BON", "GL1", "GL2", "GL3","GL4", "PVC"), 
                    Sites = c("Coral Woods, IL", "Hickory Grove, IL",
                              "Glacial Park, IL", "St.Croix Savanna SNA, MN", 
                              "Uncas Dunes SNA, MN","Englund Ecotone SNA, MN", 
                              "Mound Prairie SNA, MN", "Townsend Woods SNA, MN", 
                              "Bonanza Prairie SNA, MN","Glacial Lakes 1, MN",
                              "Glacial Lakes 2, MN", "Glacial Lakes 3, MN", "Glacial Lakes 4, MN",
                              "Pleasant Valley Conservancy, IL"))
cors.melt[order(cors.melt$months),]
cors.melt <- merge(cors.melt, sitesnames, by = "variable")
output<- ggplot(data = cors.melt, aes(months, value, fill = Sites))+
  geom_bar(stat = 'identity', position = position_dodge(width = 0.9)) + 
  #facet_grid(variable~.)+
  scale_size(range=c(5,20), guide="none")+
  scale_fill_manual("Sites",values = c('#a6cee3','#1f78b4','#b2df8a',
    '#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00',
    '#cab2d6','#6a3d9a','#ffff99','#00441b','#800026','#49006a', 'black'), 
                    limits=c("Bonanza Prairie SNA, MN","Glacial Lakes 1, MN",
                             "Glacial Lakes 2, MN", "Glacial Lakes 3, MN", "Glacial Lakes 4, MN","Townsend Woods SNA, MN", "Mound Prairie SNA, MN",
                             "Englund Ecotone SNA, MN", "Uncas Dunes SNA, MN", "St.Croix Savanna SNA, MN",
                             "Glacial Park, IL", "Coral Woods, IL", "Hickory Grove, IL","Pleasant Valley Conservancy, IL"))+
  theme_bw()+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1)) + ggtitle(paste0(clim, " site correlations"))
output
}
clim.cd <- c('tavg', 'tmax', 'tmin', 'Precip', 'PDSI')

#automate savaing these barplots to individual png files
for(i in 1:length(clim.cd)){
  mypath <- file.path("C:/Users/JMac/Documents/Kelly/TreeRings/outputs/barplots",paste("full_site_barplots_", clim.cd[i], ".png", sep = ""))
  sites.barplot(clim.cd[i])
  ggsave(filename=mypath)
}

grid_arrange_shared_legend <- function(..., nrow = 1, ncol = length(list(...)), position = c("bottom", "right")) {
  
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position = "none"))
  gl <- c(gl, nrow = nrow, ncol = ncol)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  grid.newpage()
  grid.draw(combined)
  
}

e <- sites.barplot('tavg')+ggtitle("E). Average Temperature")+ylab("correlation")
c <- sites.barplot('tmax')+ ggtitle("C). Maximum Temperature")+ylab('correlation')
d <- sites.barplot('tmin')+ ggtitle("D). Minimum Temperature")+ylab('correlation')
b <- sites.barplot('Precip')+ ggtitle("B). Precipitation")+ylab('correlation')
a <- sites.barplot('PDSI')+ ggtitle("A). Palmer Drought Severity Index")+ylab('correlation')

#plot all barplots in png for interim report
png(width = 8, height = 10, units = 'in', res = 300, 'outputs/barplots/barplots_all_sites_fig2.png')
grid_arrange_shared_legend(a,b,c,d,e,nrow = 5, ncol = 1 )
dev.off()

png(width = 10, height = 8, units = 'in', res = 300, 'outputs/barplots/barplots_all_sites_3panel.png')
grid_arrange_shared_legend(a,b,c, nrow = 3, ncol = 1 )
dev.off()


#rank the correlations based on highest to lowest for each site
highest.cor <- function(site, i){
  precip <- read.csv(paste0(site,'-WW', 'Precip', 'cor.csv'))
  tavg <- read.csv(paste0(site,'-WW', 'tavg', 'cor.csv'))
  tmin <- read.csv(paste0(site,'-WW', 'tmin', 'cor.csv'))
  tmax <- read.csv(paste0(site,'-WW', 'tmax', 'cor.csv'))
  PDSI <- read.csv(paste0(site,'-WW', 'PDSI', 'cor.csv'))
  
  clim.cors <- cbind(precip, tavg[,2], tmin[,2], tmax[,2], PDSI[,2])
  colnames(clim.cors) <- c("mono", "Precip", "tavg", "tmin", "tmax", "PDSI")
  clim.cors$months <- c("pJan", "pFeb", "pMar", "pApr", "pMay", "pJun", "pJul",
                        "pAug", "pSep", "pOct", "pNov", "pDec",
                        "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                        "Aug", "Sep", "Oct", "Nov", "Dec")
  
  melted.cors <- melt(clim.cors, id.vars = c('mono', "months"))
  reorded.cors <- melted.cors[rev(order(abs(melted.cors[, "value"]))),]
  reorded.cors[i,]
}
Highest<- rbind(
highest.cor('HIC', 1),
highest.cor('BON', 1),
highest.cor('COR', 1),
highest.cor('GLA', 1),
highest.cor('STC', 1),
highest.cor('ENG', 1),
highest.cor('UNC', 1),
highest.cor('TOW', 1),
highest.cor('MOU', 1),
highest.cor('GL1', 1),
highest.cor('GL2', 1),
highest.cor('GL3', 1),
highest.cor('GL4', 1),
highest.cor('PVC', 1)
)

Highest$site <- c('HIC', "BON", "COR", "GLA", "STC", "ENG", "UNC", "TOW", "MOU", "GL1", "GL2", "GL3", "GL4", "PVC")

write.csv(Highest, "outputs/highest_cors_table.csv")

sec.highest<- rbind(
highest.cor('HIC', 2),
highest.cor('BON', 2),
highest.cor('COR', 2),
highest.cor('GLA', 2),
highest.cor('STC', 2),
highest.cor('ENG', 2),
highest.cor('UNC', 2),
highest.cor('TOW', 2),
highest.cor('MOU', 2),
highest.cor('GL1', 2),
highest.cor('GL2', 2),
highest.cor('GL3', 2),
highest.cor('GL4', 2),
highest.cor('PVC', 2)
)



#now plot the correlations against their mean annual precips:
cor.v.clim <- function(clim,mono, pre,var){
  locs <- read.csv("outputs/priority_sites.csv")
  sites <- c("COR", "HIC", "STC", "GLA", "TOW", "ENG", "UNC", "BON", "MOU", "")

  precip <- matrix(NA ,nrow = length(sites), ncol = 2)
  for (i in 1:length(sites)){
    precip[i,1] <- sites[i]
    a <- read.csv(paste0(sites[i], "-annualP.csv"))
    precip[i,2] <- mean(a$PCP)
  }
  precip <- precip[order(as.numeric(precip[,2])),]
  site.order <- rev(precip[,1])
  precip <- data.frame(precip)
  colnames(precip) <- c("site", "MAP")
  precip <- merge(precip, locs, by.x = 'site', by.y = 'code')
month.coef <- matrix(NA, nrow = length(precip[,1]), ncol = 1)
for(i in 1:length(precip[,1])){
cors <- read.csv(paste0(precip[i,1], "-WW", clim, "cor.csv"))
month.coef[i,] <- cors[mono,]$V1
}
x <- as.data.frame(precip)
x$cor <- as.vector(month.coef)
colnames(x[,1:3]) <- c('site', "MAP", "cor")
if(var %in% "MAP"){
  x$env <- as.numeric(as.character(x$MAP))
}else{if (var %in% "awc"){
  x$env <- pre$awc
}else{if (var %in% "ksat"){
x$env <- pre$ksat
}else{if (var %in% "sand"){
  x$env <- pre$sand
}else{
   precip$env <- 8888
 }
}
}
}

lm <- lm(formula = cor ~ env, data = x)
print(summary(lm))
ggplot(x, aes(env, cor, color = cor))+#geom_text_repel(aes(label = site))+
  scale_color_continuous(low = 'red', high = 'blue')+geom_point(size = 5, aes(shape = Description))+stat_smooth(method = 'lm')+theme_bw()+ggtitle(paste0(clim, " correlation with ", var))+
  xlab(var)+ylab(paste0(clim," correlation coefficient"))+geom_text(aes(label=as.character(site)),hjust=0, vjust=2) +scale_x_continuous(expand= c(0.25,0.5)) +scale_y_continuous(expand = c(0.25, 0))+

}

#plot these in pngs for may and august for MAP
for(i in 1:length(clim.cd)){
  mypath <- file.path("C:/Users/JMac/Documents/Kelly/TreeRings/outputs/correlations/static_site_cors",paste("site_cor_MAP_jun", clim.cd[i], ".png", sep = ""))
  cor.v.clim(clim.cd[i], 18, precip, var = "MAP")
  ggsave(filename=mypath)
  
  mypath <- file.path("C:/Users/JMac/Documents/Kelly/TreeRings/outputs/correlations/static_site_cors",paste("site_cor_MAP_aug", clim.cd[i], ".png", sep = ""))
  cor.v.clim(clim.cd[i], 20, precip, var = "MAP")
  ggsave(filename=mypath)
}



pdf("outputs/cor_coef_v_MAP.pdf")
cor.v.clim("tavg", 18,precip, var = "MAP")
cor.v.clim("Precip",18, precip, var = "MAP")
cor.v.clim("tmin", 18,precip, var = "MAP")
cor.v.clim("tmax", 18,precip, var = "MAP")
cor.v.clim("PDSI", 18,precip, var = "MAP")
dev.off()

#can use the cor.v.clim function to plot correlations against soil characteristics
#read in site xys
locs <- read.csv("outputs/priority_sites_locs.csv")
sites <- c("COR", "HIC", "STC", "GLA", "TOW", "ENG", "UNC", "BON", "MOU")
precip <- matrix(NA ,nrow = length(sites), ncol = 2)
for (i in 1:length(sites)){
  precip[i,1] <- sites[i]
  a <- read.csv(paste0(sites[i], "-annualP.csv"))
  precip[i,2] <- mean(a$PCP)
}
precip <- precip[order(as.numeric(precip[,2])),]
site.order <- rev(precip[,1])
precip <- data.frame(precip)
colnames(precip) <- c("site", "MAP")
test <- merge(precip, locs, by.x = 'site', by.y = 'code')

#plot correlation coefficients with July climate variables against ksat
for(i in 1:length(clim.cd)){
  #for awc
  mypath <- file.path("C:/Users/JMac/Documents/Kelly/TreeRings/outputs/correlations/static_site_cors",paste("site_cor_awc_jun", clim.cd[i], ".png", sep = ""))
  cor.v.clim(clim.cd[i], 18, test, var = "awc")
  ggsave(filename=mypath)
  
  #for ksat
  mypath <- file.path("C:/Users/JMac/Documents/Kelly/TreeRings/outputs/correlations/static_site_cors",paste("site_cor_ksat_jun", clim.cd[i], ".png", sep = ""))
  cor.v.clim(clim.cd[i], 18, test, var = "ksat")
  ggsave(filename=mypath)
  
  #for sand
  mypath <- file.path("C:/Users/JMac/Documents/Kelly/TreeRings/outputs/correlations/static_site_cors",paste("site_cor_sand_jun", clim.cd[i], ".png", sep = ""))
  cor.v.clim(clim.cd[i], 18, test, var = "sand")
  ggsave(filename=mypath)
  
  #for sand
  mypath <- file.path("C:/Users/JMac/Documents/Kelly/TreeRings/outputs/correlations/static_site_cors",paste("site_cor_sand_aug", clim.cd[i], ".png", sep = ""))
  cor.v.clim(clim.cd[i], 20, test, var = "sand")
  ggsave(filename=mypath)
  
}

pdf("outputs/cor_coef_v_ksat.pdf")
cor.v.clim("PDSI", 18,test, var = "ksat")
cor.v.clim("tavg", 18,test, var = 'ksat')
cor.v.clim("tmin", 18,test, var = 'ksat')
cor.v.clim("tmax", 18,test, var = 'ksat')
cor.v.clim("Precip", 18,test, var = 'ksat')
dev.off()

#plot correlation coefficients with July climate variabes with awc
pdf("outputs/cor_coef_v_awc.pdf")
cor.v.clim("PDSI", 18,test, var = "awc")
cor.v.clim("tavg", 18,test, var = 'awc')
cor.v.clim("tmin", 18,test, var = 'awc')
cor.v.clim("tmax", 18,test, var = 'awc')
cor.v.clim("Precip", 18,test, var = 'awc')
dev.off()

#pdf("outputs/cor_coef_v_sand.pdf")
cor.v.clim("PDSI", 18,test, var = "sand") + ylab('June PDSI correlation coefficient') + ggtitle('June PDSI correlation')
b <- cor.v.clim("tavg", 18,test, var = 'sand')+ ylab('Correlation coefficient') + ggtitle('B). June Avg. Temperature')
cor.v.clim("tmin", 18,test, var = 'sand')
c <- cor.v.clim("tmax", 18,test, var = 'sand')+ ylab('Correlation coefficient') + ggtitle('C). June Max. Temperature')
cor.v.clim("Precip", 18,test, var = 'sand')+ ylab('Correlation coefficient') + ggtitle('June Precipitation correlation')
a <- cor.v.clim("Precip", 20,test, var = 'sand')+ ylab('Correlation coefficient') + ggtitle('A). August Precipitation')
#dev.off()

#print out a PNG
png(width = 5, height = 9, units = 'in', res = 300,'outputs/correlations/all_site_cor_v_sand.png')
pushViewport(viewport(layout = grid.layout(3, 1)))
print(a, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(b, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(c, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
dev.off()

#plot correlations for savanna and forests
cor.v.type <- function(clim,mono, pre,var){
  locs <- read.csv("outputs/priority_sites_locs.csv")
  sites <- c("COR", "HIC", "STC", "GLA", "TOW", "ENG", "UNC", "BON", "MOU")
  
  precip <- matrix(NA ,nrow = length(sites), ncol = 2)
  for (i in 1:length(sites)){
    precip[i,1] <- sites[i]
    a <- read.csv(paste0(sites[i], "-annualP.csv"))
    precip[i,2] <- mean(a$PCP)
  }
  precip <- precip[order(as.numeric(precip[,2])),]
  site.order <- rev(precip[,1])
  precip <- data.frame(precip)
  colnames(precip) <- c("site", "MAP")
  precip <- merge(precip, locs, by.x = 'site', by.y = 'code')
  month.coef <- matrix(NA, nrow = length(precip[,1]), ncol = 1)
  for(i in 1:length(precip[,1])){
    cors <- read.csv(paste0(precip[i,1], "-WW", clim, "cor.csv"))
    month.coef[i,] <- cors[mono,]$V1
  }
  x <- as.data.frame(precip)
  x$cor <- as.vector(month.coef)
  colnames(x[,1:3]) <- c('site', "MAP", "cor")
  if(var %in% "MAP"){
    x$env <- as.numeric(as.character(x$MAP))
  }else{if (var %in% "awc"){
    x$env <- pre$awc
  }else{if (var %in% "ksat"){
    x$env <- pre$ksat
  }else{if (var %in% "sand"){
    x$env <- pre$sand
  }else{
    precip$env <- 8888
  }
  }
  }
  }
  
  lm <- lm(formula = cor ~ env, data = x)
  print(summary(lm))
  ggplot(x, aes(Description, cor, color = cor))+ geom_boxplot()#geom_text_repel(aes(label = site))+
    #scale_color_continuous(low = 'red', high = 'blue')+geom_point(size = 5, aes(shape = Description))+stat_smooth(method = 'lm')+theme_bw()+ggtitle(paste0(clim, " correlation with ", var))+
   # xlab(var)+ylab(paste0(clim," correlation coefficient"))+geom_text(aes(label=as.character(site)),hjust=0, vjust=2) +scale_x_continuous(expand= c(0.25,0.5)) +scale_y_continuous(expand = c(0.25, 0))
  
}
cor.v.type("PDSI", 18,test, var = "sand")
cor.v.type("tavg", 18,test, var = 'sand')
cor.v.type("tmin", 18,test, var = 'sand')
cor.v.type("tmax", 18,test, var = 'sand')
cor.v.type("Precip", 18,test, var = 'sand')
cor.v.type("Precip", 20,test, var = 'sand')


mean.growth.barplot <- function(clim,mono, pre){
  locs <- read.csv("outputs/priority_sites_locs.csv")
  sites <- c("COR", "HIC", "STC", "GLA", "TOW", "ENG", "UNC", "BON", "MOU")
  
  meangrowth <- matrix(NA ,nrow = length(sites), ncol = 2)
  for (i in 1:length(sites)){
    meangrowth[i,1] <- sites[i]
    a <- read.csv(paste0("data/site_stats/",sites[i], "-site_stats.csv"))
    meangrowth[i,2] <- mean(a$mean)
  }
  meangrowth <- meangrowth[order(as.numeric(meangrowth[,2])),]
  site.order <- rev(meangrowth[,1])
  meangrowth <- data.frame(meangrowth)
  colnames(meangrowth) <- c("site", "Meangrowth")
  meangrowth <- merge(meangrowth, locs, by.x = 'site', by.y = 'code')
  month.coef <- matrix(NA, nrow = length(meangrowth[,1]), ncol = 1)
  for(i in 1:length(meangrowth[,1])){
    cors <- read.csv(paste0(meangrowth[i,1], "-WW", clim, "cor.csv"))
    month.coef[i,] <- cors[mono,]$V1
  }
  x <- as.data.frame(meangrowth)
  x$cor <- as.vector(month.coef)
  x$Meangrowth <- as.numeric(x$Meangrowth)
  colnames(x[,1:3]) <- c('site', "Meangrowth", "cor")
  
  ggplot(x, aes(site, Meangrowth, fill = Description))+#geom_text_repel(aes(label = site))+
    geom_bar(stat= 'identity') + theme_bw()
}
mean.growth.barplot("Precip", 20,test)


#molten.full comes from climate_growth_reg_chron.R
###################################################
#compare climate coreelaitons c
plot.cor.clim <- function(x, Climate, xlab, Site){
  yr <- 1895:1950
  x$class <- '9999'
  x[x$Year %in% yr,]$class <- 'clim_record'
  
  
  #if the dummy variable is significant, then the two slopes are different
  print(summary( cor( Climate, x$value)))
  
  # Extend the regression lines beyond the domain of the data
  
  ggplot(x, aes(x=Climate, y=value)) + geom_point(shape=1) +
    scale_colour_hue(l=50) +
    #+ylim(-1.0,1.0)
    #+xlim(-4,4)# Use a slightly darker palette than normal
    geom_smooth(method=lm,   # Add linear regression lines
                se=TRUE,    # add shaded confidence region
                fullrange=FALSE)+# Extend regression lines
    
    #scale_color_manual(values=c('Pre-1950'="red",'Post-1950'="blue"))+
    xlim(-8, 8)+
    ylim(0.5, 1.5) +
    theme_bw()+
    theme(text = element_text(size = 30))+
    ylab('Detrended Ring width Index') +
    xlab( xlab ) +
    ggtitle(Site)
  
}
plot.cor.clim(molten.BON, molten.BON$PDSI, "PDSI", "Bonanza Prairie")
plot.cor.clim(molten.HIC, molten.HIC$PDSI, "PDSI", "Hickory Grove")
plot.cor.clim(molten.COR, molten.COR$PDSI, "PDSI", "Coral Woods")
plot.cor.clim(molten.GLA, molten.GLA$PDSI, "PDSI", "Glacial Park")
plot.cor.clim(molten.STC, molten.STC$PDSI, "PDSI", "St. Croix Savanna")
plot.cor.clim(molten.TOW, molten.TOW$PDSI, "PDSI", "Townsend Woods")
plot.cor.clim(molten.UNC, molten.UNC$PDSI, "PDSI", "Uncas Dunes")
plot.cor.clim(molten.MOU, molten.MOU$PDSI, "PDSI", "Mound Prairie")

#let's see if wyckoff and bower's findings of a decreased relationship between PDSI & growth are correct

# conduct f-test to see if the relationship pre-1950 is same as post 1950

yr <- 1895:1950
yr.post <- 1950:2014

#this function runs the stats and makes plots for pre-1950 vs. post-1950
# additionally plots are saved to outputs/correlations within the function
plot.pre.post <- function(x, Climate, xlab, Site){
yr <- 1895:1950
yr.post <- 1950:2014
x$class <- '9999'
x[x$Year %in% yr,]$class <- 'Pre-1950'
x[x$Year %in% yr.post,]$class <- 'Post-1950'
#create dummy variable
x$group <- 0
x[x$Year %in% yr,]$group <- 1

#if the dummy variable is significant, then the two slopes are different
print(summary(aov(value ~ Climate * group, data = x)))
#print(summary(lm(value ~ Climate:group, data = x)))
#print(summary(aov(value~Climate*class, data=x)))
print(anova(lm(value ~ Climate*group, data = x), lm(value ~ Climate, data = x))
)#print(summary(lm(value~Climate/group-1, data=x)))
#print(summary(aov(value~Climate/group, data = x)))
# Extend the regression lines beyond the domain of the data

p<- ggplot(x, aes(x=Climate, y=value, colour=class)) + geom_point(shape=1) +
  scale_colour_hue(l=50) +
  #+ylim(-1.0,1.0)
  #+xlim(-4,4)# Use a slightly darker palette than normal
  geom_smooth(method='lm',   # Add linear regression lines
              se=TRUE,    # add shaded confidence region
              fullrange=FALSE)+# Extend regression lines
  
  scale_color_manual(values=c('Pre-1950'="red",'Post-1950'="blue"))+
  xlim(-8, 8)+
  ylim(0.5, 1.5) +
  theme_bw()+
  theme(text = element_text(size = 10), plot.title = element_text(hjust = 0.5))+
  ylab('Detrended RWI') +
  xlab( xlab ) +
  ggtitle(Site)
p
#ggsave(filename = paste0('outputs/correlations/pre_post_jul_pdsi_',Site,".png"), plot = p, width = 10, height = 7 )
}



plot.pre.post(molten.HIC, molten.HIC$JJA.p, 'Summer Precipitation (mm)', "Hickory Grove, IL") #significant
plot.pre.post(molten.BON, molten.BON$JJA.p, 'Summer Precipitation (mm)', "Bonanza Prairie, MN") #significant
plot.pre.post(molten.PLE, molten.PLE$JJA.p, 'Summer Precipitation (mm)', "Pleasant Valley Conservancy, WI") #significant
plot.pre.post(molten.TOW, molten.TOW$JJA.p, 'Summer Precipitation (mm)', "Townsend Woods, MN") #not significant
plot.pre.post(molten.STC, molten.STC$JJA.p, 'Summer Precipitation (mm)', "St.Croix Savanna, MN") #not significant
plot.pre.post(molten.DES, molten.DES$JJA.p, 'Summer Precipitation (mm)', "Bois de Soix, MN") #significant
plot.pre.post(molten.SAN, molten.SAN$JJA.p, 'Summer Precipitation (mm)', "Sandwich, IL") #significant
plot.pre.post(molten.PLP, molten.PLP$JJA.p, 'Summer Precipitation (mm)', "Pleasant Prarie, WI") #significant


plot.pre.post(molten.HIC, molten.HIC$MAY.p, 'May Precipitation (mm)', "Hickory Grove, IL") #significant
plot.pre.post(molten.BON, molten.BON$MAY.p, 'May Precipitation (mm)', "Bonanza Prairie, MN") #significant
plot.pre.post(molten.PLE, molten.PLE$MAY.p, 'May Precipitation (mm)', "Pleasant Valley Conservancy, WI") #significant
plot.pre.post(molten.TOW, molten.TOW$MAY.p, 'May Precipitation (mm)', "Townsend Woods, MN") #not significant
plot.pre.post(molten.STC, molten.STC$MAY.p, 'May Precipitation (mm)', "St.Croix Savanna, MN") #not significant
plot.pre.post(molten.DES, molten.DES$MAY.p, 'May Precipitation (mm)', "Bois de Soix, MN") #significant
plot.pre.post(molten.SAN, molten.SAN$MAY.p, 'May Precipitation (mm)', "Sandwich, IL") #significant
plot.pre.post(molten.PLP, molten.PLP$MAY.p, 'May Precipitation (mm)', "Pleasant Prarie, WI") #significant


plot.pre.post(molten.HIC, molten.HIC$JUNTmin, 'June Minimum Temperature', "Hickory Grove, IL") #significant
plot.pre.post(molten.BON, molten.BON$JUNTmin, 'June Minimum Temperature', "Bonanza Prairie, MN") #significant
plot.pre.post(molten.PLE, molten.PLE$JUNTmin, 'June Minimum Temperature', "Pleasant Valley Conservancy, WI") #significant
plot.pre.post(molten.TOW, molten.TOW$JUNTmin, 'June Minimum Temperature', "Townsend Woods, MN") #not significant
plot.pre.post(molten.STC, molten.STC$JUNTmin, 'June Minimum Temperature', "St.Croix Savanna, MN") #not significant
plot.pre.post(molten.DES, molten.DES$JUNTmin, 'June Minimum Temperature', "Bois de Soix, MN") #significant
plot.pre.post(molten.SAN, molten.SAN$JUNTmin, 'June Minimum Temperature', "Sandwich, IL") #significant
plot.pre.post(molten.PLP, molten.PLP$JUNTmin, 'June Minimum Temperature', "Pleasant Prarie, WI") #significant


plot.pre.post(molten.HIC, molten.HIC$JUNTmax, 'June Maximum Temperature', "Hickory Grove, IL") #significant
plot.pre.post(molten.BON, molten.BON$JUNTmax, 'June Maximum Temperature', "Bonanza Prairie, MN") #significant
plot.pre.post(molten.PLE, molten.PLE$JUNTmax, 'June Maximum Temperature', "Pleasant Valley Conservancy, WI") #significant
plot.pre.post(molten.TOW, molten.TOW$JUNTmax, 'June Maximum Temperature', "Townsend Woods, MN") #not significant
plot.pre.post(molten.STC, molten.STC$JUNTmax, 'June Maximum Temperature', "St.Croix Savanna, MN") #not significant
plot.pre.post(molten.DES, molten.DES$JUNTmax, 'June Maximum Temperature', "Bois de Soix, MN") #significant
plot.pre.post(molten.SAN, molten.SAN$JUNTmax, 'June Maximum Temperature', "Sandwich, IL") #significant
plot.pre.post(molten.PLP, molten.PLP$JUNTmax, 'June Maximum Temperature', "Pleasant Prarie, WI") #significant


plot.pre.post(molten.HIC, molten.HIC$JUNTavg, 'June Average Temperature', "Hickory Grove, IL") #significant
plot.pre.post(molten.BON, molten.BON$JUNTavg, 'June Average Temperature', "Bonanza Prairie, MN") #significant
plot.pre.post(molten.PLE, molten.PLE$JUNTavg, 'June Average Temperature', "Pleasant Valley Conservancy, WI") #significant
plot.pre.post(molten.TOW, molten.TOW$JUNTavg, 'June Average Temperature', "Townsend Woods, MN") #not significant
plot.pre.post(molten.STC, molten.STC$JUNTavg, 'June Average Temperature', "St.Croix Savanna, MN") #not significant
plot.pre.post(molten.DES, molten.DES$JUNTavg, 'June Average Temperature', "Bois de Soix, MN") #significant
plot.pre.post(molten.SAN, molten.SAN$JUNTavg, 'June Average Temperature', "Sandwich, IL") #significant
plot.pre.post(molten.PLP, molten.PLP$JUNTavg, 'June Average Temperature', "Pleasant Prarie, WI") #significant

#pdf('outputs/pdsi_pre_post_plots.pdf')

e <- plot.pre.post(molten.HIC, molten.HIC$Jul.pdsi, 'July PDSI', "Hickory Grove, IL") #significant
a <- plot.pre.post(molten.BON, molten.BON$Jul.pdsi, 'July PDSI', "Bonanza Prairie, MN") #significant
d <- plot.pre.post(molten.PLE, molten.PLE$Jul.pdsi, 'July PDSI', "Pleasant Valley Conservancy, WI") #not significant (only sig @ 0.15 )
h <- plot.pre.post(molten.TOW, molten.TOW$Jul.pdsi, 'July PDSI', "Townsend Woods, MN") #not significant
c <- plot.pre.post(molten.STC, molten.STC$Jul.pdsi, 'July PDSI', "St.Croix Savanna, MN") #significant
f <- plot.pre.post(molten.GLA, molten.GLA$Jul.pdsi, 'July PDSI', "Glacial Park, IL") #significant
g <- plot.pre.post(molten.COR, molten.COR$Jul.pdsi, 'July PDSI', "Coral Woods, IL") #significant
b <- plot.pre.post(molten.UNC, molten.UNC$Jul.pdsi, 'July PDSI', "Uncas Dunes, MN") #significant
i <- plot.pre.post(molten.ENG, molten.ENG$Jul.pdsi, 'July PDSI', "Englund Ecotone, MN") #significant
j <- plot.pre.post(molten.MOU, molten.MOU$Jul.pdsi, 'July PDSI', "Mound Prairie, MN") #significant

source("R/grid_arrange_shared_legend.R")
png(width = 6, height = 9, units = 'in', res = 300, 'outputs/correlations/all_site_pre_post_fig3-ModNegExp.png')
grid_arrange_shared_legend(a,b,c,d,e,f,g,h,i,j,nrow = 5, ncol = 2 )
dev.off()

# plot the 
#png(width = 6, height = 9, units = 'in', res = 300, 'outputs/correlations/all_site_pre_post_fig3.png')
#grid_arrange_shared_legend(a,b,c,d,e,f,g,h,i,j,nrow = 5, ncol = 2 )
#dev.off()

# plot out sites with slope change:
# St Croix savanna (c),
png(width = 6, height = 5, units = 'in', res = 300, 'outputs/correlations/slopechange_site_pre_post_fig3-ModNegExp.png')
grid_arrange_shared_legend(c,d,nrow = 2, ncol = 2 )
dev.off()

# plot out sites with intercept change:
# hickory grove (e), bonanza prairie (a), 
# pleasant valley, glacial park, coral woods

png(width = 6, height = 5, units = 'in', res = 300, 'outputs/correlations/interceptchange_site_pre_post_fig3-ModNegExp.png')
grid_arrange_shared_legend(a,e,f,g,nrow = 2, ncol = 2 )
dev.off()

# plot out sites with no changes:
# mound prairie (j), Townsend woods (h), 
#englund ecotone(i), Uncas dunes (b)
png(width = 6, height = 5, units = 'in', res = 300, 'outputs/correlations/nochange_site_pre_post_fig3-ModNegExp.png')
grid_arrange_shared_legend(j,b,h,i,nrow = 2, ncol = 2 )
dev.off()



plot.pre.post(molten.HIC, molten.HIC$JULTavg, 'July Average Temperature (DegF)', "Hickory Grove, IL") #significant




pdf("outputs/pre_post_1950_plots.pdf")
plot.pre.post(molten.HIC, molten.HIC$PDSI, 'PDSI', "Hickory Grove, IL") #significant
plot.pre.post(molten.HIC, molten.HIC$PCP, "Annual Precipitation", "Hickory Grove, IL") #sig
plot.pre.post(molten.HIC, molten.HIC$TMIN/10, "Mean Minimum Temperature", "Hickory Grove, IL") #sig
plot.pre.post(molten.HIC, molten.HIC$TAVG/10, "Mean Temperature", "Hickory Grove, IL") #sig
plot.pre.post(molten.BON, molten.BON$PDSI, "PDSI", "Bonanza Prairie, MN") #sig *
plot.pre.post(molten.BON, molten.BON$PCP, "Annual Precipitation", "Bonanza Prairie, MN") #sig 
plot.pre.post(molten.BON, molten.BON$TMIN, "Mean Minimum Temperature", "Bonanza Prairie, MN") #sig
plot.pre.post(molten.BON, molten.BON$TAVG, "Mean Temperature", "Bonanza Prairie, MN") #***
plot.pre.post(molten.PLE, molten.PLE$PDSI, "PDSI", "Pleasant Valley Conservancy, WI") #**
plot.pre.post(molten.PLE, molten.PLE$PCP, "Annual Precipitation", "Pleasant Valley Conservancy, WI") #***
plot.pre.post(molten.PLE, molten.PLE$TMIN,"Mean Minimum", "Pleasant Valley Conservancy, WI")#***
plot.pre.post(molten.PLE, molten.PLE$TAVG,"Mean Temperature","Pleasant Valley Conservancy, WI")#***
plot.pre.post(molten.TOW, molten.TOW$PDSI, "PDSI", "Townsend Woods, MN") #**
plot.pre.post(molten.TOW, molten.TOW$PCP, "Annual Precipitaiton", "Townsend Woods, MN") #not signficant
plot.pre.post(molten.TOW, molten.TOW$TMIN, "Minimum Temperature", "Townsend Woods, MN") #not significant
plot.pre.post(molten.TOW, molten.TOW$TAVG, "Average Temperature" ,"Townsend Woods, MN") #not significant
dev.off()



#################################################
# Linear regressions by group with bootstrapping#
#################################################
library(boot)

# are the residuals increasing over time?
test <- lm(molten.HIC$value~molten.HIC$PDSI)
plot(molten.HIC$Year, test$residuals, type = "l")
summary(lm(test$residuals ~ molten.HIC$Year))

print(summary(aov(data$value~data$PDSI*data$class)))

growth_reg = function(data, indices){
  climate <- "Jul.pdsi"
  yr <- 1895:1950
  yr.post <- 1950:2014
  data$class <- '9999'
  data[data$Year %in% yr,]$class <- 'Pre-1950'
  data[data$Year %in% yr.post,]$class <- 'Post-1950'
  #create dummy variable
  data$group <- 0
  data[data$Year %in% yr,]$group <- 1
d = data[indices, ]
H_relationship = lm(d$value~d[,c(climate)], data = d)
H_r_sq = coefficients(H_relationship)
#H_p = summary(H_relationship)$coefficients[,4]
G_relationship = lm(d$value~d[,c(climate)]*d$group, data = d)
G_r_sq = coefficients(G_relationship)
#G_p = summary(G_relationship)$coefficients[,4]
relationships = c(H_r_sq, #H_p, 
                  G_r_sq #, G_p
                  )
return(relationships)
}

# bootstrapping
results = boot(data = molten.HIC, statistic = growth_reg, R = 5000)
print (results) # view bootstrapped coefficients


plot(results, index = 1) # plot boot strapped intercept
plot(results, index = 2) # beta reg. coefficient
plot(results, index = 3) # boot strapped intercept with grouping
plot(results, index = 4) # beta reg coefficient
plot(results, index = 5) # class 1 reg coefficient
plot(results, index = 6) # class 2 reg coefficient

# still not sure which class is which in this

# the idea is to compare distributions of slope to 0 (if CI doesnt include 0, it should be different from 0)
# also compare the two slope distributions to determine if they are statistically different from each other (ftest/anova)



confidence_interval_H = boot.ci(results, index = 2, conf = 0.95, type = 'bca')
print(confidence_interval_H)
ci_H = confidence_interval_H$bca[ , c(4, 5)]
print(ci_H) # if ciH doesnt contain 0, then the reg coefficient is >0 significant

hist(results$t[,1], main = 'intercept', xlab = 'int',
     col = 'grey')
hist(results$t[,2], main = 'Beta: PDSI', xlab = 'beta',
     col = 'grey', prob = T)
lines(density(results$t[,2]), col = 'blue')
abline(v = ci_H, col = 'red')

#####################################
#plot correlations against soil type#
#####################################'

#read in soil rasters from gssurgo data
library(raster)
ksat <- raster('C:/Users/JMac/Box Sync/GSSURGOtifs/8km_UMW_ksat1.tif')
ksat.alb <- projectRaster(ksat, crs='+init=epsg:3175')

awc <- raster('C:/Users/JMac/Box Sync/GSSURGOtifs/8km_UMW_awc1.tif')
awc.alb <- projectRaster(awc, crs = '+init=epsg:3175')


priority <- readOGR('data/Treecores.kml', layer = "NAPCsites")
priority <- spTransform(priority, CRSobj = CRS('+init=epsg:3175'))
priority <- data.frame(priority)
priority$code <- c("PVC", "STC", "TOW", "HIC", "BON")
priority$Names <- c('Pleasant Valley', 'St. Croix Savanna',"Townsend Woods", "Hickory Grove", "Bonanza Prairie")
places <- c('St. Croix Savanna',"Townsend Woods", "Hickory Grove", "Bonanza Prairie")



priority$ksat <- extract(ksat.alb, priority[,c("coords.x1", "coords.x2")])
priority$awc <- extract(awc.alb, priority[,c("coords.x1", "coords.x2")])

BON.pdsi <- read.csv("BON-WWPDSIcor.csv")
HIC.pdsi <- read.csv("HIC-WWPDSIcor.csv")
STC.pdsi <- read.csv("STC-WWPDSIcor.csv")
TOW.pdsi <- read.csv("TOW-WWPDSIcor.csv")
PLE.pdsi <- read.csv("PLE-WWPDSIcor.csv")



priority$pdsiJul <- 0
priority[priority$code %in% "BON", ]$pdsiJul <- BON.pdsi[19,2]
priority[priority$code %in% "HIC", ]$pdsiJul <- HIC.pdsi[19,2]
priority[priority$code %in% "STC", ]$pdsiJul <- STC.pdsi[19,2]
priority[priority$code %in% "TOW", ]$pdsiJul <- TOW.pdsi[19,2]
priority[priority$code %in% "PVC", ]$pdsiJul <- PLE.pdsi[19,2]


plot(priority$ksat, priority$pdsiJul)
plot(priority$awc, priority$pdsiJul)


#dataset from http://scrippsco2.ucsd.edu/data/atmospheric_co2
#CO2 <- read.csv('data/merged_ice_core_yearly.csv')

CO2 <- read.csv('data/spline_merged_ice_core_yearly2.csv', header = TRUE)

#colnames(CO2) <- c('YearCE', 'ppm', 'Year')

compare.CO2<- function(CO2, x){
  yr <- 1895:1950
  yr.post <- 1950:2014
  x$class <- '9999'
  x[x$Year %in% yr,]$class <- 'Pre-1950'
  x[x$Year %in% yr.post,]$class <- 'Post-1950'
  #create dummy variable
  x$group <- 0
  x[x$Year %in% yr,]$group <- 1
  
CO2.m <- merge(x, CO2, by = 'Year')
print(summary( lm(value ~ ppm, data = CO2.m)))


# Extend the regression lines beyond the domain of the data
ggplot(CO2.m, aes(x=ppm, y=value)) + geom_point(shape=1) +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=loess,   # Add linear regression lines
              se=TRUE,    # add shaded confidence region
              fullrange=FALSE)+# Extend regression lines
  ylab('Detrended Ring width Index') +
  xlab( 'ppm') +
  ggtitle('CO2 vs growth')
}

pdf('outputs/CO2_growth_splines.pdf')
compare.CO2(CO2, molten.BON)
compare.CO2(CO2, molten.HIC)
compare.CO2(CO2, molten.TOW)
compare.CO2(CO2, molten.PLE)
compare.CO2(CO2, molten.STC)
compare.CO2(CO2, molten.SAN)
compare.CO2(CO2, molten.DES)
compare.CO2(CO2, molten.COR)
compare.CO2(CO2, molten.ENG)
compare.CO2(CO2, molten.MOU)
compare.CO2(CO2, molten.GLA)
dev.off()


compare.CO2.PDSI<- function(CO2, x){
  CO2.m <- merge(x, CO2, by = 'Year')
  plot(CO2.m$PDSI, CO2.m$ppm)
}



compare.CO2.PDSI(CO2, molten.BON)
compare.CO2.PDSI(CO2, molten.HIC)
compare.CO2.PDSI(CO2, molten.TOW)
compare.CO2.PDSI(CO2, molten.PLE)
compare.CO2.PDSI(CO2, molten.STC)

#plot theses
plot(molten.HIC[molten.HIC$Year== 1895:1950,]$PDSI, molten.HIC[molten.HIC$Year== 1895:1950,]$value)
abline(preHIClm)
points(molten.HIC[molten.HIC$Year== 1951:2014,]$PDSI, molten.HIC[molten.HIC$Year== 1951:2014,]$value, col = 'red')
abline(postHIClm, col = 'red')

var.test( lm(value ~ PDSI, data = molten.BON[molten.BON$Year %in% yr,]),
          lm(value ~ PDSI, data = molten.BON[molten.BON$Year %in% yr.post,]))
#F = 1.5127, num df = 222, denom df = 190, p-value = 0.003411

var.test( lm(value ~ PDSI, data = molten.PLE[molten.PLE$Year %in% yr,]),
          lm(value ~ PDSI, data = molten.PLE[molten.PLE$Year %in% yr.post,]))
# F = 0.82271, num df = 166, denom df = 126, p-value = 0.2389

var.test( lm(value ~ PDSI, data = molten.TOW[molten.TOW$Year %in% yr,]),
          lm(value ~ PDSI, data = molten.TOW[molten.TOW$Year %in% yr,]))

# F = 1.7999, num df = 173, denom df = 190, p-value = 8.101e-05



















