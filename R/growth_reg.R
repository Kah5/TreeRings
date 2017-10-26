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

cor.barplot <- function(site.code, climatedata){
  # since the different climate datasets have different variables, we need to make a separate set of barplots for each:
  
  #-------------------------GHCN data correlations---------------------------------
  if(climatedata == "GHCN")
  {
  # read in the bootstrapped corrlations for each climate variable:
  tavg <- read.csv(paste0("data/BootCors/",climatedata,"/",site.code, '-WWtavgcor.csv'))
  tmin <- read.csv(paste0("data/BootCors/",climatedata,"/",site.code, '-WWtmincor.csv'))
  tmax <- read.csv(paste0("data/BootCors/",climatedata,"/",site.code, '-WWtmaxcor.csv'))
  precip <- read.csv(paste0("data/BootCors/",climatedata,"/",site.code, '-WWPrecipcor.csv'))
  PDSI <- read.csv(paste0("data/BootCors/",climatedata,"/",site.code, '-WWPDSIcor.csv'))
  
  months <- c("pJan", "pFeb", "pMar", "pApr", "pMay", "pJun", "pJul",
              "pAug", "pSep", "pOct", "pNov", "pDec",
              "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
              "Aug", "Sep", "Oct", "Nov", "Dec")
  
  tavg$months <- months
  colnames(tavg) <- c('mono', 'tavg', 'months', "ci.min", "ci.max")
  full <- tavg[,c('mono', "tavg", "months")]
  cimin <- tavg[c('mono', 'months', "ci.min")]
  colnames(cimin) <- c('mono', 'months', "tavg")
  cimax <- tavg[c('mono', 'months', "ci.max")]
  colnames(cimax) <- c('mono', 'months', "tavg")
  
  full$tmin <- tmin$cor
  cimin$tmin <- tmin$ci.min
  cimax$tmin <- tmin$ci.max
  
  full$tmax <- tmax$cor
  cimin$tmax <- tmax$ci.min
  cimax$tmax <- tmax$ci.max
  
  full$precip <- precip$cor
  cimin$precip <- precip$ci.min
  cimax$precip <- precip$ci.max
  
  full$PDSI <- PDSI$cor
  cimin$PDSI <- PDSI$ci.min
  cimax$PDSI <- PDSI$ci.max
  
  cors.melt <- melt(full, id.vars = c('months', 'mono'))
  cimin.melt <- melt(cimin, id.vars = c("months", "mono"))
  cimax.melt <- melt(cimax, id.vars = c("months", "mono"))
  colnames(cimin.melt) <- c('months', 'mono', "variable", "ci.min")
  colnames(cimin.melt) <- c('months', 'mono', "variable", "ci.max")
  m1 <- merge(cors.melt, cimin.melt, by = c("months", "mono", "variable"))
  m2 <- merge(m1, cimax.melt, by = c("months", "mono", "variable"))
  colnames(m2)[4:6] <- c("cor","ci.min", "ci.max")
  m2$months <- factor(m2$months, levels=full$months)
  m2[order(m2$months),]
  
  # plot all the barplots in the same plot:
  output<- ggplot(data = m2, aes(months, cor, fill = variable))+
    geom_bar(stat = 'identity', position = position_dodge()) + 
    facet_grid(variable~.)+theme_bw()+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1)) + ggtitle(paste0(site.code, " Correlations"))+
    geom_errorbar(aes(ymin=ci.min, ymax=ci.max),
                   width=.2,                    # Width of the error bars
                   position=position_dodge(.9))
  output
  
  
  }else{
    
    #-------------------------PRISM data correlations---------------------------------
    
    # read in the bootstrapped corrlations for each climate variable:
    tavg <- read.csv(paste0("data/BootCors/",climatedata,"/",site.code, '-WWtavgcor.csv'))
    tmin <- read.csv(paste0("data/BootCors/",climatedata,"/",site.code, '-WWtmincor.csv'))
    tmax <- read.csv(paste0("data/BootCors/",climatedata,"/",site.code, '-WWtmaxcor.csv'))
    precip <- read.csv(paste0("data/BootCors/",climatedata,"/",site.code, '-WWPrecipcor.csv'))
    h2obal <- read.csv(paste0("data/BootCors/",climatedata,"/",site.code, '-WWBALcor.csv'))
    vpdmax <- read.csv(paste0("data/BootCors/",climatedata,"/",site.code, '-WWVPDmaxcor.csv'))
    
    months <- c("pJan", "pFeb", "pMar", "pApr", "pMay", "pJun", "pJul",
                "pAug", "pSep", "pOct", "pNov", "pDec",
                "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                "Aug", "Sep", "Oct", "Nov", "Dec")
    
    tavg$months <- months
    colnames(tavg) <- c('mono', 'tavg', 'months', "ci.min", "ci.max")
    full <- tavg[,c('mono', "tavg", "months")]
    cimin <- tavg[c('mono', 'months', "ci.min")]
    colnames(cimin) <- c('mono', 'months', "tavg")
    cimax <- tavg[c('mono', 'months', "ci.max")]
    colnames(cimax) <- c('mono', 'months', "tavg")
    
    full$tmin <- tmin$cor
    cimin$tmin <- tmin$ci.min
    cimax$tmin <- tmin$ci.max
    
    full$tmax <- tmax$cor
    cimin$tmax <- tmax$ci.min
    cimax$tmax <- tmax$ci.max
    
    full$precip <- precip$cor
    cimin$precip <- precip$ci.min
    cimax$precip <- precip$ci.max
    
    full$h2obal <- h20bal$cor
    cimin$h20bal <- h20bal$ci.min
    cimax$h20bal <- h20bal$ci.max
    
    full$VPDmax <- VPDmax$cor
    cimin$VPDmax <- VPDmax$ci.min
    cimax$VPDmax <- VPDmax$ci.max
    
    cors.melt <- melt(full, id.vars = c('months', 'mono'))
    cimin.melt <- melt(cimin, id.vars = c("months", "mono"))
    cimax.melt <- melt(cimax, id.vars = c("months", "mono"))
    colnames(cimin.melt) <- c('months', 'mono', "variable", "ci.min")
    colnames(cimin.melt) <- c('months', 'mono', "variable", "ci.max")
    m1 <- merge(cors.melt, cimin.melt, by = c("months", "mono", "variable"))
    m2 <- merge(m1, cimax.melt, by = c("months", "mono", "variable"))
    colnames(m2)[4:6] <- c("cor","ci.min", "ci.max")
    m2$months <- factor(m2$months, levels=full$months)
    m2[order(m2$months),]
    
    # plot all the barplots in the same plot:
    output<- ggplot(data = m2, aes(months, cor, fill = variable))+
      geom_bar(stat = 'identity', position = position_dodge()) + 
      facet_grid(variable~.)+theme_bw()+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1)) + ggtitle(paste0(site.code, " Correlations"))+
      geom_errorbar(aes(ymin=ci.min, ymax=ci.max),
                    width=.2,                    # Width of the error bars
                    position=position_dodge(.9))
    output
  }
  
}

site.cd <- c("COR", "STC", "BON", "HIC", "TOW", "GLA", "ENG", "UNC", "MOU", "GL1", "GL2", "GL3", "GL4", "PVC")
#run for loop and save plots to outputs/barplots

# lets do this for GHCN data
for(i in 1:length(site.cd)){
  mypath <- file.path("/Users/kah/Documents/TreeRings/outputs/barplots/GHCN/",paste("barplots_GHCN", site.cd[i], ".png", sep = ""))
  cor.barplot(site.cd[i], "GHCN") 
  ggsave(filename=mypath)
}

# make the barplots with Prism Data:
site.prism <- c("COR", "STC", "BON", "HIC", "TOW", "GLA", "ENG", "UNC", "MOU", "GL1", "GL2", "GL3", "GL4", "PVC")

for(i in 1:length(site.prism)){
  mypath <- file.path("/Users/kah/Documents/TreeRings/outputs/barplots/PRISM/",paste("barplots_PRISM", site.cd[i], ".png", sep = ""))
  cor.barplot(site.cd[i], "PRISM") 
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
  mypath <- file.path("/Users/kah/Documents/TreeRings/",paste("full_site_barplots_", clim.cd[i], ".png", sep = ""))
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

# instead of barplots, lets create tile plots:
sites.tile <- function(clim) {
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
  
  
  
  cors.melt <- melt(full, id.vars = c('months', 'mono'))
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
  output<- ggplot(cors.melt, aes(months, variable, fill = value))+geom_tile()+
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                        name="Pearson\nCorrelation")+ ggtitle(paste0(clim, " site correlations"))+
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  output
}
clim.cd <- c('tavg', 'tmax', 'tmin', 'Precip', 'PDSI')
sites.tile("tavg")

e <- sites.tile('tavg')+ggtitle("E). Average Temperature")+ylab("correlation")
c <- sites.tile('tmax')+ ggtitle("C). Maximum Temperature")+ylab('correlation')
d <- sites.tile('tmin')+ ggtitle("D). Minimum Temperature")+ylab('correlation')
b <- sites.tile('Precip')+ ggtitle("B). Precipitation")+ylab('correlation')
a <- sites.tile('PDSI')+ ggtitle("A). Palmer Drought Severity Index")+ylab('correlation')

png(width = 10, height = 8, units = 'in', res = 300, 'outputs/barplots/tileplot_cors_all_sites_3panel.png')
grid_arrange_shared_legend(a,b,c,d,e, nrow = 3, ncol = 2 )
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
  sites <- c("COR", "HIC", "STC", "GLA", "TOW", "ENG", "UNC", "BON", "MOU", "GL1", "GL2", "GL3", "GL4", "PVC")

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
  theme_bw()
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
cor.v.clim("tavg", 18, precip, var = "MAP")
cor.v.clim("Precip",18, precip, var = "MAP")
cor.v.clim("tmin", 18, precip, var = "MAP")
cor.v.clim("tmax", 18, precip, var = "MAP")
cor.v.clim("PDSI", 18, precip, var = "MAP")
dev.off()

#can use the cor.v.clim function to plot correlations against soil characteristics
#read in site xys
locs <- read.csv("outputs/priority_sites_locs.csv")
sites <- c("COR", "HIC", "STC", "GLA", "TOW", "ENG", "UNC", "BON", "MOU", "GL1", "GL2", "GL3", "GL4", "PVC")
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
  locs <- read.csv("outputs/priority_sites.csv")
  sites <- c("COR", "HIC", "STC", "GLA", "TOW", "ENG", "UNC", "BON", "MOU", "GL1", "GL2", "GL3", "GL4", "PVC")
  
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
  locs <- read.csv("outputs/priority_sites.csv")
  sites <- c("COR", "HIC", "STC", "GLA", "TOW", "ENG", "UNC", "BON", "MOU", "GL1", "GL2", "GL3", "GL4", "PVC")
  
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

molten$ecotype <- ifelse(molten$Site %in% c("Townsend", "Englund", "Coral", "GLL4"), "Forest", "Savanna")

bycover <- aov(value ~ PDSI+ecotype, data = molten)


#molten.full comes from climate_growth_reg_chron.R
###################################################
#compare climate corelations 
plot.cor.clim <- function(x, Clim, xlab, Site){
 # x <- x[x$Site %in% site,]
  yr <- 1895:1950
  x$class <- '9999'
  x[x$Year %in% yr,]$class <- 'clim_record'
  
  
  #if the dummy variable is significant, then the two slopes are different
  print(summary( cor( x[,Clim], x$value)))
  
  # Extend the regression lines beyond the domain of the data
  df <- data.frame(value <-  x$value, 
                 Climate <- x[,Clim], 
                 Year <- x$Year)
  colnames(df)<- c("value", "Climate", "Year")
  print(summary(lm(value ~ Climate, data = df)))
  ggplot(df, aes(x=Climate, y= value)) + geom_point(shape=1) +
   # scale_colour_hue(l=50) +
    geom_smooth(method = 'lm') + #,   # Add linear regression lines
                #se=TRUE,    # add shaded confidence region
                #fullrange=FALSE)+# Extend regression lines
    
    #scale_color_manual(values=c('Pre-1950'="red",'Post-1950'="blue"))+
    xlim(-8, 8)+
    ylim(0.5, 1.5) +
    theme_bw()+
    theme(text = element_text(size = 30))+
    ylab('Detrended Ring width Index') +
    xlab( xlab ) +
    ggtitle(Site)
  
}

molten <- read.csv("./outputs/full_molten_chron.csv")
plot.cor.clim(x = molten[molten$Site %in% "Bonanza",], Clim = 'PDSI', xlab = "PDSI", Site = "Bonanza Prairie")
plot.cor.clim(molten[molten$Site %in% "Hickory",], "PDSI", "PDSI", "Hickory Grove")
plot.cor.clim(molten[molten$Site %in% "Coral",], "PDSI", "PDSI", "Coral Woods")
plot.cor.clim(molten[molten$Site %in% "Glacial",], "PDSI", "PDSI", "Glacial Park")
plot.cor.clim(molten[molten$Site %in% "StCroix",], "PDSI", "PDSI", "St. Croix Savanna")
plot.cor.clim(molten[molten$Site %in% "Townsend",], "PDSI", "PDSI", "Townsend Woods")
plot.cor.clim(molten[molten$Site %in% "Uncas",], "PDSI", "PDSI", "Uncas Dunes")
plot.cor.clim(molten[molten$Site %in% "Mound",], "PDSI", "PDSI", "Mound Prairie")
plot.cor.clim(molten[molten$Site %in% "Englund",], "PDSI", "PDSI", "Englund")
plot.cor.clim(molten[molten$Site %in% "GLL1",], "PDSI", "PDSI", "GLL1")
plot.cor.clim(molten[molten$Site %in% "GLL2",], "PDSI", "PDSI", "GLL2")
plot.cor.clim(molten[molten$Site %in% "GLL3",], "PDSI", "PDSI", "GLL3")
plot.cor.clim(molten[molten$Site %in% "GLL4",], "PDSI", "PDSI", "GLL4")
plot.cor.clim(molten[molten$Site %in% "PVC",], "PDSI", "PDSI", "PVC")
plot.cor.clim(molten[molten$Site %in% "Pleasant",], "PDSI", "PDSI", "Pleasant Valley")



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
    
    #yr <- 1895:1950
    #x$grow <- '9999'
    #x[x$Year %in% yr,]$class <- 'clim_record'
    
    
    #if the dummy variable is significant, then the two slopes are different
    print(summary( cor( x[,Clim], x$value)))
    df <- data.frame(value <-  x$value, 
                     Climate <- x[,Clim], 
                     Year <- x$Year, 
                     class <- x$class)
    colnames(df)<- c("value", "Climate", "Year", "class")
    
    
    #if the dummy variable is significant, then the two slopes are different
    print(summary(aov(value ~ Climate * group, data = df)))
    #print(summary(lm(value ~ Climate:group, data = x)))
    #print(summary(aov(value~Climate*class, data=x)))
    print(anova(lm(value ~ Climate*group, data = df), lm(value ~ Climate, data = df))
    )#print(summary(lm(value~Climate/group-1, data=x)))
    #print(summary(aov(value~Climate/group, data = x)))
    # Extend the regression lines beyond the domain of the data
    
    p<- ggplot(df, aes(x=Climate, y=value, colour=class)) + geom_point(shape=1) +
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
yr <- 1895:1950
yr.post <- 1950:2014
molten$class <- '9999'
molten[molten$Year %in% yr,]$class <- 'Pre-1950'
molten[molten$Year %in% yr.post,]$class <- 'Post-1950'
#create dummy variable
molten$group <- 0
molten[molten$Year %in% yr,]$group <- 1

sav<- aov(value ~ PDSI*class, data = molten[molten$ecotype %in% "Savanna",])

fores<- aov(value ~ PDSI*class, data = molten[molten$ecotype %in% "Forest",])

plot.pre.post(x = molten[molten$Site %in% "Hickory",], "PDSI", "PDSI", "Hickory Grove")#sig
plot.pre.post(x = molten[molten$Site %in% "Bonanza",], "PDSI", "PDSI", "Bonanaza")
plot.pre.post(x = molten[molten$Site %in% "StCroix",], "PDSI", "PDSI", "St. Croix Savanna")#sig
plot.pre.post(x = molten[molten$Site %in% "Townsend",], "PDSI", "PDSI", "Townsend")
plot.pre.post(x = molten[molten$Site %in% "Englund",], "PDSI", "PDSI", "Englund")
plot.pre.post(x = molten[molten$Site %in% "Uncas",], "PDSI", "PDSI", "Uncas")
plot.pre.post(x = molten[molten$Site %in% "Coral",], "PDSI", "PDSI", "Coral")#sig
plot.pre.post(x = molten[molten$Site %in% "Glacial",], "PDSI", "PDSI", "Glacial Park")#sig at 0.1
plot.pre.post(x = molten[molten$Site %in% "Mound",], "PDSI", "PDSI", "Mound Prairie")
plot.pre.post(x = molten[molten$Site %in% "GLL1",], "PDSI", "PDSI", "Glacial Lakes 1")#sig
plot.pre.post(x = molten[molten$Site %in% "GLL2",], "PDSI", "PDSI", "Glacial Lakes 2")#sig
plot.pre.post(x = molten[molten$Site %in% "GLL3",], "PDSI", "PDSI", "Glacial Lakes 3")
plot.pre.post(x = molten[molten$Site %in% "GLL4",], "PDSI", "PDSI", "Glacial Lakes 4")#sig
plot.pre.post(x = molten[molten$Site %in% "Pleasant",], "PDSI", "PDSI", "Pleasant")#sig
plot.pre.post(x = molten[molten$Site %in% "PVC",], "PDSI", "PDSI", "PVC")




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



















