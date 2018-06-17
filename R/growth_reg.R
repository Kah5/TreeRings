# this script makes a variety of climate-growth plots, as an initial data exploration:
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
# note these functions are for Whole Wood currently, but we could make plots for EW and LW
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
    h20bal <- read.csv(paste0("data/BootCors/",climatedata,"/",site.code, '-WWBALcor.csv'))
    VPDmax <- read.csv(paste0("data/BootCors/",climatedata,"/",site.code, '-WWVPDmaxcor.csv'))
    
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
    
    full$h20bal <- h20bal$cor
    cimin$h20bal <- h20bal$ci.min
    cimax$h20bal <- h20bal$ci.max
    
    full$VPDmax <- VPDmax$cor
    cimin$VPDmax <- VPDmax$ci.min
    cimax$VPDmax <- VPDmax$ci.max
    
    cors.melt <- melt(full, id.vars = c('months', 'mono'))
    cimin.melt <- melt(cimin, id.vars = c("months", "mono"))
    cimax.melt <- melt(cimax, id.vars = c("months", "mono"))
    colnames(cimin.melt) <- c('months', 'mono', "variable", "ci.min")
    colnames(cimax.melt) <- c('months', 'mono', "variable", "ci.max")
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
cor.barplot("COR", "PRISM")
cor.barplot('STC', "PRISM")
cor.barplot('BON', "PRISM")
cor.barplot('HIC', "PRISM")
cor.barplot('TOW', "PRISM")
cor.barplot('GLA', "PRISM")
cor.barplot('ENG', "PRISM")
cor.barplot('UNC', "PRISM")
cor.barplot('MOU', "PRISM")
cor.barplot('GL1', "PRISM")
cor.barplot('GL2', "PRISM")
cor.barplot('GL3', "PRISM")
cor.barplot('GL4', "PRISM")
cor.barplot('PVC', "PRISM")


#--------------------------------Plot by climate variables-------------------------
#now make a barplot for each climate factors with the sites on it using the sites.barplot funciton

#This is a set up tocolor code sites by their total mean annual precip (using GHCN data)
sites <- c("COR", "STC", "BON", "HIC", "TOW", "GLA", "ENG", "UNC", "MOU", "GL1", "GL2", "GL3", "GL4", "PVC")

precip <- matrix(NA ,nrow = length(sites), ncol = 2)
for (i in 1:length(sites)){
  precip[i,1] <- sites[i]
  a <- read.csv(paste0("data/climate/GHCN/", sites[i], "-annualP.csv"))
  precip[i,2] <- mean(a$PCP)
}
precip <- precip[order(as.numeric(precip[,2])),]
site.order <- rev(precip[,1])

tmax <- matrix(NA ,nrow = length(sites), ncol = 2)
for (i in 1:length(sites)){
  tmax[i,1] <- sites[i]
  a <- read.csv(paste0("data/climate/GHCN/",sites[i], "-annualtmax.csv"))
  tmax[i,2] <- mean(a$TMAX)
}
tmax <- tmax[order(as.numeric(tmax[,2])),]
site.order <- rev(tmax[,1])

tmin <- matrix(NA ,nrow = length(sites), ncol = 2)
for (i in 1:length(sites)){
  tmin[i,1] <- sites[i]
  a <- read.csv(paste0("data/climate/GHCN/",sites[i], "-annualtmin.csv"))
  tmin[i,2] <- mean(a$TMIN)
}
tmin <- tmin[order(as.numeric(tmin[,2])),]
site.order <- rev(tmin[,1])

PDSI <- matrix(NA ,nrow = length(sites), ncol = 2)
for (i in 1:length(sites)){
  PDSI[i,1] <- sites[i]
  a <- read.csv(paste0("data/climate/GHCN/",sites[i], "-annualPDSI.csv"))
  PDSI[i,2] <- mean(a$PDSI)
}
PDSI <- PDSI[order(as.numeric(PDSI[,2])),]
site.order <- rev(PDSI[,1])

#this function plots all the sites on the same barplot and color codes from driest to wettest
sites.barplot <- function(clim, climatedata) {
    COR <- read.csv(paste0('data/BootCors/',climatedata,'/COR-WW', clim, 'cor.csv'))
    HIC <- read.csv(paste0('data/BootCors/',climatedata,'/HIC-WW', clim, 'cor.csv'))
    GLA <- read.csv(paste0('data/BootCors/',climatedata,'/GLA-WW', clim, 'cor.csv'))
    STC <- read.csv(paste0('data/BootCors/',climatedata,'/STC-WW', clim, 'cor.csv'))
    TOW <- read.csv(paste0('data/BootCors/',climatedata,'/TOW-WW', clim, 'cor.csv'))
    ENG <- read.csv(paste0('data/BootCors/',climatedata,'/ENG-WW', clim, 'cor.csv'))
    UNC <- read.csv(paste0('data/BootCors/',climatedata,'/UNC-WW', clim, 'cor.csv'))
    BON <- read.csv(paste0('data/BootCors/',climatedata,'/BON-WW', clim, 'cor.csv'))
    MOU <- read.csv(paste0('data/BootCors/',climatedata,'/MOU-WW', clim, 'cor.csv'))
    GL1 <- read.csv(paste0('data/BootCors/',climatedata,'/GL1-WW', clim, 'cor.csv'))
    GL2 <- read.csv(paste0('data/BootCors/',climatedata,'/GL2-WW', clim, 'cor.csv'))
    GL3 <- read.csv(paste0('data/BootCors/',climatedata,'/GL3-WW', clim, 'cor.csv'))
    GL4 <- read.csv(paste0('data/BootCors/',climatedata,'/GL4-WW', clim, 'cor.csv'))
    PVC <- read.csv(paste0('data/BootCors/',climatedata,'/PVC-WW', clim, 'cor.csv'))
    
    months <- c("pJan", "pFeb", "pMar", "pApr", "pMay", "pJun", "pJul",
                "pAug", "pSep", "pOct", "pNov", "pDec",
                "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                "Aug", "Sep", "Oct", "Nov", "Dec")
    
    COR$months <- months
    colnames(COR) <- c('mono', 'COR', 'months', "ci.min", "ci.max", "months")
    ci.min <- COR[c('mono', 'months', "ci.min")]
    colnames(ci.min) <- c('mono', 'months', "COR")
    ci.max <- COR[c('mono', 'months', "ci.max")]
    colnames(ci.max) <- c('mono', 'months', "COR")
    
    
    ci.min$COR <- COR$ci.min
    ci.max$COR <- COR$ci.max
    
    full <- COR[,c('mono', 'months', "COR")]
    full$HIC <- HIC$cor
    full$GLA <- GLA$cor
    full$STC <- STC$cor
    full$TOW <- TOW$cor
    full$ENG <- ENG$cor
    full$UNC <- UNC$cor
    full$BON <- BON$cor
    full$MOU <- MOU$cor
    full$GL1 <- GL1$cor
    full$GL2 <- GL2$cor
    full$GL3 <- GL3$cor
    full$GL4 <- GL4$cor
    full$PVC <- PVC$cor
    
   
    ci.min$HIC <- HIC$ci.min
    ci.min$GLA <- GLA$ci.min
    ci.min$STC <- STC$ci.min
    ci.min$TOW <- TOW$ci.min
    ci.min$ENG <- ENG$ci.min
    ci.min$UNC <- UNC$ci.min
    ci.min$BON <- BON$ci.min
    ci.min$MOU <- MOU$ci.min
    ci.min$GL1 <- GL1$ci.min
    ci.min$GL2 <- GL2$ci.min
    ci.min$GL3 <- GL3$ci.min
    ci.min$GL4 <- GL4$ci.min
    ci.min$PVC <- PVC$ci.min
    ci.min$months <- months
    
    ci.max$HIC <- HIC$ci.max
    ci.max$GLA <- GLA$ci.max
    ci.max$STC <- STC$ci.max
    ci.max$TOW <- TOW$ci.max
    ci.max$ENG <- ENG$ci.max
    ci.max$UNC <- UNC$ci.max
    ci.max$BON <- BON$ci.max
    ci.max$MOU <- MOU$ci.max
    ci.max$GL1 <- GL1$ci.max
    ci.max$GL2 <- GL2$ci.max
    ci.max$GL3 <- GL3$ci.max
    ci.max$GL4 <- GL4$ci.max
    ci.max$PVC <- PVC$ci.max
    ci.max$months <- months
    
    half <- full[13:24,]
    half$months <- months[13:24]
    
    cors.melt <- melt(half, id.vars = c('months', 'mono'))
    #cors.melt$months <- factor(cors.melt$months, levels=full$months)
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
    
    ci.min.melt <- melt(ci.min, id.vars = c("months", "mono"))
    ci.max.melt <- melt(ci.max, id.vars = c( "months","mono"))
    colnames(ci.min.melt) <- c('months', 'mono', "variable", "ci.min")
    colnames(ci.max.melt) <- c('months', 'mono', "variable", "ci.max")
    m1 <- merge(cors.melt, ci.min.melt[,c('months', "variable", "ci.min")], by = c("months", "variable"))
    m2 <- merge(m1, ci.max.melt[,c('months', "variable", "ci.max")], by = c("months", "variable"))
    colnames(m2)[4:6] <- c("cor","ci.min", "ci.max")
    
    
    
    m2 <- merge(m2, sitesnames, by = "variable")
    m2$cor <- as.numeric(m2$cor)
    m2$months <- factor(m2$months, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                                                        "Aug", "Sep", "Oct", "Nov", "Dec"))
    output <- ggplot(data = m2, aes(months, cor, fill = Sites))+
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
      geom_errorbar(aes(ymin=ci.min, ymax=ci.max),                    # Width of the error bars
                    position=position_dodge(.9))+
      theme_bw()+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1)) + ggtitle(paste0(clim, " site correlations"))
    output
}

# climate variables for GHCN that I want to loop through:
clim.cd <- c('tavg', 'tmax', 'tmin', 'Precip', 'PDSI')

#automate savaing these barplots to individual png files
for(i in 1:length(clim.cd)){
  mypath <- file.path("/Users/kah/Documents/TreeRings/",paste("full_site_barplots_GHCN", clim.cd[i], ".png", sep = ""))
  sites.barplot(clim.cd[i], "GHCN")
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

e <- sites.barplot('tavg',"GHCN")+ggtitle("E). Average Temperature")+ylab("correlation")
c <- sites.barplot('tmax',"GHCN")+ ggtitle("C). Maximum Temperature")+ylab('correlation')
d <- sites.barplot('tmin',"GHCN")+ ggtitle("D). Minimum Temperature")+ylab('correlation')
b <- sites.barplot('Precip',"GHCN")+ ggtitle("B). Precipitation")+ylab('correlation')
a <- sites.barplot('PDSI',"GHCN")+ ggtitle("A). Palmer Drought Severity Index")+ylab('correlation')



#plot all barplots in png for interim report
png(width = 8, height = 10, units = 'in', res = 300, 'outputs/barplots/barplots_all_sites_fig2.png')
grid_arrange_shared_legend(a,b,c,d,e,nrow = 5, ncol = 1 )
dev.off()

png(width = 10, height = 8, units = 'in', res = 300, 'outputs/barplots/barplots_all_sites_3panel.png')
grid_arrange_shared_legend(a,b,c, nrow = 3, ncol = 1 )
dev.off()
clim <- "PDSI"
climatedata <- "GHCN"
# lets plot the correlations as a line for response:
sites.lineplot <- function(clim, climatedata) {
  COR <- read.csv(paste0('data/BootCors/',climatedata,'/COR-WW', clim, 'cor.csv'))
  HIC <- read.csv(paste0('data/BootCors/',climatedata,'/HIC-WW', clim, 'cor.csv'))
  GLA <- read.csv(paste0('data/BootCors/',climatedata,'/GLA-WW', clim, 'cor.csv'))
  STC <- read.csv(paste0('data/BootCors/',climatedata,'/STC-WW', clim, 'cor.csv'))
  TOW <- read.csv(paste0('data/BootCors/',climatedata,'/TOW-WW', clim, 'cor.csv'))
  ENG <- read.csv(paste0('data/BootCors/',climatedata,'/ENG-WW', clim, 'cor.csv'))
  UNC <- read.csv(paste0('data/BootCors/',climatedata,'/UNC-WW', clim, 'cor.csv'))
  BON <- read.csv(paste0('data/BootCors/',climatedata,'/BON-WW', clim, 'cor.csv'))
  MOU <- read.csv(paste0('data/BootCors/',climatedata,'/MOU-WW', clim, 'cor.csv'))
  GL1 <- read.csv(paste0('data/BootCors/',climatedata,'/GL1-WW', clim, 'cor.csv'))
  GL2 <- read.csv(paste0('data/BootCors/',climatedata,'/GL2-WW', clim, 'cor.csv'))
  GL3 <- read.csv(paste0('data/BootCors/',climatedata,'/GL3-WW', clim, 'cor.csv'))
  GL4 <- read.csv(paste0('data/BootCors/',climatedata,'/GL4-WW', clim, 'cor.csv'))
  PVC <- read.csv(paste0('data/BootCors/',climatedata,'/PVC-WW', clim, 'cor.csv'))
  
  months <- c("pJan", "pFeb", "pMar", "pApr", "pMay", "pJun", "pJul",
              "pAug", "pSep", "pOct", "pNov", "pDec",
              "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
              "Aug", "Sep", "Oct", "Nov", "Dec")
  
  COR$months <- months
  colnames(COR) <- c('mono', 'COR', 'months', "ci.min", "ci.max", "months")
  ci.min <- COR[c('mono', 'months', "ci.min")]
  colnames(ci.min) <- c('mono', 'months', "COR")
  ci.max <- COR[c('mono', 'months', "ci.max")]
  colnames(ci.max) <- c('mono', 'months', "COR")
  
  
  ci.min$COR <- COR$ci.min
  ci.max$COR <- COR$ci.max
  
  full <- COR[,c('mono', 'months', "COR")]
  full$HIC <- HIC$cor
  full$GLA <- GLA$cor
  full$STC <- STC$cor
  full$TOW <- TOW$cor
  full$ENG <- ENG$cor
  full$UNC <- UNC$cor
  full$BON <- BON$cor
  full$MOU <- MOU$cor
  full$GL1 <- GL1$cor
  full$GL2 <- GL2$cor
  full$GL3 <- GL3$cor
  full$GL4 <- GL4$cor
  full$PVC <- PVC$cor
  
  
  ci.min$HIC <- HIC$ci.min
  ci.min$GLA <- GLA$ci.min
  ci.min$STC <- STC$ci.min
  ci.min$TOW <- TOW$ci.min
  ci.min$ENG <- ENG$ci.min
  ci.min$UNC <- UNC$ci.min
  ci.min$BON <- BON$ci.min
  ci.min$MOU <- MOU$ci.min
  ci.min$GL1 <- GL1$ci.min
  ci.min$GL2 <- GL2$ci.min
  ci.min$GL3 <- GL3$ci.min
  ci.min$GL4 <- GL4$ci.min
  ci.min$PVC <- PVC$ci.min
  ci.min$months <- months
  
  ci.max$HIC <- HIC$ci.max
  ci.max$GLA <- GLA$ci.max
  ci.max$STC <- STC$ci.max
  ci.max$TOW <- TOW$ci.max
  ci.max$ENG <- ENG$ci.max
  ci.max$UNC <- UNC$ci.max
  ci.max$BON <- BON$ci.max
  ci.max$MOU <- MOU$ci.max
  ci.max$GL1 <- GL1$ci.max
  ci.max$GL2 <- GL2$ci.max
  ci.max$GL3 <- GL3$ci.max
  ci.max$GL4 <- GL4$ci.max
  ci.max$PVC <- PVC$ci.max
  ci.max$months <- months
  
  half <- full[13:24,]
  half$months <- months[13:24]
  
  cors.melt <- melt(half, id.vars = c('months', 'mono'))
  #cors.melt$months <- factor(cors.melt$months, levels=full$months)
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
  
  ci.min.melt <- melt(ci.min, id.vars = c("months", "mono"))
  ci.max.melt <- melt(ci.max, id.vars = c( "months","mono"))
  colnames(ci.min.melt) <- c('months', 'mono', "variable", "ci.min")
  colnames(ci.max.melt) <- c('months', 'mono', "variable", "ci.max")
  m1 <- merge(cors.melt, ci.min.melt[,c('months', "variable", "ci.min")], by = c("months", "variable"))
  m2 <- merge(m1, ci.max.melt[,c('months', "variable", "ci.max")], by = c("months", "variable"))
  colnames(m2)[4:6] <- c("cor","ci.min", "ci.max")
  
  
  
  m2 <- merge(m2, sitesnames, by = "variable")
  m2$cor <- as.numeric(m2$cor)
  m2$months <- factor(m2$months, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                                            "Aug", "Sep", "Oct", "Nov", "Dec"))
  m2$months2 <- as.numeric(m2$months)
  output <- ggplot(data = m2, aes(months2, cor, color = Sites))+geom_line()+ylab("Correlation with PDSI")+xlab("Month")+scale_x_continuous(breaks = 1:12, labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                                                                                                                                                                       "Aug", "Sep", "Oct", "Nov", "Dec"))+
    #facet_grid(variable~.)+
   scale_fill_manual("Sites",values = c('#a6cee3','#1f78b4','#b2df8a',
                                         '#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00',
                                         '#cab2d6','#6a3d9a','#ffff99','#00441b','#800026','#49006a', 'black'), 
                      limits=c("Bonanza Prairie SNA, MN","Glacial Lakes 1, MN",
                               "Glacial Lakes 2, MN", "Glacial Lakes 3, MN", "Glacial Lakes 4, MN","Townsend Woods SNA, MN", "Mound Prairie SNA, MN",
                               "Englund Ecotone SNA, MN", "Uncas Dunes SNA, MN", "St.Croix Savanna SNA, MN",
                               "Glacial Park, IL", "Coral Woods, IL", "Hickory Grove, IL","Pleasant Valley Conservancy, IL"))+
   # geom_errorbar(aes(ymin=ci.min, ymax=ci.max),                    # Width of the error bars
    #              position=position_dodge(.9))+
    theme_bw()+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1)) 
  output
}

png(height = 5, width = 7, units = "in", res = 300, "outputs/correlations/monthly_lineplot_allsites_PDSI.png")
sites.lineplot("PDSI", "GHCN")+theme_black(base_size = 12)
dev.off()

png(height = 5, width = 5, units = "in", res = 300, "outputs/correlations/monthly_lineplot_allsites_tavg.png")
sites.lineplot("tavg", "GHCN")
dev.off()

png(height = 5, width = 5, units = "in", res = 300, "outputs/correlations/monthly_lineplot_allsites_tmax.png")
sites.lineplot("tmax", "GHCN")
dev.off()

png(height = 5, width = 5, units = "in", res = 300, "outputs/correlations/monthly_lineplot_allsites_tmin.png")
sites.lineplot("tmin", "GHCN")
dev.off()

png(height = 5, width = 5, units = "in", res = 300, "outputs/correlations/monthly_lineplot_allsites_precip.png")
sites.lineplot("Precip", "GHCN")
dev.off()
# instead of barplots, lets create tile plots:
sites.tile <- function(clim, climatedata) {
  
  COR <- read.csv(paste0('data/BootCors/',climatedata,'/COR-WW', clim, 'cor.csv'))
  HIC <- read.csv(paste0('data/BootCors/',climatedata,'/HIC-WW', clim, 'cor.csv'))
  GLA <- read.csv(paste0('data/BootCors/',climatedata,'/GLA-WW', clim, 'cor.csv'))
  STC <- read.csv(paste0('data/BootCors/',climatedata,'/STC-WW', clim, 'cor.csv'))
  TOW <- read.csv(paste0('data/BootCors/',climatedata,'/TOW-WW', clim, 'cor.csv'))
  ENG <- read.csv(paste0('data/BootCors/',climatedata,'/ENG-WW', clim, 'cor.csv'))
  UNC <- read.csv(paste0('data/BootCors/',climatedata,'/UNC-WW', clim, 'cor.csv'))
  BON <- read.csv(paste0('data/BootCors/',climatedata,'/BON-WW', clim, 'cor.csv'))
  MOU <- read.csv(paste0('data/BootCors/',climatedata,'/MOU-WW', clim, 'cor.csv'))
  GL1 <- read.csv(paste0('data/BootCors/',climatedata,'/GL1-WW', clim, 'cor.csv'))
  GL2 <- read.csv(paste0('data/BootCors/',climatedata,'/GL2-WW', clim, 'cor.csv'))
  GL3 <- read.csv(paste0('data/BootCors/',climatedata,'/GL3-WW', clim, 'cor.csv'))
  GL4 <- read.csv(paste0('data/BootCors/',climatedata,'/GL4-WW', clim, 'cor.csv'))
  PVC <- read.csv(paste0('data/BootCors/',climatedata,'/PVC-WW', clim, 'cor.csv'))
  
  months <- c("pJan", "pFeb", "pMar", "pApr", "pMay", "pJun", "pJul",
              "pAug", "pSep", "pOct", "pNov", "pDec",
              "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
              "Aug", "Sep", "Oct", "Nov", "Dec")
  
  COR$months <- months
  colnames(COR) <- c('mono', 'COR', 'months', "ci.min", "ci.max", "months")
  ci.min <- COR[c('mono', 'months', "ci.min")]
  colnames(ci.min) <- c('mono', 'months', "COR")
  ci.max <- COR[c('mono', 'months', "ci.max")]
  colnames(ci.max) <- c('mono', 'months', "COR")
  
  
  ci.min$COR <- COR$ci.min
  ci.max$COR <- COR$ci.max
  
  full <- COR[,c('mono', 'months', "COR")]
  full$HIC <- HIC$cor
  full$GLA <- GLA$cor
  full$STC <- STC$cor
  full$TOW <- TOW$cor
  full$ENG <- ENG$cor
  full$UNC <- UNC$cor
  full$BON <- BON$cor
  full$MOU <- MOU$cor
  full$GL1 <- GL1$cor
  full$GL2 <- GL2$cor
  full$GL3 <- GL3$cor
  full$GL4 <- GL4$cor
  full$PVC <- PVC$cor
  
  
  ci.min$HIC <- HIC$ci.min
  ci.min$GLA <- GLA$ci.min
  ci.min$STC <- STC$ci.min
  ci.min$TOW <- TOW$ci.min
  ci.min$ENG <- ENG$ci.min
  ci.min$UNC <- UNC$ci.min
  ci.min$BON <- BON$ci.min
  ci.min$MOU <- MOU$ci.min
  ci.min$GL1 <- GL1$ci.min
  ci.min$GL2 <- GL2$ci.min
  ci.min$GL3 <- GL3$ci.min
  ci.min$GL4 <- GL4$ci.min
  ci.min$PVC <- PVC$ci.min
  ci.min$months <- months
  
  ci.max$HIC <- HIC$ci.max
  ci.max$GLA <- GLA$ci.max
  ci.max$STC <- STC$ci.max
  ci.max$TOW <- TOW$ci.max
  ci.max$ENG <- ENG$ci.max
  ci.max$UNC <- UNC$ci.max
  ci.max$BON <- BON$ci.max
  ci.max$MOU <- MOU$ci.max
  ci.max$GL1 <- GL1$ci.max
  ci.max$GL2 <- GL2$ci.max
  ci.max$GL3 <- GL3$ci.max
  ci.max$GL4 <- GL4$ci.max
  ci.max$PVC <- PVC$ci.max
  ci.max$months <- months
  
  half <- full[13:24,]
  half$months <- months[13:24]
  
  cors.melt <- melt(half, id.vars = c('months', 'mono'))
  #cors.melt$months <- factor(cors.melt$months, levels=full$months)
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
  
  ci.min.melt <- melt(ci.min, id.vars = c("months", "mono"))
  ci.max.melt <- melt(ci.max, id.vars = c( "months","mono"))
  colnames(ci.min.melt) <- c('months', 'mono', "variable", "ci.min")
  colnames(ci.max.melt) <- c('months', 'mono', "variable", "ci.max")
  m1 <- merge(cors.melt, ci.min.melt[,c('months', "variable", "ci.min")], by = c("months", "variable"))
  m2 <- merge(m1, ci.max.melt[,c('months', "variable", "ci.max")], by = c("months", "variable"))
  colnames(m2)[4:6] <- c("cor","ci.min", "ci.max")
  
  
  
  m2 <- merge(m2, sitesnames, by = "variable")
  m2$cor <- as.numeric(m2$cor)
  m2$months <- factor(m2$months, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                                            "Aug", "Sep", "Oct", "Nov", "Dec"))
  
  output<- ggplot(m2, aes(months, variable, fill = cor))+geom_tile()+
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                        name="Pearson\nCorrelation")+ ggtitle(paste0(clim, " site correlations"))+
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  output
}

# climate variables to loop over
clim.cd <- c('tavg', 'tmax', 'tmin', 'Precip', 'PDSI')
sites.tile("tavg", "GHCN")

e <- sites.tile('tavg', "GHCN")+ggtitle("E). Average Temperature")+ylab("correlation")
c <- sites.tile('tmax', "GHCN")+ ggtitle("C). Maximum Temperature")+ylab('correlation')
d <- sites.tile('tmin', "GHCN")+ ggtitle("D). Minimum Temperature")+ylab('correlation')
#b <- sites.tile('Precip', "GCHN")+ ggtitle("B). Precipitation")+ylab('correlation')
a <- sites.tile('PDSI', "GHCN")+ ggtitle("A). Palmer Drought Severity Index")+ylab('correlation')

png(width = 10, height = 8, units = 'in', res = 300, 'outputs/barplots/tileplot_GHCN_cors_all_sites_3panel.png')
grid_arrange_shared_legend(a,b,c,d,e, nrow = 3, ncol = 2 )
dev.off()
# tile plot corrs for prism
e <- sites.tile('tavg', "PRISM")+ggtitle("E). Average Temperature")+ylab("correlation")
c <- sites.tile('tmax', "PRISM")+ ggtitle("C). Maximum Temperature")+ylab('correlation')
d <- sites.tile('tmin', "PRISM")+ ggtitle("D). Minimum Temperature")+ylab('correlation')
b <- sites.tile('Precip', "PRISM")+ ggtitle("B). Precipitation")+ylab('correlation')
a <- sites.tile('VPDmax', "PRISM")+ ggtitle("A). VPDmax")+ylab('correlation')
f <- sites.tile('BAL', "PRISM")+ ggtitle("A). Climatic Water Deficit P - PET")+ylab('correlation')
png(width = 10, height = 8, units = 'in', res = 300, 'outputs/barplots/tileplot_PRISM_cors_all_sites_3panel.png')
grid_arrange_shared_legend(a,b,c,d,e,f, nrow = 3, ncol = 2 )
dev.off()


# ------------Is there a variable/month that has highest correlation across all sites?
#------------------rank the correlations based on highest to lowest for each site
highest.cor <- function(site, climatedata,i){
  
  if(climatedata == "PRISM"){
  # read in the bootstrapped corrlations for each climate variable:
  tavg <- read.csv(paste0("data/BootCors/PRISM/",site, '-WWtavgcor.csv'))
  tmin <- read.csv(paste0("data/BootCors/PRISM/",site, '-WWtmincor.csv'))
  tmax <- read.csv(paste0("data/BootCors/PRISM/",site, '-WWtmaxcor.csv'))
  precip <- read.csv(paste0("data/BootCors/PRISM/",site, '-WWPrecipcor.csv'))
  h20bal <- read.csv(paste0("data/BootCors/PRISM/",site, '-WWBALcor.csv'))
  VPDmax <- read.csv(paste0("data/BootCors/PRISM/",site, '-WWVPDmaxcor.csv'))
  clim.cors <- cbind(precip[,1:2], tavg[,2], tmin[,2], tmax[,2], h20bal[,2], VPDmax[,2])
  colnames(clim.cors) <- c("mono", "Precip", "tavg", "tmin", "tmax", "P-PET", "VPDmax")
  clim.cors$months <- c("pJan", "pFeb", "pMar", "pApr", "pMay", "pJun", "pJul",
                        "pAug", "pSep", "pOct", "pNov", "pDec",
                        "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                        "Aug", "Sep", "Oct", "Nov", "Dec")
  
  melted.cors <- melt(clim.cors, id.vars = c('mono', "months"))
  reorded.cors <- melted.cors[rev(order(abs(melted.cors[, "value"]))),]
  reorded.cors[i,] # get the highest corrlated variable
  
  }else{
  precip <- read.csv(paste0("data/BootCors/GHCN/",site,'-WW', 'Precip', 'cor.csv'))
  tavg <- read.csv(paste0("data/BootCors/GHCN/",site,'-WW', 'tavg', 'cor.csv'))
  tmin <- read.csv(paste0("data/BootCors/GHCN/",site,'-WW', 'tmin', 'cor.csv'))
  tmax <- read.csv(paste0("data/BootCors/GHCN/",site,'-WW', 'tmax', 'cor.csv'))
  PDSI <- read.csv(paste0("data/BootCors/GHCN/",site,'-WW', 'PDSI', 'cor.csv'))
  
  
  clim.cors <- cbind(precip[,1:2], tavg[,2], tmin[,2], tmax[,2], PDSI[,2])
  colnames(clim.cors) <- c("mono", "Precip", "tavg", "tmin", "tmax", "PDSI")
  clim.cors$months <- c("pJan", "pFeb", "pMar", "pApr", "pMay", "pJun", "pJul",
                        "pAug", "pSep", "pOct", "pNov", "pDec",
                        "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                        "Aug", "Sep", "Oct", "Nov", "Dec")
  
  melted.cors <- melt(clim.cors, id.vars = c('mono', "months"))
  reorded.cors <- melted.cors[rev(order(abs(melted.cors[, "value"]))),]
  reorded.cors[i,] # get the highest corrlated variable
  }
}
# for GHCN Data
Highest <- list()
for(j in 1:length(site.cd)){
  Highest[[j]] <- highest.cor(site.cd[j], "GHCN", 1)
}

Highest <- do.call(rbind, Highest)

Highest$site <- c('HIC', "BON", "COR", "GLA", "STC", "ENG", "UNC", "TOW", "MOU", "GL1", "GL2", "GL3", "GL4", "PVC")

write.csv(Highest, "outputs/highest_cors_GHCN_table.csv")

# for PRISM data
Highest.p <- list()
for(j in 1:length(site.cd)){
  Highest.p[[j]] <- highest.cor(site.cd[j], "PRISM", 1)
}

Highest.p <- do.call(rbind, Highest.p) #bind together in a table

Highest.p$site <- c('HIC', "BON", "COR", "GLA", "STC", "ENG", "UNC", "TOW", "MOU", "GL1", "GL2", "GL3", "GL4", "PVC")
write.csv(Highest.p, "outputs/highest_cors_PRISM_table.csv")

colnames(Highest.p) <- c("prism_mono", "prism_months", "prism_variable", "prism_value", "site")
all.high <- merge(Highest, Highest.p, by = "site")

# map out the highest correlations:

locs <- read.csv("outputs/priority_sites_locs.csv")
locs$code <- as.character(locs$code)
#locs[9:12,]$code <- c( "GLL1", "GLL2", "GLL3", "GLL4")
#sites <- c("COR", "HIC", "STC", "GLA", "TOW", "ENG", "UNC", "BON", "MOU", "GLL4", "GLL3", "GLL2", "GLL1", "PVC", "AVO", "PLE", "UNI")

locs.highest <- merge(locs, Highest, by.x = "code", by.y = "site")
locs.highest$variable <- ifelse(locs.highest$variable %in% "PDSI", "PDSI & VPDmax", paste(locs.highest$variable))
locs.highest$highestcor <- paste(locs.highest$months, locs.highest$variable )

ggplot(locs.highest, aes(coords.x1, coords.x2, color = highestcor))+geom_point(size = 2)+theme_black()



highest.map <- ggplot()+geom_point(data = locs.highest, aes(coords.x1, coords.x2, color = highestcor))+scale_color_manual(values = c(
  '#fdcc8a',
  '#fc8d59',
  '#e34a33',
  '#b30000',
  '#984ea3',
  '#fef0d9',
  '#386cb0'
  ))+ 
geom_polygon(data=data.frame(mapdata), aes(x=long, y=lat, group=group),
                                             colour = "darkgrey", fill = NA, size  = 0.8)+theme_black()+
  coord_cartesian(xlim = c(-59495.64, 725903.4), ylim=c(308821.43, 1380021))+
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        legend.key = element_rect(),
        legend.title = element_blank(),
        
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

png("outputs/bw_tree_highes_cors_map.png")
highest.map
dev.off()

# get the highest correlation between either prism VPD and pdsi

#---------- Does correlation with climate vary according to mean climate?-------------
#now plot the correlations against their mean annual precips MAP from GHCN, but can use prism data for correlations:
cor.v.clim <- function(climatedata, mo, climatevar, var){
  
  locs <- read.csv("outputs/priority_sites_locs.csv")
  sites <- c("COR", "HIC", "STC", "GLA", "TOW", "ENG", "UNC", "BON", "MOU", "GL1", "GL2", "GL3", "GL4", "PVC")

  precip <- matrix(NA ,nrow = length(sites), ncol = 2)
  for (i in 1:length(sites)){
    precip[i,1] <- sites[i]
    a <- read.csv(paste0("data/climate/", sites[i], "-annualP.csv"))
    precip[i,2] <- mean(a$PCP)
  }
  
  precip <- precip[order(as.numeric(precip[,2])),]
  site.order <- rev(precip[,1])
  precip <- data.frame(precip)
  colnames(precip) <- c("site", "MAP")
  precip <- merge(precip, locs, by.x = 'site', by.y = 'code')
  month.coef <- matrix(NA, nrow = length(precip[,1]), ncol = 3)
  
  for(i in 1:length(precip[,1])){
  cors <- read.csv(paste0("data/BootCors/",climatedata,"/", as.character(precip[i,]$site),"-WW", climatevar, "cor.csv"))
  cors$month <- c("pJan", "pFeb", "pMar", "pApr", "pMay", "pJun", "pJul",
                            "pAug", "pSep", "pOct", "pNov", "pDec",
                            "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                            "Aug", "Sep", "Oct", "Nov", "Dec")
  month.coef[i,1] <- cors[cors$month %in% mo,]$cor
  month.coef[i,2] <- cors[cors$month %in% mo,]$ci.max
  month.coef[i,3] <- cors[cors$month %in% mo,]$ci.min
  }
  
x <- as.data.frame(precip)
x$cor <- as.vector(month.coef[,1])
x$ci.max <- as.vector(month.coef[,2])
x$ci.min <- as.vector(month.coef[,3])

if(var %in% "MAP"){
  x$env <- as.numeric(as.character(x$MAP))
}else{if (var %in% "awc"){
  x$env <- precip$awc
}else{if (var %in% "ksat"){
x$env <- precip$ksat
}else{if (var %in% "DBH"){
  x$env <- precip$DBH
}else{if (var %in% "sand"){
  x$env <- precip$sand
  }else{
   precip$env <- 8888
 }
}
}
}
}
lm <- lm(formula = cor ~ env, data = x)
lm2 <- summary(lm)
print(summary(lm))
ggplot(data = x, aes(env, cor,  color = cor))+geom_point(size = 5)+geom_errorbar( aes(ymin = ci.min, ymax = ci.max))+
  stat_smooth(method = 'lm', color = "white")+scale_color_continuous(low = 'red', high = 'blue', name = "Correlation \n coefficient")+ggtitle(paste0(mo," ",climatevar, " correlation with ", var))+
  xlab(var)+ylab(paste0(climatevar," ",mo," correlation coefficient"))+geom_text(aes(label=as.character(site)),hjust=0, vjust=2) +scale_x_continuous(expand= c(0.25,0.5)) +scale_y_continuous(expand = c(0.25, 0))+
  theme_black(base_size = 15) #+annotate(geom = 'text', x = min(x$env), y = -0.55, label =paste0("R-squared = ",lm2$coefficients[2,4]), color= "white")

}

clim.ghcn <- c( "tavg",   "tmax",   "tmin",   "Precip", "PDSI" )
#plot these in pngs for may and august for MAP:
for(i in 1:length(clim.ghcn)){
  mypath <- file.path("/Users/kah/Documents/TreeRings/outputs/correlations/static_site_cors/GHCN",paste("site_cor_MAP_jun", clim.ghcn[i], ".png", sep = ""))
  cor.v.clim("GHCN","Jun", climatevar = clim.ghcn[i], var = "MAP")
  ggsave(filename=mypath)
}

clim.prism <- c( "tavg",   "tmax",   "tmin",   "Precip", "VPDmax", "BAL" )
for(i in 1:length(clim.prism)){
  mypath <- file.path("/Users/kah/Documents/TreeRings/outputs/correlations/static_site_cors/PRISM",paste("site_cor_MAP_jun", clim.prism[i], ".png", sep = ""))
  cor.v.clim("PRISM","Jun", climatevar = clim.prism[i], var = "MAP")
  ggsave(filename=mypath)
}



# singling out some significant relationships:
cor.v.clim(climatedata = "PRISM",mo = "Jun", climatevar = "tavg", var = "DBH")+ ggtitle(" ")+ylab("Correlation with June Average Temperature")+xlab( "% Sand")

cor.v.clim("PRISM","Jun", climatevar = "tavg", var = "sand")+ ggtitle(" ")+ylab("Correlation with June Average Temperature")+xlab( "% Sand")
ggsave("outputs/correlations/static_site_cors/PRISM_site_cor_sand_junTavg.png")

cor.v.clim("PRISM","Aug", climatevar = "tmin", var = "sand")+ ggtitle(" ")+ylab("Correlation with August Minimum Temperature")+xlab( "% Sand")
ggsave("outputs/correlations/static_site_cors/PRISM_site_cor_sand_augTmin.png")

cor.v.clim("PRISM","Sep", climatevar = "tmax", var = "sand") + ggtitle(" ")+ylab("Correlation with September TMAX")+xlab( "% Sand")# sept tmax sig 0.024, rsquared = 0.3012
ggsave("outputs/correlations/static_site_cors/PRISM_site_cor_sand_junTavg.png")

cor.v.clim("PRISM","Jul", climatevar = "BAL", var = "MAP") + ggtitle(" ")+ylab("Correlation with July Water Balance (P-PET)")+xlab( "Mean Annual Precipitation (mm/yr)")# sig p = 0.025, R-sq = 0.35
ggsave("outputs/correlations/static_site_cors/PRISM_site_cor_MAP_julBAL.png")

cor.v.clim("PRISM","Jul", climatevar = "VPDmax", var = "DBH") + ggtitle(" ")+ylab("Correlation with July Water Balance (P-PET)")+xlab( "Mean Annual Precipitation (mm/yr)")# sig p = 0.025, R-sq = 0.35
ggsave("outputs/correlations/static_site_cors/PRISM_site_cor_DBH_vpdmaxL.png")

cor.v.clim("PRISM","Sep", climatevar = "precip", var = "MAP") + ggtitle(" ")+ylab("Correlation with July Water Balance (P-PET)")+xlab( "Mean Annual Precipitation (mm/yr)")# sig p = 0.025, R-sq = 0.35
ggsave("outputs/correlations/static_site_cors/PRISM_site_cor_DBH_vpdmaxL.png")


cor.v.clim("PRISM","Jul", climatevar = "precip", var = "sand") + ggtitle(" ")+ylab("Correlation with July Precipitation")+xlab( "% Sand") # sig p = 0.00047, r-sq = 0.63
ggsave("outputs/correlations/static_site_cors/PRISM_site_cor_sand_julPrecip.png")

cor.v.clim("PRISM","Jun", climatevar = "precip", var = "sand")+ ggtitle(" ")+ylab("Correlation with June Precipitation")+xlab( "% Sand")
ggsave("outputs/correlations/static_site_cors/PRISM_site_cor_sand_junprecip.png")

cor.v.clim("GHCN","Sep", climatevar = "PDSI", var = "sand") + ggtitle(" ")+ylab("Correlation with September PDSI")+xlab( "% Sand")
ggsave("outputs/correlations/static_site_cors/PRISM_site_cor_sand_SeptPDSI.png")

cor.v.clim("GHCN","Aug", climatevar = "PDSI", var = "sand")+ ggtitle(" ")+ylab("Correlation with August PDSI")+xlab( "% Sand")
ggsave("outputs/correlations/static_site_cors/PRISM_site_cor_sand_AugPDSI.png")

cor.v.clim("GHCN","Jul", climatevar = "PDSI", var = "sand") + ggtitle(" ")+ylab("Correlation with July PDSI")+xlab( "% Sand")
ggsave("outputs/correlations/static_site_cors/PRISM_site_cor_sand_JulPDSI.png")


cor.v.clim("GHCN","Jul", climatevar = "PDSI", var = "MAP") + ggtitle(" ")+ylab("Correlation with July PDSI")+xlab( "Mean Annual Precipitation (mm)")
ggsave("outputs/correlations/static_site_cors/PRISM_site_cor_MAP_JulPDSI.png")

cor.v.clim("GHCN","Jul", climatevar = "PDSI", var = "DBH") + ggtitle(" ")+ylab("Correlation with July PDSI")+xlab( "Average DBH")
ggsave("outputs/correlations/static_site_cors/PRISM_site_cor_DBH_JulPDSI.png")

#cor.v.clim("GHCN","Aug", climatevar = "PDSI", var = "sand")

#can use the cor.v.clim function to plot correlations against soil characteristics
#read in site xys
locs <- read.csv("outputs/priority_sites_locs.csv")
sites <- c("COR", "HIC", "STC", "GLA", "TOW", "ENG", "UNC", "BON", "MOU", "GL1", "GL2", "GL3", "GL4", "PVC")
precip <- matrix(NA ,nrow = length(sites), ncol = 2)
for (i in 1:length(sites)){
  precip[i,1] <- sites[i]
  a <- read.csv(paste0("data/climate/",sites[i], "-annualP.csv"))
  precip[i,2] <- mean(a$PCP)
}
precip <- precip[order(as.numeric(precip[,2])),]
site.order <- rev(precip[,1])
precip <- data.frame(precip)
colnames(precip) <- c("site", "MAP")
pre <- merge(precip, locs, by.x = 'site', by.y = 'code')


# plot correlation coefficients with monthly climate variables against ksat

mos <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Nov", "Dec")
for(i in 1:length(mos)){
  
  #for awc
  mypath <- file.path("/Users/kah/Documents/TreeRings/outputs/correlations/static_site_cors",paste("PRISM_site_cor_awc_",mos[i], "tavg", ".png", sep = ""))
  cor.v.clim("PRISM", mos[i], climatevar = "tavg", var = "awc")
  ggsave(filename=mypath)
  
  #for ksat
  mypath <- file.path("/Users/kah/Documents/TreeRings/outputs/correlations/static_site_cors",paste("PRISM_site_cor_ksat_", mos[i], "-tavg.png", sep = ""))
  cor.v.clim("PRISM",mos[i], climatevar = "tavg", var = "ksat")
  ggsave(filename=mypath)
  
  #for sand
  mypath <- file.path("/Users/kah/Documents/TreeRings/outputs/correlations/static_site_cors",paste("PRISM_site_cor_sand_jun", mos[i], "-tavg.png", sep = ""))
  cor.v.clim("PRISM",mos[i], climatevar = "tavg", var = "sand")
  ggsave(filename=mypath)

  
}

pdf("outputs/cor_PRISM_coef_v_ksat.pdf")
cor.v.clim("PRISM","Jun", climatevar = "tavg", var = "ksat")
cor.v.clim("PRISM","Jun", climatevar = "tmin", var = "ksat")
cor.v.clim("PRISM","Jun", climatevar = "tmax", var = "ksat")
cor.v.clim("PRISM","Jun", climatevar = "Precip", var = "ksat")
cor.v.clim("PRISM","Jun", climatevar = "VPDmax", var = "ksat")
cor.v.clim("PRISM","Jun", climatevar = "BAL", var = "ksat")
dev.off()

#plot correlation coefficients with July climate variabes with awc
pdf("outputs/cor_PRISM_coef_v_awc.pdf")
cor.v.clim("PRISM","Jun", climatevar = "tavg", var = "awc")
cor.v.clim("PRISM","Jun", climatevar = "tmin", var = "awc")
cor.v.clim("PRISM","Jun", climatevar = "tmax", var = "awc")
cor.v.clim("PRISM","Jun", climatevar = "Precip", var = "awc")
cor.v.clim("PRISM","Jun", climatevar = "VPDmax", var = "awc")
cor.v.clim("PRISM","Jun", climatevar = "BAL", var = "awc")
dev.off()

b <- cor.v.clim("PRISM","Jun", climatevar = "tavg", var = "sand") + ylab('June Tavg correlation coefficient') + ggtitle('June PDSI correlation')
c <- cor.v.clim("PRISM","Jun", climatevar = "tmin", var = "sand")
d <- cor.v.clim("PRISM", "Jun", climatevar = "Precip", var = 'sand')+ ylab('Correlation coefficient') + ggtitle('June Precipitation correlation')
a <- cor.v.clim("PRISM", "Aug",climatevar = "Precip", var = 'sand')+ ylab('Correlation coefficient') + ggtitle('A). August Precipitation')


#print out a PNG
png(width = 5, height = 9, units = 'in', res = 300,'outputs/correlations/all_site_cor_v_sand.png')
pushViewport(viewport(layout = grid.layout(3, 1)))
print(a, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(b, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(c, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
dev.off()

#plot correlations for savanna and forests
cor.v.type <- function(climatedata, mo, climatevar, var){
  locs <- read.csv("outputs/priority_sites.csv")
  sites <- c("COR", "HIC", "STC", "GLA", "TOW", "ENG", "UNC", "BON", "MOU", "GL1", "GL2", "GL3", "GL4", "PVC")
  
  precip <- matrix(NA ,nrow = length(sites), ncol = 2)
  for (i in 1:length(sites)){
    precip[i,1] <- sites[i]
    a <- read.csv(paste0("data/climate/GHCN/", sites[i], "-annualP.csv"))
    precip[i,2] <- mean(a$PCP)
  }
  
  precip <- precip[order(as.numeric(precip[,2])),]
  site.order <- rev(precip[,1])
  precip <- data.frame(precip)
  colnames(precip) <- c("site", "MAP")
  precip <- merge(precip, locs, by.x = 'site', by.y = 'code')
  month.coef <- matrix(NA, nrow = length(precip[,1]), ncol = 3)
  
  for(i in 1:length(precip[,1])){
    cors <- read.csv(paste0("data/BootCors/",climatedata,"/", as.character(precip[i,]$site),"-WW", climatevar, "cor.csv"))
    cors$month <- c("pJan", "pFeb", "pMar", "pApr", "pMay", "pJun", "pJul",
                    "pAug", "pSep", "pOct", "pNov", "pDec",
                    "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                    "Aug", "Sep", "Oct", "Nov", "Dec")
    month.coef[i,1] <- cors[cors$month %in% mo,]$cor
    month.coef[i,2] <- cors[cors$month %in% mo,]$ci.max
    month.coef[i,3] <- cors[cors$month %in% mo,]$ci.min
    
  }
  
  x <- as.data.frame(precip)
  x$cor <- as.vector(month.coef[,1])
  x$ci.max <- as.vector(month.coef[,2])
  x$ci.min <- as.vector(month.coef[,3])
  
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
  ggplot(x, aes(Description, cor, fill = Description))+ geom_boxplot()+theme_bw()+ylab(paste0("Correlation with ", mos," ",climatevar))#geom_text_repel(aes(label = site))+
    #scale_color_continuous(low = 'red', high = 'blue')+geom_point(size = 5, aes(shape = Description))+stat_smooth(method = 'lm')+theme_bw()+ggtitle(paste0(clim, " correlation with ", var))+
   # xlab(var)+ylab(paste0(clim," correlation coefficient"))+geom_text(aes(label=as.character(site)),hjust=0, vjust=2) +scale_x_continuous(expand= c(0.25,0.5)) +scale_y_continuous(expand = c(0.25, 0))
  
}

cor.v.type("PRISM","Jun", climatevar = "tavg", var = "awc")
cor.v.type("PRISM","Jun", climatevar = "tmin", var = "awc")
cor.v.type("PRISM","Jun", climatevar = "tmax", var = "awc")
cor.v.type("PRISM","Jun", climatevar = "Precip", var = "awc")
cor.v.type("PRISM","Jun", climatevar = "VPDmax", var = "awc")
cor.v.type("PRISM","Jun", climatevar = "BAL", var = "awc")

# plot mean growth at each site:
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
  month.coef <- matrix(NA, nrow = length(meangrowth[,1]), ncol = 3)
  
  for(i in 1:length(meangrowth[,1])){
    cors <- read.csv(paste0("data/BootCors/PRISM/",meangrowth[i,1], "-WW", clim, "cor.csv"))
    month.coef[i,1] <- cors[mono,]$cor
    month.coef[i,2] <- cors[mono,]$ci.min
    month.coef[i,3] <- cors[mono,]$ci.max
  }
  
  
  x <- as.data.frame(meangrowth)
  x$cor <- as.vector(month.coef[,1])
  x$Meangrowth <- as.numeric(x$Meangrowth)
  colnames(x[,1:3]) <- c('site', "Meangrowth", "cor")
  
  ggplot(x, aes(site, Meangrowth, fill = Description))+#geom_text_repel(aes(label = site))+
    geom_bar(stat= 'identity') + theme_bw()
}
mean.growth.barplot("Precip", 20,test)



