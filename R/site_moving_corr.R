# new R script to repplot moving correlations as a heatmap overtime
# this is an additional plot to show that we observe decline in sensitivity

# This makes use of the treeclim package: 
# Zang, C., and F. Biondi. 2015. treeclim: an R package for the numerical calibration of proxy-climate relationships. Ecography 38:431–436.

# need to do this for:
#       -each site
#       -each cohort class at each site
#       -for mean temperature, maximum temperature, MAP, SP06, SP01
#       -based on detrending methods
library(treeclim)
library(dplR)
library(tidyr)
library(dplyr)
library(DescTools)

# the crns for each site are listed in outputs/chron
# list the ones with splines:
setwd("/Users/kah/Documents/TreeRings/outputs/chron/")

spline.chron <- list.files(pattern = "Spline.crn$") # get all the data files

all.chrons <- lapply(spline.chron, FUN = read.crn) 



setwd("/Users/kah/Documents/TreeRings")
# now get climate for each site
site.names <- stringr::str_remove(spline.chron, "_rwi_Spline.crn")
names(all.chrons) <- site.names
# read in climate data

prisms <- paste0("climate/", list.files("climate", pattern = "PRISMfull"))
ghcns <- paste0("climate/", list.files("climate", pattern = "GHCNfull"))


prism <- lapply( prisms, read.csv )
ghcn <- lapply( ghcns, read.csv )

ghcn.df <- do.call(rbind, ghcn)
prism.df <- do.call(rbind, prism)


long.prism  <- prism.df %>% group_by(site, Year) %>% gather(key = climate, value = value, PRISM_pcp_1:PRISM_BAL_12)
long.prism$ month <- do.call(rbind, stringr::str_split(string = long.prism$climate, pattern = "_"))[,3]# get the month value
long.prism$climate <- do.call(rbind, stringr::str_split(string = long.prism$climate, pattern = "_"))[,2]# get the cliimate variable 


# -------------------Now plot static correlations-----------------------

# make a function to make simple plots of static correlation at each site (these are uglyb, but just to check)
make.static.cor <- function(x){
          site.name <- stringr::str_remove(colnames(x)[1], "std")
          site.name <- ifelse( site.name %in% "GL1", "GLL1",
                               ifelse( site.name %in% "GL2", "GLL2",
                                      ifelse( site.name %in% "GL3", "GLL2",
                                             ifelse( site.name %in% "GL4", "GLL4",site.name))))
          
        AVO.prism <- long.prism %>% dplyr::filter(site %in% site.name) 
        AVO.prism <- AVO.prism[,c("Year", "climate", "month","value")]
        AVO.prism$month <- as.integer(AVO.prism$month)
        
        yrs <- as.numeric(unique(row.names(x)))
        AVO.prism <- AVO.prism %>% filter(Year %in% yrs) %>%  spread(key = climate, value = value )
        
        
        # run moving corrlation for all the sites:
        BAL <- dcc(data.frame(x), data.frame(AVO.prism[,c("Year", "month", "BAL")]) , dynamic = "static", win_size = 20)
        #ggplot(coef(BAL), aes(coef,))
        #BAL.data <- coef(BAL)

        # define ggplto function (should make it easier to output and save using cowplot)        
             plt.data <-  function(data){  ggplot(data = data, aes(x = id, y = coef) ) +
                geom_point(aes(color = significant), size = 3) +
                scale_x_continuous( breaks = data$id, labels = data$month) +
                ylab("Coefficients") +
                xlab("Months") +  
                theme_bw() +
                theme(axis.title.x = element_blank()) +
                geom_errorbar(data = BAL.data, aes(x = id, ymin = ci_lower, ymax = ci_upper, lty = significant, color = significant), size = 0.5)+scale_linetype_manual(values = c("dashed", "solid"))+
                 scale_color_manual(values = c("grey", "red"))+ylim(-1,1) }
                  
                          
        BAL.data <- coef(BAL)   
        BAL.plt <- ggplot(data = BAL.data, aes(x = id, y = coef) ) +
          geom_point(aes(color = significant), size = 3) +
          scale_x_continuous( breaks = BAL.data$id, labels =BAL.data$month) +
          ylab("Coefficients") +
          xlab("Months") +  
          theme_bw() +
          theme(axis.title.x = element_blank()) +
          geom_errorbar(data = BAL.data, aes(x = id, ymin = ci_lower, ymax = ci_upper, lty = significant, color = significant), size = 0.5)+scale_linetype_manual(values = c("dashed", "solid"))+
          scale_color_manual(values = c("grey", "red"))+ylim(-1,1)+ggtitle("Moisture Balance")+theme(panel.grid.minor = element_blank())
        
        pcp <- dcc(data.frame(x), data.frame(AVO.prism[,c("Year", "month", "pcp")]) , dynamic = "static", win_size = 20)
        pcp.data <- coef(pcp)   
        pcp.plt <- ggplot(data = pcp.data, aes(x = id, y = coef) ) +
          geom_point(aes(color = significant), size = 3) +
          scale_x_continuous( breaks = pcp.data$id, labels = pcp.data$month) +
          ylab("Coefficients") +
          xlab("Months") +  
          theme_bw() +
          theme(axis.title.x = element_blank()) +
          geom_errorbar(data = pcp.data, aes(x = id, ymin = ci_lower, ymax = ci_upper, lty = significant, color = significant), size = 0.5)+scale_linetype_manual(values = c("dashed", "solid"))+
          scale_color_manual(values = c("grey", "red"))+ylim(-1,1)+ggtitle("Precip")+theme(panel.grid.minor = element_blank())
        
        tavg <- dcc(data.frame(x), data.frame(AVO.prism[,c("Year", "month", "tavg")]) , dynamic = "static", win_size = 20)
        tavg.data <- coef(tavg)   
        tavg.plt <- ggplot(data = tavg.data, aes(x = id, y = coef) ) +
          geom_point(aes(color = significant), size = 3) +
          scale_x_continuous( breaks = tavg.data$id, labels = tavg.data$month) +
          ylab("Coefficients") +
          xlab("Months") +  
          theme_bw() +
          theme(axis.title.x = element_blank()) +
          geom_errorbar(data = tavg.data, aes(x = id, ymin = ci_lower, ymax = ci_upper, lty = significant, color = significant), size = 0.5)+scale_linetype_manual(values = c("dashed", "solid"))+
          scale_color_manual(values = c("grey", "red"))+ylim(-1,1)+ggtitle("Tavg")+theme(panel.grid.minor = element_blank())
        
        tmax <- dcc(data.frame(x), data.frame(AVO.prism[,c("Year", "month", "tmax")]) , dynamic = "static", win_size = 20)
        tmax.data <- coef(tmax)   
        tmax.plt <- ggplot(data = tmax.data, aes(x = id, y = coef) ) +
          geom_point(aes(color = significant), size = 3) +
          scale_x_continuous( breaks = tmax.data$id, labels = tmax.data$month) +
          ylab("Coefficients") +
          xlab("Months") +  
          theme_bw() +
          theme(axis.title.x = element_blank()) +
          geom_errorbar(data = tmax.data, aes(x = id, ymin = ci_lower, ymax = ci_upper, lty = significant, color = significant), size = 0.5)+scale_linetype_manual(values = c("dashed", "solid"))+
          scale_color_manual(values = c("grey", "red"))+ylim(-1,1)+ggtitle("Tmax")+theme(panel.grid.minor = element_blank())
        
        tmin <- dcc(data.frame(x), data.frame(AVO.prism[,c("Year", "month", "tmin")]) , dynamic = "static", win_size = 20)
        tmin.data <- coef(tmin)   
        tmin.plt <- ggplot(data = tmin.data, aes(x = id, y = coef) ) +
          geom_point(aes(color = significant), size = 3) +
          scale_x_continuous( breaks = tmin.data$id, labels = tmin.data$month) +
          ylab("Coefficients") +
          xlab("Months") +  
          theme_bw() +
          theme(axis.title.x = element_blank()) +
          geom_errorbar(data = tmin.data, aes(x = id, ymin = ci_lower, ymax = ci_upper, lty = significant, color = significant), size = 0.5)+scale_linetype_manual(values = c("dashed", "solid"))+
          scale_color_manual(values = c("grey", "red"))+ylim(-1,1)+ggtitle("Tmin")+theme(panel.grid.minor = element_blank())
        
        vpdmax <- dcc(data.frame(x), data.frame(AVO.prism[,c("Year", "month", "vpdmax")]) , dynamic = "static", win_size = 20)
        vpdmax.data <- coef(vpdmax)   
        vpdmax.plt <- ggplot(data = vpdmax.data, aes(x = id, y = coef) ) +
          geom_point(aes(color = significant), size = 3) +
          scale_x_continuous( breaks = vpdmax.data$id, labels = vpdmax.data$month) +
          ylab("Coefficients") +
          xlab("Months") +  
          theme_bw() +
          theme(axis.title.x = element_blank()) +
          geom_errorbar(data = vpdmax.data, aes(x = id, ymin = ci_lower, ymax = ci_upper, lty = significant, color = significant), size = 0.5)+scale_linetype_manual(values = c("dashed", "solid"))+
          scale_color_manual(values = c("grey", "red"))+ylim(-1,1)+ggtitle("VPDmax")+theme(panel.grid.minor = element_blank())
        
        vpdmin <- dcc(data.frame(x), data.frame(AVO.prism[,c("Year", "month", "vpdmin")]) , dynamic = "static", win_size = 20)
        vpdmin.data <- coef(    vpdmin)   
        vpdmin.plt <- ggplot(data = vpdmin.data, aes(x = id, y = coef) ) +
          geom_point(aes(color = significant), size = 3) +
          scale_x_continuous( breaks = vpdmin.data$id, labels = vpdmin.data$month) +
          ylab("Coefficients") +
          xlab("Months") +  
          theme_bw() +
          theme(axis.title.x = element_blank()) +
          geom_errorbar(data = vpdmin.data, aes(x = id, ymin = ci_lower, ymax = ci_upper, lty = significant, color = significant), size = 0.5)+scale_linetype_manual(values = c("dashed", "solid"))+
          scale_color_manual(values = c("grey", "red"))+ylim(-1,1)+ggtitle("VPDmin")+theme(panel.grid.minor = element_blank())
        
        
        legend <- cowplot::get_legend(tmax.plt)
        png(height = 10, width = 12, units = "in", res = 200 ,paste0("outputs/correlations/static_site_cors/treeclim_correlations/", site.name, "Spline_PRISM_correlation.png"))
         print(cowplot::plot_grid(BAL.plt+theme(panel.grid.minor = element_blank(), legend.position = "none"), 
                                  pcp.plt+theme(panel.grid.minor = element_blank(), legend.position = "none"),
                  tavg.plt+theme(panel.grid.minor = element_blank(), legend.position = "none"), 
                  tmax.plt+theme(panel.grid.minor = element_blank(), legend.position = "none"),
                  tmin.plt+theme(panel.grid.minor = element_blank(), legend.position = "none"), 
                  vpdmax.plt+theme(panel.grid.minor = element_blank(), legend.position = "none"), 
                 vpdmin.plt+theme(panel.grid.minor = element_blank(), legend.position = "none"),
                 legend,
                ncol = 2, align = "hv")
               )
        dev.off()
}


# test on one site
make.static.cor(data.frame(all.chrons[[5]]))

# apply over all sites here: 
lapply(all.chrons, FUN = make.static.cor) # output plots in outputs/correlations/static_site_cors/



# -------------------Now we plot moving correlations-----------------------


make.moving.cor <- function(x){
  site.name <- stringr::str_remove(colnames(x)[1], "std")
  site.name <- ifelse( site.name %in% "GL1", "GLL1",
                       ifelse( site.name %in% "GL2", "GLL2",
                               ifelse( site.name %in% "GL3", "GLL2",
                                       ifelse( site.name %in% "GL4", "GLL4",site.name))))
  
  AVO.prism <- long.prism %>% dplyr::filter(site %in% site.name) 
  AVO.prism <- AVO.prism[,c("Year", "climate", "month","value")]
  AVO.prism$month <- as.integer(AVO.prism$month)
  
  yrs <- as.numeric(unique(row.names(x)))
  AVO.prism <- AVO.prism %>% filter(Year %in% yrs) %>%  spread(key = climate, value = value )
  
  # find appropriate start year & end years:
  
  min.yr <- ifelse(min(as.numeric(rownames(x))) <= 1895, 1895, 
         RoundTo( min(as.numeric(rownames(x))), 5, ceiling))
  
  # run moving corrlation for all the sites:
  BAL <- dcc(data.frame(x), data.frame(AVO.prism[,c("Year", "month", "BAL")]) , dynamic = "moving", win_size = 45, win_offset = 5, timespan = c(min.yr, 2014))
  saveRDS(BAL, paste0("outputs/correlations/moving_site_cors/dcc_df/BAL_", site.name ,".rds"))
  
  #BAL.test <- readRDS(paste0("outputs/correlations/moving_site_cors/dcc_df/BAL_", site.name ,".rds"))
  BAL.plt <- plot(BAL)+ggtitle("Moisture Balance")+scale_fill_gradient2(midpoint = 0, limits=c(-0.5,0.5))
  

  
  pcp <- dcc(data.frame(x), data.frame(AVO.prism[,c("Year", "month", "pcp")]) , dynamic = "moving", win_size = 45, win_offset = 5, timespan = c(min.yr, 2014))
  saveRDS(pcp, paste0("outputs/correlations/moving_site_cors/dcc_df/pcp_", site.name ,".rds"))
  pcp.plt <- plot(pcp) + ggtitle("Precip")+scale_fill_gradient2(midpoint = 0, limits=c(-0.5,0.5))
  
  pcp.sum <- dcc(data.frame(x), data.frame(AVO.prism[,c("Year", "month", "pcp")]), .sum(1:12, "pcp")+.sum(-9:10, "pcp") , dynamic = "moving", win_size = 45, win_offset = 5, timespan = c(min.yr, 2014))
  saveRDS(pcp.sum, paste0("outputs/correlations/moving_site_cors/dcc_df/pcp_sum_", site.name ,".rds"))
  pcp.sum.plt <- plot(pcp.sum) + ggtitle("Precip")+scale_fill_gradient2(midpoint = 0, limits=c(-0.5,0.5))
  
  
  tavg <- dcc(data.frame(x), data.frame(AVO.prism[,c("Year", "month", "tavg")]) , dynamic = "moving", win_size = 45, win_offset = 5, timespan = c(min.yr, 2014))
  saveRDS(tavg, paste0("outputs/correlations/moving_site_cors/dcc_df/TAVG_", site.name ,".rds"))  
  tavg.plt <- plot(tavg)+ggtitle("Tavg")+scale_fill_gradient2(midpoint = 0, limits=c(-0.5,0.5))
    
  tmax <- dcc(data.frame(x), data.frame(AVO.prism[,c("Year", "month", "tmax")]) , dynamic = "moving", win_size = 45, win_offset = 5, timespan = c(min.yr, 2014))
  saveRDS(tmax, paste0("outputs/correlations/moving_site_cors/dcc_df/TMAX_", site.name ,".rds"))
  tmax.plt <- plot(tmax)+ggtitle("Tmax")+scale_fill_gradient2(midpoint = 0, limits=c(-0.5,0.5))
    
    
  tmin <- dcc(data.frame(x), data.frame(AVO.prism[,c("Year", "month", "tmin")]) , dynamic = "moving", win_size = 45, win_offset = 5, timespan = c(min.yr, 2014))
  saveRDS(tmin, paste0("outputs/correlations/moving_site_cors/dcc_df/TMIN_", site.name ,".rds"))
  tmin.plt <- plot(tmin)+ggtitle("Tmin")+scale_fill_gradient2(midpoint = 0, limits=c(-0.5,0.5))
    
  vpdmax <- dcc(data.frame(x), data.frame(AVO.prism[,c("Year", "month", "vpdmax")]) , dynamic = "moving", win_size = 45, win_offset = 5, timespan = c(min.yr, 2014))
  saveRDS(vpdmax, paste0("outputs/correlations/moving_site_cors/dcc_df/VPDMAX_", site.name ,".rds"))
  vpdmax.plt <- plot(vpdmax)+ggtitle("VPDmax")+scale_fill_gradient2(midpoint = 0, limits=c(-0.5,0.5))
    
  vpdmin <- dcc(data.frame(x), data.frame(AVO.prism[,c("Year", "month", "vpdmin")]) , dynamic = "moving", win_size = 45, win_offset = 5, timespan = c(min.yr, 2014))
  saveRDS(vpdmin, paste0("outputs/correlations/moving_site_cors/dcc_df/VPDMIN_", site.name ,".rds"))
  vpdmin.plt <- plot(vpdmin)+ggtitle("VPDmax")+scale_fill_gradient2(midpoint = 0, limits=c(-0.5,0.5))
  
  
  legend <- cowplot::get_legend(tmax.plt)
  png(height = 12, width = 12, units = "in", res = 200 ,paste0("outputs/correlations/moving_site_cors/", site.name, "Spline_PRISM_correlation.png"))
  print(cowplot::plot_grid(BAL.plt+theme(panel.grid.minor = element_blank(), legend.position = "none"), 
                           pcp.plt+theme(panel.grid.minor = element_blank(), legend.position = "none"),
                           tavg.plt+theme(panel.grid.minor = element_blank(), legend.position = "none"), 
                           tmax.plt+theme(panel.grid.minor = element_blank(), legend.position = "none"),
                           tmin.plt+theme(panel.grid.minor = element_blank(), legend.position = "none"), 
                           vpdmax.plt+theme(panel.grid.minor = element_blank(), legend.position = "none"), 
                           vpdmin.plt+theme(panel.grid.minor = element_blank(), legend.position = "none"),
                           legend,
                           ncol = 2, align = "hv")
  )
  dev.off()
}

make.moving.cor(data.frame(all.chrons[[1]]))
lapply(all.chrons, FUN = make.moving.cor) # output plots in outputs/correlations/moving_site_cor/


# -------------------Goal: Plot JUN Tmax moving corrleations for all sites-----------------------

# list only the TMAX files
TMAX.dcc.files <- list.files(path = "outputs/correlations/moving_site_cors/dcc_df/", pattern = "TMAX.*\\.rds") 
TMAX.dcc.files.full <- paste0("/Users/kah/Documents/TreeRings/outputs/correlations/moving_site_cors/dcc_df/", TMAX.dcc.files)

TMAX.dccs <- lapply(TMAX.dcc.files.full , FUN = readRDS) 

sitelist <- substring(TMAX.dcc.files, 6, 8)

# get junTMAX datarame
getJUNTmax<- function(x){
                #x <- TMAX.dccs[[1]]
                coeff.df <- data.frame(x$coef)
                row.names(coeff.df) <- paste0()
                coeff.df[13,] 
            
                
                
}
jun.tmax.dcc <- lapply(TMAX.dccs, getJUNTmax)
names(jun.tmax.dcc) <- sitelist


# add the columns for missing timeperiods if need be here
cname <- colnames(jun.tmax.dcc[[3]])
cname <- c(cname, "site")

fncols <- function(data) {
  add <-cname[!cname%in%names(data)]
  
  if(length(add)!=0) data[add] <- NA
  
  # now reorder
  data[,cname]
  
}

jun.tmax.dcc.full <- lapply(jun.tmax.dcc, fncols )

juntmax.cors <- do.call(rbind, jun.tmax.dcc.full) 
juntmax.cors$site <- rownames(juntmax.cors) # site name

# plot basic tile plot of all the site moving correlations
cors <- juntmax.cors %>% select(coef.1900.1944:coef.1970.2014, site)
cors.df <- melt(cors)
cors.df$time.period <- paste(substring(cors.df$variable, 6, 9), "-",substring(cors.df$variable, 11, 14))
colnames(cors.df) <- c("site", "time", "coef", "time.period")

sigs <- juntmax.cors %>% select(significant.1900.1944:significant.1970.2014, site)
sigs.df <- melt(sigs)
sigs.df$time.period <- paste(substring(sigs.df$variable, 13, 16), "-",substring(sigs.df$variable, 18, 21))
sigs.df$sig <- ifelse(sigs.df$value == FALSE, " ", "*")
colnames(sigs.df) <- c("site", "time", "TFsig", "time.period", "sig")
full.df <- merge(sigs.df, cors.df, by = c("site", "time.period"))

JunTmax.moving.cors <- ggplot(full.df, aes(time.period, site, fill = coef))+geom_tile()+scale_fill_distiller(palette= "RdBu", direction = 2,limits = c(-0.55, 0.55), name = "Correlation with \n Average June \n Maximum Temperature")+
  geom_text( aes(label = sig))+theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1))+ylab("Site")+xlab("Time Period")

JunTmax.moving.cors.ms.sites <- ggplot(full.df[full.df$site %in% c("AVO", "BON", "ENG", "GLA", "GLL1", "GLL2", "GLL3", "MOU", "UNC"),], aes(time.period, site, fill = coef))+geom_tile()+scale_fill_distiller(palette= "RdBu", direction = 1,limits = c(-0.55, 0.55), name = "Correlation with \n Average June \n Maximum Temperature")+
  geom_text( aes(label = sig))+theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1))+ylab("Site")+xlab("Time Period")


# -------------------Goal: Total water year moving corrleations for all sites-----------------------

# list only the pcp.sum files
pcp.sum.dcc.files <- list.files(path = "outputs/correlations/moving_site_cors/dcc_df/", pattern = "pcp.sum.*\\.rds") 
pcp.sum.dcc.files.full <- paste0("/Users/kah/Documents/TreeRings/outputs/correlations/moving_site_cors/dcc_df/", pcp.sum.dcc.files)

pcp.sum.dccs <- lapply(pcp.sum.dcc.files.full , FUN = readRDS) 

sitelist <- substring(pcp.sum.dcc.files, 9, 11)

# get the datarame with water year correlations
getwateryr_pcp.sum <- function(x){
  #x <- pcp.sum.dccs[[1]]
  coeff.df <- data.frame(x$coef)
  coeff.df[2,] 
  
  
  
}
jun.pcp.sum.dcc <- lapply(pcp.sum.dccs, getwateryr_pcp.sum)
names(jun.pcp.sum.dcc) <- sitelist


# add the columns for missing timeperiods if need be here
cname <- colnames(jun.pcp.sum.dcc[[3]])
cname <- c(cname, "site")

fncols <- function(data) {
  add <-cname[!cname%in%names(data)]
  
  if(length(add)!=0) data[add] <- NA
  
  # now reorder
  data[,cname]
  
}

jun.pcp.sum.dcc.full <- lapply(jun.pcp.sum.dcc, fncols )

junpcp.sum.cors <- do.call(rbind, jun.pcp.sum.dcc.full) 
junpcp.sum.cors$site <- rownames(junpcp.sum.cors) # site name

# plot basic tile plot of all the site moving correlations
cors <- junpcp.sum.cors %>% select(coef.1900.1944:coef.1970.2014, site)
cors.df <- melt(cors)
cors.df$time.period <- paste(substring(cors.df$variable, 6, 9), "-",substring(cors.df$variable, 11, 14))
colnames(cors.df) <- c("site", "time", "coef", "time.period")

sigs <- junpcp.sum.cors %>% select(significant.1900.1944:significant.1970.2014, site)
sigs.df <- melt(sigs)
sigs.df$time.period <- paste(substring(sigs.df$variable, 13, 16), "-",substring(sigs.df$variable, 18, 21))
sigs.df$sig <- ifelse(sigs.df$value == FALSE, " ", "*")
colnames(sigs.df) <- c("site", "time", "TFsig", "time.period", "sig")
full.df <- merge(sigs.df, cors.df, by = c("site", "time.period"))

pcp.sum.moving.cors <- ggplot(full.df, aes(time.period, site, fill = coef))+geom_tile()+scale_fill_distiller(palette= "RdBu", direction = 1,limits = c(-0.55, 0.55), name = "Correlation with \n water year \n total precipitation")+
  geom_text( aes(label = sig))+theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1))+ylab("Site")+xlab("Time Period")

pcp.sum.moving.cors.ms.sites <- ggplot(full.df[full.df$site %in% c("AVO", "BON", "ENG", "GLA", "GLL1", "GLL2", "GLL3", "MOU", "UNC"),], aes(time.period, site, fill = coef))+geom_tile()+scale_fill_distiller(palette= "RdBu", direction = 1,limits = c(-0.55, 0.55), name = "Correlation with \n water year \n total precipitation")+
  geom_text( aes(label = sig))+theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1))+ylab("Site")+xlab("Time Period")


png(height = 6, width = 5, res = 300, units = "in", "outputs/correlations/moving_site_cors/summary_all_sites_Tmax_pcp.png")
plot_grid(pcp.sum.moving.cors, JunTmax.moving.cors, ncol = 1, labels = "AUTO", align = "hv")
dev.off()

png(height = 6, width = 5, res = 300, units = "in", "outputs/correlations/moving_site_cors/summary_ms_sites_Tmax_pcp.png")
plot_grid(pcp.sum.moving.cors.ms.sites, JunTmax.moving.cors.ms.sites, ncol = 1, labels = "AUTO", align = "hv")
dev.off()

