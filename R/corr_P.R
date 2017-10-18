#Correlating ring widths to climate factors across sites
# Author: Kelly Heilman

# note: need to update the rwl sources to the files with corrected headers
clim.corrs <- function(site, site.code){
    library(dplR)
    library(reshape2)
    library(ggplot2)
    library(plyr)
    library(boot)
    
   
    #change site --need to run this script for each site. It will output correlation coeffeiencts and save them in csv
    
    
    
    ##################################################
    #################################################
    ################################################
    ################################################
    
    stats.rwl <- rwl.stats(site)
    write.csv(stats.rwl, paste0("data/site_stats/",site.code, "-site_stats.csv"))
    
    site.code.rwi <- detrend(rwl = site, method = "Spline")
    plot(site)
    #create chronology of sites
    site.code.crn <- chron(site.code.rwi, prefix = paste(site.code))
    #write chronology to text 
    crn.trend <- chron(site, prefix= paste(site.code), prewhiten = TRUE)
    crn.prewhiten <- chron(site,prefix= paste(site.code), prewhiten = TRUE ) #also has residuals
    
    write.csv(site.code.crn, paste0(site.code, "-crn.csv"))
    crn.plot(site.code.crn, add.spline = TRUE)
    site.code.stats <- rwi.stats(site)
    
    site.code.crn$Year <- rownames(site.code.crn)
    site.code.crn$freq <- 12
    monthlys<- site.code.crn[rep(rownames(site.code.crn),
                      site.code.crn$freq),]
    write.csv(monthlys, paste0(site.code, "monthly-crn.csv"))
    
    
    
    
    
    
    ##############################################################################################
    #now using climate division data from the respective climate division for each tree ring site#
    ##############################################################################################
    # need to add climate for additional sites as needed*******
    
    if(site.code %in% c("BON", "GL1", "GL2", "GL3", "GL4")){
    MNcd.clim <- read.csv("data/West_central_MN_nclimdiv.csv")
    } else{ if(site.code == "HIC" ){
      MNcd.clim <- read.csv("data/NE_illinois_climdiv.csv")
    } else{ if(site.code %in% c("GLA", "PVC") ){
      MNcd.clim <- read.csv("data/NE_illinois_climdiv.csv")
    } else{ if(site.code == "COR" ){
        MNcd.clim <- read.csv("data/NE_illinois_climdiv.csv")
    } else{ if(site.code == "W-R" ){
      MNcd.clim <- read.csv("data/West_central_MN_nclimdiv.csv")
    } else{ if(site.code == 'SAW'){
      MNcd.clim <- read.csv("data/NE_illinois_climdiv.csv")
    }else{ if(site.code == "STC"){
      MNcd.clim <- read.csv("data/East_Central_MN_CDODiv5039587215503.csv")
    }else{ if(site.code == "ENG"){
      MNcd.clim <- read.csv("data/Central_MN_CDO.csv")
    }else{ if(site.code == "TOW"){
      MNcd.clim <- read.csv("data/South_central_MN_CDO.csv")
    }else{ if(site.code == "MOU"){
      MNcd.clim <- read.csv("data/South_East_MN_CDO.csv")
    }else{ if(site.code == "UNC"){
      MNcd.clim <- read.csv("data/East_Central_MN_CDODiv5039587215503.csv")
    }else { if(site.code == 'PLE'){
      MNcd.clim <- read.csv('data/south_central_WI_climdiv.csv')
    }else { if(site.code == 'YRF'){
      MNcd.clim <- read.csv('IA_nclim_div_northeast.csv')}
      #MNcd.clim <-read.csv('data/CDODiv2154347072867.csv')}
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    
    MNcd.clim$PCP <- MNcd.clim$PCP*25.54 # convert to mm
    keeps <- c("Year", "Month",  "PCP")
    keepstavg <- c("Year", "Month", "TAVG")
    keepst <- c("Year", "Month",  "TMAX")
    keepstmin <- c("Year", "Month",  "TMIN")
    keepspdsi <- c("Year", "Month",  "PDSI")
    #create a dataset for Precip
    MNp.df <- MNcd.clim[,keeps]
    MNp.df[MNp.df == -9999]<- NA
    
    #for tmax
    MNt.df <- MNcd.clim[,keepst]
    MNt.df[MNt.df == -9999]<- NA
    
    #for tmin
    MNtmin.df<- MNcd.clim[,keepstmin]
    MNtmin.df[MNtmin.df == -9999]<- NA
    
    #for tavg
    MNtavg.df <- MNcd.clim[,keepstavg]
    MNtavg.df[MNtavg.df == -9999]<- NA
    
    MNpdsi.df<- MNcd.clim[,keepspdsi]
    MNpdsi.df[MNpdsi.df == -9999]<- NA
    #for precipitation
    
    
    total.p <- aggregate(PCP ~ Year + Month, data=MNp.df, FUN=sum, na.rm = T) 
    total.p
    
    pr.yr <- aggregate(PCP ~ Year , data=MNp.df, FUN=sum, na.rm = T) 
    plot(pr.yr[1:120,1], pr.yr[1:120,2], type = "l", xlab = "Year", ylab = "Annual Precip (mm)")
    
    
    precip <- dcast(total.p, Year  ~ Month)
    
    annual.p <- aggregate(PCP~Year, data = MNp.df[1:1440,], FUN = sum, na.rm=T)
    annual.t <- aggregate(TAVG ~ Year, data = MNcd.clim[1:1440,], FUN = mean, na.rm=T)
    annual.mint <- aggregate(TMIN ~Year, data = MNcd.clim[1:1440,], FUN = mean, na.rm = T)
    annual.maxt <- aggregate(TMAX ~Year, data = MNcd.clim[1:1440,], FUN = mean, na.rm = T)
    annual.PDSI <- aggregate(PDSI ~Year, data = MNcd.clim[1:1440,], FUN = mean, na.rm = T)
    
    write.csv(annual.p, paste0(site.code, '-annualP.csv'))
    write.csv(annual.t, paste0(site.code, '-annualtavg.csv'))
    write.csv(annual.mint, paste0(site.code, '-annualtmin.csv'))
    write.csv(annual.maxt, paste0(site.code, '-annualtmax.csv'))
    write.csv(annual.PDSI, paste0(site.code, '-annualPDSI.csv'))
    
    par(mfrow=c(2,1))
    plot(annual.p, type = "l")
    plot(annual.t, type = "l")
    plot(annual.mint, type = "l")
    tmin.lm <- lm(annual.mint$TMIN ~ annual.mint$Year)
    dev.off()
    #create violin plot of monthly precip
    ggplot(total.p, aes(x = factor(Month), y = PCP))+ geom_violin(fill = "orange") +
      geom_point( colour= "blue")
    
    prmeans <- aggregate(PCP ~ Month, data = MNp.df, FUN=mean, na.rm = T) 
    tmean <- aggregate(TAVG ~ Month, data = MNcd.clim, FUN = mean, na.rm = T)
    
    
    
    #plot mean monthly precipitation & Temperature
    pdf(paste0("outputs/",site.code, "mean.climate.pdf"))
    plot(pr.yr[1:120,1], pr.yr[1:120,2], type = "l", xlab = "Year", ylab = "Annual Precip (mm)")
    
    op <- par(mar=c(5, 4, 4, 6) + 0.1)
    b.plot <- barplot(height = prmeans$PCP, names.arg = prmeans$Month,
                      xlab="Month", ylab="Mean Precipitation (mm)")
    bar.x <- b.plot[prmeans$Month]
    
    par(new = TRUE)
    plot(x = bar.x, y = tmean$TAVG, xlab = "", ylab = "", pch = 16,
         ylim = c(0, 100),
         axes = FALSE, col = "red")
    par(new = TRUE)
    plot(x = bar.x, y = tmean$TAVG, xlab = "", ylab = "", type = "l",
         ylim = c(0, 100),
         axes = FALSE, col = "red")
    axis(4, col = "red", col.axis = "red")
    mtext("Temperature (degF)", side = 4, line=3, cex = par("cex.lab"), col = "red")
    dev.off()
    
    
    
    # calculate bootstrapped correlations correlations of precip
    
    # need to create data frame of the current year precip & previous years precip:
    record.MN <- merge(precip, site.code.crn, by = "Year")
    prevs <- cbind(record.MN[2:121,1], record.MN[1:120,2:13])
    # rename with appropriate months
    colnames(prevs) <- c("Year","pJan", "pFeb", "pMar", "pApr", "pMay", "pJun", "pJul", "pAug", "pSep", "pOct", "pNov", "pDec")
    colnames(record.MN)[1:13] <- c("Year", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    
    #merge previous and current together:
    record.MN <- merge(prevs, record.MN, by = "Year")
    
    # function to do the bootstrapping correlations
    boot.cor <- function(d,i=c(1:n), colno ){
      d2 <- d[i,2:26]
      
      
      return(cor(d2[,colno], d2[,25], use = "pairwise.complete.obs"))
    }
    
    pcors <- matrix(0, nrow = 24, ncol = 4)
    colnos <- 1:24
    
    # run the bootstrapped correlation function over all the months and calculate CI:
    for(mo in 2:length(colnos)){
    
        boot.results <- boot(d = record.MN, colno=mo , boot.cor, R= 500)
        
        cis <- boot.ci(boot.out = boot.results, type = c("norm", "basic", "perc", "bca"))
        ci.mo <- cis$normal[2:3]
        t <- boot.results$t0
        pcors[mo,1] <- t
        pcors[mo,2] <- mo
        pcors[mo,3]<- ci.mo[1]
        pcors[mo,4]<- ci.mo[2]
        pcors <-  data.frame(pcors)
        colnames(pcors) <- c("cor", "month","ci.min", "ci.max")
    
    }
    
    # make a basic barplot with the bootstrapped CI as errorbars:
    ggplot(pcors, aes(month, cor))+geom_bar(stat="identity")+geom_errorbar(aes(ymin=ci.min, ymax=ci.max),
                    width=.2,                    # Width of the error bars
                    position=position_dodge(.9))
    
    # write for later use
    write.csv(pcors, paste0("data/BootCors/",site.code, "-", wood, "Precipcor.csv"))
    
    #----------------------------------TMAX---------------------------------------
    
    #now find bootstrapped correlations with TMAX
    mean.t <- aggregate(TMAX~Year + Month, data=MNt.df, FUN=sum, na.rm = T) 
    mean.t
    
    temp <- dcast(mean.t, Year  ~ Month)
    
    
    #create violin plot of monthly Tmax
    ggplot(mean.t, aes(x = factor(Month), y = TMAX))+ geom_violin(fill = "orange") +
      geom_point( colour= "blue")
    
    site.code.crn$Year <- rownames(site.code.crn)
    
    
    temp.MN <- merge(temp, site.code.crn, by = "Year")
    
    #correlate the chronology with temperature
    # need to create data frame of the current year precip & previous years precip:
    
    prevs <- cbind(temp.MN[2:121,1], temp.MN[1:120,2:13])
    # rename with appropriate months
    colnames(prevs) <- c("Year","pJan", "pFeb", "pMar", "pApr", "pMay", "pJun", "pJul", "pAug", "pSep", "pOct", "pNov", "pDec")
    colnames(temp.MN)[1:13] <- c("Year", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    
    #merge previous and current together:
    temp.MN <- merge(prevs,temp.MN, by = "Year")
    
    # function to do the bootstrapping correlations
    boot.cor <- function(d,i=c(1:n), colno ){
      d2 <- d[i,2:26]
      
      
      return(cor(d2[,colno], d2[,25], use = "pairwise.complete.obs"))
    }
    
    tcors <- matrix(0, nrow = 24, ncol = 4)
    colnos <- 1:24
    
    # run the bootstrapped correlation function over all the months and calculate CI:
    for(mo in 2:length(colnos)){
      
      boot.results <- boot(d = temp.MN, colno=mo , boot.cor, R= 500)
      
      cis <- boot.ci(boot.out = boot.results, type = c("norm", "basic", "perc", "bca"))
      ci.mo <- cis$normal[2:3]
      t <- boot.results$t0
      tcors[mo,1] <- t
      tcors[mo,2] <- mo
      tcors[mo,3]<- ci.mo[1]
      tcors[mo,4]<- ci.mo[2]
      tcors <-  data.frame(tcors)
      colnames(tcors) <- c("cor", "month","ci.min", "ci.max")
      
    }
    
    # make a basic barplot with the bootstrapped CI as errorbars:
    ggplot(tcors, aes(month, cor))+geom_bar(stat="identity")+geom_errorbar(aes(ymin=ci.min, ymax=ci.max),
                                                                           width=.2,                    # Width of the error bars
                                                                           position=position_dodge(.9))
    
    # write for later use
    
    write.csv(tcors, paste0("data/BootCors/",site.code, "-", wood, "tmaxcor.csv"))
    
    
    #-------------------------------------- Tmin -----------------------------------------
    min.t <- aggregate(TMIN~Year + Month, data=MNtmin.df, FUN=sum, na.rm = T) 
    min.t
    
    temp.min <- dcast(min.t, Year  ~ Month)
    
    
    #create violin plot of monthly minimum temperature
    ggplot(min.t, aes(x = factor(Month), y = TMIN))+ geom_violin(fill = "orange") +
      geom_point( colour= "blue")
    
    site.code.crn$Year <- rownames(site.code.crn)
    
    
    temp.MN <- merge(temp.min, site.code.crn, by = "Year")
    
    prevs <- cbind(temp.MN[2:121,1], temp.MN[1:120,2:13])
    # rename with appropriate months
    colnames(prevs) <- c("Year","pJan", "pFeb", "pMar", "pApr", "pMay", "pJun", "pJul", "pAug", "pSep", "pOct", "pNov", "pDec")
    colnames(temp.MN)[1:13] <- c("Year", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    
    #merge previous and current together:
    temp.MN <- merge(prevs,temp.MN, by = "Year")
    
    # function to do the bootstrapping correlations
    boot.cor <- function(d,i=c(1:n), colno ){
      d2 <- d[i,2:26]
      
      
      return(cor(d2[,colno], d2[,25], use = "pairwise.complete.obs"))
    }
    
    tcors <- matrix(0, nrow = 24, ncol = 4)
    colnos <- 1:24
    
    # run the bootstrapped correlation function over all the months and calculate CI:
    for(mo in 2:length(colnos)){
      
      boot.results <- boot(d = temp.MN, colno=mo , boot.cor, R= 500)
      
      cis <- boot.ci(boot.out = boot.results, type = c("norm", "basic", "perc", "bca"))
      ci.mo <- cis$normal[2:3]
      t <- boot.results$t0
      tcors[mo,1] <- t
      tcors[mo,2] <- mo
      tcors[mo,3]<- ci.mo[1]
      tcors[mo,4]<- ci.mo[2]
      tcors <-  data.frame(tcors)
      colnames(tcors) <- c("cor", "month","ci.min", "ci.max")
      
    }
    
    # make a basic barplot with the bootstrapped CI as errorbars:
    ggplot(tcors, aes(month, cor))+geom_bar(stat="identity")+geom_errorbar(aes(ymin=ci.min, ymax=ci.max),
                                                                           width=.2,                    # Width of the error bars
                                                                           position=position_dodge(.9))
    
    write.csv(tcors, paste0("data/BootCors/",site.code, "-", wood, "tmincor.csv"))
    
    
    #-----------------------------------------TAVG----------------------------------------
    
    #average temperature
    
    avg.t <- aggregate(TAVG~Year + Month, data=MNtavg.df, FUN=sum, na.rm = T) 
    avg.t
    
    temp.avg <- dcast(avg.t, Year  ~ Month)
    
    
    #create violin plot of monthly minimum temperature
    ggplot(avg.t, aes(x = factor(Month), y = TAVG))+ geom_violin(fill = "orange") +
      geom_point( colour= "blue")
    
    site.code.crn$Year <- rownames(site.code.crn)
    
    
    temp.MN <- merge(temp.avg, site.code.crn, by = "Year")
    
    prevs <- cbind(temp.MN[2:121,1], temp.MN[1:120,2:13])
    # rename with appropriate months
    colnames(prevs) <- c("Year","pJan", "pFeb", "pMar", "pApr", "pMay", "pJun", "pJul", "pAug", "pSep", "pOct", "pNov", "pDec")
    colnames(temp.MN)[1:13] <- c("Year", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    
    #merge previous and current together:
    temp.MN <- merge(prevs,temp.MN, by = "Year")
    
    # function to do the bootstrapping correlations
    boot.cor <- function(d,i=c(1:n), colno ){
      d2 <- d[i,2:26]
      
      
      return(cor(d2[,colno], d2[,25], use = "pairwise.complete.obs"))
    }
    
    tcors <- matrix(0, nrow = 24, ncol = 4)
    colnos <- 1:24
    
    # run the bootstrapped correlation function over all the months and calculate CI:
    for(mo in 2:length(colnos)){
      
      boot.results <- boot(d = temp.MN, colno=mo , boot.cor, R= 500)
      
      cis <- boot.ci(boot.out = boot.results, type = c("norm", "basic", "perc", "bca"))
      ci.mo <- cis$normal[2:3]
      t <- boot.results$t0
      tcors[mo,1] <- t
      tcors[mo,2] <- mo
      tcors[mo,3]<- ci.mo[1]
      tcors[mo,4]<- ci.mo[2]
      tcors <-  data.frame(tcors)
      colnames(tcors) <- c("cor", "month","ci.min", "ci.max")
      
    }
    
    # make a basic barplot with the bootstrapped CI as errorbars:
    ggplot(tcors, aes(month, cor))+geom_bar(stat="identity")+geom_errorbar(aes(ymin=ci.min, ymax=ci.max),
                                                                           width=.2,                    # Width of the error bars
                                                                           position=position_dodge(.9))
    
    write.csv(tcors, paste0("data/BootCors/",site.code, "-", wood, "tavgcor.csv"))
    
    
    #------------------------------------PDSI--------------------------------------------
    pdsi <- aggregate(PDSI~Year + Month, data=MNpdsi.df, FUN=sum, na.rm = T) 
    pdsi
    
    drought <- dcast(pdsi, Year  ~ Month)
    
    
    #create violin plot of monthly precip
    ggplot(pdsi, aes(x = factor(Month), y = PDSI))+ geom_violin(fill = "orange") +
      geom_point( colour= "blue")
    
    site.code.crn$Year <- rownames(site.code.crn)
    
    
    pdsi.MN <- merge(drought, site.code.crn, by = "Year")
    
    prevs <- cbind(pdsi.MN[2:121,1], pdsi.MN[1:120,2:13])
    # rename with appropriate months
    colnames(prevs) <- c("Year","pJan", "pFeb", "pMar", "pApr", "pMay", "pJun", "pJul", "pAug", "pSep", "pOct", "pNov", "pDec")
    colnames(pdsi.MN)[1:13] <- c("Year", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    
    #merge previous and current together:
    pdsi.MN <- merge(prevs, pdsi.MN, by = "Year")
    
    # function to do the bootstrapping correlations
    boot.cor <- function(d,i=c(1:n), colno ){
      d2 <- d[i,2:26]
      
      
      return(cor(d2[,colno], d2[,25], use = "pairwise.complete.obs"))
    }
    
    pdsicors <- matrix(0, nrow = 24, ncol = 4)
    colnos <- 1:24
    
    # run the bootstrapped correlation function over all the months and calculate CI:
    for(mo in 2:length(colnos)){
      
      boot.results <- boot(d = pdsi.MN, colno=mo , boot.cor, R= 500)
      
      cis <- boot.ci(boot.out = boot.results, type = c("norm", "basic", "perc", "bca"))
      ci.mo <- cis$normal[2:3]
      t <- boot.results$t0
      pdsicors[mo,1] <- t
      pdsicors[mo,2] <- mo
      pdsicors[mo,3]<- ci.mo[1]
      pdsicors[mo,4]<- ci.mo[2]
      pdsicors <-  data.frame(pdsicors)
      colnames(pdsicors) <- c("cor", "month","ci.min", "ci.max")
      
    }
    
    # make a basic barplot with the bootstrapped CI as errorbars:
    ggplot(pdsicors, aes(month, cor))+geom_bar(stat="identity")+geom_errorbar(aes(ymin=ci.min, ymax=ci.max),
                                                                           width=.2,                    # Width of the error bars
                                                                           position=position_dodge(.9))
    
    write.csv(pdsicors, paste0("data/BootCors/",site.code, "-", wood, "PDSIcor.csv"))

}

