#making pretty site level corrleation comparisons
#note there are several issues with this code and which sets of data it graphs
#need to update the EW and LW rwl's so that they match the whole wood rwl

if(wood == "EW"){
#EW
HICpew<- read.csv("HIC-EWPrecipcor.csv")
BONpew <- read.csv("BON-EWPrecipcor.csv")
GLApew <- read.csv("GLA-EWPrecipcor.csv")
PLEpew <- read.csv("PLE-EWPrecipcor.csv")
TOWpew <- read.csv('TOW-EWPrecipcor.csv')

HICtmaxew <- read.csv("HIC-EWtmaxcor.csv")
BONtmaxew <- read.csv("BON-EWtmaxcor.csv")
GLAtmaxew <- read.csv("GLA-EWtmaxcor.csv")
PLEtmaxew <- read.csv("PLE-EWtmaxcor.csv")
TOWtmaxew <- read.csv("TOW-EWtmaxcor.csv")

HICtminew <- read.csv("HIC-EWtmincor.csv")
BONtminew <- read.csv("BON-EWtmincor.csv")
GLAtminew <- read.csv("GLA-EWtmincor.csv")
PLEtminew <- read.csv("PLE-EWtmincor.csv")
TOWtminew <- read.csv("TOW-EWtmincor.csv")

HICtavgew <- read.csv("HIC-EWtavgcor.csv")
BONtavgew <- read.csv("BON-EWtavgcor.csv")
GLAtavgew <- read.csv("GLA-EWtavgcor.csv")
PLEtavgew <- read.csv("PLE-EWtavgcor.csv")
TOWtavgew <- read.csv('TOW-EWtavgcor.csv')

HICpdsiew <- read.csv("HIC-EWpdsicor.csv")
BONpdsiew <- read.csv("BON-EWpdsicor.csv")
GLApdsiew <- read.csv("GLA-EWpdsicor.csv")
PLEpdsiew <- read.csv("PLE-EWpdsicor.csv")
TOWpdsiew <- read.csv("TOW-EWpdsicor.csv")


HICcfsew <- read.csv("HIC-EWcfs.cor.csv")
GLAcfsew <- read.csv("GLA-EWcfs.cor.csv")


}else{if(wood == "LW"){
  HICplw<- read.csv("HIC-LWPrecipcor.csv")
  BONplw <- read.csv("BON-LWPrecipcor.csv")
  GLAplw <- read.csv("GLA-LWPrecipcor.csv")
  PLEplw <- read.csv("PLE-LWPrecipcor.csv")
  TOWplw <- read.csv("TOW-LWPrecipcor.csv")
  
  
  HICtmaxlw <- read.csv("HIC-LWtmaxcor.csv")
  BONtmaxlw <- read.csv("BON-LWtmaxcor.csv")
  GLAtmaxlw <- read.csv("GLA-LWtmaxcor.csv")
  PLEtmaxlw <- read.csv("PLE-LWtmaxcor.csv")
  TOWtmaxlw <- read.csv("TOW-LWtmaxcor.csv")
  
  HICtminlw <- read.csv("HIC-LWtmincor.csv")
  BONtminlw <- read.csv("BON-LWtmincor.csv")
  GLAtminlw <- read.csv("GLA-LWtmincor.csv")
  PLEtminlw <- read.csv("PLE-LWtmincor.csv")
  TOWtminlw <- read.csv("TOW-LWtmincor.csv")
  
  
  HICtavglw <- read.csv("HIC-LWtavgcor.csv")
  BONtavglw <- read.csv("BON-LWtavgcor.csv")
  GLAtavglw <- read.csv("GLA-LWtavgcor.csv")
  PLEtavglw <- read.csv("PLE-LWtavgcor.csv")
  TOWtavglw <- read.csv("TOW-LWtavgcor.csv")
  
  
  HICpdsilw <- read.csv("HIC-LWpdsicor.csv")
  BONpdsilw <- read.csv("BON-LWpdsicor.csv")
  GLApdsilw <- read.csv("GLA-LWpdsicor.csv")
  PLEpdsilw <- read.csv("PLE-LWpdsicor.csv")
  TOWpdsilw <- read.csv("TOW-LWpdsicor.csv")
  
  HICcfslw <- read.csv("HIC-LWcfs.cor.csv")
  GLAcfslw <- read.csv("GLA-LWcfs.cor.csv")
}else{
#WW
HICtmax <- read.csv("HIC-WWtmaxcor.csv")
BONtmax <- read.csv("BON-WWtmaxcor.csv")
SANtmax <- read.csv("SAN-WWtmaxcor.csv")
PULtmax <- read.csv("PUL-WWtmaxcor.csv")
GLAtmax <- read.csv("GLA-WWtmaxcor.csv")
PLEtmax <- read.csv("PLE-WWtmaxcor.csv")
TOWtmax <- read.csv("TOW-WWtmaxcor.csv")

HICp<- read.csv("HIC-WWPrecipcor.csv")
BONp <- read.csv("BON-WWPrecipcor.csv")
SANp <- read.csv("SAN-WWPrecipcor.csv")
PULp <- read.csv("PUL-WWPrecipcor.csv")
GLAp <- read.csv("GLA-WWPrecipcor.csv")
PLEp <- read.csv("PLE-WWPrecipcor.csv")
TOWp <- read.csv("TOW-WWPrecipcor.csv")

HICtmin <- read.csv("HIC-WWtmincor.csv")
BONtmin <- read.csv("BON-WWtmincor.csv")
SANtmin <- read.csv("SAN-WWtmincor.csv")
PULtmin <- read.csv("PUL-WWtmincor.csv")
GLAtmin <- read.csv("GLA-WWtmincor.csv")
PLEtmin <- read.csv("PLE-WWtmincor.csv")
TOWtmin <- read.csv("TOW-WWtmincor.csv")

HICtavg <- read.csv("HIC-WWtavgcor.csv")
BONtavg <- read.csv("BON-WWtavgcor.csv")
SANtavg <- read.csv("SAN-WWtavgcor.csv")
PULtavg <- read.csv("PUL-WWtavgcor.csv")
GLAtavg <- read.csv("GLA-WWtavgcor.csv")
PLEtavg <- read.csv("PLE-WWtavgcor.csv")
TOWtavg <- read.csv("TOW-WWtavgcor.csv")

HICpdsi <- read.csv("HIC-WWpdsicor.csv")
BONpdsi <- read.csv("BON-WWpdsicor.csv")
SANpdsi <- read.csv("SAN-WWPDSIcor.csv")
PULpdsi <- read.csv("PUL-WWPDSIcor.csv")
GLApdsi <- read.csv("GLA-WWPDSIcor.csv")
PLEpdsi <- read.csv("PLE-WWPDSIcor.csv")
TOWpdsi <- read.csv("TOW-WWPDSIcor.csv")

HICcfs <- read.csv("HIC-WWcfs.cor.csv")
GLAcfs <- read.csv("GLA-WWcfs.cor.csv")
}}


months <- c("pJan", "pFeb","pMar", "pApr", "pMay", "pJun",
            "pJul", "pAug", "pSep", "pOct", "pNov", "pDec",
            "Jan", "Feb","Mar", "Apr", "May", "Jun",
            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
#create dataframes of each site together
prcors<- data.frame(HIC = HICp$V1, 
                    HICew = HICpew$V1,
                    HIClw = HICplw$V1,
          BON = BONp$V1,
          BONew = BONpew$V1,
          BONlw = BONplw$V1,
          #GLA = GLAp$V1,
          #GLAew = GLApew$V1,
          #GLAlw = GLAplw$V1,
          PLE = PLEp$V1,
          PLEew = PLEpew$V1,
          PLElw = PLEplw$V1,
          TOW = TOWp$V1,
          TOWew = TOWpew$V1,
          TOWlw = TOWplw$V1,
          
          #SAN = SANp$V1,
          #PUL = PULp$V1,
          months = months)

tmincors<- data.frame(HIC = HICtmin$V1, 
                      HICew = HICtminew$V1,
                      HIClw = HICtminlw$V1,
                    BON = BONtmin$V1,
                    BONew = BONtminew$V1,
                    BONlw = BONtminlw$V1,
                    #GLA = GLAtmin$V1,
                    #GLAew = GLAtminew$V1,
                    #GLAlw = GLAtminlw$V1,
                    PLE = PLEtmin$V1,
                    PLEew = PLEtminew$V1,
                    PLElw = PLEtminlw$V1,
                    TOW = TOWtmin$V1,
                    TOWew = TOWtminew$V1,
                    TOWlw = TOWtminlw$V1,
                    #SAN = SANtmin$V1,
                    #PUL= PULtmin$V1,
                    months = months)

tmaxcors<- data.frame(HIC = HICtmax$V1, 
                      HICew = HICtmaxew$V1,
                      HIClw = HICtmaxlw$V1,
                      BON = BONtmax$V1,
                      BONew = BONtmaxew$V1,
                      BONlw = BONtmaxlw$V1,
                      #GLA = GLAtmax$V1,
                      #GLAew = GLAtmaxew$V1,
                      #GLAlw = GLAtmaxlw$V1,
                      PLE = PLEtmax$V1,
                      PLEew = PLEtmaxew$V1,
                      PLElw = PLEtmaxlw$V1,
                      TOW = TOWtmax$V1,
                      TOWew = TOWtmaxew$V1,
                      TOWlw = TOWtmaxlw$V1,
                      #SAN = SANtmax$V1,
                      #PUL = PULtmax$V1,
                      months = months)

pdsicors<- data.frame(HIC = HICpdsi$V1, 
                      HICew = HICpdsiew$V1,
                      HIClw = HICpdsilw$V1,
                      BON = BONpdsi$V1,
                      BONew = BONpdsiew$V1,
                      BONlw = BONpdsilw$V1,
                      #GLA = GLApdsi$V1,
                      #GLAew = GLApdsiew$V1,
                      #GLAlw = GLApdsilw$V1,
                      PLE = PLEpdsi$V1,
                      PLEew = PLEpdsiew$V1,
                      PLElw = PLEpdsilw$V1,
                      TOW = TOWpdsi$V1,
                      TOWew = TOWpdsiew$V1,
                      TOWlw = TOWpdsilw$V1,
                      
                      #SAN = SANpdsi$V1,
                      #PUL = PULpdsi$V1,
                      months = months)


tavgcors<- data.frame(HIC = HICtavg$V1, 
                      HICew = HICtavgew$V1,
                      HIClw = HICtavglw$V1,
                      BON = BONtavg$V1,
                      BONew = BONtavgew$V1,
                      BONlw = BONtavglw$V1,
                      #GLA = GLAtavg$V1,
                      #GLAew = GLAtavgew$V1,
                      #GLAlw = GLAtavglw$V1,
                      PLE = PLEtavg$V1,
                      PLEew = PLEtavgew$V1,
                      PLElw = PLEtavglw$V1,
                      TOW = TOWtavg$V1,
                      TOWew = TOWtavgew$V1,
                      TOWlw = TOWtavglw$V1,
                      #SAN = SANtavg$V1,
                      #PUL = PULtavg$V1,
                      months = months)

cfscors<- data.frame(HIC = HICcfs$V1, 
                      HICew = HICcfsew$V1,
                      HIClw = HICcfslw$V1,
                     HIC = HICcfs$V1, 
                     HICew = HICcfsew$V1,
                     HIClw = HICcfslw$V1,
                      #BON = BONtavg$V1,
                      #BONew = BONtavgew$V1,
                      #BONlw = BONtavglw$V1,
                      GLA = GLAcfs$V1,
                      GLAew = GLAcfsew$V1,
                      GLAlw = GLAcfslw$V1,
                      #SAN = SANtavg$V1,
                      #PUL = PULtavg$V1,
                      months = months)

#########################
#left off adding GLA here
#########################




#function to calculate the critical value for peasron correlation coeff
#n is the sample size and alpha is the critical p value 
critical.r <- function(n, alpha=0.05){
  df <- n- 2
  critical.t <- qt(alpha/2, df, lower.tail = F)
  critical.r <- sqrt((critical.t^2)/((critical.t^2)+ df))
  return(critical.r)
}

# afunction to plot correlations of growth to climate in a barplot
make.barplot <- function(x,y){
x <- as.matrix(x[,c("HIC", "PUL")])
months <- c("pJan", "pFeb","pMar", "pApr", "pMay", "pJun",
            "pJul", "pAug", "pSep", "pOct", "pNov", "pDec",
            "Jan", "Feb","Mar", "Apr", "May", "Jun",
            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
row.names(x)<- months
colours <- c("brown1", "darkblue")
barplot(t(x),ylim= c(-0.4, 0.5), beside = TRUE, col = colours, las = 2, main = paste(y))
legend("topleft", c("HICKORY GROVE", "PULASKI"),cex=1.3, bty="n", fill = colours )
abline(h = critical.r(120))
abline(h = critical.r(120)*-1)
barplot(t(x),ylim= c(-0.4, 0.5), beside = TRUE, col = colours, las = 2, main = paste(y), add = T)
box()
}

pdf(paste0(wood, "-PUL-HIC-site.compare.pdf"))
make.barplot(prcors, paste0(wood, "-Precipitation"))
make.barplot(tavgcors, paste0(wood,"-Temperature"))
make.barplot(tmincors, paste0(wood,"Max. Temperature"))
make.barplot(tmaxcors, paste0(wood,"Min. Temperature"))
make.barplot(pdsicors, paste0(wood,"PDSI"))
dev.off()


# afunction to plot correlations of growth to climate in a barplot
make.barplotHIC <- function(x,y){
  x <- as.matrix(x[,c(2,3,1)])
  months <- c("pJan", "pFeb","pMar", "pApr", "pMay", "pJun",
              "pJul", "pAug", "pSep", "pOct", "pNov", "pDec",
              "Jan", "Feb","Mar", "Apr", "May", "Jun",
              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  row.names(x)<- months
  colours <- c("green", "red", "black")
  barplot(t(x),ylim= c(-0.4, 0.5), beside = TRUE, col = colours, las = 2, main = paste(y))
  legend("topleft", c( "Earlywood", "Latewood", "Annual"),cex=1.3, bty="n", fill = colours )
  abline(h = critical.r(120))
  abline(h = critical.r(120)*-1)
  barplot(t(x),ylim= c(-0.4, 0.5), beside = TRUE, col = colours, las = 2, main = paste(y), add = T)
  box()
}

make.barplotBON <- function(x,y){
  x <- as.matrix(x[,c(5,6,4)])
  months <- c("pJan", "pFeb","pMar", "pApr", "pMay", "pJun",
              "pJul", "pAug", "pSep", "pOct", "pNov", "pDec",
              "Jan", "Feb","Mar", "Apr", "May", "Jun",
              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  row.names(x)<- months
  colours <- c("green", "red", "black")
  barplot(t(x),ylim= c(-0.4, 0.5), beside = TRUE, col = colours, las = 2, main = paste(y))
  legend("topleft", c( "Earlywood", "Latewood", "Annual"),cex=1.3, bty="n", fill = colours )
  abline(h = critical.r(120))
  abline(h = critical.r(120)*-1)
  barplot(t(x),ylim= c(-0.4, 0.5), beside = TRUE, col = colours, las = 2, main = paste(y), add = T)
  box()
}

make.barplotTOW <- function(x,y){
  x <- as.matrix(x[,c(11,12,10)])
  months <- c("pJan", "pFeb","pMar", "pApr", "pMay", "pJun",
              "pJul", "pAug", "pSep", "pOct", "pNov", "pDec",
              "Jan", "Feb","Mar", "Apr", "May", "Jun",
              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  row.names(x)<- months
  colours <- c("green", "red", "black")
  barplot(t(x),ylim= c(-0.4, 0.5), beside = TRUE, col = colours, las = 2, main = paste(y))
  legend("bottomleft", c("EW", "LW", "WW"),cex=1.3, bty="n", fill = colours )
  abline(h = critical.r(120))
  abline(h = critical.r(120)*-1)
  barplot(t(x),ylim= c(-0.4, 0.5), beside = TRUE, col = colours, las = 2, main = paste(y), add = T)
  box()
}


make.barplotGLA <- function(x,y){
  x <- as.matrix(x[,c(8,9,7)])
  months <- c("pJan", "pFeb","pMar", "pApr", "pMay", "pJun",
              "pJul", "pAug", "pSep", "pOct", "pNov", "pDec",
              "Jan", "Feb","Mar", "Apr", "May", "Jun",
              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  row.names(x)<- months
  colours <- c("green", "red", "black")
  barplot(t(x),ylim= c(-0.4, 0.5), beside = TRUE, col = colours, las = 2, main = paste(y))
  legend("bottomleft", c("EW", "LW", "WW"),cex=1.3, bty="n", fill = colours )
  abline(h = critical.r(120))
  abline(h = critical.r(120)*-1)
  barplot(t(x),ylim= c(-0.4, 0.5), beside = TRUE, col = colours, las = 2, main = paste(y), add = T)
  box()
}

make.barplotPLE <- function(x,y){
  x <- as.matrix(x[,c(8,9,7)])
  months <- c("pJan", "pFeb","pMar", "pApr", "pMay", "pJun",
              "pJul", "pAug", "pSep", "pOct", "pNov", "pDec",
              "Jan", "Feb","Mar", "Apr", "May", "Jun",
              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  row.names(x)<- months
  colours <- c("green", "red", "black")
  barplot(t(x),ylim= c(-0.4, 0.5), beside = TRUE, col = colours, las = 2, main = paste(y))
  legend("topleft", c( "Earlywood", "Latewood", "Annual"),cex=1.3, bty="n", fill = colours )
  abline(h = critical.r(120))
  abline(h = critical.r(120)*-1)
  barplot(t(x),ylim= c(-0.4, 0.5), beside = TRUE, col = colours, las = 2, main = paste(y), add = T)
  box()
}


#pdf("streamflow_GLA_HICsites.pdf")
#make.barplotHIC(cfscors, "HIC Streamflow")
#make.barplotGLA(cfscors, "GLA Steamflow")
#dev.off()

pdf("all-wood-sites.pdf")
make.barplotHIC(prcors, "Hickory Grove, IL Precipitation")
make.barplotBON(prcors, "Bonanza Prairie, MN Precipitation")
make.barplotPLE(prcors, "Pleasant Valley precipiation")
make.barplotTOW(prcors, 'Townsend woods MN Precipiation')

make.barplotHIC(tavgcors, "HIC Avg. Temperature")
make.barplotBON(tavgcors, "BON Avg. Temperature")
make.barplotPLE(tavgcors, "PLE Avg. Temperature")
make.barplotTOW(tavgcors, 'Townsend woods Avg. Temperature')

make.barplotHIC(tmincors, "HIC Min. Temperature")
make.barplotBON(tmincors, "BON Min. Temperature")
make.barplotPLE(tmincors, "PLE Min. Temperature")
make.barplotTOW(tmincors, 'Townsend woods MIN. Temperature')

make.barplotHIC(tmaxcors, "HIC Max. Temperature")
make.barplotBON(tmaxcors, "BON Max. Temperature")
make.barplotPLE(tmaxcors, "PLE Max. Temperature")
make.barplotTOW(tmaxcors, 'Townsend woods Max. Temperature')

make.barplotHIC(pdsicors, "HIC PDSI")
make.barplotBON(pdsicors, "BON PDSI")
make.barplotPLE(pdsicors, "PLE PDSI")
make.barplotTOW(pdsicors, 'Townsend woods Avg. Temperature')

dev.off()