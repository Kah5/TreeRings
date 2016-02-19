#making pretty site level corrleation comparisons
if(wood == "EW"){
#EW
HICp<- read.csv("HIC-EWPrecipcor.csv")
BONp <- read.csv("BON-EWPrecipcor.csv")

HICtmax <- read.csv("HIC-EWtmaxcor.csv")
BONtmax <- read.csv("BON-EWtmaxcor.csv")

HICtmin <- read.csv("HIC-EWtmincor.csv")
BONtmin <- read.csv("BON-EWtmincor.csv")

HICtavg <- read.csv("HIC-EWtavgcor.csv")
BONtavg <- read.csv("BON-EWtavgcor.csv")

HICpdsi <- read.csv("HIC-EWpdsicor.csv")
BONpdsi <- read.csv("BON-EWpdsicor.csv")
}else{if(wood == "LW"){
  HICp<- read.csv("HIC-LWPrecipcor.csv")
  BONp <- read.csv("BON-LWPrecipcor.csv")
  
  HICtmax <- read.csv("HIC-LWtmaxcor.csv")
  BONtmax <- read.csv("BON-LWtmaxcor.csv")
  
  HICtmin <- read.csv("HIC-LWtmincor.csv")
  BONtmin <- read.csv("BON-LWtmincor.csv")
  
  HICtavg <- read.csv("HIC-LWtavgcor.csv")
  BONtavg <- read.csv("BON-LWtavgcor.csv")
  
  HICpdsi <- read.csv("HIC-LWpdsicor.csv")
  BONpdsi <- read.csv("BON-LWpdsicor.csv")
}else{
#WW
HICtmax <- read.csv("HIC-WWtmaxcor.csv")
BONtmax <- read.csv("BON-WWtmaxcor.csv")

HICp<- read.csv("HIC-WWPrecipcor.csv")
BONp <- read.csv("BON-WWPrecipcor.csv")

HICtmin <- read.csv("HIC-WWtmincor.csv")
BONtmin <- read.csv("BON-WWtmincor.csv")

HICtavg <- read.csv("HIC-WWtavgcor.csv")
BONtavg <- read.csv("BON-WWtavgcor.csv")

HICpdsi <- read.csv("HIC-WWpdsicor.csv")
BONpdsi <- read.csv("BON-WWpdsicor.csv")
}}

months <- c("pJan", "pFeb","pMar", "pApr", "pMay", "pJun",
            "pJul", "pAug", "pSep", "pOct", "pNov", "pDec",
            "Jan", "Feb","Mar", "Apr", "May", "Jun",
            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
#create dataframes of each site together
prcors<- data.frame(HIC = HICp$V1, 
          BON = BONp$V1,
           months = months)

tmincors<- data.frame(HIC = HICtmin$V1, 
                    BON = BONtmin$V1,
                    months = months)

tmaxcors<- data.frame(HIC = HICtmax$V1, 
                      BON = BONtmax$V1,
                      months = months)

pdsicors<- data.frame(HIC = HICpdsi$V1, 
                      BON = BONpdsi$V1,
                      months = months)


tavgcors<- data.frame(HIC = HICtavg$V1, 
                      BON = BONtavg$V1,
                      months = months)

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
x <- as.matrix(x[,1:2])
months <- c("pJan", "pFeb","pMar", "pApr", "pMay", "pJun",
            "pJul", "pAug", "pSep", "pOct", "pNov", "pDec",
            "Jan", "Feb","Mar", "Apr", "May", "Jun",
            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
row.names(x)<- months
colours <- c("brown1", "darkblue")
barplot(t(x),ylim= c(-0.4, 0.5), beside = TRUE, col = colours, las = 2, main = paste(y))
legend("topleft", c("HICKORY GROVE", "BONANZA PRAIRIE"),cex=1.3, bty="n", fill = colours )
abline(h = critical.r(120))
abline(h = critical.r(120)*-1)
barplot(t(x),ylim= c(-0.4, 0.5), beside = TRUE, col = colours, las = 2, main = paste(y), add = T)
box()
}

pdf(paste0(wood, "-site.compare.pdf"))
make.barplot(prcors, paste0(wood, "-Precipitation"))
make.barplot(tavgcors, paste0(wood,"-Temperature"))
make.barplot(tmincors, paste0(wood,"Max. Temperature"))
make.barplot(tmaxcors, paste0(wood,"Min. Temperature"))
make.barplot(pdsicors, paste0(wood,"PDSI"))
dev.off()


# afunction to plot correlations of growth to climate in a barplot
make.barplotHIC <- function(x,y){
  x <- as.matrix(x[,1])
  months <- c("pJan", "pFeb","pMar", "pApr", "pMay", "pJun",
              "pJul", "pAug", "pSep", "pOct", "pNov", "pDec",
              "Jan", "Feb","Mar", "Apr", "May", "Jun",
              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  row.names(x)<- months
  colours <- c("brown1")
  barplot(t(x),ylim= c(-0.4, 0.5), beside = TRUE, col = colours, las = 2, main = paste(y))
  legend("topleft", c("HICKORY GROVE"),cex=1.3, bty="n", fill = colours )
  abline(h = critical.r(120))
  abline(h = critical.r(120)*-1)
  barplot(t(x),ylim= c(-0.4, 0.5), beside = TRUE, col = colours, las = 2, main = paste(y), add = T)
  box()
}

make.barplotBON <- function(x,y){
  x <- as.matrix(x[,2])
  months <- c("pJan", "pFeb","pMar", "pApr", "pMay", "pJun",
              "pJul", "pAug", "pSep", "pOct", "pNov", "pDec",
              "Jan", "Feb","Mar", "Apr", "May", "Jun",
              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  row.names(x)<- months
  colours <- c("darkblue")
  barplot(t(x),ylim= c(-0.4, 0.5), beside = TRUE, col = colours, las = 2, main = paste(y))
  legend("topleft", c("Bonanza Prairie"),cex=1.3, bty="n", fill = colours )
  abline(h = critical.r(120))
  abline(h = critical.r(120)*-1)
  barplot(t(x),ylim= c(-0.4, 0.5), beside = TRUE, col = colours, las = 2, main = paste(y), add = T)
  box()
}

#make separate barplots here
pdf(paste0(wood ,"sidebysidesites.pdf"))
par(mfrow = c(2,1))
make.barplotHIC(prcors, paste0(wood,"-HIC Precipitation"))
make.barplotBON(prcors, paste0(wood,"-BON Precipitation"))
                
make.barplotHIC(tavgcors, paste0(wood, "-HIC Precipitation"))
make.barplotBON(tavgcors, paste0(wood, "-BON Precipitation"))

make.barplotHIC(tmincors, paste0(wood,"-HIC Max. Temperature"))
make.barplotBON(tmincors, paste0(wood,"-BON Max. Temperature"))

make.barplotHIC(tmaxcors, paste0(wood,"-HIC Min. Temperature"))
make.barplotBON(tmaxcors, paste0(wood,"-BON Min. Temperature"))

make.barplotHIC(pdsicors, paste0(wood,"-HIC PDSI"))
make.barplotBON(pdsicors, paste0(wood,"-BON PDSI"))

dev.off()
