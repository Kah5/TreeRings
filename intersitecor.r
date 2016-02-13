#making pretty site level corrleation comparisons
HICp<- read.csv("HIC-WWPrecipcor.csv")
BONp <- read.csv("BON-WWPrecipcor.csv")

months <- c("pJan", "pFeb","pMar", "pApr", "pMay", "pJun",
            "pJul", "pAug", "pSep", "pOct", "pNov", "pDec",
            "Jan", "Feb","Mar", "Apr", "May", "Jun",
            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

prcors<- data.frame(HIC = HICp$V1, 
          BON = BONp$V1,
           months = months)

HICtmin <- read.csv("HIC-WWtmincor.csv")
BONtmin <- read.csv("BON-WWtmincor.csv")

tmincors<- data.frame(HIC = HICtmin$V1, 
                    BON = BONtmin$V1,
                    months = months)

HICtmax <- read.csv("HIC-WWtmaxcor.csv")
BONtmax <- read.csv("BON-WWtmaxcor.csv")

tmaxcors<- data.frame(HIC = HICtmax$V1, 
                      BON = BONtmax$V1,
                      months = months)

HICpdsi <- read.csv("HIC-WWpdsicor.csv")
BONpdsi <- read.csv("BON-WWpdsicor.csv")

pdsicors<- data.frame(HIC = HICpdsi$V1, 
                      BON = BONpdsi$V1,
                      months = months)

HICtavg <- read.csv("HIC-WWtavgcor.csv")
BONtavg <- read.csv("BON-WWtavgcor.csv")

tavgcors<- data.frame(HIC = HICtavg$V1, 
                      BON = BONtavg$V1,
                      months = months)

# afunction to plot correlations of growth to climate in a barplot
make.barplot <- function(x){
x <- as.matrix(x[,1:2])
months <- c("Jan", "Feb","Mar", "Apr", "May", "Jun",
            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
            "pJan", "pFeb","pMar", "pApr", "pMay", "pJun",
            "pJul", "pAug", "pSep", "pOct", "pNov", "pDec")
row.names(x)<- months
colours <- c("brown1", "darkblue")
barplot(t(x),ylim= c(-0.4, 0.5), beside = TRUE, col = colours, las = 2)
legend("bottomleft", c("HICKORY GROVE", "BONANZA PRAIRIE"),cex=1.3, bty="n", fill = colours )
box()
}
make.barplot(prcors)
make.barplot(tavgcors)
make.barplot(tmincors)
make.barplot(tmaxcors)
make.barplot(pdsicors)





##########older code
library(reshape2)
prs.long<-melt(prcors,id.vars="months")

g1<-ggplot(prs.long, aes(x = factor(months), y = value , 
                       fill=factor(variable),color=factor(variable))) + 
  geom_bar(stat = "identity", position=position_dodge(3)) 
  