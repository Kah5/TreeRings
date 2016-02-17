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

pdf("site.compare.pdf")
make.barplot(prcors, "Precipitation")
make.barplot(tavgcors, "Temperature")
make.barplot(tmincors, "Max. Temperature")
make.barplot(tmaxcors, "Min. Temperature")
make.barplot(pdsicors, "PDSI")
dev.off()




##########older code
library(reshape2)
prs.long<-melt(prcors,id.vars="months")

g1<-ggplot(prs.long, aes(x = factor(months), y = value , 
                       fill=factor(variable),color=factor(variable))) + 
  geom_bar(stat = "identity", position=position_dodge(3)) 
  