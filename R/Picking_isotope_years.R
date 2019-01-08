# find the similar drought and wet years before + after 1950:
sites <- c( "HIC","STC",#"GLA",
            "TOW", "ENG","UNC","BON","MOU",#"GLL4",
            "GLL3","GLL2", "GLL1", "PVC","AVO" )

for(j in 1:length(sites)){
  
# read in the similar years script + the wet + dry climates
sim.yr <- read.csv(paste0("outputs/data/Isotope_climate/",sites[j],"_most_similar_years.csv"))
dry.clim <- read.csv(paste0("outputs/data/Isotope_climate/",sites[j],"_wet_dry_climate_age_class.csv"))
dry.clim.u <- unique(dry.clim[,c("year", "class", "site", "PCP", "Jul.pdsi", "PDSI", "climclass")])

dry.similar.years <- dry.clim.u[dry.clim.u$climclass %in% "Dry_0.25" & dry.clim.u$year %in% sim.yr$Year | dry.clim.u$year %in% sim.yr$nearestyear,]
wet.similar.years <- dry.clim.u[dry.clim.u$climclass %in% "Wet_0.25" & dry.clim.u$year %in% sim.yr$Year | dry.clim.u$year %in% sim.yr$nearestyear,]


#dry.clim.u[dry.clim.u$climclass %in% "Dry_0.25" & dry.clim.u$Jul.pdsi < -4,]


# find the year with the closest jul pdsi in the other time period:

library(sp)
df <- spDists(as.matrix(dry.clim.u[,"Jul.pdsi"]))
row.names(df) <- dry.clim.u$year
colnames(df) <- dry.clim.u$year
i <- apply(df[as.numeric(rownames(df)) < 1950,as.numeric(colnames(df)) >= 1950],2, function (x) which(x %in% min(x[x != 0]) ))
dist <- apply(df[as.numeric(rownames(df)) < 1950, as.numeric(colnames(df)) >= 1950],2, function (x) min(x[x != 0]))

if(is.list(i)){
    i.df<- do.call(rbind, i)
    #i.df <- i
    yr.index <- row.names(df[as.numeric(rownames(df)) < 1950,as.numeric(colnames(df)) >= 1950])#<- yr.index[i.df[,2]]
    closest <- data.frame(Year = rownames(i.df),  closest = yr.index[i.df])
}else{
    i.df <- i
    yr.index <- row.names(df[as.numeric(rownames(df)) < 1950,as.numeric(colnames(df)) >= 1950])#<- yr.index[i.df[,2]]
    closest <- data.frame(Year = names(i.df),  closest = yr.index[i.df])
  }
# print out the closest year into a column:
#closest$nearestyear <- closest[closest$closest,]$Year 

#closest <- data.frame(Year = names(i.df),  closest = yr.index[i.df])
#find the years of closest high drought conditions:
test <- merge(dry.clim.u[,c("year", "Jul.pdsi")], closest, by.x = "year", by.y = "Year")
closest_PDSI <- merge(dry.clim.u[,c("year", "Jul.pdsi")], test, by.x = "year", by.y = "closest")
colnames(closest_PDSI) <- c("pre-1950", "pre-1950_jul_PSDI", "post-1950","post-1950_jul_PDSI")
closest.ordered <- closest_PDSI[order(closest_PDSI$`pre-1950_jul_PSDI`),] # order by PDSI


write.csv(closest.ordered, paste0("/Users/kah/Documents/TreeRings/outputs/data/Isotope_climate/",sites[j],"_closest_drought_yrs.csv"), row.names = FALSE)

# find the years of closest low drought conditions:
test <- merge(dry.clim.u[,c("year", "Jul.pdsi")], closest, by.x = "year", by.y = "Year")
closest_PDSI <- merge(dry.clim.u[,c("year", "Jul.pdsi")], test, by.x = "year", by.y = "closest")
colnames(closest_PDSI) <- c("pre-1950", "pre-1950_jul_PSDI", "post-1950","post-1950_jul_PDSI")
closest.ordered <- closest_PDSI[order(closest_PDSI$`pre-1950_jul_PSDI`),] # order by PDSI


write.csv(closest.ordered, paste0("/Users/kah/Documents/TreeRings/outputs/data/Isotope_climate/",sites[j],"_closest_drought_yrs.csv"), row.names = FALSE)



}
