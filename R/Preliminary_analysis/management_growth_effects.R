# 
# looking at managment an release data as well as management and climate data

MCCD <- readOGR(dsn = "/Users/kah/Documents/TreeRings/data/MCCD_LandManagement/MCCD_LandManagement/", layer = "MCCD_LandManagement.gdb" )

require(rgdal)

# The input file geodatabase
fgdb <- "/Users/kah/Documents/TreeRings/data/MCCD_LandManagement/MCCD_LandManagement.gdb"

# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)
print(fc_list)

# Read the feature class
fc <- readOGR(dsn=fgdb,layer="BurnMaster_New")

# Determine the FC extent, projection, and attribute information
summary(fc)

# View the feature class
plot(fc)

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> compare climate data and release data: <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# read in the release data:
releases <- read.csv("outputs/release/lorimer/full_site_releases_lorimer.csv")

# read in climate data:
det.age.clim.prism.df <- read.csv("outputs/data/full_det_prism_rwi.csv")
det.age.clim.ghcn.df <- read.csv("outputs/data/full_det_ghcn_rwi.csv")

prism.df <- unique(det.age.clim.prism.df[,c("year", "site", "PCP", "TMIN", "TAVG", "VPDmax", "jja.VPDmax", "BAL",
                                "MAY.p", "JJA.p", "JUNTmin", "JUNTavg", "JUNTmax", "jul.VPDmax", "jul.BAL")])

ghcn.df <- unique(det.age.clim.ghcn.df[,c("year", "site", "PCP", "TMIN", "TAVG", "PDSI", "JJA.pdsi", "MAY.p",
                                             "JJA.p", "JUNTmin", "JUNTavg", "JUNTmax", "Jul.pdsi")])

prism.releases <- merge(releases, prism.df, by.x = c("AllReleasesYear", "site"),by.y = c("year", "site"))
ghcn.releases <- merge(releases, ghcn.df, by.x = c("AllReleasesYear", "site"),by.y = c("year", "site"))


ggplot(prism.releases, aes(VPDmax, NumberOfAllReleses))+geom_point()
ggplot(prism.releases, aes(PCP, NumberOfAllReleses))+geom_point()
ggplot(prism.releases, aes(JUNTmin, NumberOfAllReleses))+geom_point()


ggplot(prism.releases, aes(AllReleasesYear, PCP))+geom_line()
ggplot(prism.releases, aes(AllReleasesYear, NumberOfAllReleses))+geom_point()

head(prism.releases)

ggplot(prism.releases, aes(AllReleasesYear, NumberOfAllReleses))+geom_density(stat = "identity")
