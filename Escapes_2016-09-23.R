
require(ggplot2)
require(lubridate)
require(dplyr)

### read in data on farmed escapes from Morris et al paper
morris <- read.csv("./Data/Morris et al 2008 data.csv")

str(morris)

### convert formatting and get rid of "bad" rows
morris$Year <- as.character(morris$Year)
morris$YearSimp <- as.numeric(morris$Year)

morris$PropFarmed <- as.character(morris$PropFarmed)
morris$PropSimp <- as.numeric(morris$PropFarmed)

morris$clean <- ifelse(is.na(morris$YearSimp) == "TRUE" | is.na(morris$PropSimp)=="TRUE", "BAD", "GOOD")

morris_clean <- subset(morris, clean=="GOOD")

### subset by province
morris_cleanME <- subset(morris_clean, ProvState=="ME")
morris_cleanNB <- subset(morris_clean, ProvState=="NB")
morris_cleanNL <- subset(morris_clean, ProvState=="NL")
morris_cleanNS <- subset(morris_clean, ProvState=="NS")

### plot by province
plot1 <- ggplot() + geom_point(data=morris_cleanME, aes(YearSimp, PropSimp)) + theme_bw() +
  facet_grid(~River)
plot2 <- ggplot() + geom_point(data=morris_cleanNB, aes(YearSimp, PropSimp)) + theme_bw() +
  facet_grid(~River)
plot3 <- ggplot() + geom_point(data=morris_cleanNL, aes(YearSimp, PropSimp)) + theme_bw() +
  facet_grid(~River)
plot4 <- ggplot() + geom_point(data=morris_cleanNS, aes(YearSimp, PropSimp)) + theme_bw() +
  facet_grid(~River)

require(gridExtra)

grid.arrange(plot1, plot2, plot3, plot4, nrow=4)


### read in open cage site coordinates
sites <- read.csv("./Data/Open Cage Sites.csv")
sites$Lat <- as.character(sites$Lat)

### convert from deg min sec to decimal deg
coords.lat <- colsplit(sites$Lat, ",", c("deg", "min", "sec"))
coords.lon <- colsplit(sites$Lon, ",", c("deg", "min", "sec"))

sites$LatDeg <- as.numeric(coords.lat$deg)
sites$LatMin <- as.numeric(coords.lat$min)
sites$LatSec <- as.numeric(sub(",", "", coords.lat$sec))
sites$LonDeg <- as.numeric(coords.lon$deg)
sites$LonMin <- as.numeric(coords.lon$min)
sites$LonSec <- as.numeric(sub(",", "", coords.lon$sec))

sites$LatMin <- sites$LatMin/60
sites$LatSec <- sites$LatSec/3600
sites$LonMin <- sites$LonMin/60
sites$LonSec <- sites$LonSec/3600
sites$LatMinSec <- sites$LatMin+sites$LatSec
sites$LonMinSec <- sites$LonMin+sites$LonSec

sites$Latitude <- paste0(sites$LatDeg, ".", sub("0.", "", sites$LatMinSec))
sites$Longitude <- paste0(sites$LonDeg, ".", sub("0.", "", sites$LonMinSec))

sites$Latitude <- as.numeric(sites$Latitude)
sites$Longitude <- as.numeric(sites$Longitude)
### Use sites$Latitude and sites$Longitude from here on

require(sp)
require(maptools)
require(ggmap)
require(ggplot2)
require(reshape2)
NS <- readShapeSpatial("./R/canvec/canvec_250K_NS_Land_shp/shoreline_1.shp", proj4string=CRS("+proj=longlat +ellps=GRS80 +no_defs"))
NS <- fortify(model=NS)
NB <- readShapeSpatial("./R/canvec/canvec_250K_NB_Land_shp/shoreline_1.shp", proj4string=CRS("+proj=longlat +ellps=GRS80 +no_defs"))
NB <- fortify(model=NB)
NL <- readShapeSpatial("./R/canvec/canvec_250K_NL_Land_shp/shoreline_1.shp", proj4string=CRS("+proj=longlat +ellps=GRS80 +no_defs"))
NL <- fortify(model=NL)

ggplot() + geom_path(data=NS, aes(long, lat, group=group)) +
  geom_path(data=NB, aes(long, lat, group=group)) +
  geom_path(data=NL, aes(long, lat, group=group)) + 
  geom_point(data=sites, aes(Longitude, Latitude), size=5, colour="red") + 
  xlim(-69.6, -50.7)+
  ylim(42.82, 48.67) +
  theme_bw()
### MONDAY AM: continue from here with new data! 
### Can you get new shapefiles that are less complex? This takes forever to plot.
