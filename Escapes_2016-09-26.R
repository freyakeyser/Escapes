
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
sites <- read.csv("./Data/NL Stocking 2010-2013.csv")
str(sites)



require(sp)
require(maptools)
require(ggmap)
require(ggplot2)
require(reshape2)
require(RArcInfo)

## http://ftp.geogratis.gc.ca/pub/nrcan_rncan/vector/atlas/75m_prod/geog/shape/
prov <- readShapeSpatial("./R/canvec/pvp/pvp.shp", proj4string=CRS("+proj=longlat +ellps=GRS80 +no_defs"))
prov <- fortify(model=prov)

prov <- get.arcdata("./R/canvec/nflandg.e00/nflandg.e00")


ggplot() + 
  geom_polygon(data=prov, aes(long, lat, group=group), fill="grey")+
  geom_point(data=sites, aes(Long, Lat, size=Fish.Remaining...site), colour="red", shape=21) + 
  facet_wrap(~Year, nrow=4) +
  theme_bw() +
  coord_fixed(ratio=1.5, xlim=c(-57, -55), ylim=c(47, 48)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
  scale_size_continuous(name="Number\nof fish")


ggplot() + 
  geom_polygon(data=prov, aes(long, lat, group=group), fill="grey")+
  geom_point(data=sites, aes(Long, Lat, size=Number.of.Cages), colour="red", shape=21) + 
  facet_wrap(~Year, nrow=4) +
  theme_bw() +
  coord_fixed(ratio=1.5, xlim=c(-57, -55), ylim=c(47, 48)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
  scale_size_continuous(name="Number\nof cages")


### to calculate distances between points with boundaries, need to rasterize data
install.packages("raster")
require(raster)

r <- raster(ncol=500, nrow=1000)
prov <- subset(prov, NAME_E %in% c("Quebec", "New Brunswick", "Nova Scotia", "Newfoundland"))
extent(r) <- extent(prov)
provrast <- rasterize(prov, r, fun="first")
plot(provrast)

x <- sites$Long
y <- sites$Lat
xy <- cbind(x, y)
siterast <- as.matrix(xy)
siterast <- rasterize(xy, r, field=1)

x <- -57.0
y <- 47.5
xy <- cbind(x, y)
testrast <- as.matrix(xy)
testrast <- rasterize(xy, r, field=1)

plot(provrast, colNA="grey", col="white")

## INVERTS RASTER FILE!
provrast@data@values <- ifelse(is.na(provrast@data@values)=="TRUE", 1, 10000)
plot(provrast)
points(-57, 47.5)

## Need to add testrast and siterast to provrast in order to calculate distance between testrast and siterast
projection(siterast) <- "+proj=longlat +ellps=GRS80 +no_defs"
projection(testrast) <- "+proj=longlat +ellps=GRS80 +no_defs"
provrast <- addLayer(siterast, provrast)
provrast <- addLayer(testrast, provrast)

plot(provrast$layer, col="blue")

require(gdistance)
tr1 <- transition(siterast, transitionFunction=mean, directions=8)
tr2 <- transition(testrast, transitionFunction=mean, directions=8)
tr3 <- transition(provrast, transitionFunction=mean, directions=8)
tr1C <- geoCorrection(tr1, type="c")
tr2C <- geoCorrection(tr2, type="c")
tr3C <- geoCorrection(tr3, type="c")

d <- costDistance(tr3C, fromCoords = siterast, toCoords = testrast)
d

plot(provrast, xlim=c(-60, -50), ylim=c(46, 49))
