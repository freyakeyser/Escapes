
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



##########################################################################################################################
#################### SOUTHERN NEWFOUNDLAND CAGE SITE DISTANCES TEMPLATE CODE

require(sp)
require(maptools)
require(ggmap)
require(ggplot2)
require(reshape2)
require(rgdal)
require(raster)
require(rgeos)
require(PBSmapping)
require(gdistance)

## read in NL open cage site coordinates
sites <- read.csv("C:/Users/keyserf/Documents/Data/NL Stocking 2010-2013.csv")
str(sites)

## for low-res generic mapping
## http://ftp.geogratis.gc.ca/pub/nrcan_rncan/vector/atlas/75m_prod/geog/shape/
prov <- readShapeSpatial("./R/canvec/pvp/pvp.shp", proj4string=CRS("+proj=longlat +ellps=GRS80 +no_defs"))
#prov <- fortify(model=prov)

## Number of fish per site
ggplot() + 
  geom_polygon(data=prov, aes(long, lat, group=group), fill="grey")+
  geom_point(data=sites, aes(Long, Lat, size=Fish.Remaining...site), colour="red", shape=21) + 
  facet_wrap(~Year, nrow=4) +
  theme_bw() +
  coord_fixed(ratio=1.5, xlim=c(-57, -55), ylim=c(47, 48)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
  scale_size_continuous(name="Number\nof fish")

## Number of cages per site
ggplot() + 
  geom_polygon(data=prov, aes(long, lat, group=group), fill="grey")+
  geom_point(data=sites, aes(Long, Lat, size=Number.of.Cages), colour="red", shape=21) + 
  facet_wrap(~Year, nrow=4) +
  theme_bw() +
  coord_fixed(ratio=1.5, xlim=c(-57, -55), ylim=c(47, 48)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
  scale_size_continuous(name="Number\nof cages")


## to calculate distances between points with boundaries, need fine scale data for raster
## SKIP DOWN IF ENVIRONMENT IS LOADED

## http://ftp.geogratis.gc.ca/pub/nrcan_rncan/vector/canvec/shp/Hydro/
#NL <- readOGR(dsn = "./R/canvec/canvec_50K_NL_Hydro_shp/waterbody_2_1.shp", layer="waterbody_2_1")
## THIS TAKES WAY TOO LONG, try lower resolution:
## http://ftp.geogratis.gc.ca/pub/nrcan_rncan/vector/canvec/shp/Land/
## NL.low <- readOGR(dsn = "./canvec/canvec_250K_NL_Hydro_shp/waterbody_2.shp", layer="waterbody_2")

# #cropping shapefile to the area and details I need
# NL.low.ocean <- crop(NL.low, extent(-57, -54, 47, 48))
# NL.low.ocean <- subset(NL.low.ocean, perm_en=="Permanent")
# writeOGR(obj=NL.low.ocean, dsn="canvec", layer="NL.low.ocean", driver="ESRI Shapefile")
# rm(NL.low)

# read in cropped shapefile: NL.low.ocean
# NL.low.ocean <- readOGR(dsn="./canvec/NL.low.ocean.dbf", layer="NL.low.ocean")
# 
# plot(NL.low.ocean)
# points(siterast, pch=16, col="red")
# # creating raster
# r <- raster(ncol=3000, nrow=2000)
# extent(r) <- extent(NL.low.ocean)
# NLrast <- rasterize(NL.low.ocean, r, fun="first")
# plot(NLrast)

##### START HERE IF ENVIRONMENT IS READY #####
# site coords
x <- sites$Long
y <- sites$Lat
xy <- cbind(x, y)
siterast <- as.matrix(xy)

# test escaped fish location
x <- -56.4
y <- 47.5
xy <- cbind(x, y)
x <- -55.7
y <- 47.35
xy2 <- cbind(x, y)
xy <- rbind(xy, xy2)
testrast <- as.matrix(xy)

# SKIP IF ENVIRONMENT LOADED! change values in raster so that land is impassable (i.e. distances would be massive). 
# Also get rid of ocean blob
# NLrast@data@values <- ifelse(is.na(NLrast@data@values)=="TRUE", 10000, 1)
# NLrast@data@values <- ifelse(coordinates(NLrast)[,1] < -56 & coordinates(NLrast)[,2] < 47.4, 1, NLrast@data@values)

# create transition layer for raster (likelihood of presence within cell)
# tr3 <- transition(NLrast, transitionFunction=mean, directions=8)
# tr3c <- geoCorrection(tr3, type="c", scl=TRUE)

# calculate least-cost distance between siterast (site location) and testrast (escaped fish location)
d <- costDistance(tr3c, fromCoords = testrast, toCoords = siterast)
d 

# pick the shortest spatiallines obj
distances <- data.frame(test1 = d[1,], test2=d[2,])
distances <- cbind(siterast, distances)
distances$shorter <- ifelse(distances$test1 < distances$test2, "test1", "test2")
short1 <- subset(distances, shorter=="test1")
short2 <- subset(distances, shorter=="test2")

# get spatiallines obj for shortest path
sPath1 <- shortestPath(tr3c, testrast[1,], as.matrix(cbind(short1$x, short1$y)), output="SpatialLines")
sPath2 <- shortestPath(tr3c, testrast[2,], as.matrix(cbind(short2$x, short2$y)), output="SpatialLines")

plot(NLrast)
points(testrast, pch=16, col="red")
points(-55.5714, 47.5533, pch=16)
points(siterast)
lines(sPath1)
lines(sPath2)

# what about using ggplot?
# need to make a df first
# NLrastplot <- data.frame(rasterToPoints(NLrast))
# ggplot() + geom_raster(data=NLrastplot, aes(x, y, fill=as.factor(layer))) 


