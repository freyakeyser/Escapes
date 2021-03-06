
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
# r <- raster(ncol=2250, nrow=1120)
r <- raster(ncol=1125, nrow=560)
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
tr3 <- transition(1/NLrast, transitionFunction=mean, directions=8)
tr3c <- geoCorrection(tr3, type="c")

# calculate least-cost distance between siterast (site location) and testrast (escaped fish location)
d2 <- costDistance(tr3c, fromCoords = testrast, toCoords = siterast)
d2 

# pick the shortest spatiallines obj
distances <- data.frame(test1 = d2[1,], test2=d2[2,])
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
points(-56.2197, 47.6495, pch=16, col="yellow") ## 22.212 km

# what about using ggplot?
# need to make a df first
# NLrastplot <- data.frame(rasterToPoints(NLrast))
# ggplot() + geom_raster(data=NLrastplot, aes(x, y, fill=as.factor(layer))) 


##########################################################################################################################
#################### NOVA SCOTIA CAGE SITE DISTANCES TEMPLATE CODE

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

## read in NS open cage site coordinates
# sites <- read.csv("C:/Users/keyserf/Documents/Data/NL Stocking 2010-2013.csv")
# str(sites)

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
NS.low <- readOGR(dsn = "./canvec/canvec_250K_NS_Hydro_shp/waterbody_2.shp", layer="waterbody_2")

# #cropping shapefile to the area and details I need
# NS.low.ocean <- crop(NS.low, extent(-66.7, -59.54, 43.2, 47.3))
# writeOGR(obj=NS.low.ocean, dsn="canvec", layer="NS.low.ocean", driver="ESRI Shapefile")
# rm(NS.low)

# read in cropped shapefile: NS.low.ocean
# NS.low.ocean <- readOGR(dsn="./canvec/NS.low.ocean.dbf", layer="NS.low.ocean")
# 

# break up NS into reasonable components: CB, Eastern Shore, South Shore, Northumberland. Add BOF with NB data.
NS.CB <- crop(NS.low.ocean, extent(-61.65, -59.54, 45.4, 47.1))
plot(NS.CB)

NS.ES <- crop(NS.low.ocean, extent(-63.7, -60.9, 44.5, 45.4))
plot(NS.ES)

NS.SS <- crop(NS.low.ocean, extent(-66.4, -63.35, 43.33, 44.7))
plot(NS.SS)

NS.NO <- crop(NS.low.ocean, extent(-64.88, -61.41, 45.54, 46.58))
plot(NS.NO)
# 2500 long, 1500 tall

# creating raster
#NS.CB
r <- raster(nrow=1850, ncol=1600)
extent(r) <- extent(NS.CB)
CBrast <- rasterize(NS.CB, r, fun="first")
plot(CBrast)

#NS.ES
r <- raster(nrow=1000, ncol=2200)
extent(r) <- extent(NS.ES)
ESrast <- rasterize(NS.ES, r, fun="first")
plot(ESrast)

#NS.SS
r <- raster(nrow=1650, ncol=2300)
extent(r) <- extent(NS.SS)
SSrast <- rasterize(NS.SS, r, fun="first")
plot(SSrast)

#NS.NO
r <- raster(nrow=1400, ncol=2700)
extent(r) <- extent(NS.NO)
NOrast <- rasterize(NS.NO, r, fun="first")
plot(NOrast)

##### START HERE IF ENVIRONMENT IS READY #####
# site coords
# x <- 
# y <- sites$Lat
xy <- cbind(-60.869, 45.612)
siterast <- as.matrix(xy)

# test escaped fish location
# x <- -56.4
# y <- 47.5
xy <- cbind(-60.614, 45.606)
# x <- -55.7
# y <- 47.35
# xy2 <- cbind(x, y)
# xy <- rbind(xy, xy2)
testrast <- as.matrix(xy)

# SKIP IF ENVIRONMENT LOADED! change values in raster so that land is impassable (i.e. distances would be massive). 
# Also get rid of ocean blob
CBrast@data@values <- ifelse(is.na(CBrast@data@values)=="TRUE", 10000, 1)

ESrast@data@values <- ifelse(is.na(ESrast@data@values)=="TRUE", 10000, 1)
SSrast@data@values <- ifelse(is.na(SSrast@data@values)=="TRUE", 10000, 1)
NOrast@data@values <- ifelse(is.na(NOrast@data@values)=="TRUE", 10000, 1)
# rast@data@values <- ifelse(coordinates(NLrast)[,1] < -56 & coordinates(NLrast)[,2] < 47.4, 1, NLrast@data@values)

# create transition layer for raster (likelihood of presence within cell)
trCB <- transition(CBrast, transitionFunction=mean, directions=8)
trCBc <- geoCorrection(trCB, type="c")

# trES <- transition(ESrast, transitionFunction=mean, directions=8)
# trESc <- geoCorrection(trES, type="c", scl=TRUE)

# trSS <- transition(SSrast, transitionFunction=mean, directions=8)
# trSSc <- geoCorrection(trSS, type="c", scl=TRUE)

# trNO <- transition(NOrast, transitionFunction=mean, directions=8)
# trNOc <- geoCorrection(trNO, type="c", scl=TRUE)


# calculate least-cost distance between siterast (site location) and testrast (escaped fish location)
d <- costDistance(trCBc, fromCoords = testrast, toCoords = siterast)
d 

# pick the shortest spatiallines obj
distances <- data.frame(test1 = d[1,], test2=d[2,])
distances <- cbind(siterast, distances)
distances$shorter <- ifelse(distances$test1 < distances$test2, "test1", "test2")
short1 <- subset(distances, shorter=="test1")
short2 <- subset(distances, shorter=="test2")

# get spatiallines obj for shortest path
sPath1 <- shortestPath(trCBc, testrast, siterast, output="SpatialLines")
sPath2 <- shortestPath(trCBc, testrast[2,], as.matrix(cbind(short2$x, short2$y)), output="SpatialLines")

plot(CBrast)
points(testrast, pch=16, col="red")
points(siterast)
lines(sPath1)
lines(sPath2)

# what about using ggplot?
# need to make a df first
# NLrastplot <- data.frame(rasterToPoints(NLrast))
# ggplot() + geom_raster(data=NLrastplot, aes(x, y, fill=as.factor(layer))) 

########################################################################################################
################### Distances to all coastal points
#getwd()
setwd("C:/Users/keyserf/Documents/")
NLline <- readOGR("./R/canvec/canvec_250K_NL_Land_shp/shoreline_1_small.shp", "shoreline_1_small")
extent(NLline)
NLline <- crop(NLline, extent(-57, -54, 47, 48))

r <- raster(ncol=1800, nrow=1550)
extent(r) <- extent(NLline)
NLlinerast <- rasterize(NLline, r, fun="first")
NLlinerast@data@values <- ifelse(is.na(NLlinerast@data@values)=="FALSE", 10000, NA)

plot(NLrast)
plot(NLlinerast, col="black", add=TRUE)
plot(NLlinerast, col="black")
points(-55.5714, 47.5533, col="red")

NLpoints <- rasterToPoints(NLlinerast, spatial=TRUE)
NLpoints <- coordinates(NLpoints)

### need to fix site 47 because it is on land. move it to coast. 
ggplot() + geom_point(data=NLpoints, aes(point.long, point.lat), size=0.5) + 
  geom_point(data=sites[47,], aes(Long, Lat), colour="red") +
  coord_map() + 
  xlim(-55.5, -55.75) +
  ylim(47.4, 47.6)+
  annotate(geom="point", x=-55.605, y=47.532)

sites[47,6] <- 47.532
sites[47,7] <- -55.605

### also need to fix site 2 coords. 
ggplot() + geom_point(data=NLpoints, aes(point.long, point.lat), size=0.5) + 
  geom_point(data=sites[2,], aes(Long, Lat), colour="red") +
  coord_map() + 
  xlim(-56.5, -56.25) +
  ylim(47.6, 47.8)+
  annotate(geom="point", x=-56.325, y=47.708)

siterast[2,] <- c(-56.325, 47.708)
siterast[47,] <- c(-55.605, 47.532)

## use transition layer from polygons shapefile
d <- costDistance(tr3c, fromCoords = NLpoints, toCoords = siterast)

shortest.dist <- apply(d, 2, min)
shortest.col <- apply(d, 2, which.min)

coastdist <- data.frame(lat=sites$Lat, long = sites$Long, shortest.dist = shortest.dist, shortest.col = shortest.col)

NLpoints <- data.frame(NLpoints)
NLpoints <- data.frame(shortest.col = 1:37923, point.lat = NLpoints$y, point.long = NLpoints$x)

coastdist.join <- join(coastdist, NLpoints, type="left")

write.csv(coastdist.join, "./Data/coastdist.join.csv")

sPath <- shortestPath(tr3c, cbind(coastdist.join[,2], coastdist.join[,1]), cbind(coastdist.join[,6], coastdist.join[,5]), output="SpatialLines")

### test
sPathtest <- shortestPath(tr3c, cbind(coastdist.join[47,2], coastdist.join[47,1]), 
                          cbind(coastdist.join[47,6], coastdist.join[47,5]), output="SpatialLines")

??linelength
SpatialLinesLengths(sPathtest) 

plot(NLrast)
plot(NLlinerast, col="black")
lines(sPathtest)
points(coastdist.join[,2], coastdist.join[,1], pch=16, cex=2, col="red")
points(coastdist.join[,6], coastdist.join[,5], pch=16, col="yellow")


########################################################################################################
################### Import escapes data

locs <- readOGR("./Data/Angling Locations 2016.kml", "Angling Locations 2016")
plot(NLrast)
points(locs)
points(escapes$Longitude..E., escapes$Latitude..N., pch=16, col="red")


escapes <- read.csv("./Data/Gillnetting2015 - Master Copy.csv")
