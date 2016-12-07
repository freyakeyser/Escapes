---
title: "AQ Sites NB_2016-11-22"
output: html_document
---

```{r}
require(reshape2)
require(plyr)
require(sp)
require(maptools)
require(ggplot2)
require(rgdal)
require(lubridate)
require(dplyr)
require(ggmap)
require(raster)
require(rgeos)
require(PBSmapping)
require(gdistance)
```

```{r}
NB.low <- readOGR(dsn = "C:/Users/keyserf/Documents/R/canvec/canvec_250K_NB_Hydro_shp/waterbody_2.shp", layer="waterbody_2")
NBprov <- fortify(NB.low)
rm(NB.low)

plot(NB.low)
NB.small <- crop(NB.low, extent(-59.7, -52.4, 46.6, 51.8))
NBborder <- click(n=4)
NB.BOF <- crop(NB.low, extent(-67.2, -66.3, 44.5, 45.25))

plot(NB.BOF)


# Passamaquoddy Bay
NB.PMQ <- crop(NB.BOF, extent(-67.1, -66.85, 44.85, 45.19))               
plot(NB.PMQ)

ID <- as.character(c("1077", "0342", "0061", "0504", "0084", "0214", "0502", "0290", "0370", "0256", "0251", "0042",
                     "0377", "0228", "0222", "0215", "0045", "0046", "0044", "0179", "0320", "0060", "0059", "0058",
                     "0057", "0051", "0050", "0049", "0411", "0052", "0053", "0054", "0206", "0186", "0055", "0168", "0056"))

#coords.PMQ <- click(n=37)
#write.csv(coords.PMQ, "C:/Users/keyserf/Documents/Data/R Output/coords.PMQ.csv")
coords.PMQ <- read.csv("C:/Users/keyserf/Documents/Data/R Output/coords.PMQ.csv")

PMQ <- data.frame(cbind(ID, coords.PMQ))

# Grand Manan Island
GMI <- click(n=4)
NB.GMI <- crop(NB.BOF, extent(-66.92, -66.65, 44.55, 44.8))               
plot(NB.GMI)

ID <- as.character(c("0213", "0368", "0350", "0002", "0349", "0282", "0298", "0300", "0172", "0503", "0316", "0381", "0416",
                     "0303", "0202", "0292", "0003", "0403", "0270", "0491", "0408", "0413"))

# coords.GMI <- click(n=22)
# write.csv(coords.GMI, "C:/Users/keyserf/Documents/Data/R Output/coords.GMI.csv")
coords.GMI <- read.csv("C:/Users/keyserf/Documents/Data/R Output/coords.GMI.csv")
GMI <- data.frame(cbind(ID, coords.GMI))

# Back Bay
BBA <- click(n=4)
NB.BBA <- crop(NB.BOF, extent(-66.92, -66.76, 45.0, 45.1))               
plot(NB.BBA)

ID <- as.character(c("0040", "0039", "0038", "0037", "0276", "0036", "0035", "0034", "0033", "0159", "0095", 
                      "0032", "0029", "0027", "0026", "0025", "0023", "0022", "0024", "0020", "0018", "0016", "0014", "0017"))

coords.BBA <- click(n=24)
# write.csv(coords.BBA, "C:/Users/keyserf/Documents/Data/R Output/coords.BBA.csv")
coords.BBA <- read.csv("C:/Users/keyserf/Documents/Data/R Output/coords.BBA.csv")

BBA <- data.frame(cbind(ID, coords.BBA))

# Maces Bay
MAB <- click(n=4)
NB.MAB <- crop(NB.BOF, extent(-66.79, -66.32, 45.02, 45.16))               
plot(NB.MAB)

ID <- as.character(c("0010", "0012", "0508", "0378", "0496", "0400", "0507", "0412", "0404", "0501", "0495", "0494"))

coords.MAB <- click(n=12)
# write.csv(coords.MAB, "C:/Users/keyserf/Documents/Data/R Output/coords.MAB.csv")
coords.MAB <- read.csv("C:/Users/keyserf/Documents/Data/R Output/coords.MAB.csv")

MAB <- data.frame(cbind(ID, coords.MAB))

NB.BOF.sites <- rbind(PMQ, GMI, BBA, MAB)
NB.BOF.sites$Area <- c(rep("PMQ", 37), rep("GMI", 22), rep("BBA", 24), rep("MAB", 12))

NB.BOF.sites[,2] <- as.numeric(as.character(NB.BOF.sites[,2]))
NB.BOF.sites[,3] <- as.numeric(as.character(NB.BOF.sites[,3]))

plot(NB.BOF)
points(NB.BOF.sites$x, NB.BOF.sites$y, col="red")

NB.BOF.sites$ID <- as.character(NB.BOF.sites$ID)

NB.BOF.sites <- arrange(NB.BOF.sites, ID)

#write.csv(NB.BOF.sites, "C:/Users/keyserf/Documents/Data/R output/NB sites 2016-11-22.csv")

NB.BOF.sites <- read.csv("C:/Users/keyserf/Documents/Data/R output/NB sites 2016-11-22.csv")
NB.BOF.sites


ggplot() + geom_polygon(data=NBprov, aes(long, lat, group=group)) +
  geom_point(data=NB.BOF.sites, aes(x, y, colour=Area)) +
  coord_map(xlim = c(-68, -66), ylim=c(44.5, 45.5))

## http://www.gulfofmaine-census.org/data-mapping/gis-data-layers/
## http://pubs.usgs.gov/of/2003/of03-001/data/basemaps/
GOM <- readOGR(dsn = "C:/Users/keyserf/Documents/R/canvec/PhysioRegions_WGS84.shp", layer="PhysioRegions_WGS84")
GOM <- fortify(GOM)
# USA <- readOGR(dsn = "C:/Users/keyserf/Documents/R/canvec/usa.shp", layer="usa")
# USA <- fortify(USA)
# CANE <- readOGR(dsn = "C:/Users/keyserf/Documents/R/canvec/canada.shp", layer="canada")
# CANE <- fortify(CANE)

GOMclip <- gDifference(GOM, NB.low)

#writeOGR(obj = GOMclip, dsn = "C:/Users/keyserf/Documents/R/canvec/", layer="GOMclip", driver="ESRI Shapefile")

GOMclip <- fortify(GOMclip)

require(ggspatial)

ggplot() + 
  geom_spatial(data=NB.low, aes(fill=ctry_en)) +
  scale_fill_manual(values=c("grey"), guide=FALSE) +
  geom_polygon(data=GOMclip, aes(long, lat, group=group), fill="grey") +
  geom_point(data=NB.BOF.sites, aes(x, y, colour=Area)) +
  coord_map(xlim = c(-67.25, -66.5), ylim=c(44.5, 45.25)) +
  theme_classic() +
  theme(panel.border=element_rect(fill=NA, colour="black"))

plot(NB.BOF)

```

### Now read in and clean-up stocking data
```{r}

NB_licence <- read.csv("C:/Users/keyserf/Documents/Data/Inventory data/NB 1998-2016 licensing_salar.csv")
head(NB_licence)

str(NB_licence)

NB_licence <- dplyr::select(NB_licence, Company, Species, Number, Site.ID, Issue.Date)

NB_licence$Issue.Date <- dmy(NB_licence$Issue.Date)
NB_licence$Year <- year(NB_licence$Issue.Date)

head(NB_licence)

```

### NB and NS AQ site map
```{r}

NSsitecoords <- arrange(NSsitecoords, County)
NSsitecoords$region <- NA
NSsitecoords$region[1:8] <- "OBF"
NSsitecoords$region[c(9:11, 16:19, 23:29, 31)] <- "SU"
NSsitecoords$region[c(12:15, 20:22, 30)] <- "CB"

NS_OBF <- subset(NSsitecoords, region=="OBF")
NS_SU <- subset(NSsitecoords, region=="SU")
NS_CB <- subset(NSsitecoords, region=="CB")

PEprov <- fortify(PE.low)

NB.land <- readOGR(dsn = "C:/Users/keyserf/Documents/R/canvec/canvec_250K_NB_Land_shp/island.shp", layer="island")
NBland <- fortify(NB.land)

rm(NB.low)

str(NB.land)

canada <- readOGR(dsn = "C:/Users/keyserf/Documents/R/canvec/canada.shp", layer="canada")
usa <- readOGR(dsn = "C:/Users/keyserf/Documents/R/canvec/usa.shp", layer="usa")

canada@data$cty <- "can"

can <- fortify(canada)
usa <- fortify(usa)

ggplot() + 
  #geom_polygon(data=NBprov, aes(long,lat, group=group), fill="grey") +
  # geom_spatial(data=NS.low, aes(fill=ctry_en)) +
  # geom_spatial(data=PE.low, aes(fill=ctry_en)) +
  #scale_fill_manual(values=c("grey"), guide=FALSE) +
  geom_polygon(data=NSprov, aes(long, lat, group=group), fill="grey") +
  geom_polygon(data=PEprov, aes(long, lat, group=group), fill="grey") +
  geom_polygon(data=GOM, aes(long, lat, group=group), fill="grey") +
  annotate(geom="rect", xmin=-60, xmax=-59.5, ymin=47.5, ymax= 48, fill="grey") +
  annotate(geom="rect", xmin=-64, xmax=-59.5, ymin=43.2, ymax= 44, fill="grey") +
  annotate(geom="rect", xmin=-61, xmax=-59.5, ymin=44, ymax= 45, fill="grey") +
  coord_map(xlim=c(-67.7, -59.5), ylim=c(43.2, 48)) +
  theme_classic() +
  theme(panel.border=element_rect(fill=NA, colour="black"))+
  # geom_point(data=NB.BOF.sites, aes(mean(x), mean(y)), size=5, colour="red") +
  # geom_point(data=NS_OBF, aes(mean(Longitude), mean(Latitude)), size=5, colour="red") +
  # geom_point(data=NS_SU, aes(mean(Longitude), mean(Latitude)), size=5, colour="red") +
  # geom_point(data=NS_CB, aes(mean(Longitude), mean(Latitude)), size=5, colour="red") 
  stat_density_2d(data=NSsitecoords, geom="polygon", position="identity", aes(Longitude, Latitude, group=region, fill=..level..), alpha=0.5) +
  stat_density_2d(data=NB.BOF.sites, geom="polygon", position="identity", aes(x,y, fill=..level..), alpha=0.5) +
  scale_fill_gradientn(colours=c("yellow", "red")) +
  geom_polygon(data=canada, aes(long, lat, group=group), fill="white")+
  geom_spatial(data=usa, aes(long, lat, group=group), fill="white") +
  geom_path(data=CA.df, aes(long, lat, group=group),colour="black")


NSnew<- readOGR(dsn = "C:/Users/keyserf/Documents/R/canvec/NS_NSUSER_NS_Roads_HW2_line.shp", layer="NS_NSUSER_NS_Roads_HW2_line")

