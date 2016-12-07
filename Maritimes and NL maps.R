## Freya's hacky ways to make Atl. Canada maps look good enough...

require(ggspatial)
require(ggplot2)
require(rgdal)
require(rgeos)
require(sp)
require(plyr)

### HIGHLY RECOMMENDED: once you've read in shapefiles and cropped them, use writeOGR to save the cropped versions 
### for future use. This means you won't have to read in the massive shapefiles ever again.


##### Maritimes (canvec 250K hydro shapefiles, canvec_1M_CA_Land_shp shoreline_1 shapefile, GOMclip shapefile)

## http://ftp.geogratis.gc.ca/pub/nrcan_rncan/vector/canvec/shp/Land/
#CAshp <- readOGR(dsn = "C:/Users/keyserf/Documents/R/canvec/canvec_1M_CA_Land_shp/shoreline_1.shp", layer="shoreline_1")
#CA.low <- crop(CAshp, extent(-67.7, -59.5, 43.2, 48.1))
#CA.low@data$id <- rownames(CA.low@data)
#writeOGR(CA.low, "C:/Users/keyserf/Documents/R/Escapes/canvec/CA.low.shp", layer="CA.low", driver="ESRI Shapefile")
CA.low <- readOGR("C:/Users/keyserf/Documents/R/Escapes/canvec/CA.low.shp", layer="CA.low")
CA.df <- fortify(CA.low, region="id")
# CA.df = join(CA.low.points, CA.low@data, by="id")

## http://ftp.geogratis.gc.ca/pub/nrcan_rncan/vector/canvec/shp/Hydro/
# NS.low <- readOGR(dsn = "C:/Users/keyserf/Documents/R/canvec/canvec_250K_NS_Hydro_shp/waterbody_2.shp", layer="waterbody_2")
NS.low <- crop(NS.low, extent(-66.25, -59.5, 43.4, 47.5))

## http://ftp.geogratis.gc.ca/pub/nrcan_rncan/vector/canvec/shp/Hydro/
NB.low <- readOGR(dsn = "C:/Users/keyserf/Documents/R/canvec/canvec_250K_NB_Hydro_shp/waterbody_2.shp", layer="waterbody_2")

## http://ftp.geogratis.gc.ca/pub/nrcan_rncan/vector/canvec/shp/Hydro/
PE.low <- readOGR(dsn = "C:/Users/keyserf/Documents/R/canvec/canvec_250K_PE_Hydro_shp/waterbody_2.shp", layer="waterbody_2")

## http://www.gulfofmaine-census.org/data-mapping/gis-data-layers/
# GOM <- readOGR(dsn = "C:/Users/keyserf/Documents/R/canvec/PhysioRegions_WGS84.shp", layer="PhysioRegions_WGS84")
GOMclip <- gDifference(GOM, NB.low)
GOMclip <- fortify(GOMclip)


ggplot() + 
  geom_spatial(data=NB.low, aes(fill=ctry_en)) + ### canvec spatialpolygonsdataframe
  geom_spatial(data=PE.low, aes(fill=ctry_en)) + ### canvec spatialpolygonsdataframe
  geom_spatial(data=NS.low, aes(fill=ctry_en)) + ### canvec spatialpolygonsdataframe
  scale_fill_manual(values=c("grey"), guide=FALSE) + ### fills geom_spatials with grey
  geom_polygon(data=GOMclip, aes(long, lat, group=group), fill="grey") + ### Gulf of Maine shapefile to get Grand Manan & Maine
  geom_path(data=CA.df, aes(long, lat, group=group),colour="black") + ### plots black outline on provinces
  annotate(geom="rect", xmin=-60, xmax=-59.5, ymin=47.5, ymax= 48, fill="grey") + ### these annotates hide white space!
  annotate(geom="rect", xmin=-64, xmax=-59.5, ymin=43.2, ymax= 44, fill="grey") +
  annotate(geom="rect", xmin=-61, xmax=-59.5, ymin=44, ymax= 45, fill="grey") +
  coord_map(xlim=c(-67.7, -59.5), ylim=c(43.2, 48)) + ### sets boundaries
  theme_classic() + ### my favourite theme settings here and below (makes land white)
  theme(panel.background = element_blank(), panel.border=element_rect(colour="black", fill=NA)) 





### Newfoundland (canvec 250K hydro shapefile, canvec_1M_CA_Land_shp shoreline_1 shapefile cropped)

## http://ftp.geogratis.gc.ca/pub/nrcan_rncan/vector/canvec/shp/Land/
# CAshp <- readOGR(dsn = "C:/Users/keyserf/Documents/R/canvec/canvec_1M_CA_Land_shp/shoreline_1.shp", layer="shoreline_1")
# CA.NL <- crop(CAshp, extent(-60, -52, 46.5, 52))
# CA.NL@data$id <- rownames(CA.NL@data)
CA.NL <- readOGR("C:/Users/keyserf/Documents/R/canvec/CA.NL.shp", layer="CA.NL")
CA.NL <- fortify(CA.NL, region="id")

## http://ftp.geogratis.gc.ca/pub/nrcan_rncan/vector/canvec/shp/Hydro/
# NLshp <- readOGR(dsn = "C:/Users/keyserf/Documents/R/canvec/canvec_250K_NL_Hydro_shp/waterbody_2.shp", layer="waterbody_2")
# NL.low <- crop(NLshp, extent(-59.92, -52.3, 46.52, 51.78))
NL.low <- readOGR("C:/Users/keyserf/Documents/R/canvec/NL.low.shp", layer="NL.low")

##Southern NL

ggplot() + 
  geom_spatial(data=NL.low, aes(fill=ctry_en)) + ### canvec spatialpolygonsdataframe
  scale_fill_manual(values=c("grey"), guide=FALSE) + ### fills geom_spatial with grey
  geom_path(data=CA.NL, aes(long, lat, group=group),colour="black") + ### black outline
  annotate(geom="rect", xmin=-60, xmax=-56.5, ymin=46.5, ymax= 47.25, fill="grey") + ### hides stupid white spaces
  annotate(geom="rect", xmin=-60, xmax=-56.1, ymin=46.5, ymax= 47.4, fill="grey") +
  annotate(geom="rect", xmin=-60, xmax=-59.42, ymin=46.5, ymax= 50.21, fill="grey")+
  annotate(geom="rect", xmin=-56.1, xmax=-55.9, ymin=46.5, ymax= 46.83, fill="grey") +
  annotate(geom="rect", xmin=-54, xmax=-52.2, ymin=50, ymax= 51.8, fill="grey") +
  coord_map(xlim=c(-59.9, -52.3), ylim=c(46.52, 51.79)) + ### sets boundaries
  theme_classic() + ### favourite theme settings plus makes sure land is white
  theme(panel.background = element_blank(), panel.border=element_rect(colour="black", fill=NA)) 

### Entire island of Newfoundland

NLprov <- readOGR("C:/Users/keyserf/Documents/R/Escapes/canvec/NLprov.shp", layer="NLprov")

plot(NLprov)
