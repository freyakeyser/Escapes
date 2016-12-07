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

##### Calculate standardized measure of fish for each site

inventory <- read.csv("C:/Users/keyserf/Documents/data/Inventory data/NL inventory master2_fixed.csv", colClasses = "character")

names(inventory) <- c("ID", "Cage", "NStart", "YC", "YCAdj", "NIntro", "NMort", "NHarvest", 
                      "NTransfer", "CountDev", "CountDevAbs", "NEscape", "NRemain", "ReportYear", "Bay", "Coord1", "Coord2")

str(inventory)

clean <- function(i) { 
  temp=gsub(pattern=",", replacement="",x=i)
  temp=gsub(pattern=" ", replacement="",x=temp)
  return(temp)
}

inventory[] <- sapply(inventory, clean)

inventory$NStart <- as.numeric(inventory$NStart)
inventory$NIntro <- as.numeric(inventory$NIntro)
inventory$NMort <- as.numeric(inventory$NMort)
inventory$NHarvest <- as.numeric(inventory$NHarvest)
inventory$NTransfer <- as.numeric(inventory$NTransfer)
inventory$CountDev <- as.numeric(inventory$CountDev)
inventory$CountDevAbs <- as.numeric(inventory$CountDevAbs)
inventory$NEscape <- as.numeric(inventory$NEscape)
inventory$NRemain <- as.numeric(inventory$NRemain)

inventory_clean <- subset(inventory, !ID=="")

inventory_clean[, 6:13][is.na(inventory_clean[, 6:13])] <- 0
inventory_clean[,3][is.na(inventory_clean[, 3])] <- 0
inventory_clean[, 6:13] <- abs(inventory_clean[, 6:13])

inventory_clean$ReportYear <- as.numeric(inventory_clean$ReportYear)

#inventory <- subset(inventory, ReportYear>2009)

################### CALCULATE FISH YEARS ########################

# 2 rules: 
#   1) morts, harvests and transfers occur mid-year
#   2) always assume max reported number of fish is most accurate
#   3) Introductions occur at beginning of year
# 3 cases: 
#   1) if NRemain = 0 and CountDev > 0    --->    (NStart + NIntro + CountDev)/2
#   2) if NRemain = 0 and CountDev <= 0   --->    (NStart + NIntro)/2
#   3) if NRemain > 0                     --->    NRemain + (NMort + NTransfer + NHarvest)/2

inventory_clean$case <- ifelse(inventory_clean$NRemain==0 & inventory_clean$CountDev > 0, 1, 
                         ifelse(inventory_clean$NRemain==0 & (inventory_clean$CountDev < 0 | inventory_clean$CountDev == 0), 2,
                                ifelse(inventory_clean$NRemain>0 , 3, NA)))

inventory_clean$fishcount <- ifelse(inventory_clean$case==1, (inventory_clean$NStart + inventory_clean$NIntro + inventory_clean$CountDev)/2,
                              ifelse(inventory_clean$case==2, (inventory_clean$NStart + inventory_clean$NIntro)/2, 
                                     ifelse(inventory_clean$case==3, inventory_clean$NRemain + 
                                              ((abs(inventory_clean$NMort) + abs(inventory_clean$NHarvest) + abs(inventory_clean$NTransfer))/2), NA)))

# events <- inventory

## need to clean up old data a bit. many missing ID value.

# old <- subset(inventory, ReportYear<2010)
# old_clean <- subset(old, !ID=="")
# 
# missing <- subset(old, ID=="")
# IDs <- ddply(.data=inventory, .(Bay),
#              summarize,
#              one = max(ID))
# 
# ### problem: some old data rows are missing IDs, and are for multiple sites. How to proceed? Average out between n locations??
### Fixed a lot in Excel. Moving ahead by excluding all pooled data.
# best <- subset(inventory, is.na(as.numeric(ID))==FALSE)

### until i get that figured out:
inventory <- subset(inventory, ReportYear >2009)

# ggplot() + 
#   geom_point(data=inventory, aes(ReportYear, fishcount)) + 
#   facet_wrap(~ID)

annual <- ddply(.data=inventory_clean, .(ID, ReportYear),
                summarize,
                totalfish=sum(fishcount))


standard <- ddply(.data=inventory_clean, .(ID),
                  summarize,
                  totalfish = sum(fishcount),
                  totalyears = length(unique(ReportYear)))

### Combine with coordinate data

sites <- read.csv("C:/Users/keyserf/Documents/Data/Inventory data/Site coordinates_NL_all.csv", header=TRUE)
str(sites)

sites <- subset(sites, select=c("Licence...", "Latitude", "Longitude"))
names(sites) <- c("ID", "Lat", "Long")

sites <- rbind(sites, data.frame(ID=c("1079", "1085", "1096", "1107"), 
                                 Lat=c(47.92217, 47.66130, 47.66155, 47.73218),
                                 Long=c(-55.95247, -55.13680, -56.51965, -56.32793)))

sites$ID <- as.character(sites$ID)
sites


### for only 2013 year
inv2013 <- subset(inventory_clean, ReportYear==2013)

totfish2013 <- ddply(.data=inv2013, .(ID),
                     summarize,
                     totalfish = sum(fishcount))

totfish2013 <- join(totfish2013, sites, type="left")
totfish2013
#######################################################################

totfishall <- ddply(.data=inventory_clean, .(ID),
                   summarize,
                   cages = length(unique(Cage)),
                   years = length(unique(ReportYear)))

totfishall <- join(totfishall, standard, type="left")

totfishall <- join(totfishall, sites, type="left")
#### totfishall$totalfish is the column I need! 

### just 2002 - 2012
totfish0212 <- ddply(.data=inventory_clean[inventory_clean$ReportYear %in% c(2002, 2003, 2004, 2005, 2006, 2007, 
                                                                             2008, 2009, 2010, 2011, 2012),], .(ID),
                     summarize,
                     totalfish = sum(fishcount))

totfish0212 <- join(totfish0212, sites, type="left")
#####

empty <- subset(totfishall, is.na(Lat)==TRUE)

full <- subset(inventory_clean, !Coord1=="")

comp <- join(empty, full, type="left")
comp$Lat <- ifelse(is.na(comp$Lat) == TRUE | comp$Lat == "", comp$Coord1, NA)
comp$Long <- ifelse(is.na(comp$Long) == TRUE | comp$Long == "", comp$Coord2, NA)

comp <- unique(select(comp, ID, Lat, Long))
comp$Lat <- as.numeric(comp$Lat)
comp$Long <- as.numeric(comp$Long)

totfishall <- join(totfishall, comp, type="left", by="ID")
totfishall[,6] <- ifelse(is.na(totfishall[,6]) == TRUE, totfishall[,8], totfishall[,6])
totfishall[,7] <- ifelse(is.na(totfishall[,7]) == TRUE, totfishall[,9], totfishall[,7])

totfishall <- totfishall[!is.na(totfishall$Lat)==TRUE, 1:7]


sites <- unique(select(totfishall, ID, Lat, Long))

annualinventory <- join(annual, sites, type="left")
write.csv(annualinventory, "C:/Users/keyserf/Documents/Data/NL annual inventory and coords_summary_2016-11-29.csv")


totfish0212 <- join(totfish0212, comp, type="left", by="ID")
totfish0212[,3] <- ifelse(is.na(totfish0212[,3]) == TRUE, totfish0212[,5], totfish0212[,3])
totfish0212[,4] <- ifelse(is.na(totfish0212[,4]) == TRUE, totfish0212[,6], totfish0212[,4])

totfish0212 <- totfish0212[!is.na(totfish0212$Lat)==TRUE, 1:4]

#########################
prov <- readOGR(dsn="C:/Users/keyserf/Documents/R/canvec/NL.low.ocean.dbf", layer="NL.low.ocean")
prov <- fortify(prov)
str(prov)

totfishall$totalfish <- ifelse(totfishall$totalfish == 0, "NA", inventory$totalfish)
totfishall$totalfish <- as.numeric(totfishall$totalfish)
## Number of fish per site
ggplot() + 
  geom_polygon(data=prov, aes(long, lat, group=group), fill="grey")+
  geom_point(data=totfishall, aes(Long, Lat, size=totalfish), fill="red", shape=21, colour="black") + 
  theme_bw() +
  coord_map(xlim=c(-56.4, -55.0), ylim=c(47.4, 47.8)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
  scale_size_continuous(name="totalfish")

inventory_FB <- subset(inventory, Bay=="Fortune Bay")

ggplot() + 
  geom_polygon(data=prov, aes(long, lat, group=group), fill="grey")+
  geom_point(data=inventory, aes(Long, Lat), colour="red", shape=16) + 
  #facet_wrap(~Bay) +
  theme_bw() +
  coord_map(xlim=c(-56.4, -55.0), ylim=c(47.4, 47.8)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())

ggplot() + 
  geom_polygon(data=prov, aes(long, lat, group=group), fill="grey")+
  geom_point(data=inventory, aes(Long, Lat, size=No..Fish.Escaped), colour="red", shape=16) + 
  facet_wrap(~Year.Class..Adj.) +
  theme_bw() +
  coord_map(xlim=c(-56.4, -55.0), ylim=c(47.4, 47.8)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())



