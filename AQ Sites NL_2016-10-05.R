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

inventory <- read.csv("C:/Users/keyserf/Documents/data/NL inventory master2.csv", colClasses = "character")

names(inventory) <- c("ID", "Cage", "NStart", "YC", "YCAdj", "NIntro", "NMort", "NHarvest", 
                      "NTransfer", "CountDev", "CountDevAbs", "NEscape", "NRemain", "Trout",
                      "ReportYear", "Bay")

str(inventory)

clean <- function(i) { 
  gsub(i, pattern=",", replacement="")
  gsub(i, pattern=" ", replacement="")
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

inventory[, 6:13][is.na(inventory[, 6:13])] <- 0
inventory[,3][is.na(inventory[, 3])] <- 0


################### CALCULATE FISH YEARS ########################

# 2 rules: 
#   1) morts, harvests and transfers occur mid-year
#   2) always assume max reported number of fish is most accurate
#   3) Introductions occur at beginning of year
# 3 cases: 
#   1) if NRemain = 0 and CountDev > 0    --->    (NStart + NIntro + CountDev)/2
#   2) if NRemain = 0 and CountDev <= 0   --->    (NStart + NIntro)/2
#   3) if NRemain > 0                     --->    NRemain + (NMort + NTransfer + NHarvest)/2

inventory$case <- ifelse(inventory$NRemain==0 & inventory$CountDev > 0, 1, 
                         ifelse(inventory$NRemain==0 & (inventory$CountDev < 0 | inventory$CountDev == 0), 2,
                                ifelse(inventory$NRemain>0 , 3, NA)))

inventory$fishcount <- ifelse(inventory$case==1, (inventory$NStart + inventory$NIntro + inventory$CountDev)/2,
                              ifelse(inventory$case==2, (inventory$NStart + inventory$NIntro)/2, 
                                     ifelse(inventory$case==3, inventory$NRemain + ((inventory$NMort + inventory$NHarvest + abs(inventory$NTransfer))/2), NA)))

events <- inventory

ggplot() + 
  geom_point(data=inventory, aes(ReportYear, fishcount)) + 
  facet_wrap(~ID)

standard <- ddply(.data=inventory, .(ID),
                  summarize,
                  totalfish = sum(fishcount),
                  totalyears = length(unique(ReportYear)))


### Combine with coordinate data

sites <- read.csv("C:/Users/keyserf/Documents/Data/NL Stocking 2010-2013.csv")
str(sites)

### NOTE: coordinates for Site 978 in 2011 was edited to match coords in 2010 and 2012. This site was empty in 2011, 
### so changing these coords will not affect analysis and prevents a summation problem for propagule pressure

sites$ID <- gsub(sites$ID, pattern="AQ", replacement = "")
sites$ID <- gsub(sites$ID, pattern = "a", replacement = "")
sites$ID <- gsub(sites$ID, pattern = "b", replacement = "")

sites$ID <- as.character(sites$ID)

sites <- unique(subset(sites, select=c("ID", "Lat", "Long", "Year")))
names(sites) <- c("ID", "Lat", "Long", "ReportYear")

inventory <- join(inventory, sites, type="left")

inventory <- ddply(.data=inventory, .(ID),
                   summarize,
                   cages = length(unique(Cage)),
                   years = length(unique(ReportYear)))

inventory <- join(inventory, standard, type="left")
#### inventory$totalfish is the column I need! 


prov <- readOGR(dsn="C:/Users/keyserf/Documents/R/canvec/NL.low.ocean.dbf", layer="NL.low.ocean")
prov <- fortify(prov)
str(prov)

inventory$totalfish <- ifelse(inventory$totalfish == 0, "NA", inventory$totalfish)
inventory$totalfish <- as.numeric(inventory$totalfish)
## Number of fish per site
ggplot() + 
  geom_polygon(data=prov, aes(long, lat, group=group), fill="grey")+
  geom_point(data=inventory, aes(Long, Lat, size=totalfish), fill="red", shape=21, colour="black") + 
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



