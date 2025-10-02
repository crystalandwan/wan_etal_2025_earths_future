library(sf)

# Set working directory
setwd("PATH_TO_DATA")

# Read in NERC shapefile
NERC <- st_read("./NERC_Regions_Subregions.shp")

# Read in county shapefile 
counties <- st_read("./tl_2020_us_county.shp")

# Exclude non-CONUS counties
counties <- counties[counties$STATEFP != "02" & counties$STATEFP != "15" &
                       counties$STATEFP != "14" & counties$STATEFP != "43" &
                       counties$STATEFP != "60" & counties$STATEFP !=  "78" &
                       counties$STATEFP != "72" & counties$STATEFP != "69" &
                       counties$STATEFP != "66", ]

# Reproject NERC to match the crs of counties
NERC <- st_transform(NERC, crs = st_crs(counties))

# Find the NERC region for each county
for (i in 1:nrow(counties)){
  print(i)
  # Find all the NERC intersecting with the target county.
  intersecting_NERC <- st_intersection(counties[i, "geometry"], NERC[, c("ID", "geometry")])
  # If the county intersects with more than 1 NERC, choose the NERC with the
  # largest intersection area
  if (nrow(intersecting_NERC) > 1){
    intersecting_NERC <- intersecting_NERC[which.max(st_area(intersecting_NERC$geometry)), ]
  }
  counties[i, "ID"] <- intersecting_NERC$ID
}

st_write(counties, "./counties_in_NERC2020.shp")



