# *****************************************************************************************
# Title: Map Counties to NERC Subregions
# Author: Heng
# Date: 10/20/2025
# Purpose: Assign each CONUS county to a NERC region based on spatial intersection.
# Description: This script reads NERC and county shapefiles, excludes non-CONUS counties,
#              determines the associated NERC region for each county via spatial intersection,
#              and writes the updated county shapefile with NERC region IDs.
# Requirements: Ensure paths to input and output files are correctly configured in `config.R`.
# *****************************************************************************************

library(sf)
library(here)

# Source Configuration File ----
config_path <- here::here("scripts", "Extreme_event_library_construction/config.R")
if (!file.exists(config_path)) {
  stop("Configuration file does not exist. Please validate the path:", config_path)
}
source(config_path)

# Validate Input File Paths ----
if (!file.exists(nerc_path)) {
  stop("NERC shapefile does not exist. Please validate the path:", nerc_path)
}

if (!file.exists(county_path)) {
  stop("County shapefile does not exist. Please validate the path:", county_path)
}

# Load Input Data ----

# Read in NERC shapefile
NERC <- st_read(nerc_path)

# Read in county shapefile 
counties <- st_read(county_path)

# Exclude Non-CONUS Counties ----
counties <- counties[counties$STATEFP != "02" & counties$STATEFP != "15" &
                       counties$STATEFP != "14" & counties$STATEFP != "43" &
                       counties$STATEFP != "60" & counties$STATEFP !=  "78" &
                       counties$STATEFP != "72" & counties$STATEFP != "69" &
                       counties$STATEFP != "66", ]

# Ensure CRS Consistency ----

# Reproject NERC to match the crs of counties
NERC <- st_transform(NERC, crs = st_crs(counties))

# Map Counties to NERC Regions ----

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

# Write Output ----
st_write(counties, here("Data", "county_nerc.shp"))



