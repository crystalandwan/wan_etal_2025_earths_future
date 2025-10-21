# *****************************************************************************************
# Title: County Population Weights Within NERC Subregions
# Author: Heng Wan
# Date: 10/20/2025
# Purpose: Converts the 2019 county-level population data to weights (scaled between 0 and 1)
#          within each NERC subregion.
# Description: Reads county-NERC relationship data, aligns population data with spatial boundaries,
#              computes population weights for counties within NERC subregions, and exports results.
# Requirements: Ensure paths to required files are correctly configured in `config.R`.
# *****************************************************************************************

# Load Required Libraries ----
library(data.table)
library(dplyr)
library(sf)
library(here)

# Source Configuration File ----
config_path <- here::here("scripts", "Extreme_event_library_construction/config.R")
if (!file.exists(config_path)) {
  stop("Configuration file does not exist. Please validate the path:", config_path)
}
source(config_path)

# Load Data ----

# Read County-NERC relation shapefile
if (!file.exists(county_nerc_path)) {
  stop("County-NERC mapping file does not exist:", county_nerc_path)
}

counties <- st_read(county_nerc_path) %>%
  mutate(FIPS = GEOID %>% 
           gsub("^0+", "", .) %>% 
           as.numeric())

# Read population data
if(!file.exists(pop_path)){
  stop("Population data does not exist: ", pop_path)
}

pop <- fread(pop_path) %>%
  mutate(
    county_FIPS = ifelse(county_FIPS == 46113, 46102, county_FIPS) # Rename FIPS for Shannon county (now Oglala Lakota county)
  )

# Merge County-NERC Data with Population Data ----

# Join counties with pop
join <- merge(counties, pop[, c("county_FIPS", "pop_2019")], 
              by.x = "FIPS", by.y = "county_FIPS", all.x = TRUE)

# Calculate the weight of each county's population within its NERC subregion.
join <- join %>%
  group_by(ID) %>%
  mutate(total_pop_region = sum(pop_2019, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(pop_weight = pop_2019/total_pop_region)

# Export Results ----

join2 <- st_drop_geometry(join)
join2 <- join2[, c("FIPS", "pop_weight")]
fwrite(join2, here("Data", "county_pop2019_weights_in_NERC.csv"))

