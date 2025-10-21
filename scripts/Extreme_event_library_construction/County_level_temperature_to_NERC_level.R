# *****************************************************************************************
# Title: Aggregate County-Level Temperature Data to NERC Subregion
# Author: Heng Wan
# Date: 10/20/2025
# Purpose: Aggregate daily temperature statistics (mean, max, min) from counties to NERC 
#          subregions based on three aggregation methods: unweighted average, area-weighted,
#          and population-weighted.
# Description: This script processes temperature data from counties and calculates NERC-level
#              statistics using weights such as area and population.
# Requirements: Ensure paths to configuration file, county data, and population data are correctly set in `config.R`.
# *****************************************************************************************

# Libraries ----
library(sf)
library(dplyr)
library(data.table)
library(abind)
library(here)

# Source Configuration File ----
config_path <- here::here("scripts", "Extreme_event_library_construction/config.R")
if (!file.exists(config_path)) {
  stop("Configuration file does not exist. Validate the path:", config_path)
}
source(config_path)

# Path for output ----
output_dir <- here("Data", "NERC_level_temp_data")  # Output directory for saving results

# Validate Paths ----
if (!file.exists(county_nerc_path)) {
  stop("County-NERC shapefile does not exist. Validate the path:", county_nerc_path)
}

if (!file.exists(county_level_temp_data_path)) {
  stop("County temperature data file does not exist. Validate the path:", county_level_temp_data_path)
}

if (!file.exists(pop_path)) {
  stop("County population data file does not exist. Validate the path:", pop_path)
}

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Load Data ----

# Load county-NERC relational shapefile
county_nerc_data <- st_read(county_nerc_path) %>%
  st_drop_geometry() %>%
  mutate(FIPS = as.numeric(gsub("^0+", "", GEOID)))  # Create FIPS codes without leading zeros

# Load daily temperature data
load(county_level_temp_data_path)  # Loads `daily_stats_array` (county x stats x dates)

# Load population data
population_data <- fread(pop_path) %>%
  mutate(county_FIPS = ifelse(county_FIPS == 46113, 46102, county_FIPS))  # Adjust FIPS for Shannon County

# Merge population data with county-NERC data
county_nerc_data <- county_nerc_data %>%
  left_join(population_data[, c("county_FIPS", "pop_2019")], by = c("FIPS" = "county_FIPS"))

# Prepare for Aggregation ----

# Unique NERC IDs and Dimensions
unique_nerc_ids <- unique(county_nerc_data$ID)  # Unique NERC regions
num_nercs <- length(unique_nerc_ids)
num_stats <- 3  # Mean, Max, Min temperature
num_dates <- dim(daily_stats_array)[3]  # Daily time series length

# Initialize NERC-level temperature arrays
dim_names <- list(unique_nerc_ids, c("T_mean", "T_max", "T_min"), NULL)
nerc_average <- array(NA, dim = c(num_nercs, num_stats, num_dates), dimnames = dim_names)
nerc_average_area <- nerc_average
nerc_average_pop <- nerc_average

# NERC-Level Temperature Aggregation by Mean ----
for (i in seq_along(unique_nerc_ids)) {
  nerc_id <- unique_nerc_ids[i]
  
  # Filter counties within the current NERC region
  counties <- county_nerc_data %>% filter(ID == nerc_id) %>% pull(FIPS)
  
  # Get indices of relevant counties in `daily_stats_array`
  county_indices <- which(daily_stats_array[, 1, 1] %in% counties)
  
  # Extract temperature data for relevant counties
  target_data <- daily_stats_array[county_indices, , ]
  
  # Compute average for each statistic and day
  nerc_average[i, , ] <- apply(target_data[, 2:4, ], c(2, 3), mean, na.rm = TRUE)
}

save(nerc_average, file = file.path(output_dir, "NERC_average.RData"))

# Area-Weighted NERC-Level Temperature Aggregation ----
county_nerc_data <- county_nerc_data %>% mutate(area = as.numeric(st_area(st_geometry(county_nerc_data))))

for (i in seq_along(unique_nerc_ids)) {
  nerc_id <- unique_nerc_ids[i]
  
  # Filter counties within the current NERC region
  counties <- county_nerc_data %>% filter(ID == nerc_id) %>% pull(FIPS)
  
  # Get indices of relevant counties in `daily_stats_array`
  county_indices <- which(daily_stats_array[, 1, 1] %in% counties)
  
  # Get county areas for weights
  county_areas <- county_nerc_data %>%
    filter(FIPS %in% counties) %>%
    pull(area)
  
  # Extract temperature data for relevant counties
  target_data <- daily_stats_array[county_indices, , ]
  
  # Compute area-weighted averages for each statistic and day
  nerc_average_area[i, , ] <- apply(target_data[, 2:4, ], c(2, 3), function(x) weighted.mean(x, w = county_areas, na.rm = TRUE))
}

save(nerc_average_area, file = file.path(output_dir, "NERC_average_area.RData"))

# Population-Weighted NERC-Level Temperature Aggregation ----
for (i in seq_along(unique_nerc_ids)) {
  nerc_id <- unique_nerc_ids[i]
  
  # Filter counties within the current NERC region
  counties <- county_nerc_data %>% filter(ID == nerc_id) %>% pull(FIPS)
  
  # Get indices of relevant counties in `daily_stats_array`
  county_indices <- which(daily_stats_array[, 1, 1] %in% counties)
  
  # Get population data for weights
  county_populations <- county_nerc_data %>%
    filter(FIPS %in% counties) %>%
    pull(pop_2019)
  
  # Extract temperature data for relevant counties
  target_data <- daily_stats_array[county_indices, , ]
  
  # Compute population-weighted averages for each statistic and day
  nerc_average_pop[i, , ] <- apply(target_data[, 2:4, ], c(2, 3), function(x) weighted.mean(x, w = county_populations, na.rm = TRUE))
}

save(nerc_average_pop, file = file.path(output_dir, "NERC_average_pop.RData"))