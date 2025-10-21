# *****************************************************************************************
# Title: Cold Snap Spatial Coverage Calculation 1
# Author: Heng Wan
# Date: 10/20/2025
# Purpose: Calculate NERC subregion-level spatial coverage of cold snap events under various
#          cold snap definitions.
# Description: This script processes county-level temperature data to identify county-level 
#              cold snap events under definition 1, 2, 3, 4, 5, and 9. The spatial coverage of 
#              NERC subregion-level cold snap event is calculated as the percentage of 
#              counties experiencing that cold snap event within the target NERC subregion.
# Requirements: Ensure paths to temperature and event files are correctly set in the 
#               configuration file (config.R).
# ******************************************************************************************

# Load Required Libraries ----
library(data.table)
library(sf)
library(dplyr)
library(abind)
library(parallel)
library(here)

# Source Configuration File ----
config_path <- here::here("scripts", "Extreme_event_library_construction/config.R")
if (!file.exists(config_path)) {
  stop("The configuration file does not exist. Ensure the correct path:", config_path)
}
source(config_path)

# Define thresholds and column index for temperature types ----
definitions <- list(
  list(threshold = 0.10, col_idx = 1, hw_id = 1), # 10th percentile, mean temperature
  list(threshold = 0.05, col_idx = 1, hw_id = 2), # 5th percentile, mean temperature
  list(threshold = 0.02, col_idx = 1, hw_id = 3), # 2th percentile, mean temperature
  list(threshold = 0.01, col_idx = 1, hw_id = 4), # 1th percentile, mean temperature
  list(threshold = 0.05, col_idx = 2, hw_id = 5), # 5th percentile, max temperature
  list(threshold = 0.10, col_idx = 3, hw_id = 9)  # 10th percentile, min temperature
)

# Utility Functions ----

#' Calculate temperature thresholds for different temperature aggregation methods.
#' @param input_string String. Name of the temperature aggregation method (e.g., "area", "pop").
#' @param def List. Threshold definition (percentile and column index).
#' @return Numeric vector. Threshold temperatures for NERC regions.
calculate_thresholds <- function(input_string, def) {
  if (grepl("area", input_string, ignore.case = TRUE)) {
    load(paste0("nerc_level_temp_data_path", "NERC_average_area.RData"))
    NERC_temp <- NERC_average_area[, def$col_idx, ]
    thresholds <- apply(NERC_temp, 1, quantile, probs = def$threshold)
    
  } else if (grepl("pop", input_string, ignore.case = TRUE)) {
    load(paste0("nerc_level_temp_data_path", "NERC_average_pop.RData"))
    NERC_temp <- NERC_average_pop[, def$col_idx, ]
    thresholds <- apply(NERC_temp, 1, quantile, probs = def$threshold)
  } else {
    load(paste0("nerc_level_temp_data_path", "NERC_average.RData"))
    NERC_temp <- NERC_average[, def$col_idx, ]
    thresholds <- apply(NERC_temp, 1, quantile, probs = def$threshold)
  }
  return (thresholds)
}

#' Calculate days passed since 1980-01-01 for a given date.
#' @param date_str String. Target date string in "YYYY-MM-DD" format.
#' @return Numeric. Days elapsed since "1980-01-01".
date_to_days_since_1980 <- function(date_str) {
  # Define the start date
  start_date <- as.Date("1980-01-01")
  # Convert the input date string to a Date object
  current_date <- as.Date(date_str)
  # Calculate the difference in days
  delta <- as.numeric(current_date - start_date)
  # Return the number of days since 1980-01-01
  return(delta + 1)  # +1 to make the count start from 1
}

#' Check if a cold snap event criteria is satisfied for a county.
#' @param county_temps Numeric vector. Daily temperatures for a county.
#' @param threshold Numeric. Cold snap temperature threshold.
#' @return Logical. TRUE if event is satisfied, FALSE if not.
check_for_event <- function(county_temps, threshold) {
  # Find days below the threshold
  low_temp_days <- county_temps < threshold
  
  # Apply run length encoding
  wave_lengths <- rle(low_temp_days)
  
  # Extract lengths where temperatures are below threshold
  low_temp_periods <- wave_lengths$lengths[wave_lengths$values]
  
  # Check if any period of high temps is at least 2 days long
  any(low_temp_periods >= 2)
}

# Prepare Input Data ----

# Load county mapping file
if (!file.exists(county_nerc_path)) {
  stop("County-NERC mapping file does not exist:", county_nerc_path)
}

counties <- st_read(county_nerc_path) %>%
  st_drop_geometry() %>%
  dplyr::select(GEOID, ID) %>%
  mutate(
    GEOID = gsub("^0+", "", GEOID),  # Remove leading zeros in county IDs
    ID = paste0("NERC", ID)
  )

# Load county-level temperature data
if (!file.exists(county_level_temp_data_path)) {
  stop("County-level temperature data file does not exist:", county_level_temp_data_path)
}

load(county_level_temp_data_path)  # County-level data is stored in `daily_stats_array`.

# Identify Event Definition Files ----
event_def_files <- list.files(
  here::here("Data", "cold_snap_library"),
  pattern = "def1.csv|def2.csv|def3.csv|def4.csv|def5.csv|def9.csv",
  full.names = TRUE
)

# Set Up Parallel Processing ----
no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)

clusterEvalQ(cl, {
  library(data.table)
})

clusterExport(cl, list("calculate_thresholds", "date_to_days_since_1980", 
                       "check_for_event", "daily_stats_array", "counties", 
                       "definitions", "event_defs"))

# Main Processing ----
results <- parLapply(cl, event_defs, function(event_def) {
  # Obtain the event definition ID (ranges from 1 to 12)
  hw_id <- as.numeric(gsub("[^0-9]", "", event_def))
  # Obtain detailed info about the target definition
  def <- Filter(function(x) x$hw_id == hw_id, definitions)
  # Check if more than 1 definitions are selected
  if(length(def) > 1){
    print("Warning: over two definitions detected at the same time")
  }
  def <- def[[1]]
  
  # Calculate the NERC-level thresholds
  thresholds <- calculate_thresholds(event_def, def)
  
  # Read in the target NERC-level event file
  NERC_event <- fread(paste0(here("Data", "cold_snap_library"), event_def))
  # Initiate the spatial coverage column
  NERC_event$spatial_coverage <- -100
  
  for(i in c(1:nrow(NERC_event))){
    # Extract the target event
    target_event <- NERC_event[i, ]
    # Get the correct threshold value for the target NERC region
    target_ID <- gsub("[^0-9]", "", target_event$NERC_ID)
    threshold <- thresholds[names(thresholds) == target_ID]
    
    # Calculate the start and end index related to the target event
    start_idx <- date_to_days_since_1980(target_event$start_date)
    end_idx <- date_to_days_since_1980(target_event$end_date)
    
    # Find the row indices related to counties within the target NERC region
    target_counties <- counties$GEOID[counties$ID == target_event$NERC_ID]
    row_idx <- which(daily_stats_array[ , 1, 1] %in% target_counties)
    
    # Extract the temperature data
    target_temp <- daily_stats_array[row_idx, def$col_idx + 1, start_idx:end_idx]
    
    # Check whether the event is satisfied for each row (county)
    event_results <- apply(target_temp, 1, function(x) check_for_event(x, threshold))
    
    spatial_coverage <- sum(event_results)/nrow(target_temp) * 100
    NERC_event[i, "spatial_coverage"] <- spatial_coverage
  }
  fwrite(NERC_event, here("Data", "cold_snap_library/With_spatial_coverage/event_def"))
})

stopCluster(cl)  # Stop the cluster



