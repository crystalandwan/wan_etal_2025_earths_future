# *****************************************************************************************
# Title: Cold Snap Spatial Coverage Calculation 2
# Author: Heng Wan
# Date: 10/20/2025
# Purpose: Calculate NERC subregion-level spatial coverage of cold snap events under various
#          cold snap definitions.
# Description: This script processes county-level temperature data to identify county-level 
#              cold snap events under definition 6, 7, 10, and 11. The spatial coverage of 
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
  list(threshold1 = 0.025, threshold2 = 0.19, col_idx = 2, hw_id = 6), # max temperature
  list(threshold1 = 0.1, threshold2 = 0.25, col_idx = 2, hw_id = 7), # max temperature
  list(threshold1 = 0.025, threshold2 = 0.19, col_idx = 3, hw_id = 10), # min temperature
  list(threshold1 = 0.1, threshold2 = 0.25, col_idx = 3, hw_id = 11) # min temperature
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
    thresholds1 <- apply(NERC_temp, 1, quantile, probs = def$threshold1)
    thresholds2 <- apply(NERC_temp, 1, quantile, probs = def$threshold2)
    
  } else if (grepl("pop", input_string, ignore.case = TRUE)) {
    load(paste0("nerc_level_temp_data_path", "NERC_average_pop.RData"))
    NERC_temp <- NERC_average_pop[, def$col_idx, ]
    thresholds1 <- apply(NERC_temp, 1, quantile, probs = def$threshold1)
    thresholds2 <- apply(NERC_temp, 1, quantile, probs = def$threshold2)
  } else {
    load(paste0("nerc_level_temp_data_path", "NERC_average.RData"))
    NERC_temp <- NERC_average[, def$col_idx, ]
    thresholds1 <- apply(NERC_temp, 1, quantile, probs = def$threshold1)
    thresholds2 <- apply(NERC_temp, 1, quantile, probs = def$threshold2)
  }
  return (list(thresholds1, thresholds2))
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
#' @param county_temps Numeric vector. Daily temperature values for a county.
#' @param threshold1 Numeric. Lower temperature threshold.
#' @param threshold2 Numeric. Upper temperature threshold.
#' @return Logical. TRUE if the county satisfies all cold snap criteria, FALSE otherwise.
check_for_event <- function(county_temps, threshold1, threshold2) {
  #Check if all temperatures are less than threshold 2:
  all_below_threshold2 <- all(county_temps < threshold2)
  
  # Check for 3+ consecutive days with temperatures less than threshold1
  low_temp_days <- county_temps < threshold1
  wave_lengths <- rle(low_temp_days)
  
  # Safely compute the maximum number of consecutive low temperature days
  if (any(wave_lengths$values)) {  # Check if there are any TRUE values
    max_consecutive_high_temps <- max(wave_lengths$lengths[wave_lengths$values])
  } else {
    max_consecutive_high_temps <- 0  # If no TRUE values, set to 0
  }
  
  three_plus_consecutive_days <- max_consecutive_high_temps >= 3
  
  # Check if the average temperature is less than threshold1
  average_below_threshold1 <- mean(county_temps) < threshold1
  
  # Return TRUE only if all conditions are met
  return(all_below_threshold2 && three_plus_consecutive_days && average_below_threshold1)
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
  pattern = "def6.csv|def7.csv|def10.csv|def11.csv",
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
  thresholds_list <- calculate_thresholds(event_def, def)
  thresholds1 <- thresholds_list[[1]]
  thresholds2 <- thresholds_list[[2]]
  
  # Read in the target NERC-level event file
  NERC_event <- fread(paste0(here("Data", "cold_snap_library"), event_def))
  # Initiate the spatial coverage column
  NERC_event$spatial_coverage <- -100
  
  for(i in c(1:nrow(NERC_event))){
    # Extract the target event
    target_event <- NERC_event[i, ]
    # Get the correct threshold value for the target NERC region
    target_ID <- gsub("[^0-9]", "", target_event$NERC_ID)
    threshold1 <- thresholds1[names(thresholds1) == target_ID]
    threshold2 <- thresholds2[names(thresholds2) == target_ID]
    
    # Calculate the start and end index related to the target event
    start_idx <- date_to_days_since_1980(target_event$start_date)
    end_idx <- date_to_days_since_1980(target_event$end_date)
    
    # Find the row indices related to counties within the target NERC region
    target_counties <- counties$GEOID[counties$ID == target_event$NERC_ID]
    row_idx <- which(daily_stats_array[ , 1, 1] %in% target_counties)
    
    # Extract the temperature data
    target_temp <- daily_stats_array[row_idx, def$col_idx + 1, start_idx:end_idx]
    
    # Check whether the event is satisfied for each row (county)
    event_results <- apply(target_temp, 1, function(x) check_for_event(x, threshold1, threshold2))
    
    spatial_coverage <- sum(event_results)/nrow(target_temp) * 100
    NERC_event[i, "spatial_coverage"] <- spatial_coverage
  }
  fwrite(NERC_event, here("Data", "cold_snap_library/With_spatial_coverage/event_def"))
})

stopCluster(cl)  # Stop the cluster

















