# *****************************************************************************************
# Title: Cold Snap Library Construction 1
# Author: Heng Wan
# Date: 10/20/2025
# Purpose: Analyze temperature scenarios and identify cold snap events based on thresholds.
# Description: This script processes NERC subregion-level temperature data under various 
#              spatial aggregation scenarios to build cold snap libraries 
#              based on cold snap definition 1, 2, 3, 4, 5, and 9
# Requirements: Ensure that paths to data and output directories are properly set in the 
#               configuration file (config.R).
# *****************************************************************************************

# Load required packages ----
library(tools)
library(here)

# Source the config file for paths ----
config_path <- here::here("scripts", "Extreme_event_library_construction/config.R")
if (!file.exists(config_path)) {
  stop("The config file does not exist. Ensure the path to the config file is correct:", config_path)
}
source(config_path)

# Ensure working directory ----
if (!dir.exists(nerc_level_temp_data_path)) {
  stop("The NERC temperature data path does not exist. Check the 'nerc_level_temp_data_path' variable in your config.")
}
setwd(nerc_level_temp_data_path)

# Create output directory ----
output_dir <- here("Data", "cold_snap_library")
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}


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

#' Convert index to date given the year.
#' @param start_idx Numeric. Index of the start day in the year.
#' @param year Numeric. The year for conversion.
#' @return Date object.
index_to_date <- function(start_idx, year) {
  start_date <- as.Date(paste(year, "01", "01", sep="-"))
  return(start_date + start_idx - 1)
}

#' Calculate accumulated days from 1980 to ensure proper indexing.
#' @param year Numeric. The target year.
#' @return Numeric. Accumulated day count from 1980.
# Function to calculate the start index of each year considering leap years
calculate_days <- function(year) {
  # Ensure the function returns 1 if the year is 1980
  if (year == 1980) {
    return(1)
  }
  days <- sum(sapply(1980:(year-1), function(y) {
    if ((y %% 4 == 0 && y %% 100 != 0) || y %% 400 == 0) {
      return(366)  # Leap year
    } else {
      return(365)  # Non-leap year
    }
  }))
  
  return(days+1)
}


#' Identify cold snap events based on temperature thresholds.
#' @param temps Numeric vector. Temperature data for the year.
#' @param t_min Numeric vector. Minimum temperature data for the year.
#' @param threshold Numeric. Cold snap threshold.
#' @param year Numeric. Year of the data.
#' @return Data frame of identified cold snap events.
detect_cold_snaps <- function(temps, t_min, threshold, year) {
  high_temp_days <- temps < threshold
  wave_lengths <- rle(high_temp_days)
  
  # Construct a data frame to store the events
  events_df <- data.frame(start_date = as.Date(character()),
                          end_date = as.Date(character()), 
                          centroid_date = as.Date(character()),
                          lowest_temperature = numeric(), 
                          duration = numeric())
  current_day <- 1
  
  for (i in 1:length(wave_lengths$lengths)) {
    if (wave_lengths$values[i] && wave_lengths$lengths[i] >= 2) {
      start_idx <- current_day
      end_idx <- start_idx + wave_lengths$lengths[i] - 1
      events_df <- rbind(events_df, data.frame(
        start_date = index_to_date(start_idx, year),
        end_date = index_to_date(end_idx, year),
        centroid_date = index_to_date(start_idx + which.min(t_min[start_idx:end_idx]) - 1, year),
        lowest_temperature = min(t_min[start_idx:end_idx]),
        duration = end_idx - start_idx + 1))
    }
    current_day <- current_day + wave_lengths$lengths[i]
  }
  return(events_df)
}

# Main processing ----

# List of scenarios to process
scenarios <- c("NERC_average.RData", "NERC_average_area.RData", "NERC_average_pop.RData")

# Process each scenario
for (scenario in scenarios) {
  load(scenario)
  NERC_temp_data <- eval(as.name(file_path_sans_ext(scenario)))
  
  for (def in definitions) {
    t_min <- NERC_temp_data[, 3, ]
    temps <- NERC_temp_data[, def$col_idx, ]
    thresholds <- apply(temps, 1, quantile, probs = def$threshold)
    
    cold_snap_events <- data.frame()
    
    for (NERC_idx in 1:nrow(NERC_temp_data)) {
      for (year in 1980:2024) {
        start_idx <- calculate_days(year)
        end_idx <- start_idx + ifelse((year %% 4 == 0 && year %% 100 != 0) || 
                                        year %% 400 == 0, 365, 364)
        NERC_temps <- temps[NERC_idx, start_idx:end_idx]
        NERC_t_min <- t_min[NERC_idx, start_idx:end_idx]
        events <- detect_cold_snaps(NERC_temps, NERC_t_min, thresholds[NERC_idx], year)
        
        if (nrow(events) > 0) {  # Check if events is not empty
          events$NERC_ID <- paste0("NERC", rownames(NERC_temp_data)[NERC_idx])
          cold_snap_events <- rbind(cold_snap_events, events)
        }
      }
    }
    
    if (nrow(cold_snap_events) > 0) {  # Check before writing to file
      file_name <- sprintf("%s/cold_snap_library_%s_def%d.csv", output_dir, 
                           file_path_sans_ext(basename(scenario)), def$hw_id)
      write.csv(cold_snap_events, file_name, row.names = FALSE)
    } else {
      cat("No cold snap events detected for definition", def$hw_id, "in file", scenario, "\n")
    }
  }
}

