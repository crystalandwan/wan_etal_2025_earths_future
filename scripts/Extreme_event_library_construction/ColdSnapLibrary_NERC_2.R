# *****************************************************************************************
# Title: Cold Snap Library Construction 2
# Author: Heng Wan
# Date: 10/20/2025
# Purpose: Analyze temperature scenarios and identify cold snap events based on thresholds.
# Description: This script processes NERC subregion-level temperature data under various 
#              spatial aggregation scenarios to build cold snap libraries 
#              based on cold snap definition 6, 7, 10, and 11
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
  list(threshold1 = 0.025, threshold2 = 0.19, col_idx = 2, hw_id = 6), # max temperature
  list(threshold1 = 0.1, threshold2 = 0.25, col_idx = 2, hw_id = 7), # max temperature
  list(threshold1 = 0.025, threshold2 = 0.19, col_idx = 3, hw_id = 10), # min temperature
  list(threshold1 = 0.1, threshold2 = 0.25, col_idx = 3, hw_id = 11) # min temperature
)

# Utility Functions ----

#' Convert index to date given the year.
#' @param start_idx Numeric. Index of the start day in the year.
#' @param year Numeric. The year for conversion.
#' @return Date object.
index_to_date <- function(start_idx, year) {
  as.Date(paste(year, "01", "01", sep="-")) + start_idx - 1
}

#' Calculate accumulated days from 1980 to ensure proper indexing.
#' @param year Numeric. The target year.
#' @return Numeric. Accumulated day count from 1980.
# Function to calculate the start index of each year considering leap years
calculate_days <- function(year) {
  if (year == 1980) { return(1) }
  sum(sapply(1980:(year-1), function(y) {
    if ((y %% 4 == 0 && y %% 100 != 0) || y %% 400 == 0) { 366 } else { 365 }
  })) + 1
}

#' Identify cold snap events based on temperature thresholds.
#' @param temps Numeric vector. Temperature data for the year.
#' @param t_min Numeric vector. Minimum temperature data for the year.
#' @param threshold1 Numeric. Cold snap threshold 1.
#' @param threshold2 Numeric. Cold snap threshold 2.
#' @param year Numeric. Year of the data.
#' @return Data frame of identified cold snap events.
detect_cold_snaps <- function(temps, t_min, threshold1, threshold2, year) {
  min_days <- 3
  
  # Construct a data frame to store the events
  events_df <- data.frame(start_date = as.Date(character()),
                          end_date = as.Date(character()), 
                          centroid_date = as.Date(character()),
                          lowest_temperature = numeric(), 
                          duration = numeric())
  i <- 1
  
  while (i <= (length(temps) - min_days + 1)) {
    # Check if the next `min_days` consecutive days are above threshold1
    if (all(temps[i:(i + min_days - 1)] < threshold1)) {
      start <- i
      end <- i + min_days - 1
      
      # Attempt to extend the segment to the right
      while (end < length(temps) && temps[end + 1] < threshold2) {
        if (mean(temps[start:(end + 1)]) < threshold1) {
          end <- end + 1
        } else {
          break
        }
      }
      
      # Attempt to extend the segment to the left
      while (start > 1 && temps[start - 1] < threshold2) {
        if (mean(temps[(start - 1):end]) < threshold1) {
          start <- start - 1
        } else {
          break
        }
      }
      
      
      # Save the segment if it's valid
      if (end - start + 1 >= min_days) {
        events_df <- rbind(events_df, data.frame(
          start_date = index_to_date(start, year),
          end_date = index_to_date(end, year),
          centroid_date = index_to_date(start + which.min(t_min[start:end]) - 1, year),
          lowest_temperature = min(t_min[start:end]), 
          duration = end - start + 1))
      }
      i <- end + 1
    } else {
      i <- i + 1
    }
  }
  return(events_df)
}

#' Check for overlap between two events.
#' @param event1 Data frame row. First event.
#' @param event2 Data frame row. Second event.
#' @return Logical. True if overlap exists.
check_overlap <- function(event1, event2) {
  # Check if event1 and event2 overlap
  overlap <- (event1$start_date <= event2$end_date) && (event1$end_date >= event2$start_date)
  return(overlap)
}

#' Merge two overlapping events into one.
#' @param event1 Data frame row. First event.
#' @param event2 Data frame row. Second event to be merged.
#' @return Data frame. Merged event.
merge_events <- function(event1, event2) {
  # Determine the start and end dates of the merged event
  start_date <- min(event1$start_date, event2$start_date)
  end_date <- max(event1$end_date, event2$end_date)
  
  # Determine the centroid date based on the merged event's duration
  centroid_date <- start_date + as.numeric(end_date - start_date) / 2
  
  # Determine the lowest temperature between the two events
  lowest_temperature <- min(event1$lowest_temperature, event2$lowest_temperature)
  
  # Calculate the duration of the merged event
  duration <- as.numeric(end_date - start_date) + 1
  
  # Return the merged event as a data frame
  merged_event <- data.frame(
    start_date = start_date,
    end_date = end_date,
    centroid_date = centroid_date,
    lowest_temperature = lowest_temperature,
    duration = duration,
    NERC_ID = event1$NERC_ID
  )
  
  return(merged_event)
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
    thresholds1 <- apply(temps, 1, quantile, probs=def$threshold1)
    thresholds2 <- apply(temps, 1, quantile, probs=def$threshold2)
    
    cold_snap_events <- data.frame()    
    
    for (NERC_idx in 1:nrow(NERC_temp_data)) {
      for (year in 1980:2024) {
        start_idx <- calculate_days(year)
        end_idx <- start_idx + ifelse((year %% 4 == 0 && year %% 100 != 0) || year %% 400 == 0, 365, 364)
        NERC_temps <- temps[NERC_idx, start_idx:end_idx]
        NERC_t_min <- t_min[NERC_idx, start_idx:end_idx]
        
        events <- detect_cold_snaps(NERC_temps, NERC_t_min, thresholds1[NERC_idx], thresholds2[NERC_idx], year)
        
        if (nrow(events) > 0) {  # Check if events is not empty
          events$NERC_ID <- paste0("NERC", rownames(NERC_temp_data)[NERC_idx])
          
          events <- events[order(events$start_date), ] # Sort by start date
          j <- 1
          while (j < nrow(events)) {
            k <- j + 1
            while (k <= nrow(events)) {
              if (check_overlap(events[j, ], events[k, ])) {
                # Merge the events if they overlap
                events[j, ] <- merge_events(events[j, ], events[k, ])
                events <- events[-k, ]  # Remove the merged event (k)
              } else {
                k <- k + 1
              }
            }
            j <- j + 1
          }
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

