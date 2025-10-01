library(data.table)
library(sf)
library(abind)
library(parallel)

# Set the working directory
setwd("C:/Users/wanh535/OneDrive - PNNL/Desktop/IM3/Heat Waves/Climate_data")

# Define thresholds and column index for temperature types
definitions <- list(
  list(threshold1 = 0.975, threshold2 = 0.81, col_idx = 2, hw_id = 6), # max temperature
  list(threshold1 = 0.9, threshold2 = 0.75, col_idx = 2, hw_id = 7), # max temperature
  list(threshold1 = 0.975, threshold2 = 0.81, col_idx = 3, hw_id = 10), # min temperature
  list(threshold1 = 0.9, threshold2 = 0.75, col_idx = 3, hw_id = 11) # min temperature
)

# Define a function to calculate the threshold temperature based on different temp aggregation method
calculate_thresholds <- function(input_string, def) {
  if (grepl("area", input_string, ignore.case = TRUE)) {
    load("./historic/NERC_average_area.RData")
    NERC_temp <- NERC_average_area[, def$col_idx, ]
    thresholds1 <- apply(NERC_temp, 1, quantile, probs = def$threshold1)
    thresholds2 <- apply(NERC_temp, 1, quantile, probs = def$threshold2)
    
  } else if (grepl("pop", input_string, ignore.case = TRUE)) {
    load("./historic/NERC_average_pop.RData")
    NERC_temp <- NERC_average_pop[, def$col_idx, ]
    thresholds1 <- apply(NERC_temp, 1, quantile, probs = def$threshold1)
    thresholds2 <- apply(NERC_temp, 1, quantile, probs = def$threshold2)
  } else {
    load("./historic/NERC_average.RData")
    NERC_temp <- NERC_average[, def$col_idx, ]
    thresholds1 <- apply(NERC_temp, 1, quantile, probs = def$threshold1)
    thresholds2 <- apply(NERC_temp, 1, quantile, probs = def$threshold2)
  }
  return (list(thresholds1, thresholds2))
}

# Function to calculate days since 1980-01-01
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

# Function to check if the event is satisfied for a county
check_for_event <- function(county_temps, threshold1, threshold2) {
  #Check if all temperatures are greater than threshold 2:
  all_above_threshold2 <- all(county_temps > threshold2)
  
  # Check for 3+ consecutive days with temperatures greater than threshold1
  high_temp_days <- county_temps > threshold1
  wave_lengths <- rle(high_temp_days)
  
  # Safely compute the maximum number of consecutive high temperature days
  if (any(wave_lengths$values)) {  # Check if there are any TRUE values
    max_consecutive_high_temps <- max(wave_lengths$lengths[wave_lengths$values])
  } else {
    max_consecutive_high_temps <- 0  # If no TRUE values, set to 0
  }
  
  three_plus_consecutive_days <- max_consecutive_high_temps >= 3
  
  # Check if the average temperature is greater than threshold1
  average_above_threshold1 <- mean(county_temps) > threshold1
  
  # Return TRUE only if all conditions are met
  return(all_above_threshold2 && three_plus_consecutive_days && average_above_threshold1)
}

# Read in NERC-county file
counties <- st_read("counties_in_NERC2020.shp")
counties <- st_drop_geometry(counties)
counties <- counties[, c("GEOID", "ID")]
counties$GEOID <- gsub("^0+", "", counties$GEOID) # Remove the leading 0 in the county ID
counties$ID <- paste0("NERC", counties$ID)

# Read in county-level daily temperature data
load("./historic/daily_stats_array_historic1980_2019.RData")
daily_stats_1980_2019 <- daily_stats_array

load("./historic/daily_stats_array_historic2020_2024.RData")
daily_stats_2020_2024 <- daily_stats_array

daily_stats_array <- abind(daily_stats_1980_2019, daily_stats_2020_2024, along = 3) # Merge the two datasets for complete years


# List all definitions
event_defs <- list.files("./historic/heat_wave_library",
                         pattern = "def6.csv|def7.csv|def10.csv|def11.csv")


# Set up parallel backend
no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)
clusterEvalQ(cl, {
  library(data.table)
})
clusterExport(cl, list("calculate_thresholds", "date_to_days_since_1980", 
                       "check_for_event", "daily_stats_array", "counties", 
                       "definitions", "event_defs"))


# Parallel computing 
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
  NERC_event <- fread(paste0("./historic/heat_wave_library/", event_def))
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
  fwrite(NERC_event, paste0("./historic/heat_wave_library/With_spatial_coverage/", 
                            event_def))
})

















