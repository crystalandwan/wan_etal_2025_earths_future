library(data.table)
library(sf)
library(abind)
library(parallel)

# Set the working directory
setwd("PATH_TO_COUNTY_LEVEL_CLIMATE_DATA")

# Define thresholds and column index for temperature types
definitions <- list(
  list(threshold = 0.1, col_idx = 2, hw_id = 8), # max temperature
  list(threshold = 0.1, col_idx = 3, hw_id = 12) # min temperature
)

# Function to calculate the start index of each year considering leap years
calculate_days <- function(year) {
  if (year == 1980) { return(1) }
  sum(sapply(1980:(year-1), function(y) {
    if ((y %% 4 == 0 && y %% 100 != 0) || y %% 400 == 0) { 366 } else { 365 }
  })) + 1
}

# Function to extract data for a specific year
extract_year_data <- function(data, year) {
  start_index <- calculate_days(year)
  is_leap_year <- (year %% 4 == 0 && year %% 100 != 0) || year %% 400 == 0
  end_index <- start_index + 364 + is_leap_year  # 364 + 1 for leap years, 364 otherwise
  
  # Extract the data for the year, ensure it has 366 entries
  year_data <- data[start_index:end_index]
  if (!is_leap_year) {
    year_data <- c(year_data, NA)  # Append NA for the 366th day in non-leap years
  }
  return(year_data)
}


# Define a function to calculate the threshold temperature based on different temp aggregation method
calculate_thresholds <- function(input_string, def) {
  threshold_list <- list()
  years <- 1980:2024
  if (grepl("area", input_string, ignore.case = TRUE)) {
    load("./historic/NERC_average_area.RData")
    NERC_temp <- NERC_average_area
  } else if (grepl("pop", input_string, ignore.case = TRUE)) {
    load("./historic/NERC_average_pop.RData")
    NERC_temp <- NERC_average_pop
  } else {
    load("./historic/NERC_average.RData")
    NERC_temp <- NERC_average
  }
  
  # Obtain the NERC region names for the threshold_list
  threshold_list_names <- rownames(NERC_temp)
  
  for(j in c(1:nrow(NERC_temp))){
    # Extract all the daily temperature data for the target NERC
    temps <- NERC_temp[j, def$col_idx, ]
    # Create a list to hold each year's data
    yearly_data <- lapply(years, function(year) extract_year_data(temps, year))
    # Convert list to a data frame
    temp_data_df <- do.call(cbind, yearly_data)
    colnames(temp_data_df) <- years  # setting column names as years
    # Data boundary (start and end 6 days) are padded with 'NA' to let the 15-day window fit.
    padded_temp_df <- rbind(matrix(rep(NA, 7 * ncol(temp_data_df)), nrow = 7),
                            temp_data_df,
                            matrix(rep(NA, 7 * ncol(temp_data_df)), nrow = 7))
    # Calculate the threshold for each day using a 15-day rolling window
    thresholds <- c()
    for(i in c(8:373)){
      data_slice <- padded_temp_df[(i-7):(i+7), ]
      data_slice <- as.vector(data_slice)
      thresholds[i-7] <- quantile(data_slice, probs = def$threshold, na.rm = TRUE)
    }
    threshold_list[[j]] <- thresholds
  }
  return(list(threshold_list_names, threshold_list))
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
check_for_event <- function(county_temps, threshold) {
  # Find days below the threshold
  low_temp_days <- county_temps < threshold
  
  # Apply run length encoding
  wave_lengths <- rle(low_temp_days)
  
  # Extract lengths where temperatures are below threshold
  low_temp_periods <- wave_lengths$lengths[wave_lengths$values]
  
  # Check if any period of high temps is at least 3 days long
  any(low_temp_periods >= 3)
}

# Read in NERC-county file
counties <- st_read("counties_in_NERC2020.shp")
counties <- st_drop_geometry(counties)
counties <- counties[, c("GEOID", "ID")]
counties$GEOID <- gsub("^0+", "", counties$GEOID) # Remove the leading 0 in the county ID
counties$ID <- paste0("NERC", counties$ID)

# Load county-level temperature data
load("./historic/daily_stats_array_historic1980_2019.RData")
daily_stats_1980_2019 <- daily_stats_array

load("./historic/daily_stats_array_historic2020_2024.RData")
daily_stats_2020_2024 <- daily_stats_array

daily_stats_array <- abind(daily_stats_1980_2019, daily_stats_2020_2024, along = 3) # Merge the two datasets for complete years


# List all definitions
event_defs <- list.files("./historic/cold_snap_library", pattern = "def8.csv|def12.csv")

# Set up parallel backend
no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)
clusterEvalQ(cl, {
  library(data.table)
})
clusterExport(cl, list("calculate_thresholds", "date_to_days_since_1980", 
                       "check_for_event", "daily_stats_array", "counties", 
                       "definitions", "event_defs", "extract_year_data", 
                       "calculate_days", "calculate_thresholds"))


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
  results <- calculate_thresholds(event_def, def)
  thresholds_list_names <- results[[1]]
  thresholds_list <- results[[2]]
  
  # Read in the target NERC-level event file
  NERC_event <- fread(paste0("./historic/cold_snap_library/", event_def))
  # Initiate the spatial coverage column
  NERC_event$spatial_coverage <- -100
  
  for(i in c(1:nrow(NERC_event))){
    # Extract the target event
    target_event <- NERC_event[i, ]
    # Get the correct threshold value for the target NERC region
    target_ID <- gsub("[^0-9]", "", target_event$NERC_ID)
    ind <- which(thresholds_list_names == target_ID)
    thresholds <- thresholds_list[[ind]] # This is the thresholds for 366 days for the target NERC
    
    # Calculate the start and end index related to the target event
    start_idx <- date_to_days_since_1980(target_event$start_date)
    end_idx <- date_to_days_since_1980(target_event$end_date)
    
    # Find the row indices related to counties within the target NERC region
    target_counties <- counties$GEOID[counties$ID == target_event$NERC_ID]
    row_idx <- which(daily_stats_array[ , 1, 1] %in% target_counties)
    
    # Extract the temperature data
    target_temp <- daily_stats_array[row_idx, def$col_idx + 1, start_idx:end_idx]
    
    # Obtain the corresponding thresholds for the targeted days
    start_day <- as.integer(format(target_event$start_date, "%j"))
    end_day <- as.integer(format(target_event$end_date, "%j"))
    final_thresholds <- thresholds[start_day:end_day]
    
    # Check whether the event is satisfied for each row (county)
    event_results <- apply(target_temp, 1, function(x) check_for_event(x, final_thresholds))
    
    spatial_coverage <- sum(event_results)/nrow(target_temp) * 100
    NERC_event[i, "spatial_coverage"] <- spatial_coverage
  }
  fwrite(NERC_event, "PATH_TO_SAVE_DATA")
})


















