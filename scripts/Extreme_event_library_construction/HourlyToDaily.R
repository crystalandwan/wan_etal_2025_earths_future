# *****************************************************************************************
# Title: Daily Temperature Statistics Calculation
# Author: Heng Wan
# Date: 10/20/2025
# Purpose: Aggregate hourly temperature data into daily statistics (mean, max, min) for each
#          county.
# Description: Extracts hourly temperature data for each county, calculates daily statistics
#              (mean, max, min), and saves results as a 3D array for downstream analysis.
# Requirements: Ensure paths to input files and output files are correctly configured.
# *****************************************************************************************

# Load Required Libraries ----
library(dplyr)
library(data.table)
library(doParallel)
library(here)

# Define Utility Functions ----

#' Calculate daily temperature statistics (mean, max, min) for each county.
#' @param files Character vector. List of hourly temperature file paths.
#' @return 3D array. Dimensions: Counties x (FIPS + Stats) x Dates.
calculate_daily_stats <- function(files) {
  unique_dates <- unique(substr(files, 1, 10))  # Extract unique dates from file names
  num_stats <- 3  # mean, max, and min
  
  # Set up parallel backend to use multiple cores
  num_cores <- detectCores() - 1
  cl <- makeCluster(num_cores)
  registerDoParallel(cl)
  
  # Use foreach to iterate over each day (unique_dates) in parallel
  daily_stats_list <- foreach(i = seq_along(unique_dates), 
                              .packages = c('data.table', "dplyr")) %dopar% {
    date_pattern <- unique_dates[i]
    day_files <- files[grep(date_pattern, files)] # Get all the files related to the target date
    
    # Calculate daily mean, max, and min temperature for each county
    daily_data <- rbindlist(lapply(day_files, fread)) %>%
      .[, .(T_mean = mean(T2, na.rm = TRUE),
            T_max = max(T2, na.rm = TRUE),
            T_min = min(T2, na.rm = TRUE)), by = FIPS]
    
    return(daily_data)
  }
  
  stopCluster(cl)
    

  # Convert the list to a 3D array (Dimensions: counties x (FIPS+ stats) x dates)
  num_counties <- nrow(daily_stats_list[[1]])
  daily_stats_array <- array(NA, dim = c(num_counties, num_stats + 1, length(unique_dates)))
  num_dates <- length(daily_stats_list)
  
  for (date_idx in 1:num_dates) {
    daily_stats_array[, 1, date_idx] <- daily_stats_list[[date_idx]]$FIPS
    daily_stats_array[, 2, date_idx] <- daily_stats_list[[date_idx]]$T_mean
    daily_stats_array[, 3, date_idx] <- daily_stats_list[[date_idx]]$T_max
    daily_stats_array[, 4, date_idx] <- daily_stats_list[[date_idx]]$T_min
  }
  
  # Set dimension names
  dimnames(daily_stats_array) <- list(
    County = daily_stats_list[[1]]$FIPS,
    Statistic = c("FIPS","T_mean", "T_max", "T_min"),
    Date = unique_dates
  )
  
  return(daily_stats_array)
}

# Main Execution ----

# Define Input and Output Paths
input_dir <- here("Data", "historic")  # Directory containing hourly temperature data
output_file <- here("Data", "daily_stats_array.RData")  # Output file path for daily statistics array

# Validate Input Directory
if (!dir.exists(input_dir)) {
  stop("Input directory does not exist: ", input_dir)
}
setwd(input_dir)  # Set working directory to input directory

# Get all the input file names
file_list = list.files(pattern = "UTC") # list all hourly data
file_list = sort(file_list) # sort the file names to rank from oldest data to newest

if (length(file_list) == 0) {
  stop("No files found in the specified directory: ", input_dir)
}

# Calculate Daily Statistics
daily_stats_array <- calculate_daily_stats(file_list)

# Validate Output Directory
output_dir <- dirname(output_file)
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Save the Array to File
tryCatch({
  save(daily_stats_array, file = output_file)
  message("Daily statistics successfully saved to: ", output_file)
}, error = function(e) {
  stop("Failed to save daily statistics to file: ", output_file, "\nError: ", e$message)
})


