library(dplyr)
library(data.table)
library(doParallel)

# Function to read and calculate daily temperature statistics for each county
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

# Set working directory
setwd("PATH TO TGW DATA")
# Get all the input file names
file_list = list.files(pattern = "UTC") # list all hourly data
file_list = sort(file_list) # sort the file names to rank from oldest data to newest

# Apply the function to the file_list to aggregate hourly temperature data to daily min/max/mean
daily_stats_array <- calculate_daily_stats(file_list)

# Write out the daily climate data
save(daily_stats_array, file = OUTPUT_FILE_PATH))


