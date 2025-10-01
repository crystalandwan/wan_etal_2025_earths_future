library(tools)

# Set the working directory
setwd("PATH_TO_NERC_LEVEL_CLIMATE_DATA")

# Create the directory for heat wave library files if it does not exist
output_dir <- "heat_wave_library"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Set the historical years range
years <- 1980:2024

# List of aggregation methods to process
scenarios <- c("NERC_average.RData", "NERC_average_area.RData", "NERC_average_pop.RData")

# Define thresholds and column index for temperature types
definitions <- list(
  list(threshold = 0.9, col_idx = 2, hw_id = 8), # max temperature
  list(threshold = 0.9, col_idx = 3, hw_id = 12) # min temperature
)

# Function to convert index to date
index_to_date <- function(start_idx, year) {
  as.Date(paste(year, "01", "01", sep="-")) + start_idx - 1
}

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


# Function to detect and record heat wave events
detect_heat_waves <- function(temps, thresholds, Tmax, year) {
  # For non-leap years, delete the final threshold value
  if(length(temps) == 365){
    thresholds = thresholds[1:365]
  }
  
  high_temp_days <- temps > thresholds # Find all days with high temp exceeding threshold
  wave_lengths <- rle(high_temp_days) # get the consecutive sequences of identical values (TRUE for days exceeding threshold; FALSE for days below threshold)
  
  # Construct a data frame to store the events
  events_df <- data.frame(start_date = as.Date(character()),
                          end_date = as.Date(character()), 
                          centroid_date = as.Date(character()),
                          highest_temperature = numeric(), 
                          duration = numeric())
  current_day <- 1
  for (i in 1:length(wave_lengths$lengths)) {
    if (wave_lengths$values[i] && wave_lengths$lengths[i] >= 3) {
      start_idx <- current_day
      end_idx <- start_idx + wave_lengths$lengths[i] - 1
      events_df <- rbind(events_df, data.frame(
        start_date = index_to_date(start_idx, year),
        end_date = index_to_date(end_idx, year),
        centroid_date = index_to_date(start_idx + which.max(Tmax[start_idx:end_idx]) - 1, year),
        highest_temperature = max(Tmax[start_idx:end_idx]), 
        duration = end_idx - start_idx + 1
      ))
    }
    current_day <- current_day + wave_lengths$lengths[i]
  }
  return(events_df)
}


# Process each scenario
for (scenario in scenarios) {
  load(scenario)
  NERC_temp_data <- eval(as.name(file_path_sans_ext(scenario)))
  
  for (def in definitions) {
    heat_wave_events <- data.frame()    
    
    for (NERC_idx in 1:dim(NERC_temp_data)[1]) {
      # Extract all the daily temperature data for the target NERC
      temps <- NERC_temp_data[NERC_idx, def$col_idx, ]
      # Get all the daily Tmax data
      Tmax <- NERC_temp_data[NERC_idx, 2, ]
      # Create a list to hold each year's data
      yearly_data <- lapply(years, function(year) extract_year_data(temps, year))
      # Convert list to a data frame
      temp_data_df <- do.call(cbind, yearly_data)
      colnames(temp_data_df) <- years  # setting column names as years
      
      # Data boundary (start and end 6 days) are padded with 'NA' to let the 15-day window fit.
      padded_temp_df <- rbind(matrix(rep(NA, 7 * ncol(temp_data_df)), nrow = 7),
                              temp_data_df,
                              matrix(rep(NA, 7 * ncol(temp_data_df)), nrow = 7))
      # Calculate the 90th percentile for each day using a 15-day rolling window
      thresholds <- c()
      for(i in c(8:373)){
        data_slice <- padded_temp_df[(i-7):(i+7), ]
        data_slice <- as.vector(data_slice)
        thresholds[i-7] <- quantile(data_slice, probs = def$threshold, na.rm = TRUE)
      }
      
      for (year in years) {
        NERC_temps <- extract_year_data(temps, year)
        NERC_Tmax <- extract_year_data(Tmax, year)
        events <- detect_heat_waves(NERC_temps, thresholds, NERC_Tmax, year)
        
        if (nrow(events) > 0) {  # Check if events is not empty
          events$NERC_ID <- paste0("NERC", rownames(NERC_temp_data)[NERC_idx])
          heat_wave_events <- rbind(heat_wave_events, events)
        }
      }
    }
    if (nrow(heat_wave_events) > 0) {  # Check before writing to file
      file_name <- sprintf("%s/heat_wave_library_%s_def%d.csv", output_dir, 
                           file_path_sans_ext(basename(scenario)), def$hw_id)
      write.csv(heat_wave_events, file_name, row.names = FALSE)
    } else {
      cat("No heat wave events detected for definition", def$hw_id, "in file", scenario, "\n")
    }
  }
}

