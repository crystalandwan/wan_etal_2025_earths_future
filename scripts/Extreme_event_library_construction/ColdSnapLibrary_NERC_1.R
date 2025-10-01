library(tools)

# Set working directory
setwd("C:/Users/wanh535/OneDrive - PNNL/Desktop/IM3/Heat Waves/Climate_data/historic")

# Create the directory for heat wave library files if it does not exist
output_dir <- "cold_snap_library"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# List of scenarios to process
scenarios <- c("NERC_average.RData", "NERC_average_area.RData", "NERC_average_pop.RData")

# Define thresholds and column index for temperature types
definitions <- list(
  list(threshold = 0.10, col_idx = 1, hw_id = 1), # 10th percentile, mean temperature
  list(threshold = 0.05, col_idx = 1, hw_id = 2), # 5th percentile, mean temperature
  list(threshold = 0.02, col_idx = 1, hw_id = 3), # 2th percentile, mean temperature
  list(threshold = 0.01, col_idx = 1, hw_id = 4), # 1th percentile, mean temperature
  list(threshold = 0.05, col_idx = 2, hw_id = 5), # 5th percentile, max temperature
  list(threshold = 0.10, col_idx = 3, hw_id = 9)  # 10th percentile, min temperature
)


#  Function to convert index to date
index_to_date <- function(start_idx, year) {
  start_date <- as.Date(paste(year, "01", "01", sep="-"))
  return(start_date + start_idx - 1)
}

# Function to calculate the start index of each year considering leap years
calculate_days <- function(year) {
  # Ensure the function returns 1 if the year is 1980
  if (year == 1980) {
    return(1)
  }
  
  # Calculate the total number of days from 1980 to the year before the input year
  days <- sum(sapply(1980:(year-1), function(y) {
    if ((y %% 4 == 0 && y %% 100 != 0) || y %% 400 == 0) {
      return(366)  # Leap year
    } else {
      return(365)  # Non-leap year
    }
  }))
  
  return(days+1)
}


# Function to identify and record heat wave events
cold_snap_identify <- function(temps, Tmin, threshold, year) {
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
        centroid_date = index_to_date(start_idx + which.min(Tmin[start_idx:end_idx]) - 1, year),
        lowest_temperature = min(Tmin[start_idx:end_idx]),
        duration = end_idx - start_idx + 1))
    }
    current_day <- current_day + wave_lengths$lengths[i]
  }
  return(events_df)
}


# Process each scenario and record heat wave events
for (scenario in scenarios) {
  load(scenario)
  NERC_temp_data <- eval(as.name(file_path_sans_ext(scenario)))
  
  for (def in definitions) {
    Tmin <- NERC_temp_data[, 3, ]
    temps <- NERC_temp_data[, def$col_idx, ]
    thresholds <- apply(temps, 1, quantile, probs = def$threshold)
    
    cold_snap_events <- data.frame()
    
    for (NERC_idx in 1:nrow(NERC_temp_data)) {
      for (year in 1980:2024) {
        start_idx <- calculate_days(year)
        end_idx <- start_idx + ifelse((year %% 4 == 0 && year %% 100 != 0) || 
                                        year %% 400 == 0, 365, 364)
        NERC_temps <- temps[NERC_idx, start_idx:end_idx]
        NERC_Tmin <- Tmin[NERC_idx, start_idx:end_idx]
        events <- cold_snap_identify(NERC_temps, NERC_Tmin, thresholds[NERC_idx], year)
        
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
