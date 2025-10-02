library(sf)
library(abind)
library(data.table)

# Set working directory
setwd("PATH_TO_DATA")

# Read in NERC shapefile
NERC <- st_read("./NERC_Regions_Subregions.shp")

# Read in county shapefile 
counties <- st_read("./tl_2020_us_county.shp")

# Exclude non-CONUS counties
counties <- counties[counties$STATEFP != "02" & counties$STATEFP != "15" &
                       counties$STATEFP != "14" & counties$STATEFP != "43" &
                       counties$STATEFP != "60" & counties$STATEFP !=  "78" &
                       counties$STATEFP != "72" & counties$STATEFP != "69" &
                       counties$STATEFP != "66", ]

# Reproject NERC to match the crs of counties
NERC <- st_transform(NERC, crs = st_crs(counties))

# Find the NERC region for each county
for (i in 1:nrow(counties)){
  print(i)
  # Find all the NERC intersecting with the target county.
  intersecting_NERC <- st_intersection(counties[i, "geometry"], NERC[, c("ID", "geometry")])
  # If the county intersects with more than 1 NERC, choose the NERC with the
  # largest intersection area
  if (nrow(intersecting_NERC) > 1){
    intersecting_NERC <- intersecting_NERC[which.max(st_area(intersecting_NERC$geometry)), ]
  }
  counties[i, "ID"] <- intersecting_NERC$ID
}

joined_results <- counties
# Create FIPS by removing the leading 0
joined_results$FIPS <- joined_results$GEOID
joined_results$FIPS <- gsub("^0+", "", joined_results$FIPS)
joined_results$FIPS <- as.numeric(joined_results$FIPS)

# Read in county-level daily temperature data
load("PATH_TO_DAILY_TEMPERATURE_DATA") # daily_stats_array

# Obtain the unique NERC regions 
unique_NERC <- unique(joined_results$ID)

# Create an empty 3-D array to store temperature data at NERC-level
num_NERC <- length(unique(joined_results$ID)) # Number of NERC regions
num_stats <- 3 # min, max, and mean temperature
num_dates <- dim(daily_stats_array)[3] # Number of days
NERC_average <- array(NA, dim = c(num_NERC, num_stats, num_dates))
dim_names <- list(unique_NERC, c("T_mean", "T_max", "T_min"), NULL)
dimnames(NERC_average) <- dim_names

NERC_average_pop <- NERC_average
NERC_average_area <- NERC_average

### Average temperatre across counties
for (i in c(1:length(unique_NERC))){
  NERC_id <- unique_NERC[i]
  
  # Obtain counties within the target NERC region
  counties <- joined_results$FIPS[joined_results$ID == NERC_id]
  
  # Obtain the corresponding county indices in the temp data array
  county_indices <- which(daily_stats_array[, 1, 1] %in% counties)
  
  # Obtain the corresponding temperature data for the targeted counties within the NERC
  target_data <- daily_stats_array[county_indices, ,]
  
  # Calculate average temperature for each temperature metric and each day
  NERC_average[i, , ] <- apply(target_data[, c(2:4), ], c(2, 3), mean)
}

save(NERC_average, file = "./NERC_average.RData")

### Average temperature across counties weighted by area
joined_results$area <- st_area(joined_results)

for (i in c(1:length(unique_NERC))){
  NERC_id <- unique_NERC[i]
  
  # Obtain counties within the target NERC region
  counties <- joined_results$FIPS[joined_results$ID == NERC_id]
  
  # Obtain the corresponding county indices in the temp data array
  county_indices <- which(daily_stats_array[, 1, 1] %in% counties)
  
  # Obtain the county area
  area <- as.numeric(joined_results[county_indices, ]$area)
  
  # Obtain the corresponding temperature data for the targeted counties within the NERC
  target_data <- daily_stats_array[county_indices, ,]
  
  # Calculate average temperature for each temperature metric and each day weighted by area
  NERC_average_area[i, , ] <- apply(target_data[, c(2:4), ], c(2, 3), 
                                    function(x) weighted.mean(x, w = area))
}

save(NERC_average_area, file = "./NERC_average_area.RData")

### Average temperature across counties weighted by population
# Read in population data
pop <- fread("./county_populations_2000_to_2019.csv")
# Shannon county (FIPS 46113) in SD was renamed as Oglala Lakota county (FIPS 46102) in 2015.
# In order to match the join with the county shapefile, rename the FIPS
pop[pop$county_FIPS == 46113, "county_FIPS"] <- 46102

# Merge population data with joined_results
joined_results <- merge(joined_results, pop[, c("county_FIPS", "pop_2019")], 
                        by.x = "FIPS", by.y = "county_FIPS", all.x = TRUE)

for (i in seq_along(unique_NERC)) {
  print(i)
  NERC_id <- unique_NERC[i]
  
  # Obtain counties within the target NERC region
  counties <- joined_results$FIPS[joined_results$ID == NERC_id]
  
  # Obtain the corresponding county indices in the temp data array
  county_indices <- which(daily_stats_array[, 1, 1] %in% counties)
  
  # Obtain the pop 2019 data
  pop2019 <- as.numeric(joined_results[county_indices, ]$pop_2019)
  
  # Obtain the corresponding temperature data for the targeted counties within the NERC
  target_data <- daily_stats_array[county_indices, ,]
    
  # Calculate the mean temperature weighted by population for each temperature metric and each day
  NERC_average_pop[i, , ] <- apply(target_data[, c(2:4), ], c(2, 3), 
                                                      function(x) weighted.mean(x, w = pop2019))
    
}
                                   
save(NERC_average_pop, file = "./NERC_average_pop.RData")





