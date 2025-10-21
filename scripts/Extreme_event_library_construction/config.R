# config.R

# Load the 'here' package to enable relative paths
library(here)

# Define the file path to NERC subregion shapefile
nerc_path <- here("Data", "NERC_Regions_Subregions/NERC_Regions_Subregions.shp")

# Define the file path for county shapefile
county_path <- here("Data", "us_county_shapefile/tl_2020_us_county.shp")

# Define the file path for population data
pop_path <- here("Data", "county_populations_2000_to_2019.csv")
 
# Define the file path for county-nerc relational shapefile ("county_to_NERC.R" shall be run first to generate this intermediate output)
county_nerc_path <- here("Data", "county_nerc.shp")

# Define the county-level data temperature data path ("HourlyToDaily.R" shall be run first to generate this intermediate output)
county_level_temp_data_path <- here("Data", "daily_stats_array.RData")

#Define the file path for population weight data ("Calculate_pop_weights_in_NERC.R" shall be run first to generate this intermediate output)
pop_weight_path <- here("Data", "county_pop2019_weights_in_NERC.csv")

# Define the path for NERC_level_temp_data ("County_level_temperature_to_NERC_level.R" shall be run first to generate this data folder)
nerc_level_temp_data_path <- here("Data", "NERC_level_temp_data")