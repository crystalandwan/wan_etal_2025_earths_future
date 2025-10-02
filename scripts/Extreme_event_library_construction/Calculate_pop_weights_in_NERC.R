### This script converts the county-level population in 2019 to the weights (between 0 and 1)
### within each NERC subregions.

library(data.table)
library(dplyr)
library(sf)

setwd("PATH_TO_DATA")

## Read in county-NERC relation shapefile
counties <- st_read("./counties_in_NERC2020.shp")
# Create FIPS by removing the leading 0
counties$FIPS <- counties$GEOID
counties$FIPS <- gsub("^0+", "", counties$FIPS)
counties$FIPS <- as.numeric(counties$FIPS)

## Read in population data
pop <- fread("./county_populations_2000_to_2019.csv")
# Shannon county (FIPS 46113) in SD was renamed as Oglala Lakota county (FIPS 46102) in 2015.
# In order to match the join with the county shapefile, rename the FIPS
pop[pop$county_FIPS == 46113, "county_FIPS"] <- 46102

## Join counties with pop
join <- merge(counties, pop[, c("county_FIPS", "pop_2019")], 
              by.x = "FIPS", by.y = "county_FIPS", all.x = TRUE)

## Calculate the weight of each county's population within its NERC subregion.
join <- join %>%
  group_by(ID) %>%
  mutate(total_pop_region = sum(pop_2019, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(pop_weight = pop_2019/total_pop_region)

## Write out the results
join2 <- st_drop_geometry(join)
join2 <- join2[, c("FIPS", "pop_weight")]
fwrite(join2, "./county_pop2019_weights_in_NERC.csv")

