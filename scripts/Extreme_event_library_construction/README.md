All the scripts within this folder are used to generate extreme thermal event library (heat wave and cold snap). Data processing sequences are as below: 
1. "HourlyToDaily.R" is used to aggregate the county-level hourly temperature data to daily.
2. "County_to_NERC.R" is to map each U.S. county to the corresponding NERC subregion.
3. "Calculate_pop_weights_in_NERC.R" converts county-level population to aggregation weights, which are then used for MWP spatial aggregation to aggregate county-level temperature to NERC subregion-level.
4. "County_level_temperature_to_NERC_level" is to aggregate county-level temperature data to NERC subregion- level based on three spatial aggregation methods, including SM, MWP, and MWA.
5. "HeatWave/ColdSnapLibrary_NERC_1.R" is to generate NERC subregion-level heat wave/cold snap library under event definition 1, 2, 3, 4, 5, and 9;
   "HeatWave/ColdSnapLibrary_NERC_2.R" is to generate NERC subregion-level heat wave/cold snap library under event definition 6, 7, 10, and 11;
   "HeatWave/ColdSnapLibrary_NERC_3.R" is to generate NERC subregion-level heat wave/cold snap library under event definition 8 and 12.
6. "HW/CS_1_spatial_coverage.R" is to calculate spatial coverage of each heat wave/cold snap event under event definition 1, 2, 3, 4, 5, and 9;
   "HW/CS_2_spatial_coverage.R" is to calculate spatial coverage of each heat wave/cold snap event under event definition 6, 7, 10, and 11;
   "HW/CS_3_spatial_coverage.R" is to calculate spatial coverage of each heat wave/cold snap event under event definition 8 and 12. 
