All the scripts within this folder are used to generate extreme thermal event library (heat wave and cold snap). Data processing sequences are as below: 
1. The raw TGW data can be downloaded from MSD-live (https://tgw-data.msdlive.org/).
   "HourlyToDaily.R" is used to aggregate the county-level hourly temperature data to daily.
2. "Calculate_pop_weights_in_NERC.R" converts county-level population to weights, which is used for MWP spatial aggregation to aggregate county-level temperature to NERC subregion-level.
3. "County_to_NERC.R" is to aggregate county-level temperature data to NERC subregion- level.
4. "HeatWave/ColdSnapLibrary_NERC_1.R" is to generate NERC subregion-level heat wave/cold snap library under event definition 1, 2, 3, 4, 5, and 9;
   "HeatWave/ColdSnapLibrary_NERC_2.R" is to generate NERC subregion-level heat wave/cold snap library under event definition 6, 7, 10, and 11;
   "HeatWave/ColdSnapLibrary_NERC_3.R" is to generate NERC subregion-level heat wave/cold snap library under event definition 8 and 12.
5. "HW/CS_1_spatial_coverage.R" is to calculate spatial coverage of each heat wave/cold snap event under event definition 1, 2, 3, 4, 5, and 9;
   "HW/CS_2_spatial_coverage.R" is to calculate spatial coverage of each heat wave/cold snap event under event definition 6, 7, 10, and 11;
   "HW/CS_3_spatial_coverage.R" is to calculate spatial coverage of each heat wave/cold snap event under event definition 8 and 12. 
