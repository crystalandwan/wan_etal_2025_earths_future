# wan_etal_2025_earths_future

**What technical choices matter to characterize heat wave and cold snap events in support of bulk power grid reliability studies? **

Heng Wan<sup>1\*</sup>, Casey D. Burleyson<sup>1</sup>,  and Nathalie Voisin<sup>1, 2</sup>

<sup>1 </sup>Pacific Northwest National Laboratory, Richland, WA, USA.

<sup>2 </sup>University of Washington, Seattle, WA 98109, USA.

\* corresponding author:  heng.wan@pnnl.gov

## Abstract
Extreme weather events, such as heat waves and cold snaps, pose significant risks to the power grid. The United States (U.S.) Federal Energy Regulatory Commission (FERC) Order No. 896 mandates regional coordination standards that account for extreme thermal events. However, the lack of a universal definition for extreme thermal events may lead to inconsistent compliance efforts among neighboring entities, undermining reliability of the transmission system. This study evaluates the impact of different definitions of heat waves and cold snaps on constructing extreme event libraries for reliability studies. We used 12 event definitions and multiple temperature spatial aggregation approaches to construct historical (1980-2024) regional extreme thermal event libraries across North American Electric Reliability Corporation (NERC) subregions in the conterminous U.S. We examined the sensitivity of event characteristics (e.g., duration, frequency, intensity, and spatial coverage) to different definitions. While some definitions produced similar libraries and top event rankings, definitions based on moving-window-averaged temperatures yielded markedly different characteristics. Spatial aggregation methods had minimal impact on heat wave or cold snap intensity, frequency and duration but significantly influenced spatial coverage. The top events identified by different aggregation methods were largely consistent, exhibiting only minor variations in rankings, which were slightly more influenced by aggregation methods than by event definitions.. These findings offer critical insights for characterizing and selecting extreme thermal events and for supporting local and cross-regional coordination as required by reliability standards. Beyond its application in the power sector, this library supports diverse domains, such as climate research, resilient infrastructure design, agricultural risk management, public health, and disaster preparedness.

## Journal reference
_your journal reference_

## Data reference

### Input data
Burleyson, C., Thurber, T., & Vernon, C. (2023). Projections of Hourly Meteorology by County Based on the IM3/HyperFACETS Thermodynamic Global Warming (TGW) Simulations (v1.0.0) [Data set]. MSD-LIVE Data Repository. https://doi.org/10.57931/1960548

### Output data

Wan, H., Burleyson, C., & Voisin, N. (2025). Heat wave and cold snap event library under various technical choices for NERC subregions in the conterminous U.S. (1980 - 2024) [Data set]. American Geophysical Union Annual Meeting 2024 (AGU2024), Washington D.C. Zenodo. https://doi.org/10.5281/zenodo.15306963

## Reproduce my experiment
Clone this repository to get access to the R codes used to generate the extreme thermal event library datasets. You'll also need to download the input TGW temperature data from MSD-live (https://tgw-data.msdlive.org/). 
|Script Name | Description |
| --- | --- |
|HourlyToDaily.R | Aggregate the county-level hourly TGW temperature data to daily |
|County_to_NERC.R | Map each U.S. county to the corresponding NERC subregion. |
|Calculate_pop_weights_in_NERC.R  | Converts county-level population to aggregation weights, which are then used for MWP spatial aggregation to aggregate county-level temperature to NERC subregion-level.
|County_level_temperature_to_NERC_level | Aggregate county-level temperature data to NERC subregion- level based on three spatial aggregation methods, including SM, MWP, and MWA.
|HeatWaveLibrary_NERC1.R | Generate NERC subregion-level heat wave library under event definition 1, 2, 3, 4, 5 and 9 |
|HeatWaveLibrary_NERC2.R | Generate NERC subregion-level heat wave library under event definition 6, 7, 10 and 11|
|HeatWaveLibrary_NERC3.R | Generate NERC subregion-level heat wave library under event definition 8 and 12|
|ColdSnapLibrary_NERC1.R | Generate NERC subregion-level cold snap library under event definition 1, 2, 3, 4, 5 and 9 |
|ColdSnapLibrary_NERC2.R | Generate NERC subregion-level cold snap library under event definition 6, 7, 10 and 11|
|ColdSnapLibrary_NERC3.R | Generate NERC subregion-level cold snap library under event definition 8 and 12|
|HW_1_spatial_coverage.R | Calculate spatial coverage of each heat wave event under event definition 1, 2, 3, 4, 5 and 9 |
|HW_2_spatial_coverage.R | Calculate spatial coverage of each heat wave event under event definition 6, 7, 10 and 11 |
|HW_3_spatial_coverage.R | Calculate spatial coverage of each heat wave event under event definition 8 and 12 |
|CS_1_spatial_coverage.R | Calculate spatial coverage of each cold snap event under event definition 1, 2, 3, 4, 5 and 9 |
|CS_2_spatial_coverage.R | Calculate spatial coverage of each cold snap event under event definition 6, 7, 10 and 11 |
|CS_3_spatial_coverage.R | Calculate spatial coverage of each cold snap event under event definition 8 and 12 |

## Reproduce my figures
Use the scripts found in the `figures` directory to reproduce the figures used in this publication.

| Figure Number(s) | Script Name | Description |
| --- | --- | --- |
| 2, 3, 4, 5 | `Average_by_NERC.ipynb` | Plot extreme thermal event characteristics across different NERC subregions based on MWA and MWP spatial aggregation methods, including event intensity, event frequency, event duration, and event spatial coverage.|
| 6 | `Visualizations_of_events.py` | Plot the visualization of extreme thermal events in 1980 under event definition 4 based on MWP spatial aggregation method. |
|S1, S2, S3, S4 | `Average_by_NERC_supplementary_document.ipynb` | Plot extreme thermal event characteristics across different NERC subregions based on SM spatial aggregation method, including event intensity, event frequency, event duration, and event spatial coverage.|

