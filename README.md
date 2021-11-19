# US_precipitation_regions

This repository contains data, code, and results of the US precipitation regions analysis described in Lammers et al. (Effects of design and climate on bioretention effectiveness for watershed-scale hydrologic benefits - submitted to Journal of Sustainable Water in the Built Environment). All rainfall data were obtained from PRISM Climate Group. A description of the files included are described below. See the published paper (and supplemental materials) for details on specific metrics and methods.

## Data
The data folder contains zipped csv files of precipitation metrics calculated from daily PRISM data (1981-2010) for the entire conterminous US. Precipitation metrics were calculated separately by year using the "Daily precip analysis.R" script. In order to replicate this analysis, you would need to download daily PRISM data from the PRISM website or directly using the 'prism' R package.

## R
This folder contains the R scripts used in this analysis.

"Daily precip analysis.R" - Calculates precipitation metrics from daily PRISM data.
"PET Calc.R" - Calculates monthly average potential evapotranspiration and monthly average precipitation. Then aggregates these values to estimate average annual precipitation - potential evapotranspiration.
"Snowfall Analysis.R" - Calculates the percent of annual average precipitation that falls as snow by aggregating monthly climate normal precipitation and temperature data.
"PRISM Analysis.R" - Main script for analyzing results and creating precipitation regions. Uses outputs from above and monthly precipitation normals from PRISM to calculate remaining precipitation metrics. Using a clustering algorithim to divide the US into 10 precipitation regions.

## Results
This folder contains intermediate and final outputs from this analysis.

