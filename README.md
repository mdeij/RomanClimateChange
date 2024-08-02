# Using Simulation to Understand the Influence of Climate Instability on Grape and Wheat Yields in the Roman Empire

## Overview
This project aims to analyze how crop yields of landrace spring wheat and wine grapes were affected by changing climate conditions, specifically the shift from the Roman Climate Optimum to a cooler and more variable climate in the transitional period and later in the Late Antique Little Ice Age. We are specifically interested in crop yields during the transition from the Roman Empire's expansion to its later decline, by examining the impact of climate variability on agricultural productivity in the Roman Empire.

## Usage
1. Clone this repository to your local machine.
2. Set the working directory to the downloaded zip file.
3. Download the following historic climate files from [Copernicus Climate Data Store](https://cds.climate.copernicus.eu/cdsapp#!/dataset/projections-cmip6?tab=form).
   - **NOTE**: This is about 40 GB of data, so downloading takes a while. It is possible to run the code without downloading the data. See step 5.
   - Create an account to download the data.
   - Temporal resolution > Daily
   - Experiment > Historical
   - Variable > Precipitation, wind speed, average temperature, maximum temperature, and minimum temperature
   - Model > CMCC-ESM2 (Italy)
   - Year > 1850 - 2014
   - Month + Day > All
   - Geographical area > Whole area
4. When this data is downloaded, place it in the map: `data/source/modern climate`
5. Open the main file in R. If you have downloaded the data, you can run the entire file at once. If downloading the data was not possible, skip the first 150 lines and run from line 151 onward.

**NOTE**: The STICS model runs in JavaSTICS, which is not possible to run from `main.R`. The results, however, are placed into the temp and output files so that they can be analyzed using `main.R`.

## Folder Structure
- `data`: Contains various datasets used and created in the project.
    - `output`: Contains CSV output data generated during the analysis.
        - `plots`: Contains plots generated during the analysis.
        - `z-value stats`: Contains information about yields corresponding to specific z-value groups.
    - `source`: Contains the source data used in the analysis.
        - `modern climate`: Contains modern climate data from 1850 to 2014. Specifically daily precipitation, wind speed, average temperature, maximum temperature, and minimum temperature. This data can be downloaded from [Copernicus Climate Data Store](https://cds.climate.copernicus.eu/cdsapp#!/dataset/projections-cmip6?tab=form) using the CMCC-CM2-HR4 climate model.
        - `paleoclimatic`: Contains paleoclimatic data specifically for the Gulf of Taranto and central Europe.
        - `solar radiation`: Contains solar radiation data for Italy.
    - `temp`: Contains temporary files that are created during the analysis.
        - `colder yearly files`: Contains the created yearly climate files with colder data.
        - `dummy data`: Contains dummy climate data files to test LINTUL and STICS.
        - `grape yields`: Contains the created grape yield results.
        - `modern climate data`: Contains the created modern climate data for specific months that were used.
        - `wheat yields`: Contains the created wheat yield results.
        - `yearly files`: Contains yearly data files to run LINTUL and STICS.

- `LINTUL2`: Contains the script to run the LINTUL2 model and the model's components.
    - `Components`: Contains the parts of the LINTUL model.
    - `Results`: Contains the results of the LINTUL model for both modern runs and dummy runs.
    - `Resultscolder`: Contains the results of the LINTUL model for modern colder runs.
    - `Weather`: Contains weather data in the form of CABO files, to be used in the LINTUL model.
    - `Weathercolder`: Contains colder weather data in the form of CABO files, to be used in the LINTUL model.

- `src`: Contains source code files used in the project.

## Contact
For any inquiries about this repository, please contact marthedeij@hotmail.com or marthedeij@gmail.com
