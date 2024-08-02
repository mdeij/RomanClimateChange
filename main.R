# Load libraries
library(ncdf4)
library(deSolve)
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)


#### Create the CABO and csv files ####
# Source the script that creates CABO files, to be used in the LINTUL model
source("src/1a_CABOfiles.R")

# Load the solar radiation data for the 10 locations in Italy
solardata <- read.table("data/source/solar radiation/solarradiation_daily.txt", header = FALSE)

# Define a list of file paths, used to load the NC-files that contain modern climate data for Italy
file_paths <- c(
  "18500101-18620521",
  "18620522-18741009",
  "18741010-18870227",
  "18870228-18990718",
  "18990719-19111206",
  "19111207-19240426",
  "19240427-19360914",
  "19360915-19490202",
  "19490203-19610623",
  "19610624-19731111",
  "19731112-19860401",
  "19860402-19980820",
  "19980821-20110108",
  "20110109-20141231"
)

# Create empty lists to store the nc files
max_ncfiles <- list()
min_ncfiles <- list()
wind_ncfiles <- list()
pr_ncfiles <- list()
avt_ncfiles <- list()

# Loop through file paths and load nc files
for (path in file_paths) {
  max_ncfiles[[path]] <- nc_open(paste0("data/source/modern climate/tasmax_day_CMCC-ESM2_historical_r1i1p1f1_gn_", path, "_v20210114.nc"))
  min_ncfiles[[path]] <- nc_open(paste0("data/source/modern climate/tasmin_day_CMCC-ESM2_historical_r1i1p1f1_gn_", path, "_v20210114.nc"))
  wind_ncfiles[[path]] <- nc_open(paste0("data/source/modern climate/sfcWind_day_CMCC-ESM2_historical_r1i1p1f1_gn_", path, "_v20210114.nc"))
  pr_ncfiles[[path]] <- nc_open(paste0("data/source/modern climate/pr_day_CMCC-ESM2_historical_r1i1p1f1_gn_", path, "_v20210114.nc"))
  avt_ncfiles[[path]] <- nc_open(paste0("data/source/modern climate/tas_day_CMCC-ESM2_historical_r1i1p1f1_gn_", path, "_v20210114.nc"))
}

# Define a list of coordinates for the five locations for wheat 
locations <- list(
  c(45.2, 11.4),   # Po valley, wheat 1
  c(44.0, 12),      # Appenines, wheat 2
  c(42.25, 11.8),   # Lazio, wheat 3
  c(40.8, 16.35),   # Puglia, wheat 4
  c(37.55, 14.35)   # Sicilia, wheat 5
)

# Loop through locations and call modify_wheat_data function for each location
for (i in seq_along(locations)) {
  # Assign coordinates for the current location
  target_lat <- locations[[i]][1]
  target_lon <- locations[[i]][2]
  
  # Define the station number for this location
  station_number <- i
  
  # Call the modify_data function, this creates a large CABO file containing all modern weather data
  modify_wheat_data(max_ncfiles, min_ncfiles, wind_ncfiles, pr_ncfiles, avt_ncfiles, solardata, station_number)
}

# Source the script that creates csv's, to be used in the STICS model
source("src/1b_CSVfiles.R")

# Define a list of coordinates for the five locations for grapes
locations <- list(
  c(45.42, 11.55),  # Po valley, grapes 1
  c(43.67, 11.35),  # Florence, grapes 2
  c(42.23, 14.31),  # Abruzzo, grapes 3
  c(40.39, 17.77),  # Puglia, grapes 4
  c(37.87, 12.6)    # Sicilia, grapes 5
)

# Loop through locations and call modify_grapes_data function for each location
for (i in seq_along(locations)) {
  # Assign coordinates for the current location
  target_lat <- locations[[i]][1]
  target_lon <- locations[[i]][2]
  
  # Define the station number for this location
  station_number <- i
  
  # Call the modify_data function, this creates a large csv file containing all modern weather data
  modify_grapes_data(max_ncfiles, min_ncfiles, wind_ncfiles, pr_ncfiles, avt_ncfiles, solardata, station_number)
}




#### Create modern climate files to compare to paleoclimatic data ####
# Source the script that creates modern climate data, specifically for spring and summer
source("src/1c_create_modern_climdata.R")

# Define a list of coordinates for the five locations for wheat 
locations <- list(
  c(45.2, 11.4),   # Po valley, wheat 1
  c(44.0, 12),      # Appenines, wheat 2
  c(42.25, 11.8),   # Lazio, wheat 3
  c(40.8, 16.35),   # Puglia, wheat 4
  c(37.55, 14.35)   # Sicilia, wheat 5
)

# Loop through locations and call the function that obtains average temperature and precipitation data for each location
for (i in seq_along(locations)) {
  # Assign coordinates for the current location
  target_lat <- locations[[i]][1]
  target_lon <- locations[[i]][2]
  
  # Define the station number for this location
  station_number <- i
  
  # Call the modern_wheat function, this creates a file with modern average temperature and precipitation data for the wheat locations
  modern_wheat(pr_ncfiles, avt_ncfiles, station_number, i)
}

# Define a list of coordinates for the five locations for grapes
locations <- list(
  c(45.42, 11.55),  # Po valley, grapes 1
  c(43.67, 11.35),  # Florence, grapes 2
  c(42.23, 14.31),  # Abruzzo, grapes 3
  c(40.39, 17.77),  # Puglia, grapes 4
  c(37.87, 12.6)    # Sicilia, grapes 5
)

# Loop through locations and call the function for each location
for (i in seq_along(locations)) {
  # Assign coordinates for the current location
  target_lat <- locations[[i]][1]
  target_lon <- locations[[i]][2]
  
  # Define the station number for this location
  station_number <- i
  
  # Call the modern_grapes function, this creates a file with modern average temperature and precipitation data for the grape locations
  modern_grapes(pr_ncfiles, avt_ncfiles, station_number, i)
}


# If you are unable to download the climate data, start running the code from this point forward, not including the previous lines of code. 
# Load libraries
library(ncdf4)
library(deSolve)
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)




#### Create folder structure ####
# List of folders to create
folders <- c(
  "LINTUL2/Results",
  "LINTUL2/Resultscolder",
  "LINTUL2/Resultsdummy",
  "LINTUL2/Weather",
  "LINTUL2/Weathercolder",
  "LINTUL2/Weatherdummy",
  "data/output/plots",
  "data/output/z_value stats",
  "data/source/modern climate",
  "data/temp/colder yearly files",
  "data/temp/dummy data",
  "data/temp/wheat yields",
  "data/temp/yearly files"
)

# Function to create folders
create_folders <- function(folders) {
  for (folder in folders) {
    if (!dir.exists(folder)) {
      dir.create(folder, recursive = TRUE)
      cat("Created folder:", folder, "\n")
    } else {
      cat("Folder already exists:", folder, "\n")
    }
  }
}

# Create the folders
create_folders(folders)




#### Split into yearly files ####
# Source the script that splits the created data into yearly files, to be used in both models
source("src/2a_split_files.R")

# Create an empty list to store the wheat data
wheat_data <- list()

# Loop through all locations for wheat
for (i in 1:5) {
  file_name <- paste0("data/temp/wheat_", i, ".cabo")
  # Read the file and store the data in the list
  wheat_data[[i]] <- read.table(file_name, header = FALSE, sep = "")
  
  # Call the function that splits the CABO file for each iteration
  split_wheat(wheat_data[[i]], i)
}

# Create and empty list to store the grape data
grapes_data <- list()

# Loop through all locations for grapes
for (i in 1:5) {
  file_name <- paste0("data/temp/grapes_", i, ".csv")
  # Read the file and store the data in the list
  grapes_data[[i]] <- read.table(file_name, sep = ";", header = FALSE)
  
  # Split the grapes data using the function split_grapes
  split_grapes(grapes_data[[i]], i)
}




#### Create colder yearly files ####
# Create colder yearly files
source("src/2a_split_files.R")
source("src/2b_generate_colder_years.R")

# Give centers of normal distributions
winter <- -5.74
spring <- -5.74
summer <- -3.74
autumn <- -4.74

# Loop through all locations for wheat and generate colder years
for (i in 1:5) {
  file_name <- paste0("data/temp/wheat_", i, ".CABO")
  
  # Load CABO wheat file
  wheat_CABO <- read.table(file_name)
  
  # Modify wheat climate data with colder temperatures
  create_colder_wheatyears(wheat_CABO, winter, spring, summer, autumn, i)
}

# Create an empty list to store the wheat data to split it
wheat_data <- list()

# Loop through all locations for wheat and split the colder data
for (i in 1:5) {
  file_name <- paste0("data/temp/wheat_colder_", i, ".cabo")
  # Read the file and store the data in the list
  wheat_data[[i]] <- read.table(file_name, header = FALSE, sep = "")
  
  # Call the function that splits the CABO file for each iteration
  split_colder_wheat(wheat_data[[i]], i)
}


# Load all csv grape files with modern climate data
# Loop through all locations for wheat and generate colder years
for (i in 1:5) {
  file_name <- paste0("data/temp/grapes_", i, ".csv")
  
  # Load csv grapes file
  grapes_CSV <- read.table(file_name, sep = ";", header = FALSE)
  
  # Modify wheat climate data with colder temperatures
  create_colder_grapeyears(grapes_CSV, winter, spring, summer, autumn, i)
}

# Generate colder years for the grapes climate data
# Create and empty list to store the grape data
grapes_data <- list()

# Loop through all locations for grapes
for (i in 1:5) {
  file_name <- paste0("data/temp/grapes_colder_", i, ".csv")
  # Read the file and store the data in the list
  grapes_data[[i]] <- read.table(file_name, sep = ";", header = FALSE)
  
  # Split the grapes data using the function split_grapes
  split_colder_grapes(grapes_data[[i]], i)
}




#### Find modern average temp and precip for colder data ####
source("src/2c_create_modern_climdata_colder.R")

# Create a file with the daily average temperature and precipitation per wheat location
# Loop through all locations for wheat and split the colder data
for (i in 1:5) {
  file_name <- paste0("data/temp/wheat_colder_", i, ".cabo")
  # Call function
  new_file <- modern_colderclim_wheat(file_name, i)
  print(paste("New file created:", new_file))
}

# Create a file with the daily average temperature and precipitation per grape location
# Loop through all locations for grapes
for (i in 1:5) {
  file_name <- paste0("data/temp/grapes_colder_", i, ".csv")
  # Call function
  new_file <- modern_colderclim_grapes(file_name, i)
  print(paste("New file created:", new_file))
} 




#### Modify CABO files to be able to run winter wheat ####
# Source the right file:
source("src/2d_add_text_CABO_files.R")

# Call the function that adds text on top and modifies the year, this is saved in the LINTUL model
add_text("data/temp/yearly files", "LINTUL2/Weather")

# Call the function to add text on top for the colder years
add_text("data/temp/colder yearly files", "LINTUL2/Weathercolder")




#### Run LINTUL for wheat, for the normal years (not colder) ####
# Source the LINTUL model 
source("LINTUL2/LINTUL2_runs.R")

# Run LINTUL for (winter) bread wheat for the years 1850 - 1899 for locations 1, 2 and 3
run_simulation_breadwheat_1899(1:3)

# Run LINTUL for (winter) bread wheat for the years 1901 - 1905 for locations 1, 2 and 3
run_simulation_breadwheat_1905(1:3)

# Run LINTUL for (winter) bread wheat for the years 1907 - 2013 for locations 1, 2 and 3
run_simulation_breadwheat_2013(1:3)

# Run LINTUL for durum for locations 4 and 5
run_simulation_durum(4:5)

# Run LINTUL for (winter) bread wheat for the years 1850 - 1899 for locations 1, 2 and 3
run_simulation_breadwheat_1899_colder(1:3)

# Run LINTUL for (winter) bread wheat for the years 1901 - 1905 for locations 1, 2 and 3
run_simulation_breadwheat_1905_colder(1:3)

# Run LINTUL for (winter) bread wheat for the years 1907 - 2013 for locations 1, 2 and 3
run_simulation_breadwheat_2013_colder(1:3)

# Run LINTUL for durum for locations 4 and 5
run_simulation_durum_1899_colder(4:5)

# Run LINTUL for durum for locations 4 and 5
run_simulation_durum_1905_colder(4:5)

# Run LINTUL for durum for locations 4 and 5
run_simulation_durum_2013_colder(4:5)

                                     


#### Get LINTUL yields per year from the model results####
# Source the script that obtains LINTUL yields
source("src/3a_get_LINTUL_yields.R")

# Create a vector of years from 1850 to 2014
years <- 1850:2014

# Define the number of locations
num_locations <- 5

# Create an empty list to store the data frames
data_list <- list()

# Loop through each location and load the CSV files with normal results
for (location in 1:num_locations) {
  location_data <- list()
  for (year in years) {
    filename <- paste0("LINTUL2/Results/Location_", location, "_results_for_", year, ".csv")
    if (file.exists(filename)) {
      location_data[[year]] <- read.csv(filename, header = TRUE)
    } else {
      # If file doesn't exist, create an empty data frame
      location_data[[year]] <- data.frame()
    }
  }
  data_list[[location]] <- location_data
}

# Call the function that finds the yields in the results
get_WSOTHA_values(data_list, years, num_locations)

# Create an empty list to store the data frames for the colder years
data_list <- list()

# Loop through each location and load the CSV files with colder results
for (location in 1:num_locations) {
  location_data <- list()
  for (year in years) {
    filename <- paste0("LINTUL2/Resultscolder/Location_", location, "_results_for_", year, ".csv")
    if (file.exists(filename)) {
      location_data[[year]] <- read.csv(filename, header = TRUE)
    } else {
      # If file doesn't exist, create an empty data frame
      location_data[[year]] <- data.frame()
    }
  }
  data_list[[location]] <- location_data
}

# Call the function that finds the yields in the results
get_WSOTHA_values_colder(data_list, years, num_locations)




#### Combine colder years and regular years into one file ####
# Source the function to combine the yields
source("src/3b_combine_wheatyields_onefile.R")

# Read the regular yields CSV file
wheat_yields <- read.csv("data/temp/wheat yields/wheat_yields.csv")

# Read the second CSV file
wheat_yields_colder <- read.csv("data/temp/wheat yields/wheat_yields_colder.csv")

# Run the function that combines the files
combine_wheatyields(wheat_yields, wheat_yields_colder)

# Do the same for location 1 for grapes
custom_column_names <- c("Year", "Location1", "Location2", "Location3", "Location4", "Location5")

# Load the normal grape yields 
grape1_yields <- read.csv("data/temp/grape yields/grape_yields.csv", sep = ";", header = TRUE, col.names = custom_column_names)

# Read the second CSV file with the colder grape yields
grape1_yields_colder <- read.csv("data/temp/grape yields/grape_yields_colder.csv", sep = ";", header = TRUE, col.names = custom_column_names)

# Run the function that combines the files
combine_grape1yields(grape1_yields, grape1_yields_colder)

# Remove grape location 1 from the normal grape yield results
custom_column_names <- c("Year", "Location1", "Location2", "Location3", "Location4", "Location5")

# Load the normal grape yields 
grape_yields <- read.csv("data/temp/grape yields/grape_yields.csv", sep = ";", header = TRUE, col.names = custom_column_names)

# Run the function that removes location 1
combine_grapeyields(grape_yields)




#### Modify the modern climate data to compare to paleoclimatic data ####
# Obtain function from script
source("src/4a_modify_modernclimdata.R")

# List of CSV files for wheat containing modern average temperature and precipitation data 
files <- c("data/temp/modern climate data/modern_tempprecip_wheatdata_1.csv",
           "data/temp/modern climate data/modern_tempprecip_wheatdata_2.csv",
           "data/temp/modern climate data/modern_tempprecip_wheatdata_3.csv")

# Call the function to summarize the data
combined_data <- winterwheat_may(files)


# Write the combined data to a CSV file, this creates a file with spring (average temperature and precipitation) data for wheat
write.table(combined_data, "data/temp/modern climate data/modern_may_winterwheat_climdata.csv", row.names = FALSE, sep = ";", quote = FALSE)

# List of CSV files for wheat containing modern average temperature and precipitation data 
files <- c("data/temp/modern climate data/modern_tempprecip_wheatdata_4.csv",
           "data/temp/modern climate data/modern_tempprecip_wheatdata_5.csv")

# Call the function to summarize the data
combined_data <- durum_may(files)

# Write the combined data to a CSV file, this creates a file with spring (average temperature and precipitation) data for wheat
write.table(combined_data, "data/temp/modern climate data/modern_may_durum_climdata.csv", row.names = FALSE, sep = ";", quote = FALSE)


# List of CSV files for wheat containing modern average temperature and precipitation data 
files <- c("data/temp/modern climate data/colder_modern_tempprecip_wheatdata_1.csv",
           "data/temp/modern climate data/colder_modern_tempprecip_wheatdata_2.csv",
           "data/temp/modern climate data/colder_modern_tempprecip_wheatdata_3.csv")

# Call the function to summarize the data
combined_data <- winterwheat_may_colder(files)

# Write the combined data to a CSV file, this creates a file with spring (average temperature and precipitation) data for wheat
write.table(combined_data, "data/temp/modern climate data/colder_modern_may_winterwheat_climdata.csv", row.names = FALSE, sep = ";", quote = FALSE)


# List of CSV files for wheat containing modern average temperature and precipitation data 
files <- c("data/temp/modern climate data/colder_modern_tempprecip_wheatdata_4.csv",
           "data/temp/modern climate data/colder_modern_tempprecip_wheatdata_5.csv")

# Call the function to summarize the data
combined_data <- durum_may_colder(files)

# Write the combined data to a CSV file, this creates a file with spring (average temperature and precipitation) data for wheat
write.table(combined_data, "data/temp/modern climate data/colder_modern_may_durum_climdata.csv", row.names = FALSE, sep = ";", quote = FALSE)


# List of CSV files for grapes containing modern average temperature and precipitation data 
files <- c("data/temp/modern climate data/modern_tempprecip_grapedata_2.csv",
           "data/temp/modern climate data/modern_tempprecip_grapedata_3.csv",
           "data/temp/modern climate data/modern_tempprecip_grapedata_4.csv",
           "data/temp/modern climate data/modern_tempprecip_grapedata_5.csv")

# Call the function to summarize the data
combined_data <- springgrapes_am(files)

# Write the combined data to a CSV file, this creates a file with spring (average temperature and precipitation) data for grapes
write.table(combined_data, "data/temp/modern climate data/modern_am_grapes_climdata.csv", row.names = FALSE, sep = ";", quote = FALSE)


#create file for grapes with all five locations precip and average temp for august and september
# List of CSV files for grapes containing modern average temperature and precipitation data 
files <- c("data/temp/modern climate data/modern_tempprecip_grapedata_1.csv")

# Call the function to summarize the data
combined_data <- summergrapes_as(files)

# Write the combined data to a CSV file, this creates a file with summer (average temperature and precipitation) data for grapes
write.table(combined_data, "data/temp/modern climate data/modern_as_grapes_climdata.csv", row.names = FALSE, sep = ";", quote = FALSE)


# List of CSV files for grapes containing modern average temperature and precipitation data 
files <- c("data/temp/modern climate data/modern_tempprecip_grapedata_1.csv")

# Call the function to summarize the data
combined_data <- summergrapes_as_colder(files)

# Write the combined data to a CSV file, this creates a file with summer (average temperature and precipitation) data for grapes
write.table(combined_data, "data/temp/modern climate data/colder_as_grapes_climdata.csv", row.names = FALSE, sep = ";", quote = FALSE)




#### Combine the modern climdata for the regular and the colder wheat (1685 - 2014) ####
# Source the function to combine the yields
source("src/4b_combine_coldandmodernclimdata.R")

# Read the regular yields CSV file
winterwheat_regular <- read.csv("data/temp/modern climate data/modern_may_winterwheat_climdata.csv", header = TRUE, sep = ";")

# Read the second CSV file
winterwheat_colder <- read.csv("data/temp/modern climate data/colder_modern_may_winterwheat_climdata.csv", header = TRUE, sep = ";")

# Run the function that combines the files
combine_climdata_winterwheat(winterwheat_regular, winterwheat_colder)


# Read the regular yields CSV file
durum_regular <- read.csv("data/temp/modern climate data/modern_may_durum_climdata.csv", header = TRUE, sep = ";")

# Read the second CSV file
durum_colder <- read.csv("data/temp/modern climate data/colder_modern_may_durum_climdata.csv", header = TRUE, sep = ";")

# Run the function that combines the files
combine_climdata_durum(durum_regular, durum_colder)


# Read the regular yields CSV file
summergrapes_regular <- read.csv("data/temp/modern climate data/modern_as_grapes_climdata.csv", header = TRUE, sep = ";")

# Read the second CSV file
summergrapes_colder <- read.csv("data/temp/modern climate data/colder_as_grapes_climdata.csv", header = TRUE, sep = ";")

# Run the function that combines the files
combine_climdata_summergrapes(summergrapes_regular, summergrapes_colder)




#### Filter out years containing too hot temperatures ####
# Obtain functions from script
source("src/4c_filter_too_hot_springs.R")

# Load the winter wheat climate data and yields
winterwheat_yields <- read.csv("data/temp/wheat yields/combined_winterwheat_yields.csv", header = TRUE)
winterwheat_may <- read.csv("data/temp/modern climate data/combined_modern_may_winterwheat_climdata.csv", sep = ";", header = TRUE)

# Run the function for winter wheat
filtered_data_list_winterwheat <- filter_winterwheat(winterwheat_yields, winterwheat_may)

# Load the durum climate data and yields
durum_yields <- read.csv("data/temp/wheat yields/combineddurum_yields.csv", header = TRUE)
durum_may <- read.csv("data/temp/modern climate data/combined_modern_may_durum_climdata.csv", sep = ";", header = TRUE)

# Run the function for durum
filtered_data_list_durum <- filter_durum(durum_yields, durum_may)

# Load the summer grape data and yields
summergrape_yields <- read.csv("data/temp/grape yields/combined_grape1_yields.csv", header = TRUE)
summergrape_as <- read.csv("data/temp/modern climate data/combined_modern_as_grapes_climdata.csv", sep = ";", header = TRUE)

# Run the function for summer grapes
filtered_data_list_gas <- filter_summergrapes(summergrape_yields, summergrape_as)

# Load the spring grape data and yields
springgrape_yields <- read.csv("data/temp/grape yields/combined_grape_yields.csv", header = TRUE)
springgrape_am <- read.csv("data/temp/modern climate data/modern_am_grapes_climdata.csv", sep = ";", header = TRUE)

# Run the function for spring grapes 
filtered_data_list_gam <- filter_springgrapes(springgrape_yields, springgrape_am)




#### Match z-values of precipitation and temperature to yields ####
# Obtain functions from script
source("src/5a_determine_zvalues.R")

# To run the functions that determine the z-values, make sure to run lines where the data is filtered (4c)!
# Those functions return the filtered data lists that are used here. 
# Call the function to determine the yields that correspond to certain z-value groups (-3 to 3, steps of 0.5)
result_list_ww <- z_values_winterwheat(filtered_data_list_winterwheat)
result_list_dr <- z_values_durum(filtered_data_list_durum)
result_list_gas <- z_values_summergrapes(filtered_data_list_gas)
result_list_gam <- z_values_springgrapes(filtered_data_list_gam)

# Find statistical details about the groups of z-values 
z_values_winterwheat_stats(filtered_data_list_winterwheat)
z_values_durum_stats(filtered_data_list_durum)
z_values_summergrapes_stats(filtered_data_list_gas)
z_values_springgrapes_stats(filtered_data_list_gam)

# Check the corresponding precipitation and temperature values for each z-value group
check_temperature_zvalues_winterwheat(filtered_data_list_winterwheat)
check_temperature_zvalues_durum(filtered_data_list_durum)
check_temperature_zvalues_summergrapes(filtered_data_list_gas)
check_temperature_zvalues_springgrapes(filtered_data_list_gam)
check_precipitation_zvalues_winterwheat(filtered_data_list_winterwheat)
check_precipitation_zvalues_durum(filtered_data_list_durum)
check_precipitation_zvalues_summergrapes(filtered_data_list_gas)
check_precipitation_zvalues_springgrapes(filtered_data_list_gam)

# Find the corresponding precipitation z-values for paleoclimatic data
paleo_marinecore <- read.csv("data/source/paleoclimatic/coredataCSV.csv", header = TRUE, sep = ';')
check_precipitation_zvalues_paleo(paleo_marinecore)
check_temperature_zvalues_paleo(paleo_marinecore)

#Put z-value data in Excel files
winterwheat_excel(result_list_ww, "data/output/z_value stats/winterwheat_results.xlsx", "data/output/z_value stats/average_winterwheat_results.xlsx")
durum_excel(result_list_dr, "data/output/z_value stats/durum_results.xlsx", "data/output/z_value stats/average_durum_results.xlsx")
summergrapes_excel(result_list_gas, "data/output/z_value stats/summergrapes_results.xlsx", "data/output/z_value stats/average_summergrapes_results.xlsx")
springgrapes_excel(result_list_gam, "data/output/z_value stats/springgrapes_results.xlsx", "data/output/z_value stats/average_springgrapes_results.xlsx")




#### Find yield envelopes corresponding to certain climatic periods ####
# Obtain functions from script
source("src/5b_yields_per_climaticperiod.R")

# Function to find the average yields for the RCO
RCO_envelopes_winterwheat(filtered_data_list_winterwheat)
RCO_envelopes_durum(filtered_data_list_durum)
RCO_envelopes_summergrapes(filtered_data_list_gas)
RCO_envelopes_springgrapes(filtered_data_list_gam)

# Find average yields for the climatic period 160 - 180 C.E.
TP_160_180_winterwheat(filtered_data_list_winterwheat)
TP_160_180_durum(filtered_data_list_durum)
TP_160_180_summergrapes(filtered_data_list_gas)

# Find average yields for the climatic period 245 - 275 C.E.
TP_245_275_winterwheat(filtered_data_list_winterwheat)
TP_245_275_durum(filtered_data_list_durum)
TP_245_275_summergrapes(filtered_data_list_gas)

# Find LALIA average yields 
LALIA_envelopes_winterwheat(filtered_data_list_winterwheat)
LALIA_envelopes_durum(filtered_data_list_durum)
LALIA_envelopes_summergrapes(filtered_data_list_gas)




#### Create dummy climate data for LINTUL and STICS ####
source("src/6a_create_dummy_climdata.R")

# Load one random CABO file, just the format is used, not the data
CABO_example <- read.table("data/temp/yearly files/ITA3.013", header = FALSE, sep = "\t")

# Generate the dummy CABO files for increasing precipitation
generate_dummy_CABO_precipitation(CABO_example)

# Generate the dummy CABO files for increasing temperature
generate_dummy_CABO_temperature(CABO_example)

# Load one random csv, just the format is used, not the data
CSV_example <- read.table("data/temp/yearly files/grapes3_2013.csv", header = FALSE, sep = ";")

# Generate the dummy csv files for increasing precipitation
generate_dummy_CSV_precipitation(CSV_example)

# Generate the dummy csv files for increasing temperature
generate_dummy_CSV_temperature(CSV_example)




#### Run dummy data for LINTUL ####
# Source the LINTUL model 
source("LINTUL2/LINTUL2_runs.R")

# Run LINTUL for the dummy data with a changing temperature or precipitation value
run_simulation_dummy_temp(1)
run_simulation_dummy_precip(1)




#### Get LINTUL yields per year for the dummy data####
# Source the script that obtains LINTUL yields
source("src/6b_get_dummy_yields.R")

# Create a vector of years from 1850 to 2014
years_temp <- 1240:1260
years_precip <- 1140:1160

# Define the number of locations
num_locations <- 1

# Create an empty list to store the data frames
data_list_t <- list()
data_list_p <- list()

# Loop through each location and load the CSV files for temperature
for (location in 1:num_locations) {
  location_data <- list()
  for (year in years_temp) {
    filename <- paste0("LINTUL2/Resultsdummy/Location_", location, "_results_for_", year, ".csv")
    location_data[[year]] <- read.csv(filename, header = TRUE)
  }
  data_list_t[[location]] <- location_data
}

# Loop through each location and load the CSV files for precipitation
for (location in 1:num_locations) {
  location_data <- list()
  for (year in years_precip) {
    filename <- paste0("LINTUL2/Resultsdummy/Location_", location, "_results_for_", year, ".csv")
    location_data[[year]] <- read.csv(filename, header = TRUE)
  }
  data_list_p[[location]] <- location_data
}

# Call the function that finds the yields in the results
get_dummy_values_temp(data_list_t, years_temp, num_locations)
get_dummy_values_precip(data_list_p, years_precip, num_locations)




#### Make scatterplots for dummy yields #### 
#source the script with the function
source("src/6c_plot_dummy_yields.R")

# Get yield data, both LINTUL and STICS
LINTUL_dummy_temp_yields <- read.csv("data/output/LINTUL_dummy_temp_yields.csv")
LINTUL_dummy_precip_yields <- read.csv("data/output/LINTUL_dummy_precip_yields.csv")
STICS_dummy_yields <- read.csv("data/output/STICS_all_dummy_yields.csv", sep = ";")

# Plot the LINTUL dummy data and save the plot
plot_LINTULtemp_dummyyields(LINTUL_dummy_temp_yields)
plot_LINTULprecip_dummyyields(LINTUL_dummy_precip_yields)

# Plot the STICS dummy data and save the plot
plot_STICStemp_dummyyields(STICS_dummy_yields)
plot_STICSprecip_dummyyields(STICS_dummy_yields)
