split_wheat <- function(wheat_data, current_loc) {
  print(paste("Currently creating yearly files for wheat ", current_loc))
  # Accessing the second column by its position to find the 'year' column
  year_col <- wheat_data[, 2]  
  
  # Split the file into different years
  yearly_data <- split(wheat_data, year_col)
  
  # Assign the last three digits of the year as the file type and save it
  for (year in names(yearly_data)) {
    year_short <- substr(year, nchar(year) - 2, nchar(year))
    year_data <- yearly_data[[year]]
    write.table(year_data, file = paste0("data/temp/yearly files/ITA", current_loc, ".", year_short), sep = "\t", row.names = FALSE, col.names = FALSE)
  }
}

split_grapes <- function(grapes_data, current_loc) {
  print(paste("Currently creating yearly files for grapes ", current_loc))
  # Assign column names to grapes_data
  colnames(grapes_data) <- c("StationNumber", "Year", "Day", "Month", "DayOfMonth", "JulianDay", "Irradiation", "MinTemperature", "MaxTemperature", "VapourPressure", "MeanWindSpeed", "Precipitation")
  
  # Extract the year from the "Year" column and create a new column for it
  grapes_data$Year <- as.integer(grapes_data$Year)  # Ensuring the Year column is in numeric format
  
  # Split the CSV data into separate years based on the "Year" column
  yearly_data <- split(grapes_data, grapes_data$Year)
  
  # Loop through each year to create separate CSV files
  for (year in names(yearly_data)) {
    year_data <- yearly_data[[year]]
    write.table(year_data, file = paste0("data/temp/yearly files/grapes", current_loc, "_", year, ".csv"), sep = ";", row.names = FALSE, col.names = FALSE, quote = FALSE)
  }
}


split_colder_wheat <- function(wheat_data, current_loc) {
  print(paste("Currently creating yearly files for the colder data for wheat ", current_loc))
  # Accessing the second column by its position to find the 'year' column
  year_col <- wheat_data[, 2]  
  
  # Split the file into different years
  yearly_data <- split(wheat_data, year_col)
  
  # Assign the last three digits of the year as the file type and save it
  for (year in names(yearly_data)) {
    year_short <- substr(year, nchar(year) - 2, nchar(year))
    year_data <- yearly_data[[year]]
    write.table(year_data, file = paste0("data/temp/colder yearly files/ITA", current_loc, ".", year_short), sep = "\t", row.names = FALSE, col.names = FALSE)
    #write.table(year_data, file = paste0("LINTUL2/Weather/ITA", current_loc, ".", year_short), sep = "\t", row.names = FALSE, col.names = FALSE)
  }
}


split_colder_grapes <- function(grapes_data, current_loc) {
  print(paste("Currently creating yearly files for the colder data for grapes ", current_loc))
  # Assign column names to grapes_data
  colnames(grapes_data) <- c("StationNumber", "Year", "Day", "Month", "DayOfMonth", "JulianDay", "Irradiation", "MinTemperature", "MaxTemperature", "VapourPressure", "MeanWindSpeed", "Precipitation")
  
  # Extract the year from the "Year" column and create a new column for it
  grapes_data$Year <- as.integer(grapes_data$Year)  # Ensuring the Year column is in numeric format
  
  # Split the CSV data into separate years based on the "Year" column
  yearly_data <- split(grapes_data, grapes_data$Year)
  
  # Loop through each year to create separate CSV files
  for (year in names(yearly_data)) {
    year_data <- yearly_data[[year]]
    write.table(year_data, file = paste0("data/temp/colder yearly files/grapes", current_loc, "_", year, ".csv"), sep = ";", row.names = FALSE, col.names = FALSE, quote = FALSE)
  }
}
