# Function to retrieve WSOTHA values from CSV files for all locations and years
get_WSOTHA_values <- function(data_list, years, num_locations) {
  # Create an empty data frame to store the results
  result_df <- data.frame(Year = years)
  
  # Loop through each location and process the loaded data frames
  for (location in 1:num_locations) {
    location_column <- character(length(years))  # Initialize an empty character vector for each location
    
    # Loop through the years and process the data frame for the current location
    for (year in years) {
      filename <- paste0("LINTUL2/Results/Location_", location, "_results_for_", year, ".csv")
      
      # Check if file exists before attempting to read it
      if (file.exists(filename)) {
        location_data <- read.csv(filename, header = TRUE)
        value <- as.character(location_data$WSOTHA[270])  # Convert to character to handle potential non-numeric values
      } else {
        # If file doesn't exist, set value to NA
        value <- NA
      }
      
      # Store the value in the location_column vector
      location_column[year - min(years) + 1] <- value
    }
    
    # Create a column in the result data frame for the current location
    col_name <- paste("Location", location, "_WSOTHA", sep = "")
    result_df[[col_name]] <- as.numeric(location_column)
  }
  
  # Write the results to a new CSV file, both to temp and output
  write.csv(result_df, file = "data/temp/wheat yields/wheat_yields.csv", row.names = FALSE)
  write.csv(result_df, file = "data/output/wheat_yields.csv", row.names = FALSE)
}


# Function to retrieve WSOTHA values from CSV files for all locations and years
get_WSOTHA_values_colder <- function(data_list, years, num_locations) {
  # Create an empty data frame to store the results
  result_df <- data.frame(Year = years)
  
  # Loop through each location and process the loaded data frames
  for (location in 1:num_locations) {
    location_column <- character(length(years))  # Initialize an empty character vector for each location
    
    # Loop through the years and process the data frame for the current location
    for (year in years) {
      filename <- paste0("LINTUL2/Resultscolder/Location_", location, "_results_for_", year, ".csv")
      
      # Check if file exists before attempting to read it
      if (file.exists(filename)) {
        location_data <- read.csv(filename, header = TRUE)
        value <- as.character(location_data$WSOTHA[270])  # Convert to character to handle potential non-numeric values
      } else {
        # If file doesn't exist, set value to NA
        value <- NA
      }
      
      # Store the value in the location_column vector
      location_column[year - min(years) + 1] <- value
    }
    
    # Create a column in the result data frame for the current location
    col_name <- paste("Location", location, "_WSOTHA", sep = "")
    result_df[[col_name]] <- as.numeric(location_column)
  }
  
  # Write the results to a new CSV file, both to temp and output
  write.csv(result_df, file = "data/temp/wheat yields/wheat_yields_colder.csv", row.names = FALSE)
  write.csv(result_df, file = "data/output/wheat_yields_colder.csv", row.names = FALSE)
}
