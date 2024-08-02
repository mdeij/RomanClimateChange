get_dummy_values_temp <- function(data_list_t, years_temp, num_locations) {
  # Create an empty data frame to store the results
  result_df <- data.frame(Year = years_temp)
  
  # Loop through each location and process the loaded data frames
  for (location in 1:num_locations) {
    location_column <- character(length(years_temp))  # Initialize an empty character vector for each location
    
    # Loop through the years and process the data frame for the current location
    for (year in years_temp) {
      # Extract the value from the "WSOTHA" column in the 270th row, WSOTHA = yield in ton per hectare
      value <- as.character(data_list_t[[location]][[year]]$WSOTHA[270])  # Convert to character to handle potential non-numeric values
      
      # Store the value in the location_column vector
      location_column[year - min(years_temp) + 1] <- value
    }
    
    # Create a column in the result data frame for the current location
    col_name <- paste("Location", location, "_WSOTHA", sep = "")
    result_df[[col_name]] <- as.numeric(location_column)
  }
  
  # Write the results to a new CSV file
  write.csv(result_df, file = "data/output/LINTUL_dummy_temp_yields.csv", row.names = FALSE)
}


get_dummy_values_precip <- function(data_list_p, years_precip, num_locations) {
  # Create an empty data frame to store the results
  result_df <- data.frame(Year = years_precip)
  
  # Loop through each location and process the loaded data frames
  for (location in 1:num_locations) {
    location_column <- character(length(years_precip))  # Initialize an empty character vector for each location
    
    # Loop through the years and process the data frame for the current location
    for (year in years_precip) {
      # Extract the value from the "WSOTHA" column in the 270th row, WSOTHA = yield in ton per hectare
      value <- as.character(data_list_p[[location]][[year]]$WSOTHA[270])  # Convert to character to handle potential non-numeric values
      
      # Store the value in the location_column vector
      location_column[year - min(years_precip) + 1] <- value
    }
    
    # Create a column in the result data frame for the current location
    col_name <- paste("Location", location, "_WSOTHA", sep = "")
    result_df[[col_name]] <- as.numeric(location_column)
  }
  
  # Write the results to a new CSV file
  write.csv(result_df, file = "data/output/LINTUL_dummy_precip_yields.csv", row.names = FALSE)
}