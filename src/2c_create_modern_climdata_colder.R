modern_colderclim_wheat <- function(file_name, index) {
  # Read the CABO file
  data <- read.table(file_name, header = FALSE)
  
  # Extract columns by index
  new_data <- data[, c(2, 3, 5, 6, 9)]
  
  # Calculate date from year (V2) and day (V3)
  new_data$date <- as.Date(paste0(new_data$V2, "-", new_data$V3), format = "%Y-%j")
  
  # Check for missing or non-numeric values
  if(any(is.na(new_data$V5)) || any(is.na(new_data$V6))) {
    warning("Missing or non-numeric values found in temperature columns.")
  }
  
  # Calculate average daily temperature
  new_data$avg_temp <- (new_data$V5 + new_data$V6) / 2
  
  # Rename columns
  colnames(new_data) <- c("Year", "Day", "min_temp", "max_temp", "Precipitation", "date", "avg_temp")
  
  # Remove unnecessary columns (min_temp and max_temp)
  new_data <- new_data[, -c(3, 4)]
  
  # Reorder columns
  new_data <- new_data[, c("Year", "Day", "date", "avg_temp", "Precipitation")]
  
  # Write the new file
  new_file_name <- paste0("data/temp/modern climate data/colder_modern_tempprecip_wheatdata_", index, ".csv")
  write.table(new_data, new_file_name, row.names = FALSE, sep = ";", quote = FALSE)
  
  # Return the new file name
  return(new_file_name)
}




modern_colderclim_grapes <- function(file_name, index) {
  # Read the CSV file
  data <- read.table(file_name, sep = ";", header = FALSE)
  
  # Extract columns by index
  new_data <- data[, c(2, 3, 8, 9, 12)]
  
  # Calculate date from year (V2) and day (V3)
  new_data$date <- as.Date(paste0(new_data$V2, "-", new_data$V3), format = "%Y-%j")
  
  # Check for missing or non-numeric values
  if(any(is.na(new_data$V8)) || any(is.na(new_data$V9))) {
    warning("Missing or non-numeric values found in temperature columns.")
  }
  
  # Calculate average daily temperature
  new_data$avg_temp <- (new_data$V8 + new_data$V9) / 2
  
  # Rename columns
  colnames(new_data) <- c("Year", "Day", "min_temp", "max_temp", "Precipitation", "date", "avg_temp")
  
  # Remove unnecessary columns (min_temp and max_temp)
  new_data <- new_data[, -c(3, 4)]
  
  # Reorder columns
  new_data <- new_data[, c("Year", "Day", "date", "avg_temp", "Precipitation")]
  
  # Write the new file
  new_file_name <- paste0("data/temp/modern climate data/colder_modern_tempprecip_grapesdata_", index, ".csv")
  write.table(new_data, new_file_name, row.names = FALSE, sep = ";", quote = FALSE)
  
  # Return the new file name
  return(new_file_name)
}