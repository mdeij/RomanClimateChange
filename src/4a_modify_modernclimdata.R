
winterwheat_may <- function(files) {
  library(dplyr)
  # Initialize an empty list to store summarized data
  summaries <- list()
  
  # Loop through each file
  for (file in files) {
    # Read the CSV file
    data <- read.csv(file, sep = ";")
    
    # Extract location number from file name
    loc_number <- gsub("[^0-9]", "", basename(file))
    
    # Filter data for May for temperature
    temp_data <- data %>%
      mutate(Date = as.Date(Date)) %>%
      filter(format(Date, "%m") %in% c("05")) %>%
      group_by(Year) %>%
      summarize(AvgTemperature = mean(AvtTemperature, na.rm = TRUE))
    
    # Filter data for May  for precipitation
    precip_data <- data %>%
      mutate(Date = as.Date(Date)) %>%
      filter(format(Date, "%m") %in% c("05")) %>%
      group_by(Year) %>%
      summarize(Precipitation = sum(Precipitation, na.rm = TRUE))
    
    # Combine temperature and precipitation data
    combined_data <- cbind(temp_data, precip_data)
    
    # Rename columns with location-specific prefixes
    colnames(combined_data)[-1] <- paste0(colnames(combined_data)[-1], "_LOC", loc_number)
    
    # Store the summarized data for this location
    summaries[[file]] <- combined_data
  }
  
  # Merge all summaries by year
  final_summary <- Reduce(function(x, y) merge(x, y, by = "Year", all = TRUE), summaries)
  
  # Remove year columns
  final_summary <- final_summary[, !grepl("Year_LOC", colnames(final_summary))]
  
  return(final_summary)
}



durum_may <- function(files) {
  library(dplyr)
  # Initialize an empty list to store summarized data
  summaries <- list()
  
  # Loop through each file
  for (file in files) {
    # Read the CSV file
    data <- read.csv(file, sep = ";")
    
    # Extract location number from file name
    loc_number <- gsub("[^0-9]", "", basename(file))
    
    # Filter data for May for temperature
    temp_data <- data %>%
      mutate(Date = as.Date(Date)) %>%
      filter(format(Date, "%m") %in% c("05")) %>%
      group_by(Year) %>%
      summarize(AvgTemperature = mean(AvtTemperature, na.rm = TRUE))
    
    # Filter data for May for precipitation
    precip_data <- data %>%
      mutate(Date = as.Date(Date)) %>%
      filter(format(Date, "%m") %in% c("05")) %>%
      group_by(Year) %>%
      summarize(Precipitation = sum(Precipitation, na.rm = TRUE))
    
    # Combine temperature and precipitation data
    combined_data <- cbind(temp_data, precip_data)
    
    # Rename columns with location-specific prefixes
    colnames(combined_data)[-1] <- paste0(colnames(combined_data)[-1], "_LOC", loc_number)
    
    # Store the summarized data for this location
    summaries[[file]] <- combined_data
  }
  
  # Merge all summaries by year
  final_summary <- Reduce(function(x, y) merge(x, y, by = "Year", all = TRUE), summaries)
  
  # Remove year columns
  final_summary <- final_summary[, !grepl("Year_LOC", colnames(final_summary))]
  
  return(final_summary)
}



winterwheat_may_colder <- function(files) {
  library(dplyr)
  # Initialize an empty list to store summarized data
  summaries <- list()
  
  # Loop through each file
  for (file in files) {
    # Read the CSV file
    data <- read.csv(file, sep = ";")
    
    # Extract location number from file name
    loc_number <- gsub("[^0-9]", "", basename(file))
    
    # Filter data for May for temperature
    temp_data <- data %>%
      mutate(date = as.Date(date)) %>%
      filter(format(date, "%m") %in% c("05")) %>%
      group_by(Year) %>%
      summarize(avg_temp = mean(avg_temp, na.rm = TRUE))
    
    # Filter data for May for precipitation
    precip_data <- data %>%
      mutate(date = as.Date(date)) %>%
      filter(format(date, "%m") %in% c("05")) %>%
      group_by(Year) %>%
      summarize(Precipitation = sum(Precipitation, na.rm = TRUE))
    
    # Combine temperature and precipitation data
    combined_data <- cbind(temp_data, precip_data)
    
    # Rename columns with location-specific prefixes
    colnames(combined_data)[-1] <- paste0(colnames(combined_data)[-1], "_LOC", loc_number)
    
    # Store the summarized data for this location
    summaries[[file]] <- combined_data
  }
  
  # Merge all summaries by year
  final_summary <- Reduce(function(x, y) merge(x, y, by = "Year", all = TRUE), summaries)
  
  # Remove year columns
  final_summary <- final_summary[, !grepl("Year_LOC", colnames(final_summary))]
  
  return(final_summary)
}


durum_may_colder <- function(files) {
  library(dplyr)
  # Initialize an empty list to store summarized data
  summaries <- list()
  
  # Loop through each file
  for (file in files) {
    # Read the CSV file
    data <- read.csv(file, sep = ";")
    
    # Extract location number from file name
    loc_number <- gsub("[^0-9]", "", basename(file))
    
    # Filter data for May for temperature
    temp_data <- data %>%
      mutate(date = as.Date(date)) %>%
      filter(format(date, "%m") %in% c("05")) %>%
      group_by(Year) %>%
      summarize(avg_temp = mean(avg_temp, na.rm = TRUE))
    
    # Filter data for May for precipitation
    precip_data <- data %>%
      mutate(date = as.Date(date)) %>%
      filter(format(date, "%m") %in% c("05")) %>%
      group_by(Year) %>%
      summarize(Precipitation = sum(Precipitation, na.rm = TRUE))
    
    # Combine temperature and precipitation data
    combined_data <- cbind(temp_data, precip_data)
    
    # Rename columns with location-specific prefixes
    colnames(combined_data)[-1] <- paste0(colnames(combined_data)[-1], "_LOC", loc_number)
    
    # Store the summarized data for this location
    summaries[[file]] <- combined_data
  }
  
  # Merge all summaries by year
  final_summary <- Reduce(function(x, y) merge(x, y, by = "Year", all = TRUE), summaries)
  
  # Remove year columns
  final_summary <- final_summary[, !grepl("Year_LOC", colnames(final_summary))]
  
  return(final_summary)
}




springgrapes_am <- function(files) {
  library(dplyr)
  # Initialize an empty list to store summarized data
  summaries <- list()
  
  # Loop through each file
  for (file in files) {
    # Read the CSV file
    data <- read.csv(file, sep = ";")
    
    # Extract location number from file name
    loc_number <- gsub("[^0-9]", "", basename(file))
    
    # Filter data for April and May for temperature
    temp_data <- data %>%
      mutate(Date = as.Date(Date)) %>%
      filter(format(Date, "%m") %in% c("04", "05")) %>%
      group_by(Year) %>%
      summarize(AvgTemperature = mean(AvtTemperature, na.rm = TRUE))
    
    # Filter data for April and May for precipitation
    precip_data <- data %>%
      mutate(Date = as.Date(Date)) %>%
      filter(format(Date, "%m") %in% c("04", "05")) %>%
      group_by(Year) %>%
      summarize(Precipitation = sum(Precipitation, na.rm = TRUE))
    
    # Combine temperature and precipitation data
    combined_data <- cbind(temp_data, precip_data)
    
    # Rename columns with location-specific prefixes
    colnames(combined_data)[-1] <- paste0(colnames(combined_data)[-1], "_LOC", loc_number)
    
    # Store the summarized data for this location
    summaries[[file]] <- combined_data
  }
  
  # Merge all summaries by year
  final_summary <- Reduce(function(x, y) merge(x, y, by = "Year", all = TRUE), summaries)
  
  # Remove year columns
  final_summary <- final_summary[, !grepl("Year_LOC", colnames(final_summary))]
  
  return(final_summary)
}



summergrapes_as <- function(files) {
  library(dplyr)
  # Initialize an empty list to store summarized data
  summaries <- list()
  
  # Loop through each file
  for (file in files) {
    # Read the CSV file
    data <- read.csv(file, sep = ";")
    
    # Extract location number from file name
    loc_number <- gsub("[^0-9]", "", basename(file))
    
    # Filter data for August and September for temperature
    temp_data <- data %>%
      mutate(Date = as.Date(Date)) %>%
      filter(format(Date, "%m") %in% c("08", "09")) %>%
      group_by(Year) %>%
      summarize(AvgTemperature = mean(AvtTemperature, na.rm = TRUE))
    
    # Filter data for August and September for precipitation
    precip_data <- data %>%
      mutate(Date = as.Date(Date)) %>%
      filter(format(Date, "%m") %in% c("08", "09")) %>%
      group_by(Year) %>%
      summarize(Precipitation = sum(Precipitation, na.rm = TRUE))
    
    # Combine temperature and precipitation data
    combined_data <- cbind(temp_data, precip_data)
    
    # Rename columns with location-specific prefixes
    colnames(combined_data)[-1] <- paste0(colnames(combined_data)[-1], "_LOC", loc_number)
    
    # Store the summarized data for this location
    summaries[[file]] <- combined_data
  }
  
  # Merge all summaries by year
  final_summary <- Reduce(function(x, y) merge(x, y, by = "Year", all = TRUE), summaries)
  
  # Remove year columns
  final_summary <- final_summary[, !grepl("Year_LOC", colnames(final_summary))]
  
  return(final_summary)
}




summergrapes_as_colder <- function(files) {
  library(dplyr)
  # Initialize an empty list to store summarized data
  summaries <- list()
  
  # Loop through each file
  for (file in files) {
    # Read the CSV file
    data <- read.csv(file, sep = ";")
    
    # Extract location number from file name
    loc_number <- gsub("[^0-9]", "", basename(file))
    
    # Filter data for August and September for temperature
    temp_data <- data %>%
      mutate(Date = as.Date(Date)) %>%
      filter(format(Date, "%m") %in% c("08", "09")) %>%
      group_by(Year) %>%
      summarize(AvgTemperature = mean(AvtTemperature, na.rm = TRUE))
    
    # Filter data for August and September for precipitation
    precip_data <- data %>%
      mutate(Date = as.Date(Date)) %>%
      filter(format(Date, "%m") %in% c("08", "09")) %>%
      group_by(Year) %>%
      summarize(Precipitation = sum(Precipitation, na.rm = TRUE))
    
    # Combine temperature and precipitation data
    combined_data <- cbind(temp_data, precip_data)
    
    # Rename columns with location-specific prefixes
    colnames(combined_data)[-1] <- paste0(colnames(combined_data)[-1], "_LOC", loc_number)
    
    # Store the summarized data for this location
    summaries[[file]] <- combined_data
  }
  
  # Merge all summaries by year
  final_summary <- Reduce(function(x, y) merge(x, y, by = "Year", all = TRUE), summaries)
  
  # Remove year columns
  final_summary <- final_summary[, !grepl("Year_LOC", colnames(final_summary))]
  
  return(final_summary)
}