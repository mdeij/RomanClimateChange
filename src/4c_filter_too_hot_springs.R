filter_winterwheat <- function(winterwheat_yields, winterwheat_may) {
  
  # Merge the yields to the climate data
  merge_yields_winterwheat <- merge(winterwheat_may, winterwheat_yields, by = "Year")
  
  # Define a function to filter and subset the data for wheat
  filter_and_subset <- function(data, location, threshold) {
    filtered_data <- data[data[[paste0("AvgTemperature_LOC", location)]] <= threshold, ]
    desired_columns <- c("Year", paste0("AvgTemperature_LOC", location), 
                         paste0("Precipitation_LOC", location), paste0("Location", location, "_WSOTHA"))
    filtered_data <- subset(filtered_data, select = desired_columns)
    return(filtered_data)
  }
  
  # Run this function for spring wheat 
  # List of temperature thresholds per location, if the May temperature is above this threshold, it is warmer than the hottest measured recent year (2018)
  thresholds_wh_may <- c(23.05, 22.84, 23.82)
  
  # Initialize an empty list to store filtered data
  filtered_data_list_winterwheat <- list()
  
  # Loop over each location
  for (i in 1:3) {
    filtered_data_list_winterwheat[[i]] <- filter_and_subset(merge_yields_winterwheat, i, thresholds_wh_may[i])
  }
  
  return(filtered_data_list_winterwheat)
}


filter_durum <- function(durum_yields, durum_may) {
  
  # Merge the yields to the climate data
  merge_yields_durum <- merge(durum_may, durum_yields, by = "Year")
  
  # Define a function to filter and subset the data for wheat
  filter_and_subset <- function(data, location, threshold) {
    filtered_data <- data[data[[paste0("AvgTemperature_LOC", location)]] <= threshold, ]
    desired_columns <- c("Year", paste0("AvgTemperature_LOC", location), 
                         paste0("Precipitation_LOC", location), paste0("Location", location, "_WSOTHA"))
    filtered_data <- subset(filtered_data, select = desired_columns)
    return(filtered_data)
  }
  
  # Run this function for spring wheat 
  # List of temperature thresholds per location, if the May temperature is above this threshold, it is warmer than the hottest measured recent year (2018)
  thresholds_wh_may <- c(25.58, 25.73)
  
  # Initialize an empty list to store filtered data
  filtered_data_list_durum <- list()
  
  # Loop over each location
  for (i in 4:5) {
    filtered_data_list_durum[[i]] <- filter_and_subset(merge_yields_durum, i, thresholds_wh_may[i - 3])
  }
  
  return(filtered_data_list_durum)
}


filter_summergrapes <- function(summergrape_yields, summergrape_as) {
  
  # Merge the yields to the modern climate data
  merge_yields_grape_as <- merge(summergrape_as, summergrape_yields, by = "Year")
  
  # Make grape yields numeric 
  # List with column names to be converted to numeric
  locations <- c("Location1")
  
  # Loop over every column and convert to numeric for summer grapes
  for (loc in locations) {
    merge_yields_grape_as[[loc]] <- as.numeric(merge_yields_grape_as[[loc]])
  }
  
  # Define a function to filter and subset the data for grapes
  filter_and_subset <- function(data, location, threshold) {
    filtered_data <- data[data[[paste0("AvgTemperature_LOC", location)]] <= threshold, ]
    desired_columns <- c("Year", paste0("AvgTemperature_LOC", location), 
                         paste0("Precipitation_LOC", location), paste0("Location", location))
    filtered_data <- subset(filtered_data, select = desired_columns)
    return(filtered_data)
  }
  
  # Run this function for summer grapes
  # List of temperature thresholds per location, if the summer temperature is above this threshold, it is warmer than the hottest measured recent year
  thresholds_gr_as <- c(28.56)
  
  # Initialize an empty list to store filtered data
  filtered_data_list_gas <- list()
  
  # Loop over only the first column
  i <- 1
  filtered_data_list_gas[[i]] <- filter_and_subset(merge_yields_grape_as, i, thresholds_gr_as[i])
  
  
  return(filtered_data_list_gas)
}


filter_springgrapes <- function(springgrape_yields, springgrape_am) {
  # Merge the yields to the modern climate data
  merge_yields_grape_am <- merge(springgrape_am, springgrape_yields, by = "Year")
  
  # Make grape yields numeric 
  # List with column names to be converted to numeric
  locations <- c("Location2", "Location3", "Location4", "Location5")
  
  # Loop over every column and convert to numeric for spring grapes
  for (loc in locations) {
    merge_yields_grape_am[[loc]] <- as.numeric(merge_yields_grape_am[[loc]])
  }
  
  # Define a function to filter and subset the data for grapes
  filter_and_subset <- function(data, location, threshold) {
    filtered_data <- data[data[[paste0("AvgTemperature_LOC", location)]] <= threshold, ]
    desired_columns <- c("Year", paste0("AvgTemperature_LOC", location), 
                         paste0("Precipitation_LOC", location), paste0("Location", location))
    filtered_data <- subset(filtered_data, select = desired_columns)
    return(filtered_data)
  }
  
  # Run this function for spring grapes 
  # List of temperature thresholds per location, if the spring temperature is above this threshold, it is warmer than the hottest measured recent year
  thresholds_gr_am <- c(22.24, 17.96, 23.28, 23.03)
  
  # Initialize an empty list to store filtered data
  filtered_data_list_gam <- list()
  
  # Loop over each location
  for (i in 2:5) {
    filtered_data_list_gam[[i]] <- filter_and_subset(merge_yields_grape_am, i, thresholds_gr_am[i - 1])
  }
  
  return(filtered_data_list_gam)
}