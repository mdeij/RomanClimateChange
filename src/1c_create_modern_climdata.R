modern_wheat <- function(pr_ncfiles, avt_ncfiles, station_number, i) {
  library(ncdf4)
  library(dplyr)
  
  # Print the coordinates of the current location
  cat("This is the data for wheat station number:", station_number, "\n")
  
  #Get the exact lat and lon values from the avt temperature nc file
  lat_values <- ncvar_get(avt_ncfiles[[1]], "lat")  
  lon_values <- ncvar_get(avt_ncfiles[[1]], "lon")  
  
  # Find the indices of the closest latitude and longitude values
  lat_idx <- which.min(abs(lat_values - target_lat))
  lon_idx <- which.min(abs(lon_values - target_lon))
  
  print("Precipitation:")
  # Pre-allocate precipitation to store the combined preciptitation data
  prdata <- numeric(0)
  # Loop through all nc files and get the precipitation from each nc file
  for (file in pr_ncfiles) {
    # Get the precipitation data from the current nc file
    pr_temp <- ncvar_get(file, "pr")[lon_idx, lat_idx,]
    pr_temp <- pr_temp * 86400 # Convert to mm per day (1 kg/m²/s is approximately equivalent to 86,400 mm/day)
    # Append the precipitation data to prdata
    prdata <- c(prdata, pr_temp)
  }
  # Print the first few values of precipitation data for all combined files
  print(head(prdata))
  
  print("Average temperature:")
  # Pre-allocate avtdata to store the combined average temperature data
  avtdata <- numeric(0)
  # Loop through all nc files and get the average temperature from each nc file
  for (file in avt_ncfiles) {
    # Get the average temperature data from the current nc file
    avt_temp <- ncvar_get(file, "tas")[lon_idx, lat_idx,]
    avt_temp <- avt_temp - 273.15 # Convert from Kelvin to Celsius
    # Append the average temperature data to avgdata
    avtdata <- c(avtdata, avt_temp)
  }
  # Print the first few values of average temperature data for all combined files
  print(head(avtdata))
  
  # Initialize an empty list to store timedata
  all_timedata <- NULL
  # Extract year and day information from nc files
  for (file in avt_ncfiles) {
    timedata <- ncvar_get(file, "time")
    all_timedata <- c(all_timedata, timedata)
  }
  # Calculate dates for all combined timedata, relative to 1 January 1850
  dates <- as.Date("1850-01-01") + all_timedata
  # Find the number of valid temperature data points for the combined data
  valid_length <- min(length(dates), length(avtdata))
  
  # Create the CABO data frame, combining all data
  cabo_data <- data.frame(
    Year = as.integer(format(dates[1:valid_length], "%Y")),
    Day = as.integer(format(dates[1:valid_length], "%j")),
    Date = as.Date(dates[1:valid_length]),
    AvtTemperature = avtdata[1:valid_length],
    Precipitation = prdata[1:valid_length]
  )
  
  # Generate the file name with the station number included
  file_name <- sprintf("data/temp/modern climate data/modern_tempprecip_wheatdata_%d.csv", i)
  
  write.table(cabo_data, file_name, row.names = FALSE, sep = ";", quote = FALSE)
}





modern_grapes <- function(pr_ncfiles, avt_ncfiles, station_number, i) {
  library(ncdf4)
  library(dplyr)
  
  # Print the coordinates of the current location
  cat("This is the data for grapes station number:", station_number, "\n")
  
  #Get the exact lat and lon values from the avt temperature nc file
  lat_values <- ncvar_get(avt_ncfiles[[1]], "lat")  
  lon_values <- ncvar_get(avt_ncfiles[[1]], "lon")  
  
  # Find the indices of the closest latitude and longitude values
  lat_idx <- which.min(abs(lat_values - target_lat))
  lon_idx <- which.min(abs(lon_values - target_lon))
  
  print("Precipitation:")
  # Pre-allocate precipitation to store the combined preciptitation data
  prdata <- numeric(0)
  # Loop through all nc files and get the precipitation from each nc file
  for (file in pr_ncfiles) {
    # Get the precipitation data from the current nc file
    pr_temp <- ncvar_get(file, "pr")[lon_idx, lat_idx,]
    pr_temp <- pr_temp * 86400 # Convert to mm per day (1 kg/m²/s is approximately equivalent to 86,400 mm/day)
    # Append the precipitation data to prdata
    prdata <- c(prdata, pr_temp)
  }
  # Print the first few values of precipitation data for all combined files
  print(head(prdata))
  
  print("Average temperature:")
  # Pre-allocate avtdata to store the combined average temperature data
  avtdata <- numeric(0)
  # Loop through all nc files and get the average temperature from each nc file
  for (file in avt_ncfiles) {
    # Get the average temperature data from the current nc file
    avt_temp <- ncvar_get(file, "tas")[lon_idx, lat_idx,]
    avt_temp <- avt_temp - 273.15 # Convert from Kelvin to Celsius
    # Append the average temperature data to avgdata
    avtdata <- c(avtdata, avt_temp)
  }
  # Print the first few values of average temperature data for all combined files
  print(head(avtdata))
  
  # Initialize an empty list to store timedata
  all_timedata <- NULL
  # Extract year and day information from nc files
  for (file in avt_ncfiles) {
    timedata <- ncvar_get(file, "time")
    all_timedata <- c(all_timedata, timedata)
  }
  # Calculate dates for all combined timedata, relative to 1 January 1850
  dates <- as.Date("1850-01-01") + all_timedata
  # Find the number of valid temperature data points for the combined data
  valid_length <- min(length(dates), length(avtdata))
  
  # Create the CABO data frame, combining all data
  cabo_data <- data.frame(
    Year = as.integer(format(dates[1:valid_length], "%Y")),
    Day = as.integer(format(dates[1:valid_length], "%j")),
    Date = as.Date(dates[1:valid_length]),
    AvtTemperature = avtdata[1:valid_length],
    Precipitation = prdata[1:valid_length]
  )
  
  # Generate the file name with the station number included
  file_name <- sprintf("data/temp/modern climate data/modern_tempprecip_grapedata_%d.csv", i)
  
  write.table(cabo_data, file_name, row.names = FALSE, sep = ";", quote = FALSE)
}




