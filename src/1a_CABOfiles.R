modify_wheat_data <- function(max_ncfiles, min_ncfiles, wind_ncfiles, pr_ncfiles, avt_ncfiles, solardata, station_number) {
  library(ncdf4)
  
  # Select the right column of solardata per location
  soldata <- solardata[, station_number + 1]
  
  # Explanatory print statement
  print("The CABO files are created for each location, it takes about 8 minutes to create 1 cabo file. The total runtime for the function modify_wheat_data is around 40-45 minutes")

  # Print the coordinates of the current location
  cat("This is the data for wheat station number:", station_number, "\n")
  
  # Get the exact lat and lon values from the max temperature nc file
  lat_values <- ncvar_get(max_ncfiles[[1]], "lat")  
  lon_values <- ncvar_get(max_ncfiles[[1]], "lon")  
  
  # Find the indices of the closest latitude and longitude values
  lat_idx <- which.min(abs(lat_values - target_lat))
  lon_idx <- which.min(abs(lon_values - target_lon))
  
  print("Max temperature:")
    # Pre-allocate maxdata to store the combined maximum temperature data
  maxdata <- numeric(0)
    # Loop through all nc files and get the maximum temperature from each nc file
  for (file in max_ncfiles) {
    # Get the maximum temperature data from the current nc file
    max_temp <- ncvar_get(file, "tasmax")[lon_idx, lat_idx,]
    max_temp <- max_temp - 273.15 # Convert from Kelvin to Celsius
    # Append the maximum temperature data to maxdata
    maxdata <- c(maxdata, max_temp)
  }
  # Print the first few values of maximum temperature data for all combined files
  print(head(maxdata))
  
  print("Min temperature:")
  # Pre-allocate mindata to store the combined minimum temperature data
  mindata <- numeric(0)
  # Loop through all nc files and get the minimum temperature from each nc file
  for (file in min_ncfiles) {
    # Get the minimum temperature data from the current nc file
    min_temp <- ncvar_get(file, "tasmin")[lon_idx, lat_idx,]
    min_temp <- min_temp - 273.15 # Convert from Kelvin to Celsius
    # Append the minimum temperature data to mindata
    mindata <- c(mindata, min_temp)
  }
  # Print the first few values of minimum temperature data for all combined files
  print(head(mindata))
  
  print("Wind speed:")
  # Pre-allocate winddata to store the combined wind data
  winddata <- numeric(0)
  # Loop through all nc files and get the wind speed from each nc file
  for (file in wind_ncfiles) {
    # Get the wind speed data from the current nc file
    wind_temp <- ncvar_get(file, "sfcWind")[lon_idx, lat_idx,]
    # Append the wind speed data to winddata
    winddata <- c(winddata, wind_temp)
  }
  # Print the first few values of wind speed data for all combined files
  print(head(winddata))
  
  print("Precipitation:")
  # Pre-allocate precipitation to store the combined precipitation data
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

  print("Vapor pressure:")
  # Pre-allocate vpdata to store the combined vapor pressure data
  vpdata <- numeric(0)
    # Loop through all nc files and get the vapor pressure data from each nc file
  for (file in avt_ncfiles) {
    # Get the air temperature data from the current nc file
    avtdata <- ncvar_get(file, "tas")[lon_idx, lat_idx,]
    avtdata <- avtdata - 273.15
    # Define constants
    A <- 0.61094
    B <- 17.625
    C <- 243.04
    # Calculate vapor pressure in kPa
    vp <- A * exp((B * avtdata) / (avtdata + C))
    # Append the vapor pressure data to vpdata
    vpdata <- c(vpdata, vp)
  }
  # Print the vapor pressure data for the current nc file
  print(head(vpdata))

  # Initialize an empty list to store timedata
  all_timedata <- NULL
    # Extract year and day information from nc files
  for (file in max_ncfiles) {
    timedata <- ncvar_get(file, "time")
    all_timedata <- c(all_timedata, timedata)
  }
    # Calculate dates for all combined timedata, relative to 1 January 1850
  dates <- as.Date("1850-01-01") + all_timedata
    # Find the number of valid temperature data points for the combined data
  valid_length <- min(length(dates), length(maxdata))
  
  # Create the CABO data frame, combining all data
  cabo_data <- data.frame(
    StationNumber = rep(station_number, valid_length),
    Year = as.integer(format(dates[1:valid_length], "%Y")),
    Day = as.integer(format(dates[1:valid_length], "%j")),
    Irradiation = soldata,
    MinTemperature = mindata[1:valid_length],
    MaxTemperature = maxdata[1:valid_length],
    VapourPressure = vpdata[1:valid_length],
    MeanWindSpeed = winddata[1:valid_length],
    Precipitation = prdata[1:valid_length]
  )
  
  # Generate the folder path
  subfolder <- "data/temp"  
  folder_path <- file.path(".", subfolder)  # "." refers to the current directory
  
  # Generate the file name with the station number included
  file_name <- paste("wheat_", station_number, ".cabo", sep = "")
  
  # Generate the full file path including the subfolder
  file_path <- file.path(folder_path, file_name)
  
  # Write the data frame to a CABO file
  write.table(cabo_data, file_path, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
  
}
