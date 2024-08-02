RCO_envelopes_winterwheat <- function(filtered_data_list_winterwheat) {
  
  # Initialize an empty vector to store mean values for each location
  mean_values <- numeric(length(filtered_data_list_winterwheat))
  
  # Loop over each location
  for (i in 1:3) {
    # Scale the temperature and precipitation columns
    wheat_scaled <- filtered_data_list_winterwheat[[i]] %>%
      mutate(AvgTemperature_scaled = scale(get(paste0("AvgTemperature_LOC", i))),
             Precipitation_scaled = scale(get(paste0("Precipitation_LOC", i))))
    
    # Define thresholds
    temp_threshold <- c(0, 0.5)
    precip_threshold <- c(0.5, 1)
    
    # Filter rows based on the defined thresholds
    filtered_rows <- wheat_scaled %>%
      filter(AvgTemperature_scaled >= temp_threshold[1] & AvgTemperature_scaled <= temp_threshold[2] &
               Precipitation_scaled >= precip_threshold[1] & Precipitation_scaled <= precip_threshold[2])
    
    # Extract values from Location_WSOTHA for the filtered rows
    selected_values <- filtered_rows[[paste0("Location", i, "_WSOTHA")]]
    
    # Calculate the mean and store it
    mean_values[i] <- mean(selected_values, na.rm = TRUE)
  }
  
  # Display the mean values for each location
  for (i in 1:3) {
    cat("Mean value for Location", i, ":", mean_values[i], "\n")
  }
  
}

RCO_envelopes_durum <- function(filtered_data_list_durum) {
  
  # Initialize an empty vector to store mean values for each location
  mean_values <- numeric(length(filtered_data_list_durum))
  
  # Loop over each location
  for (i in 4:5) {
    # Scale the temperature and precipitation columns
    wheat_scaled <- filtered_data_list_durum[[i]] %>%
      mutate(AvgTemperature_scaled = scale(get(paste0("AvgTemperature_LOC", i))),
             Precipitation_scaled = scale(get(paste0("Precipitation_LOC", i))))
    
    # Define thresholds
    temp_threshold <- c(0, 0.5)
    precip_threshold <- c(0.5, 1)
    
    # Filter rows based on the defined thresholds
    filtered_rows <- wheat_scaled %>%
      filter(AvgTemperature_scaled >= temp_threshold[1] & AvgTemperature_scaled <= temp_threshold[2] &
               Precipitation_scaled >= precip_threshold[1] & Precipitation_scaled <= precip_threshold[2])
    
    # Extract values from Location_WSOTHA for the filtered rows
    selected_values <- filtered_rows[[paste0("Location", i, "_WSOTHA")]]
    
    # Calculate the mean and store it
    mean_values[i] <- mean(selected_values, na.rm = TRUE)
  }
  
  # Display the mean values for each location
  for (i in 4:5) {
    cat("Mean value for Location", i, ":", mean_values[i], "\n")
  }
  
}


RCO_envelopes_summergrapes <- function(filtered_data_list_gas) {
  # Scale the temperature and precipitation columns for Location 1
  summergrape_scaled <- filtered_data_list_gas[[1]] %>%
    mutate(AvgTemperature_scaled = scale(AvgTemperature_LOC1),
           Precipitation_scaled = scale(Precipitation_LOC1))
  
  # Define thresholds
  temp_threshold <- c(0.5, 1)
  precip_threshold <- c(0.5, 1)
  
  # Filter rows based on the defined thresholds
  filtered_rows <- summergrape_scaled %>%
    filter(AvgTemperature_scaled >= temp_threshold[1] & AvgTemperature_scaled <= temp_threshold[2] &
             Precipitation_scaled >= precip_threshold[1] & Precipitation_scaled <= precip_threshold[2])
  
  # Extract values from Location1 for the filtered rows
  selected_values <- filtered_rows$Location1
  
  # Calculate the mean and store it
  mean_value <- mean(selected_values, na.rm = TRUE)
  
  # Display the mean value for Location 1
  cat("Mean value for Location 1:", mean_value, "\n")
}



RCO_envelopes_springgrapes <- function(filtered_data_list_gam) {
  # Load necessary libraries
  library(dplyr)
  
  # Initialize an empty vector to store mean values for each location (4 locations: 2 to 5)
  mean_values <- numeric(4)
  
  # Loop over each location from 2 to 5
  for (i in 2:5) {
    # Scale the temperature and precipitation columns
    springgrape_scaled <- filtered_data_list_gam[[i]] %>%
      mutate(AvgTemperature_scaled = scale(get(paste0("AvgTemperature_LOC", i))),
             Precipitation_scaled = scale(get(paste0("Precipitation_LOC", i))))
    
    # Define thresholds
    temp_threshold <- c(0, 0.5)
    precip_threshold <- c(0.5, 1)
    
    # Filter rows based on the defined thresholds
    filtered_rows <- springgrape_scaled %>%
      filter(AvgTemperature_scaled >= temp_threshold[1] & AvgTemperature_scaled <= temp_threshold[2] &
               Precipitation_scaled >= precip_threshold[1] & Precipitation_scaled <= precip_threshold[2])
    
    # Extract values from Location_WSOTHA for the filtered rows
    selected_values <- filtered_rows[[paste0("Location", i)]]
    
    # Calculate the mean and store it in the appropriate position
    mean_values[i - 1] <- mean(selected_values, na.rm = TRUE)
  }
  
  # Display the mean values for each location
  for (i in 2:5) {
    cat("Mean value for Location", i, ":", mean_values[i - 1], "\n")
  }
}

##############################################################################
TP_160_180_winterwheat <-  function(filtered_data_list_winterwheat) {
  # Initialize an empty vector to store mean values for each location
  mean_values <- numeric(length(filtered_data_list_winterwheat))
  
  # Loop over each location
  for (i in 1:3) {
    # Scale the temperature and precipitation columns
    wheat_scaled <- filtered_data_list_winterwheat[[i]] %>%
      mutate(AvgTemperature_scaled = scale(get(paste0("AvgTemperature_LOC", i))),
             Precipitation_scaled = scale(get(paste0("Precipitation_LOC", i))))
    
    # Define thresholds
    temp_threshold <- c(-0.5, 0)
    precip_threshold <- c(0.5, 1)
    
    # Filter rows based on the defined thresholds
    filtered_rows <- wheat_scaled %>%
      filter(AvgTemperature_scaled >= temp_threshold[1] & AvgTemperature_scaled <= temp_threshold[2] &
               Precipitation_scaled >= precip_threshold[1] & Precipitation_scaled <= precip_threshold[2])
    
    # Extract values from Location_WSOTHA for the filtered rows
    selected_values <- filtered_rows[[paste0("Location", i, "_WSOTHA")]]
    
    # Calculate the mean and store it
    mean_values[i] <- mean(selected_values, na.rm = TRUE)
  }
  
  # Display the mean values for each location
  for (i in 1:3) {
    cat("Mean value for Location", i, ":", mean_values[i], "\n")
  }
}

TP_160_180_durum <- function(filtered_data_list_durum) {
  # Initialize an empty vector to store mean values for each location
  mean_values <- numeric(length(filtered_data_list_durum))
  
  # Loop over each location
  for (i in 4:5) {
    # Scale the temperature and precipitation columns
    wheat_scaled <- filtered_data_list_durum[[i]] %>%
      mutate(AvgTemperature_scaled = scale(get(paste0("AvgTemperature_LOC", i))),
             Precipitation_scaled = scale(get(paste0("Precipitation_LOC", i))))
    
    # Define thresholds
    temp_threshold <- c(-0.5, 0)
    precip_threshold <- c(0.5, 1)
    
    # Filter rows based on the defined thresholds
    filtered_rows <- wheat_scaled %>%
      filter(AvgTemperature_scaled >= temp_threshold[1] & AvgTemperature_scaled <= temp_threshold[2] &
               Precipitation_scaled >= precip_threshold[1] & Precipitation_scaled <= precip_threshold[2])
    
    # Extract values from Location_WSOTHA for the filtered rows
    selected_values <- filtered_rows[[paste0("Location", i, "_WSOTHA")]]
    
    # Calculate the mean and store it
    mean_values[i] <- mean(selected_values, na.rm = TRUE)
  }
  
  # Display the mean values for each location
  for (i in 4:5) {
    cat("Mean value for Location", i, ":", mean_values[i], "\n")
  }
}

TP_160_180_summergrapes <- function(filtered_data_list_gas) {
  # Scale the temperature and precipitation columns for Location 1
  summergrape_scaled <- filtered_data_list_gas[[1]] %>%
    mutate(AvgTemperature_scaled = scale(AvgTemperature_LOC1),
           Precipitation_scaled = scale(Precipitation_LOC1))
  
  # Define thresholds
  temp_threshold <- c(-0.5, 0)
  precip_threshold <- c(0.5, 1)
  
  # Filter rows based on the defined thresholds
  filtered_rows <- summergrape_scaled %>%
    filter(AvgTemperature_scaled >= temp_threshold[1] & AvgTemperature_scaled <= temp_threshold[2] &
             Precipitation_scaled >= precip_threshold[1] & Precipitation_scaled <= precip_threshold[2])
  
  # Extract values from Location1 for the filtered rows
  selected_values <- filtered_rows$Location1
  
  # Calculate the mean and store it
  mean_value <- mean(selected_values, na.rm = TRUE)
  
  # Display the mean value for Location 1
  cat("Mean value for Location 1:", mean_value, "\n")
}


TP_245_275_winterwheat <- function(filtered_data_list_winterwheat) {
  # Initialize an empty vector to store mean values for each location
  mean_values <- numeric(length(filtered_data_list_winterwheat))
  
  # Loop over each location
  for (i in 1:3) {
    # Scale the temperature and precipitation columns
    wheat_scaled <- filtered_data_list_winterwheat[[i]] %>%
      mutate(AvgTemperature_scaled = scale(get(paste0("AvgTemperature_LOC", i))),
             Precipitation_scaled = scale(get(paste0("Precipitation_LOC", i))))
    
    # Define thresholds
    temp_threshold <- c(-0.5, 0)
    precip_threshold <- c(-1, -0.5)
    
    # Filter rows based on the defined thresholds
    filtered_rows <- wheat_scaled %>%
      filter(AvgTemperature_scaled >= temp_threshold[1] & AvgTemperature_scaled <= temp_threshold[2] &
               Precipitation_scaled >= precip_threshold[1] & Precipitation_scaled <= precip_threshold[2])
    
    # Extract values from Location_WSOTHA for the filtered rows
    selected_values <- filtered_rows[[paste0("Location", i, "_WSOTHA")]]
    
    # Calculate the mean and store it
    mean_values[i] <- mean(selected_values, na.rm = TRUE)
  }
  
  # Display the mean values for each location
  for (i in 1:3) {
    cat("Mean value for Location", i, ":", mean_values[i], "\n")
  }
}

TP_245_275_durum <- function(filtered_data_list_durum) {
  # Initialize an empty vector to store mean values for each location
  mean_values <- numeric(length(filtered_data_list_durum))
  
  # Loop over each location
  for (i in 4:5) {
    # Scale the temperature and precipitation columns
    wheat_scaled <- filtered_data_list_durum[[i]] %>%
      mutate(AvgTemperature_scaled = scale(get(paste0("AvgTemperature_LOC", i))),
             Precipitation_scaled = scale(get(paste0("Precipitation_LOC", i))))
    
    # Define thresholds
    temp_threshold <- c(-0.5, 0)
    precip_threshold <- c(-1, -0.5)
    
    # Filter rows based on the defined thresholds
    filtered_rows <- wheat_scaled %>%
      filter(AvgTemperature_scaled >= temp_threshold[1] & AvgTemperature_scaled <= temp_threshold[2] &
               Precipitation_scaled >= precip_threshold[1] & Precipitation_scaled <= precip_threshold[2])
    
    # Extract values from Location_WSOTHA for the filtered rows
    selected_values <- filtered_rows[[paste0("Location", i, "_WSOTHA")]]
    
    # Calculate the mean and store it
    mean_values[i] <- mean(selected_values, na.rm = TRUE)
  }
  
  # Display the mean values for each location
  for (i in 4:5) {
    cat("Mean value for Location", i, ":", mean_values[i], "\n")
  }
}

TP_245_275_summergrapes <- function(filtered_data_list_gas) {
  # Scale the temperature and precipitation columns for Location 1
  summergrape_scaled <- filtered_data_list_gas[[1]] %>%
    mutate(AvgTemperature_scaled = scale(AvgTemperature_LOC1),
           Precipitation_scaled = scale(Precipitation_LOC1))
  
  # Define thresholds
  temp_threshold <- c(-1, -0.5)
  precip_threshold <- c(-1, -0.5)
  
  # Filter rows based on the defined thresholds
  filtered_rows <- summergrape_scaled %>%
    filter(AvgTemperature_scaled >= temp_threshold[1] & AvgTemperature_scaled <= temp_threshold[2] &
             Precipitation_scaled >= precip_threshold[1] & Precipitation_scaled <= precip_threshold[2])
  
  # Extract values from Location1 for the filtered rows
  selected_values <- filtered_rows$Location1
  
  # Calculate the mean and store it
  mean_value <- mean(selected_values, na.rm = TRUE)
  
  # Display the mean value for Location 1
  cat("Mean value for Location 1:", mean_value, "\n")
}
###############################################################################
LALIA_envelopes_winterwheat <- function(filtered_data_list_winterwheat) {
  # Initialize an empty vector to store mean values for each location
  mean_values <- numeric(length(filtered_data_list_winterwheat))
  
  # Loop over each location
  for (i in 1:3) {
    # Scale the temperature and precipitation columns
    wheat_scaled <- filtered_data_list_winterwheat[[i]] %>%
      mutate(AvgTemperature_scaled = scale(get(paste0("AvgTemperature_LOC", i))),
             Precipitation_scaled = scale(get(paste0("Precipitation_LOC", i))))
    
    # Define thresholds
    temp_threshold <- c(-1, -0.5)
    precip_threshold <- c(-0.5, 0)
    
    # Filter rows based on the defined thresholds
    filtered_rows <- wheat_scaled %>%
      filter(AvgTemperature_scaled >= temp_threshold[1] & AvgTemperature_scaled <= temp_threshold[2] &
               Precipitation_scaled >= precip_threshold[1] & Precipitation_scaled <= precip_threshold[2])
    
    # Extract values from Location_WSOTHA for the filtered rows
    selected_values <- filtered_rows[[paste0("Location", i, "_WSOTHA")]]
    
    # Calculate the mean and store it
    mean_values[i] <- mean(selected_values, na.rm = TRUE)
  }
  
  # Display the mean values for each location
  for (i in 1:3) {
    cat("Mean value for Location", i, ":", mean_values[i], "\n")
  }
}

LALIA_envelopes_durum <- function(filtered_data_list_durum) {
  # Initialize an empty vector to store mean values for each location
  mean_values <- numeric(length(filtered_data_list_durum))
  
  # Loop over each location
  for (i in 4:5) {
    # Scale the temperature and precipitation columns
    wheat_scaled <- filtered_data_list_durum[[i]] %>%
      mutate(AvgTemperature_scaled = scale(get(paste0("AvgTemperature_LOC", i))),
             Precipitation_scaled = scale(get(paste0("Precipitation_LOC", i))))
    
    # Define thresholds
    temp_threshold <- c(-1, -0.5)
    precip_threshold <- c(-0.5, 0)
    
    # Filter rows based on the defined thresholds
    filtered_rows <- wheat_scaled %>%
      filter(AvgTemperature_scaled >= temp_threshold[1] & AvgTemperature_scaled <= temp_threshold[2] &
               Precipitation_scaled >= precip_threshold[1] & Precipitation_scaled <= precip_threshold[2])
    
    # Extract values from Location_WSOTHA for the filtered rows
    selected_values <- filtered_rows[[paste0("Location", i, "_WSOTHA")]]
    
    # Calculate the mean and store it
    mean_values[i] <- mean(selected_values, na.rm = TRUE)
  }
  
  # Display the mean values for each location
  for (i in 4:5) {
    cat("Mean value for Location", i, ":", mean_values[i], "\n")
  }
}

LALIA_envelopes_summergrapes <- function(filtered_data_list_gas) {
  # Scale the temperature and precipitation columns for Location 1
  summergrape_scaled <- filtered_data_list_gas[[1]] %>%
    mutate(AvgTemperature_scaled = scale(AvgTemperature_LOC1),
           Precipitation_scaled = scale(Precipitation_LOC1))
  
  # Define thresholds
  temp_threshold <- c(-2, -1.5)
  precip_threshold <- c(-0.5, 0)
  
  # Filter rows based on the defined thresholds
  filtered_rows <- summergrape_scaled %>%
    filter(AvgTemperature_scaled >= temp_threshold[1] & AvgTemperature_scaled <= temp_threshold[2] &
             Precipitation_scaled >= precip_threshold[1] & Precipitation_scaled <= precip_threshold[2])
  
  # Extract values from Location1 for the filtered rows
  selected_values <- filtered_rows$Location1
  
  # Calculate the mean and store it
  mean_value <- mean(selected_values, na.rm = TRUE)
  
  # Display the mean value for Location 1
  cat("Mean value for Location 1:", mean_value, "\n")
}

