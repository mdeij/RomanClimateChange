z_values_winterwheat <- function(filtered_data_list_winterwheat) {
  # Define the z-value intervals
  z_value_intervals <- seq(-3, 3, 0.5)
  
  # To find the corresponding yields per z-value group for wheat:
  # Initialize an empty list to store the results for all locations
  result_list <- list()
  
  # Loop over each location
  for (location in 1:3) {
    # Initialize an empty list to store the results for the current location
    location_result_list <- list()
    
    # Loop over the columns for temperature and precipitation
    for (col in 2:3) { # The relevant columns are 2 (avgtemp), 3 (precip), and 4 (yields)
      # Calculate the z-values
      z_values <- scale(filtered_data_list_winterwheat[[location]][[col]])
      
      # Loop over the z-value intervals and calculate the Location_WSOTHA (yields) values
      for (i in 1:(length(z_value_intervals) - 1)) {
        lower_bound <- z_value_intervals[i]
        upper_bound <- z_value_intervals[i + 1]
        
        result <- filtered_data_list_winterwheat[[location]][[paste0("Location", location, "_WSOTHA")]][z_values >= lower_bound & z_values <= upper_bound]
        
        # Round the results to 3 decimal places
        result_rounded <- round(result, 3)
        
        # Store the results in the location_result_list
        location_result_list[[paste0("w", col, "____", lower_bound, "to", upper_bound)]] <- result_rounded
      }
    }
    
    # Store the results for the current location in the main result_list
    result_list[[paste0("Location", location)]] <- location_result_list
  }
  # Print statement to explain the results
  print("These are the yields for winter wheat that fall into specific z-value groups for either temperature or precipitation. The results show the yields per location starting at value -3 up to 3. The first values per location correspond to average temperature. The results after that, for the same location, correspond to precipitation.")
  
  # View the results
  print(result_list)
  
}


z_values_durum <- function(filtered_data_list_durum) {
  # Define the z-value intervals
  z_value_intervals <- seq(-3, 3, 0.5)
  
  # To find the corresponding yields per z-value group for wheat:
  # Initialize an empty list to store the results for all locations
  result_list <- list()
  
  # Loop over each location
  for (location in 4:5) {
    # Initialize an empty list to store the results for the current location
    location_result_list <- list()
    
    # Loop over the columns for temperature and precipitation
    for (col in 2:3) { # The relevant columns are 2 (avgtemp), 3 (precip), and 4 (yields)
      # Calculate the z-values
      z_values <- scale(filtered_data_list_durum[[location]][[col]])
      
      # Loop over the z-value intervals and calculate the Location_WSOTHA (yields) values
      for (i in 1:(length(z_value_intervals) - 1)) {
        lower_bound <- z_value_intervals[i]
        upper_bound <- z_value_intervals[i + 1]
        
        result <- filtered_data_list_durum[[location]][[paste0("Location", location, "_WSOTHA")]][z_values >= lower_bound & z_values <= upper_bound]
        
        # Round the results to 3 decimal places
        result_rounded <- round(result, 3)
        
        # Store the results in the location_result_list
        location_result_list[[paste0("w", col, "____", lower_bound, "to", upper_bound)]] <- result_rounded
      }
    }
    
    # Store the results for the current location in the main result_list
    result_list[[paste0("Location", location)]] <- location_result_list
  }
  # Print statement to explain the results
  print("These are the yields for durum that fall into specific z-value groups for either temperature or precipitation. The results show the yields per location starting at value -3 up to 3. The first values per location correspond to average temperature. The results after that, for the same location, correspond to precipitation.")
  
  # View the results
  print(result_list)
  
}



z_values_summergrapes <- function(filtered_data_list_gas) {
  # Define the z-value intervals
  z_value_intervals <- seq(-3, 3, 0.5)
  
  # Initialize an empty list to store the results for all locations
  result_list <- list()
  
  # Set the location to 1
  location <- 1
  
  # Initialize an empty list to store the results for the current location
  location_result_list <- list()
  
  # Loop over the columns for temperature and precipitation
  for (col in 2:3) { # The relevant columns are 2 (avgtemp), 3 (precip), and 4 (yields)
    # Calculate the z-values
    z_values <- scale(filtered_data_list_gas[[location]][[col]])
    
    # Loop over the z-value intervals and calculate the Location_WSOTHA (yields) values
    for (i in 1:(length(z_value_intervals) - 1)) {
      lower_bound <- z_value_intervals[i]
      upper_bound <- z_value_intervals[i + 1]
      
      result <- filtered_data_list_gas[[location]][[paste0("Location", location)]][z_values >= lower_bound & z_values <= upper_bound]
      
      # Round the results to 3 decimal places
      result_rounded <- round(result, 3)
      
      # Store the results in the location_result_list
      location_result_list[[paste0("w", col, "____", lower_bound, "to", upper_bound)]] <- result_rounded
    }
  }
  
  # Store the results for the current location in the main result_list
  result_list[[paste0("Location", location)]] <- location_result_list
  
  # Print statement to explain the results
  print("These are the yields for summer grapes that fall into specific z-value groups for either temperature or precipitation. The results show the yields per location starting at value -3 up to 3. The first values per location correspond to average temperature. The results after that, for the same location, correspond to precipitation.")
  
  # View the results
  print(result_list)
  
}



z_values_springgrapes <- function(filtered_data_list_gam) {
  # Define the z-value intervals
  z_value_intervals <- seq(-3, 3, 0.5)
  
  # To find the corresponding yields per z-value group for wheat:
  # Initialize an empty list to store the results for all locations
  result_list <- list()
  
  # Loop over each location
  for (location in 2:5) {
    # Initialize an empty list to store the results for the current location
    location_result_list <- list()
    
    # Loop over the columns for temperature and precipitation
    for (col in 2:3) { # The relevant columns are 2 (avgtemp), 3 (precip), and 4 (yields)
      # Calculate the z-values
      z_values <- scale(filtered_data_list_gam[[location]][[col]])
      
      # Loop over the z-value intervals and calculate the Location_WSOTHA (yields) values
      for (i in 1:(length(z_value_intervals) - 1)) {
        lower_bound <- z_value_intervals[i]
        upper_bound <- z_value_intervals[i + 1]
        
        result <- filtered_data_list_gam[[location]][[paste0("Location", location)]][z_values >= lower_bound & z_values <= upper_bound]
        
        # Round the results to 3 decimal places
        result_rounded <- round(result, 3)
        
        # Store the results in the location_result_list
        location_result_list[[paste0("w", col, "____", lower_bound, "to", upper_bound)]] <- result_rounded
      }
    }
    
    # Store the results for the current location in the main result_list
    result_list[[paste0("Location", location)]] <- location_result_list
  }
  # Print statement to explain the results
  print("These are the yields for spring grapes that fall into specific z-value groups for either temperature or precipitation. The results show the yields per location starting at value -3 up to 3. The first values per location correspond to average temperature. The results after that, for the same location, correspond to precipitation.")
  
  # View the results
  print(result_list)
  
}

#################################################################################
z_values_winterwheat_stats <- function(filtered_data_list_winterwheat) {
  # Define the z-value intervals
  z_value_intervals <- seq(-3, 3, 0.5)
  
  # Initialize an empty list to store the results for all locations
  result_list <- list()
  
  # Loop over each location
  for (location in 1:3) {
    # Initialize an empty list to store the results for the current location
    location_result_list <- list()
    
    # Loop over the columns for temperature and precipitation
    for (col in 2:3) { # The relevant columns are 2 (avgtemp) and 3 (precip), and 4 (yields)
      # Calculate the z-values
      z_values <- scale(filtered_data_list_winterwheat[[location]][[col]])
      
      # Loop over the z-value intervals and calculate the statistics for Location_WSOTHA (yields) values
      for (i in 1:(length(z_value_intervals) - 1)) {
        lower_bound <- z_value_intervals[i]
        upper_bound <- z_value_intervals[i + 1]
        
        yields <- filtered_data_list_winterwheat[[location]][[paste0("Location", location, "_WSOTHA")]][z_values >= lower_bound & z_values <= upper_bound]
        
        if (length(yields) > 0) {
          # Calculate statistics
          min_yield <- min(yields, na.rm = TRUE)
          max_yield <- max(yields, na.rm = TRUE)
          avg_yield <- mean(yields, na.rm = TRUE)
          
          # Round the results to 3 decimal places
          min_yield <- round(min_yield, 3)
          max_yield <- round(max_yield, 3)
          avg_yield <- round(avg_yield, 3)
          
          # Store the results in the location_result_list
          location_result_list[[paste0("w", col, "____", lower_bound, "to", upper_bound)]] <- list(
            min = min_yield,
            max = max_yield,
            avg = avg_yield
          )
        } else {
          location_result_list[[paste0("w", col, "____", lower_bound, "to", upper_bound)]] <- list(
            min = NA,
            max = NA,
            avg = NA
          )
        }
      }
    }
    
    # Store the results for the current location in the main result_list
    result_list[[paste0("Location", location)]] <- location_result_list
  }
  
  # Print statement to explain the results
  print("These are the statistics (min, max, avg) of yields for winter wheat that fall into specific z-value groups for either temperature or precipitation. The results show the yields per location starting at value -3 up to 3. The first values per location correspond to average temperature. The results after that, for the same location, correspond to precipitation.")
  
  # View the results
  print(result_list)
}


z_values_durum_stats <- function(filtered_data_list_durum) {
  # Define the z-value intervals
  z_value_intervals <- seq(-3, 3, 0.5)
  
  # Initialize an empty list to store the results for all locations
  result_list <- list()
  
  # Loop over each location
  for (location in 4:5) {
    # Initialize an empty list to store the results for the current location
    location_result_list <- list()
    
    # Loop over the columns for temperature and precipitation
    for (col in 2:3) { # The relevant columns are 2 (avgtemp) and 3 (precip), and 4 (yields)
      # Calculate the z-values
      z_values <- scale(filtered_data_list_durum[[location]][[col]])
      
      # Loop over the z-value intervals and calculate the statistics for Location_WSOTHA (yields) values
      for (i in 1:(length(z_value_intervals) - 1)) {
        lower_bound <- z_value_intervals[i]
        upper_bound <- z_value_intervals[i + 1]
        
        yields <- filtered_data_list_durum[[location]][[paste0("Location", location, "_WSOTHA")]][z_values >= lower_bound & z_values <= upper_bound]
        
        if (length(yields) > 0) {
          # Calculate statistics
          min_yield <- min(yields, na.rm = TRUE)
          max_yield <- max(yields, na.rm = TRUE)
          avg_yield <- mean(yields, na.rm = TRUE)
          
          # Round the results to 3 decimal places
          min_yield <- round(min_yield, 3)
          max_yield <- round(max_yield, 3)
          avg_yield <- round(avg_yield, 3)
          
          # Store the results in the location_result_list
          location_result_list[[paste0("w", col, "____", lower_bound, "to", upper_bound)]] <- list(
            min = min_yield,
            max = max_yield,
            avg = avg_yield
          )
        } else {
          location_result_list[[paste0("w", col, "____", lower_bound, "to", upper_bound)]] <- list(
            min = NA,
            max = NA,
            avg = NA
          )
        }
      }
    }
    
    # Store the results for the current location in the main result_list
    result_list[[paste0("Location", location)]] <- location_result_list
  }
  
  # Print statement to explain the results
  print("These are the statistics (min, max, avg) of yields for durum that fall into specific z-value groups for either temperature or precipitation. The results show the yields per location starting at value -3 up to 3. The first values per location correspond to average temperature. The results after that, for the same location, correspond to precipitation.")
  
  # View the results
  print(result_list)
}


z_values_summergrapes_stats <- function(filtered_data_list_gas) {
  # Define the z-value intervals
  z_value_intervals <- seq(-3, 3, 0.5)
  
  # Initialize an empty list to store the results for all locations
  result_list <- list()
  
  # Set the location to 1
  location <- 1
  
  # Initialize an empty list to store the results for the current location
  location_result_list <- list()
  
  # Loop over the columns for temperature and precipitation
  for (col in 2:3) { # The relevant columns are 2 (avgtemp) and 3 (precip), and 4 (yields)
    # Calculate the z-values
    z_values <- scale(filtered_data_list_gas[[location]][[col]])
    
    # Loop over the z-value intervals and calculate the statistics for Location_WSOTHA (yields) values
    for (i in 1:(length(z_value_intervals) - 1)) {
      lower_bound <- z_value_intervals[i]
      upper_bound <- z_value_intervals[i + 1]
      
      yields <- filtered_data_list_gas[[location]][[paste0("Location", location)]][z_values >= lower_bound & z_values <= upper_bound]
      
      if (length(yields) > 0) {
        # Calculate statistics
        min_yield <- min(yields, na.rm = TRUE)
        max_yield <- max(yields, na.rm = TRUE)
        avg_yield <- mean(yields, na.rm = TRUE)
        
        # Round the results to 3 decimal places
        min_yield <- round(min_yield, 3)
        max_yield <- round(max_yield, 3)
        avg_yield <- round(avg_yield, 3)
        
        # Store the results in the location_result_list
        location_result_list[[paste0("w", col, "____", lower_bound, "to", upper_bound)]] <- list(
          min = min_yield,
          max = max_yield,
          avg = avg_yield
        )
      } else {
        location_result_list[[paste0("w", col, "____", lower_bound, "to", upper_bound)]] <- list(
          min = NA,
          max = NA,
          avg = NA
        )
      }
    }
  }
  
  # Store the results for the current location in the main result_list
  result_list[[paste0("Location", location)]] <- location_result_list
  
  # Print statement to explain the results
  print("These are the statistics (min, max, avg) of yields for summer grapes that fall into specific z-value groups for either temperature or precipitation. The results show the yields per location starting at value -3 up to 3. The first values per location correspond to average temperature. The results after that, for the same location, correspond to precipitation.")
  
  # View the results
  print(result_list)
}

z_values_springgrapes_stats <- function(filtered_data_list_gam) {
  # Define the z-value intervals
  z_value_intervals <- seq(-3, 3, 0.5)
  
  # Initialize an empty list to store the results for all locations
  result_list <- list()
  
  # Loop over each location
  for (location in 2:5) {
    # Initialize an empty list to store the results for the current location
    location_result_list <- list()
    
    # Loop over the columns for temperature and precipitation
    for (col in 2:3) { # The relevant columns are 2 (avgtemp) and 3 (precip)
      # Calculate the z-values
      z_values <- scale(filtered_data_list_gam[[location]][[col]])
      
      # Loop over the z-value intervals and calculate the statistics for Location_WSOTHA (yields) values
      for (i in 1:(length(z_value_intervals) - 1)) {
        lower_bound <- z_value_intervals[i]
        upper_bound <- z_value_intervals[i + 1]
        
        yields <- filtered_data_list_gam[[location]][[paste0("Location", location)]][z_values >= lower_bound & z_values <= upper_bound]
        
        if (length(yields) > 0) {
          # Calculate statistics
          min_yield <- min(yields, na.rm = TRUE)
          max_yield <- max(yields, na.rm = TRUE)
          avg_yield <- mean(yields, na.rm = TRUE)
          
          # Round the results to 3 decimal places
          min_yield <- round(min_yield, 3)
          max_yield <- round(max_yield, 3)
          avg_yield <- round(avg_yield, 3)
          
          # Store the results in the location_result_list
          location_result_list[[paste0("w", col, "____", lower_bound, "to", upper_bound)]] <- list(
            min = min_yield,
            max = max_yield,
            avg = avg_yield
          )
        } else {
          location_result_list[[paste0("w", col, "____", lower_bound, "to", upper_bound)]] <- list(
            min = NA,
            max = NA,
            avg = NA
          )
        }
      }
    }
    
    # Store the results for the current location in the main result_list
    result_list[[paste0("Location", location)]] <- location_result_list
  }
  
  # Print statement to explain the results
  print("These are the statistics (min, max, avg) of yields for spring grapes that fall into specific z-value groups for either temperature or precipitation. The results show the yields per location starting at value -3 up to 3. The first values per location correspond to average temperature. The results after that, for the same location, correspond to precipitation.")
  
  # View the results
  print(result_list)
}


################################################################################
check_temperature_zvalues_winterwheat <- function(filtered_data_list_winterwheat) {
  # Loop over thr_amj_wheat variables
  for (j in 1:3) {
    dataset <- filtered_data_list_winterwheat[[j]]
    column_name <- paste0("AvgTemperature_LOC", j)
    
    # Make a new column with scaled values
    dataset$scaled_column <- scale(dataset[[column_name]])[, 1]  # Neem de eerste kolom van de geschaalde matrix
    
    # Fit the scaled values in the column
    model <- lm(paste(column_name, "~ scaled_column"), data = dataset)
    
    # Define z-value groups
    z_value_intervals <- seq(-3, 3, 0.5)
    
    # Predict temperature values
    new_data <- data.frame(scaled_column = z_value_intervals)
    predicted_values <- predict(model, newdata = new_data)
    
    # Round the results
    rounded_predicted_values <- round(predicted_values, 1)
    
    # Loop over the z-value groups
    for (k in seq_along(rounded_predicted_values)) {
      
      # Print predicted values
      print(paste("Values for", column_name, "z-value group", k, ":", rounded_predicted_values[k]))
    }
  }
}

check_temperature_zvalues_durum <- function(filtered_data_list_durum) {
  # Loop over thr_amj_wheat variables
  for (j in 4:5) {
    dataset <- filtered_data_list_durum[[j]]
    column_name <- paste0("AvgTemperature_LOC", j)
    
    # Make a new column with scaled values
    dataset$scaled_column <- scale(dataset[[column_name]])[, 1]  # Neem de eerste kolom van de geschaalde matrix
    
    # Fit the scaled values in the column
    model <- lm(paste(column_name, "~ scaled_column"), data = dataset)
    
    # Define z-value groups
    z_value_intervals <- seq(-3, 3, 0.5)
    
    # Predict temperature values
    new_data <- data.frame(scaled_column = z_value_intervals)
    predicted_values <- predict(model, newdata = new_data)
    
    # Round the results
    rounded_predicted_values <- round(predicted_values, 1)
    
    # Loop over the z-value groups
    for (k in seq_along(rounded_predicted_values)) {
      
      # Print predicted values
      print(paste("Values for", column_name, "z-value group", k, ":", rounded_predicted_values[k]))
    }
  }
}

check_temperature_zvalues_summergrapes <- function(filtered_data_list_gas) {
  # Fixed location to check
  j <- 1
  
  dataset <- filtered_data_list_gas[[j]]
  column_name <- paste0("AvgTemperature_LOC", j)
  
  # Create a new column with scaled values
  dataset$scaled_column <- scale(dataset[[column_name]])[, 1]  # Take the first column of the scaled matrix
  
  # Fit a linear regression model
  model <- lm(paste(column_name, "~ scaled_column"), data = dataset)
  
  # Define the bounds for the z-values
  z_value_intervals <- seq(-3, 3, 0.5)
  
  # Predict precipitation values for the given z-value intervals
  new_data <- data.frame(scaled_column = z_value_intervals)
  predicted_values <- predict(model, newdata = new_data)
  
  # Round the predicted values to one decimal place
  rounded_predicted_values <- round(predicted_values, 1)
  
  # Loop over the z-value groups
  for (k in seq_along(rounded_predicted_values)) {
    # View the predicted values
    print(paste("Values for", column_name, "z-value group", k, ":", rounded_predicted_values[k]))
  }
}


check_temperature_zvalues_springgrapes <- function(filtered_data_list_gam) {
  # Loop over thr_amj_grape variables
  for (j in 2:5) {
    dataset <- filtered_data_list_gam[[j]]
    column_name <- paste0("AvgTemperature_LOC", j)
    
    # Create a new column with scaled values
    dataset$scaled_column <- scale(dataset[[column_name]])[, 1]  # Take the first column of the scaled matrix
    
    # Fit a linear regression model
    model <- lm(paste(column_name, "~ scaled_column"), data = dataset)
    
    # Define the bounds for the z-values
    z_value_intervals <- seq(-3, 3, 0.5)
    
    # Predict precipitation values for the given z-value intervals
    new_data <- data.frame(scaled_column = z_value_intervals)
    predicted_values <- predict(model, newdata = new_data)
    
    # Round the predicted values to one decimal place
    rounded_predicted_values <- round(predicted_values, 1)
    
    # Loop over the z-value groups
    for (k in seq_along(rounded_predicted_values)) {
      # View the predicted values
      print(paste("Values for", column_name, "z-value group", k, ":", rounded_predicted_values[k]))
    }
  }
}

check_precipitation_zvalues_winterwheat <- function(filtered_data_list_winterwheat) {
  # Loop over thr_amj_wheat variables
  for (j in 1:3) {
    dataset <- filtered_data_list_winterwheat[[j]]
    column_name <- paste0("Precipitation_LOC", j)
    
    # Create a new column with scaled values
    dataset$scaled_column <- scale(dataset[[column_name]])[, 1]  # Take the first column of the scaled matrix
    
    # Fit a linear regression model
    model <- lm(paste(column_name, "~ scaled_column"), data = dataset)
    
    # Define the bounds for the z-values
    z_value_intervals <- seq(-3, 3, 0.5)
    
    # Predict precipitation values for the given z-value intervals
    new_data <- data.frame(scaled_column = z_value_intervals)
    predicted_values <- predict(model, newdata = new_data)
    
    # Round the predicted values to one decimal place
    rounded_predicted_values <- round(predicted_values, 1)
    
    # Loop over the z-value groups
    for (k in seq_along(rounded_predicted_values)) {
      # View the predicted values
      print(paste("Values for", column_name, "z-value group", k, ":", rounded_predicted_values[k]))
    }
  }
}

check_precipitation_zvalues_durum <- function(filtered_data_list_durum) {
  # Loop over thr_amj_wheat variables
  for (j in 4:5) {
    dataset <- filtered_data_list_durum[[j]]
    column_name <- paste0("Precipitation_LOC", j)
    
    # Create a new column with scaled values
    dataset$scaled_column <- scale(dataset[[column_name]])[, 1]  # Take the first column of the scaled matrix
    
    # Fit a linear regression model
    model <- lm(paste(column_name, "~ scaled_column"), data = dataset)
    
    # Define the bounds for the z-values
    z_value_intervals <- seq(-3, 3, 0.5)
    
    # Predict precipitation values for the given z-value intervals
    new_data <- data.frame(scaled_column = z_value_intervals)
    predicted_values <- predict(model, newdata = new_data)
    
    # Round the predicted values to one decimal place
    rounded_predicted_values <- round(predicted_values, 1)
    
    # Loop over the z-value groups
    for (k in seq_along(rounded_predicted_values)) {
      # View the predicted values
      print(paste("Values for", column_name, "z-value group", k, ":", rounded_predicted_values[k]))
    }
  }
}

check_precipitation_zvalues_summergrapes <- function(filtered_data_list_gas) {
  # Work with the first dataset
  dataset <- filtered_data_list_gas[[1]]
  column_name <- "Precipitation_LOC1"
  
  # Create a new column with scaled values
  dataset$scaled_column <- scale(dataset[[column_name]])[, 1]  # Take the first column of the scaled matrix
  
  # Fit a linear regression model
  model <- lm(paste(column_name, "~ scaled_column"), data = dataset)
  
  # Define the bounds for the z-values
  z_value_intervals <- seq(-3, 3, 0.5)
  
  # Predict precipitation values for the given z-value intervals
  new_data <- data.frame(scaled_column = z_value_intervals)
  predicted_values <- predict(model, newdata = new_data)
  
  # Round the predicted values to one decimal place
  rounded_predicted_values <- round(predicted_values, 1)
  
  # Loop over the z-value groups
  for (k in seq_along(rounded_predicted_values)) {
    # View the predicted values
    print(paste("Values for", column_name, "z-value group", k, ":", rounded_predicted_values[k]))
  }
}


check_precipitation_zvalues_springgrapes <- function(filtered_data_list_gas) {
  # Loop over thr_amj_grape variables
  for (j in 2:5) {
    dataset <- filtered_data_list_gam[[j]]
    column_name <- paste0("Precipitation_LOC", j)
    
    # Create a new column with scaled values
    dataset$scaled_column <- scale(dataset[[column_name]])[, 1]  # Take the first column of the scaled matrix
    
    # Fit a linear regression model
    model <- lm(paste(column_name, "~ scaled_column"), data = dataset)
    
    # Define the bounds for the z-values
    z_value_intervals <- seq(-3, 3, 0.5)
    
    # Predict precipitation values for the given z-value intervals
    new_data <- data.frame(scaled_column = z_value_intervals)
    predicted_values <- predict(model, newdata = new_data)
    
    # Round the predicted values to one decimal place
    rounded_predicted_values <- round(predicted_values, 1)
    
    # Loop over the z-value groups
    for (k in seq_along(rounded_predicted_values)) {
      # View the predicted values
      print(paste("Values for", column_name, "z-value group", k, ":", rounded_predicted_values[k]))
    }
  }
}

###############################################################################
winterwheat_excel <- function(result_list_ww, filename = "winterwheat_results.xlsx", avg_filename = "average_winterwheat_results.xlsx") {
  library(writexl)
  
  # Create a new list to store data frames for each location
  excel_data <- list()
  avg_excel_data <- list()
  
  # Loop over each location in the result list
  for (location in names(result_list_ww)) {
    # Initialize an empty data frame to store results for the current location
    location_data <- data.frame(
      Interval = character(),
      Yields = character(),
      stringsAsFactors = FALSE
    )
    
    # Initialize an empty data frame to store average results for the current location
    avg_location_data <- data.frame(
      Interval = character(),
      Avg_Yield = numeric(),
      stringsAsFactors = FALSE
    )
    
    # Loop over each key in the location's result list
    for (key in names(result_list_ww[[location]])) {
      # Get the yields and interval information
      interval <- key
      yields <- result_list_ww[[location]][[key]]
      yields[is.na(yields)] <- 0
      yields_str <- paste(yields, collapse = ", ")
      
      # Add a new row to the data frame
      location_data <- rbind(location_data, data.frame(Interval = interval, Yields = yields_str, stringsAsFactors = FALSE))
      
      # Calculate the average yield for the interval
      avg_yield <- mean(as.numeric(yields))
      
      # Add a new row to the average data frame
      avg_location_data <- rbind(avg_location_data, data.frame(Interval = interval, Avg_Yield = avg_yield, stringsAsFactors = FALSE))
    }
    
    # Store the data frames in the respective excel_data lists with the location name as the key
    excel_data[[location]] <- location_data
    avg_excel_data[[location]] <- avg_location_data
  }
  
  # Write the list of data frames to the Excel files
  write_xlsx(excel_data, path = filename)
  write_xlsx(avg_excel_data, path = avg_filename)
}


durum_excel <- function(result_list_dr, filename = "durum_results.xlsx", avg_filename = "average_durum_results.xlsx") {
  library(writexl)
  
  # Create a new list to store data frames for each location
  excel_data <- list()
  avg_excel_data <- list()
  
  # Loop over each location in the result list
  for (location in names(result_list_dr)) {
    # Initialize an empty data frame to store results for the current location
    location_data <- data.frame(
      Interval = character(),
      Yields = character(),
      stringsAsFactors = FALSE
    )
    
    # Initialize an empty data frame to store average results for the current location
    avg_location_data <- data.frame(
      Interval = character(),
      Avg_Yield = numeric(),
      stringsAsFactors = FALSE
    )
    
    # Loop over each key in the location's result list
    for (key in names(result_list_dr[[location]])) {
      # Get the yields and interval information
      interval <- key
      yields <- result_list_dr[[location]][[key]]
      yields[is.na(yields)] <- 0
      yields_str <- paste(yields, collapse = ", ")
      
      # Add a new row to the data frame
      location_data <- rbind(location_data, data.frame(Interval = interval, Yields = yields_str, stringsAsFactors = FALSE))
      
      # Calculate the average yield for the interval
      avg_yield <- mean(as.numeric(yields))
      
      # Add a new row to the average data frame
      avg_location_data <- rbind(avg_location_data, data.frame(Interval = interval, Avg_Yield = avg_yield, stringsAsFactors = FALSE))
    }
    
    # Store the data frames in the respective excel_data lists with the location name as the key
    excel_data[[location]] <- location_data
    avg_excel_data[[location]] <- avg_location_data
  }
  
  # Write the list of data frames to the Excel files
  write_xlsx(excel_data, path = filename)
  write_xlsx(avg_excel_data, path = avg_filename)
}


summergrapes_excel <- function(result_list_gas, filename = "summergrapes_results.xlsx", avg_filename = "average_summergrapes_results.xlsx") {
  library(writexl)
  
  # Create a new list to store data frames for each location
  excel_data <- list()
  avg_excel_data <- list()
  
  # Loop over each location in the result list
  for (location in names(result_list_gas)) {
    # Initialize an empty data frame to store results for the current location
    location_data <- data.frame(
      Interval = character(),
      Yields = character(),
      stringsAsFactors = FALSE
    )
    
    # Initialize an empty data frame to store average results for the current location
    avg_location_data <- data.frame(
      Interval = character(),
      Avg_Yield = numeric(),
      stringsAsFactors = FALSE
    )
    
    # Loop over each key in the location's result list
    for (key in names(result_list_gas[[location]])) {
      # Get the yields and interval information
      interval <- key
      yields <- result_list_gas[[location]][[key]]
      yields[is.na(yields)] <- 0
      yields_str <- paste(yields, collapse = ", ")
      
      # Add a new row to the data frame
      location_data <- rbind(location_data, data.frame(Interval = interval, Yields = yields_str, stringsAsFactors = FALSE))
      
      # Calculate the average yield for the interval
      avg_yield <- mean(as.numeric(yields))
      
      # Add a new row to the average data frame
      avg_location_data <- rbind(avg_location_data, data.frame(Interval = interval, Avg_Yield = avg_yield, stringsAsFactors = FALSE))
    }
    
    # Store the data frames in the respective excel_data lists with the location name as the key
    excel_data[[location]] <- location_data
    avg_excel_data[[location]] <- avg_location_data
  }
  
  # Write the list of data frames to the Excel files
  write_xlsx(excel_data, path = filename)
  write_xlsx(avg_excel_data, path = avg_filename)
}


springgrapes_excel <- function(result_list_gam, filename = "springgrapes_results.xlsx", avg_filename = "average_springgrapes_results.xlsx") {
  library(writexl)
  
  # Create a new list to store data frames for each location
  excel_data <- list()
  avg_excel_data <- list()
  
  # Loop over each location in the result list
  for (location in names(result_list_gam)) {
    # Initialize an empty data frame to store results for the current location
    location_data <- data.frame(
      Interval = character(),
      Yields = character(),
      stringsAsFactors = FALSE
    )
    
    # Initialize an empty data frame to store average results for the current location
    avg_location_data <- data.frame(
      Interval = character(),
      Avg_Yield = numeric(),
      stringsAsFactors = FALSE
    )
    
    # Loop over each key in the location's result list
    for (key in names(result_list_gam[[location]])) {
      # Get the yields and interval information
      interval <- key
      yields <- result_list_gam[[location]][[key]]
      yields[is.na(yields)] <- 0
      yields_str <- paste(yields, collapse = ", ")
      
      # Add a new row to the data frame
      location_data <- rbind(location_data, data.frame(Interval = interval, Yields = yields_str, stringsAsFactors = FALSE))
      
      # Calculate the average yield for the interval
      avg_yield <- mean(as.numeric(yields))
      
      # Add a new row to the average data frame
      avg_location_data <- rbind(avg_location_data, data.frame(Interval = interval, Avg_Yield = avg_yield, stringsAsFactors = FALSE))
    }
    
    # Store the data frames in the respective excel_data lists with the location name as the key
    excel_data[[location]] <- location_data
    avg_excel_data[[location]] <- avg_location_data
  }
  
  # Write the list of data frames to the Excel files
  write_xlsx(excel_data, path = filename)
  write_xlsx(avg_excel_data, path = avg_filename)
}

###############################################################################
check_precipitation_zvalues_paleo <- function(paleo_marinecore) {
  # Make numeric columns
  paleo_marinecore$calculated.temperature...C. <- as.numeric(gsub(",", ".", paleo_marinecore$calculated.temperature...C.))
  paleo_marinecore$DI <- as.numeric(gsub(",", ".", paleo_marinecore$DI))
  paleo_marinecore$Age..years.BCE.CE. <- as.numeric(gsub(",", ".", paleo_marinecore$Age..years.BCE.CE.))
  paleo_marinecore <- paleo_marinecore[paleo_marinecore$DI >= 4, ]
  
  # Determine z-values for DI
  paleo_marinecore$scaled_DI <- scale(paleo_marinecore$DI)[, 1]  # Take the first column of the scaled matrix
  
  # Fit a linear regression model
  model <- lm(DI ~ scaled_DI, data = paleo_marinecore)
  
  # Define the bounds for the z-values
  z_value_intervals <- seq(-3, 3, 0.5)
  
  # Predict DI values for the given z-value intervals
  new_data <- data.frame(scaled_DI = z_value_intervals)
  predicted_values <- predict(model, newdata = new_data)
  
  # Round the predicted values to one decimal place
  rounded_predicted_values <- round(predicted_values, 1)
  
  # Loop over the z-value groups
  for (k in seq_along(rounded_predicted_values)) {
    # View the predicted values
    print(paste("Values for DI z-value group", k, ":", rounded_predicted_values[k]))
  }
}

check_temperature_zvalues_paleo <- function(paleo_marinecore) {
  # Make numeric columns
  paleo_marinecore$calculated.temperature...C. <- as.numeric(gsub(",", ".", paleo_marinecore$calculated.temperature...C.))
  paleo_marinecore$DI <- as.numeric(gsub(",", ".", paleo_marinecore$DI))
  paleo_marinecore$Age..years.BCE.CE. <- as.numeric(gsub(",", ".", paleo_marinecore$Age..years.BCE.CE.))
  paleo_marinecore <- paleo_marinecore[paleo_marinecore$calculated.temperature...C. >= 4, ]
  
  # Determine z-values for temp
  paleo_marinecore$scaled_temp <- scale(paleo_marinecore$calculated.temperature...C.)[, 1]  # Take the first column of the scaled matrix
  
  # Fit a linear regression model
  model <- lm(calculated.temperature...C. ~ scaled_temp, data = paleo_marinecore)
  
  # Define the bounds for the z-values
  z_value_intervals <- seq(-3, 3, 0.5)
  
  # Predict DI values for the given z-value intervals
  new_data <- data.frame(scaled_temp = z_value_intervals)
  predicted_values <- predict(model, newdata = new_data)
  
  # Round the predicted values to one decimal place
  rounded_predicted_values <- round(predicted_values, 1)
  
  # Loop over the z-value groups
  for (k in seq_along(rounded_predicted_values)) {
    # View the predicted values
    print(paste("Values for DI z-value group", k, ":", rounded_predicted_values[k]))
  }
}


