combine_climdata_winterwheat <- function(winterwheat_regular, winterwheat_colder) {
  
  # Set the correct column names
  colder_colnames = c("Year", "AvgTemperature_LOC1", "Precipitation_LOC1", "AvgTemperature_LOC2", "Precipitation_LOC2", "AvgTemperature_LOC3", "Precipitation_LOC3")
  colnames(winterwheat_colder) <- colder_colnames
  
  # Convert "Year" column in wheat_colder to numeric and then subtract 165
  winterwheat_colder$Year <- as.numeric(winterwheat_colder$Year) -165
  
  # Combine the two data frames
  combined_weatherdata <- rbind(winterwheat_colder, winterwheat_regular)
  
  # Sort the combined data frame by "Year"
  combined_weatherdata <- combined_weatherdata[order(combined_weatherdata$Year), ]
  
  # Write the combined data frame to a new CSV file
  write.table(combined_weatherdata, "data/temp/modern climate data/combined_modern_may_winterwheat_climdata.csv", row.names = FALSE, sep = ";", quote = FALSE)
}


combine_climdata_durum <- function(durum_regular, durum_colder) {
  
  # Set the correct column names
  colder_colnames = c("Year", "AvgTemperature_LOC4", "Precipitation_LOC4", "AvgTemperature_LOC5", "Precipitation_LOC5")
  colnames(durum_colder) <- colder_colnames
  
  # Convert "Year" column in wheat_colder to numeric and then subtract 165
  durum_colder$Year <- as.numeric(durum_colder$Year) -165
  
  # Combine the two data frames
  combined_weatherdata <- rbind(durum_colder, durum_regular)
  
  # Sort the combined data frame by "Year"
  combined_weatherdata <- combined_weatherdata[order(combined_weatherdata$Year), ]
  
  # Write the combined data frame to a new CSV file
  write.table(combined_weatherdata, "data/temp/modern climate data/combined_modern_may_durum_climdata.csv", row.names = FALSE, sep = ";", quote = FALSE)
}


combine_climdata_summergrapes <- function(summergrapes_regular, summergrapes_colder) {
  
  # Convert "Year" column in wheat_colder to numeric and then subtract 162
  summergrapes_colder$Year <- as.numeric(summergrapes_colder$Year) -165
  
  # Combine the two data frames
  combined_weatherdata <- rbind(summergrapes_colder, summergrapes_regular)
  
  # Sort the combined data frame by "Year"
  combined_weatherdata <- combined_weatherdata[order(combined_weatherdata$Year), ]
  
  # Write the combined data frame to a new CSV file
  write.table(combined_weatherdata, "data/temp/modern climate data/combined_modern_as_grapes_climdata.csv", row.names = FALSE, sep = ";", quote = FALSE)
}

