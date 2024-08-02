combine_wheatyields <- function(wheat_yields, wheat_yields_colder) {
  # Modify the "Year" column in wheat_yields_colder
  wheat_yields_colder$Year <- wheat_yields_colder$Year - 165
  
  # Combine the two data frames
  combined_yields <- rbind(wheat_yields_colder, wheat_yields)
  
  # Sort the combined data frame by "Year"
  combined_yields <- combined_yields[order(combined_yields$Year), ]
  
  # Split into winterwheat (loc 1-3) and durum (4-5)
  winterwheat <- combined_yields[, -c(5, 6)]
  durum <- combined_yields[, -c(2, 3, 4)]
  
  # Write the combined data frame to a new CSV file
  write.csv(winterwheat, "data/output/combined_winterwheat_yields.csv", row.names = FALSE)
  write.csv(winterwheat, "data/temp/wheat yields/combined_winterwheat_yields.csv", row.names = FALSE)
  write.csv(durum, "data/output/combined_durum_yields.csv", row.names = FALSE)
  write.csv(durum, "data/temp/wheat yields/combineddurum_yields.csv", row.names = FALSE)
  
}


combine_grape1yields <- function(grape1_yields, grape1_yields_colder) {
  # Modify the "Year" column in grape1_yields_colder
  grape1_yields_colder$Year <- grape1_yields_colder$Year - 163
  
  # Remove columns
  grape1_yields <- grape1_yields[, -c(3, 4, 5, 6)]
  grape1_yields_colder <- grape1_yields_colder[, -c(3, 4, 5, 6)]
  
  # Combine the two data frames
  combined_yields <- rbind(grape1_yields_colder, grape1_yields)
  
  # Sort the combined data frame by "Year"
  combined_yields <- combined_yields[order(combined_yields$Year), ]
  
  # Remove extra columns 
  
  # Write the combined data frame to a new CSV file
  write.csv(combined_yields, "data/output/combined_grape1_yields.csv", row.names = FALSE)
  write.csv(combined_yields, "data/temp/grape yields/combined_grape1_yields.csv", row.names = FALSE)
  
}




combine_grapeyields <- function(grape1_yields) {
  # Remove column
  grape_yields <- grape_yields[, -c(2)]
  
  # Write the data frame to a new CSV file
  write.csv(grape_yields, "data/output/combined_grape_yields.csv", row.names = FALSE)
  write.csv(grape_yields, "data/temp/grape yields/combined_grape_yields.csv", row.names = FALSE)
  
}