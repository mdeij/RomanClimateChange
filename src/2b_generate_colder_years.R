# Function to generate colder temperatures for wheat climate data
create_colder_wheatyears <- function(wheat_CABO, winter, spring, summer, autumn, file_index) {
  # Define normal distributions for each season
  winter_dist <- rnorm(nrow(wheat_CABO), mean = winter, sd = 1)
  spring_dist <- rnorm(nrow(wheat_CABO), mean = spring, sd = 1)
  summer_dist <- rnorm(nrow(wheat_CABO), mean = summer, sd = 1)
  autumn_dist <- rnorm(nrow(wheat_CABO), mean = autumn, sd = 1)
  
  # Generate colder temperatures based on season
  wheat_CABO <- wheat_CABO %>%
    mutate(
      V5 = case_when( # Adjusting original minimum temperature values
        V3 >= 60 & V3 <= 151 ~ V5 + spring_dist, # Adjust with spring distribution for March 1st to May 31st
        V3 >= 152 & V3 <= 243 ~ V5 + summer_dist,
        V3 >= 244 & V3 <= 335 ~ V5 + autumn_dist,
        TRUE ~ V5 + winter_dist
      ),
      V6 = case_when( # Adjusting original maximum temperature values
        V3 >= 60 & V3 <= 151 ~ V6 + spring_dist, # Adjust with spring distribution for March 1st to May 31st
        V3 >= 152 & V3 <= 243 ~ V6 + summer_dist,
        V3 >= 244 & V3 <= 335 ~ V6 + autumn_dist,
        TRUE ~ V6 + winter_dist
      )
    )
  
  # Save modified wheat climate data to a new CABO file
  write.table(wheat_CABO, paste0("data/temp/wheat_colder_", file_index, ".CABO"), sep = "\t", row.names = FALSE, col.names = FALSE)
}



create_colder_grapeyears <- function(grapes_CSV, winter, spring, summer, autumn, file_index) {
  # Define normal distributions for each season
  winter_dist <- rnorm(nrow(grapes_CSV), mean = winter, sd = 1)
  spring_dist <- rnorm(nrow(grapes_CSV), mean = spring, sd = 1)
  summer_dist <- rnorm(nrow(grapes_CSV), mean = summer, sd = 1)
  autumn_dist <- rnorm(nrow(grapes_CSV), mean = autumn, sd = 1)
  
  # Generate colder temperatures based on season
  grapes_CSV <- grapes_CSV %>%
    mutate(
      V8 = case_when( # Adjusting original minimum temperature values
        V3 >= 60 & V3 <= 151 ~ V8 + spring_dist, # Adjust with spring distribution for March 1st to May 31st
        V3 >= 152 & V3 <= 243 ~ V8 + summer_dist,
        V3 >= 244 & V3 <= 335 ~ V8 + autumn_dist,
        TRUE ~ V8 + winter_dist
      ),
      V9 = case_when( # Adjusting original maximum temperature values
        V3 >= 60 & V3 <= 151 ~ V9 + spring_dist, # Adjust with spring distribution for March 1st to May 31st
        V3 >= 152 & V3 <= 243 ~ V9 + summer_dist,
        V3 >= 244 & V3 <= 335 ~ V9 + autumn_dist,
        TRUE ~ V9 + winter_dist
      )
    )
  
  # Save modified wheat climate data to a new CABO file
  write.table(grapes_CSV, paste0("data/temp/grapes_colder_", file_index, ".csv"), sep = ";", row.names = FALSE, col.names = FALSE, quote = FALSE)
}