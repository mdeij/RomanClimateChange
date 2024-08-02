add_text <- function(directory, output_directory) {
  # List all files in the directory
  files <- list.files(directory, full.names = TRUE)
  
  # Loop through each file
  for (file in files) {
    # Extract the last three digits from the file name
    year <- as.numeric(sub(".*\\.([0-9]{3})$", "\\1", basename(file)))
    
    # Check if the file corresponds to the desired naming convention
    if (!is.na(year)) {
      cat("Processing file:", basename(file), "Year:", year, "\n") # Debugging line
      
      # Adjust the year based on the conditions
      if (year >= 0 && year <= 14) {
        year <- year + 2000
      } else {
        year <- year + 1000
      }
      
      # Read the content of the file
      content <- readLines(file)
      
      # Create the text to be added at the beginning of the file
      # This text is the original header of the CABO files that came with the LINTUL model
      text_to_add <- paste(
        "*---------------------------------------------------------*",
        "*   Country: Netherlands",
        "*   Station: Wageningen",
        "*      Year:", year,
        "*    Source: Dep. of Meteorology, Wageningen Agricultural ",
        "*            University.",
        "*            www.met.wau.nl/haarwegdata/dayfiles/2006/",
        "*    Author: Peter Uithol",
        "* Longitude: 05 40 E",
        "*  Latitude: 51 58 N",
        "* Elevation: 7 m.",
        "*  Comments: Location Haarweg.",
        "*",
        "*  Columns:",
        "*  ========",
        "*  station number",
        "*  year",
        "*  day",
        "*  irradiation (kJ m-2 d-1)",
        "*  minimum temperature (degrees Celsius)",
        "*  maximum temperature (degrees Celsius)",
        "*  vapour pressure (kPa)",
        "*  mean wind speed (m s-1)",
        "*  precipitation (mm d-1)",
        "*---------------------------------------------------------*",
        sep = "\n"
      )
      
      # Check if the text_to_add is not already at the beginning of the content
      if (!identical(head(content, length(text_to_add)), text_to_add)) {
        # Add the text to the beginning of the content
        content <- c(text_to_add, content)
        
        # Define the output file path
        output_file <- file.path(output_directory, basename(file))
        
        # Write the modified content back to the file in the output directory
        writeLines(content, output_file)
        cat("Text added to file:", basename(file), "\n") # Debugging line
      } else {
        cat("Text already exists at the top of the file:", basename(file), "\n") # Debugging line
      }
    }
  }
}
