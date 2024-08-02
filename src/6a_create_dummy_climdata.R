generate_dummy_CABO_precipitation <- function(CABO_example) {
  for (i in 140:160) {
    
    # Modify data as needed
    CABO_example$V1 <- 6
    CABO_example$V4 <- 20703.3
    CABO_example$V5 <- 18.3
    CABO_example$V6 <- 18.3
    CABO_example$V7 <- 2.150019
    CABO_example$V8 <- 4.838803
    CABO_example$V9 <- 1 + (i - 150) * 0.10  # Adjust V9 based on file number
    CABO_example$V2 <- 1000 + i
    
    # Save as a CABO file
    write.table(CABO_example, paste0("data/temp/dummy data/dummychangeprecip1.", i), sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
    write.table(CABO_example, file = paste0("LINTUL2/Weatherdummy/dummychangeprecip1.", i), sep = "\t", row.names = FALSE, col.names = FALSE)
  }
}


generate_dummy_CABO_temperature <- function(CABO_example) {
  for (i in 240:260) {
    
   # Modify data as needed
   CABO_example$V1 <- 6
   CABO_example$V4 <- 20703.3
   CABO_example$V5 <- 18.3 + (i - 250) * 1 # Adjust V9 based on file number
   CABO_example$V6 <- 18.3 + (i - 250) * 1  # Adjust V9 based on file number
   CABO_example$V7 <- 2.150019
   CABO_example$V8 <- 4.838803
   CABO_example$V9 <- 1 
   CABO_example$V2 <- 1000 + i
    
   # Save as a CABO file
   write.table(CABO_example, paste0("data/temp/dummy data/dummychangetemp1.", i), sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
   write.table(CABO_example, file = paste0("LINTUL2/Weatherdummy/dummychangetemp1.", i), sep = "\t", row.names = FALSE, col.names = FALSE)
  }
}



generate_dummy_CSV_precipitation <- function(CSV_example) {
  for (i in 140:160) {
    
    # Modify data as needed
    CSV_example$V1 <- 6
    CSV_example$V7 <- 20.703
    CSV_example$V8 <- 18.3
    CSV_example$V9 <- 18.3
    CSV_example$V10 <- 2.150019
    CSV_example$V11 <- 4.838803
    CSV_example$V12 <- 1 + (i - 150) * 0.10  # Adjust V9 based on file number
    CSV_example$V2 <- 1000 + i
    
    filename <- paste("data/temp/dummy data/dummychangeprecip1", i, ".csv", sep = "")
    write.table(CABO_example, filename, sep = ";", row.names = FALSE, col.names = FALSE, quote = FALSE)
  }
  
}


generate_dummy_CSV_temperature <- function(CSV_example) {
  for (i in 240:260) {
  
    # Modify data as needed
    CSV_example$V1 <- 6
    CSV_example$V7 <- 20.703
    CSV_example$V8 <- 18.3 + (i - 250) * 1 # Adjust V9 based on file number
    CSV_example$V9 <- 18.3 + (i - 250) * 1  # Adjust V9 based on file number
    CSV_example$V10 <- 2.150019
    CSV_example$V11 <- 4.838803
    CSV_example$V12 <- 1 
    CSV_example$V2 <- 1000 + i
    
    filename <- paste("data/temp/dummy data/dummychangetemp1", i, ".csv", sep = "")
    write.table(CABO_example, filename, sep = ";", row.names = FALSE, col.names = FALSE, quote = FALSE)
  }
  
}
