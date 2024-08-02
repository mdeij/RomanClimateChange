run_simulation_breadwheat_1899 <- function(station_number) {
  print("The LINTUL2 model takes about 20 minutes to run for three locations")
  # Load necessary libraries
  library(deSolve)
  
  # General settings
  wdirectory <- "./LINTUL2/weather/"
  country <- "ITA"
  SOILTYPE <- "sand"  # only sand or clay can be selected at this moment
  STTIME <- 297        # d     :     start time of simulation
  FINTIM <- 572       # d     :     finish time 
  DELT <- 1           # d     :     time step 
  # Define a vector of years from 1850 to 2014
  years <- 1850:1899
  
  # Loop through stations
  for (station in station_number) {
    # Loop through the years
    for (YR in years) {
      # Load necessary scripts and libraries
      source("LINTUL2/Components/LINTUL_parameters.r")  
      source("LINTUL2/Components/LINTUL_get_weather.r")  
      source("LINTUL2/Components/LINTUL2_functions.r")  
      source("LINTUL2/Components/lINTUL2_model.r")  
      require('deSolve')    # used for solving ODEs
      
      # Load weather data for the selected year
      wdata <- get_weather(directory = wdirectory, country = country, station = station, year = substr(toString(YR), 2, 4))
      
      # LINTUL without irrigation: water limited production
      LintulParms <- LINTUL2_PARAMETERS(irri = FALSE, soiltype = SOILTYPE, crop = "springwheat")
      results_wlim <- ode(LINTUL2_iniSTATES(LintulParms), 
                          seq(STTIME, FINTIM, by = DELT), 
                          LINTUL2, 
                          parms = LintulParms,
                          WDATA = wdata,
                          method = "euler")
      
      # Write the results to a file
      write.csv(data.frame(results_wlim), file = paste0("LINTUL2/Results/Location_", station, "_results_for_", YR, ".csv"))
    }
  }
}


run_simulation_breadwheat_1905 <- function(station_number) {
  print("The LINTUL2 model takes about 20 minutes to run for three locations")
  # Load necessary libraries
  library(deSolve)
  
  # General settings
  wdirectory <- "./LINTUL2/weather/"
  country <- "ITA"
  SOILTYPE <- "sand"  # only sand or clay can be selected at this moment
  STTIME <- 297        # d     :     start time of simulation
  FINTIM <- 572       # d     :     finish time 
  DELT <- 1           # d     :     time step 
  # Define a vector of years from 1850 to 2014
  years <- 1901:1905
  
  # Loop through stations
  for (station in station_number) {
    # Loop through the years
    for (YR in years) {
      # Load necessary scripts and libraries
      source("LINTUL2/Components/LINTUL_parameters.r")  
      source("LINTUL2/Components/LINTUL_get_weather.r")  
      source("LINTUL2/Components/LINTUL2_functions.r")  
      source("LINTUL2/Components/lINTUL2_model.r")  
      require('deSolve')    # used for solving ODEs
      
      # Load weather data for the selected year
      wdata <- get_weather(directory = wdirectory, country = country, station = station, year = substr(toString(YR), 2, 4))
      
      # LINTUL without irrigation: water limited production
      LintulParms <- LINTUL2_PARAMETERS(irri = FALSE, soiltype = SOILTYPE, crop = "springwheat")
      results_wlim <- ode(LINTUL2_iniSTATES(LintulParms), 
                          seq(STTIME, FINTIM, by = DELT), 
                          LINTUL2, 
                          parms = LintulParms,
                          WDATA = wdata,
                          method = "euler")
      
      # Write the results to a file
      write.csv(data.frame(results_wlim), file = paste0("LINTUL2/Results/Location_", station, "_results_for_", YR, ".csv"))
    }
  }
}



run_simulation_breadwheat_2013 <- function(station_number) {
  print("The LINTUL2 model takes about 20 minutes to run for three locations")
  # Load necessary libraries
  library(deSolve)
  
  # General settings
  wdirectory <- "./LINTUL2/weather/"
  country <- "ITA"
  SOILTYPE <- "sand"  # only sand or clay can be selected at this moment
  STTIME <- 297        # d     :     start time of simulation
  FINTIM <- 572       # d     :     finish time 
  DELT <- 1           # d     :     time step 
  # Define a vector of years from 1850 to 2014
  years <- 1907:2013
  
  # Loop through stations
  for (station in station_number) {
    # Loop through the years
    for (YR in years) {
      # Load necessary scripts and libraries
      source("LINTUL2/Components/LINTUL_parameters.r")  
      source("LINTUL2/Components/LINTUL_get_weather.r")  
      source("LINTUL2/Components/LINTUL2_functions.r")  
      source("LINTUL2/Components/lINTUL2_model.r")  
      require('deSolve')    # used for solving ODEs
      
      # Load weather data for the selected year
      wdata <- get_weather(directory = wdirectory, country = country, station = station, year = substr(toString(YR), 2, 4))
      
      # LINTUL without irrigation: water limited production
      LintulParms <- LINTUL2_PARAMETERS(irri = FALSE, soiltype = SOILTYPE, crop = "springwheat")
      results_wlim <- ode(LINTUL2_iniSTATES(LintulParms), 
                          seq(STTIME, FINTIM, by = DELT), 
                          LINTUL2, 
                          parms = LintulParms,
                          WDATA = wdata,
                          method = "euler")
      
      # Write the results to a file
      write.csv(data.frame(results_wlim), file = paste0("LINTUL2/Results/Location_", station, "_results_for_", YR, ".csv"))
    }
  }
}



run_simulation_durum <- function(station_number) {
  print("The LINTUL2 model takes about 15 minutes to run for two locations")
  # Load necessary libraries
  library(deSolve)
  
  # General settings
  wdirectory <- "./LINTUL2/weather/"
  country <- "ITA"
  SOILTYPE <- "sand"  # only sand or clay can be selected at this moment
  STTIME <- 30        # d     :     start time of simulation
  FINTIM <- 300       # d     :     finish time 
  DELT <- 1           # d     :     time step 
  # Define a vector of years from 1850 to 2014
  years <- 1850:2010
  
  # Loop through stations
  for (station in station_number) {
    # Loop through the years
    for (YR in years) {
      # Load necessary scripts and libraries
      source("LINTUL2/Components/LINTUL_parameters_durum.r")  
      source("LINTUL2/Components/LINTUL_get_weather_durum.r")  
      source("LINTUL2/Components/LINTUL2_functions.r")  
      source("LINTUL2/Components/lINTUL2_model.r")  
      require('deSolve')    # used for solving ODEs
      
      # Load weather data for the selected year
      wdata <- get_weather(directory = wdirectory, country = country, station = station, year = substr(toString(YR), 2, 4))
      
      # LINTUL without irrigation: water limited production
      LintulParms <- LINTUL2_PARAMETERS(irri = FALSE, soiltype = SOILTYPE, crop = "springwheat")
      results_wlim <- ode(LINTUL2_iniSTATES(LintulParms), 
                          seq(STTIME, FINTIM, by = DELT), 
                          LINTUL2, 
                          parms = LintulParms,
                          WDATA = wdata,
                          method = "euler")
      
      # Write the results to a file
      write.csv(data.frame(results_wlim), file = paste0("LINTUL2/Results/Location_", station, "_results_for_", YR, ".csv"))
    }
  }
}


#### Colder years #####
run_simulation_breadwheat_1899_colder <- function(station_number) {
  print("The LINTUL2 model takes about 20 minutes to run for three locations")
  # Load necessary libraries
  library(deSolve)
  
  # General settings
  wdirectory <- "./LINTUL2/weathercolder/"
  country <- "ITA"
  SOILTYPE <- "sand"  # only sand or clay can be selected at this moment
  STTIME <- 297        # d     :     start time of simulation
  FINTIM <- 572       # d     :     finish time 
  DELT <- 1           # d     :     time step 
  # Define a vector of years from 1850 to 2014
  years <- 1850:1899
  
  # Loop through stations
  for (station in station_number) {
    # Loop through the years
    for (YR in years) {
      # Load necessary scripts and libraries
      source("LINTUL2/Components/LINTUL_parameters.r")  
      source("LINTUL2/Components/LINTUL_get_weather.r")  
      source("LINTUL2/Components/LINTUL2_functions.r")  
      source("LINTUL2/Components/lINTUL2_model.r")  
      require('deSolve')    # used for solving ODEs
      
      # Load weather data for the selected year
      wdata <- get_weather(directory = wdirectory, country = country, station = station, year = substr(toString(YR), 2, 4))
      
      # LINTUL without irrigation: water limited production
      LintulParms <- LINTUL2_PARAMETERS(irri = FALSE, soiltype = SOILTYPE, crop = "springwheat")
      results_wlim <- ode(LINTUL2_iniSTATES(LintulParms), 
                          seq(STTIME, FINTIM, by = DELT), 
                          LINTUL2, 
                          parms = LintulParms,
                          WDATA = wdata,
                          method = "euler")
      
      # Write the results to a file
      write.csv(data.frame(results_wlim), file = paste0("LINTUL2/Resultscolder/Location_", station, "_results_for_", YR, ".csv"))
    }
  }
}


run_simulation_breadwheat_1905_colder <- function(station_number) {
  print("The LINTUL2 model takes about 20 minutes to run for three locations")
  # Load necessary libraries
  library(deSolve)
  
  # General settings
  wdirectory <- "./LINTUL2/weathercolder/"
  country <- "ITA"
  SOILTYPE <- "sand"  # only sand or clay can be selected at this moment
  STTIME <- 297        # d     :     start time of simulation
  FINTIM <- 572       # d     :     finish time 
  DELT <- 1           # d     :     time step 
  # Define a vector of years from 1850 to 2014
  years <- 1901:1905
  
  # Loop through stations
  for (station in station_number) {
    # Loop through the years
    for (YR in years) {
      # Load necessary scripts and libraries
      source("LINTUL2/Components/LINTUL_parameters.r")  
      source("LINTUL2/Components/LINTUL_get_weather.r")  
      source("LINTUL2/Components/LINTUL2_functions.r")  
      source("LINTUL2/Components/lINTUL2_model.r")  
      require('deSolve')    # used for solving ODEs
      
      # Load weather data for the selected year
      wdata <- get_weather(directory = wdirectory, country = country, station = station, year = substr(toString(YR), 2, 4))
      
      # LINTUL without irrigation: water limited production
      LintulParms <- LINTUL2_PARAMETERS(irri = FALSE, soiltype = SOILTYPE, crop = "springwheat")
      results_wlim <- ode(LINTUL2_iniSTATES(LintulParms), 
                          seq(STTIME, FINTIM, by = DELT), 
                          LINTUL2, 
                          parms = LintulParms,
                          WDATA = wdata,
                          method = "euler")
      
      # Write the results to a file
      write.csv(data.frame(results_wlim), file = paste0("LINTUL2/Resultscolder/Location_", station, "_results_for_", YR, ".csv"))
    }
  }
}



run_simulation_breadwheat_2013_colder <- function(station_number) {
  print("The LINTUL2 model takes about 20 minutes to run for three locations")
  # Load necessary libraries
  library(deSolve)
  
  # General settings
  wdirectory <- "./LINTUL2/weathercolder/"
  country <- "ITA"
  SOILTYPE <- "sand"  # only sand or clay can be selected at this moment
  STTIME <- 297        # d     :     start time of simulation
  FINTIM <- 572       # d     :     finish time 
  DELT <- 1           # d     :     time step 
  # Define a vector of years from 1850 to 2014
  years <- 1907:2013
  
  # Loop through stations
  for (station in station_number) {
    # Loop through the years
    for (YR in years) {
      # Load necessary scripts and libraries
      source("LINTUL2/Components/LINTUL_parameters.r")  
      source("LINTUL2/Components/LINTUL_get_weather.r")  
      source("LINTUL2/Components/LINTUL2_functions.r")  
      source("LINTUL2/Components/lINTUL2_model.r")  
      require('deSolve')    # used for solving ODEs
      
      # Load weather data for the selected year
      wdata <- get_weather(directory = wdirectory, country = country, station = station, year = substr(toString(YR), 2, 4))
      
      # LINTUL without irrigation: water limited production
      LintulParms <- LINTUL2_PARAMETERS(irri = FALSE, soiltype = SOILTYPE, crop = "springwheat")
      results_wlim <- ode(LINTUL2_iniSTATES(LintulParms), 
                          seq(STTIME, FINTIM, by = DELT), 
                          LINTUL2, 
                          parms = LintulParms,
                          WDATA = wdata,
                          method = "euler")
      
      # Write the results to a file
      write.csv(data.frame(results_wlim), file = paste0("LINTUL2/Resultscolder/Location_", station, "_results_for_", YR, ".csv"))
    }
  }
}



run_simulation_durum_1899_colder <- function(station_number) {
  print("The LINTUL2 model takes about 15 minutes to run for two locations")
  # Load necessary libraries
  library(deSolve)
  
  # General settings
  wdirectory <- "./LINTUL2/weathercolder/"
  country <- "ITA"
  SOILTYPE <- "sand"  # only sand or clay can be selected at this moment
  STTIME <- 30        # d     :     start time of simulation
  FINTIM <- 300       # d     :     finish time 
  DELT <- 1           # d     :     time step 
  # Define a vector of years from 1850 to 2014
  years <- 1850:1899
  
  # Loop through stations
  for (station in station_number) {
    # Loop through the years
    for (YR in years) {
      # Load necessary scripts and libraries
      source("LINTUL2/Components/LINTUL_parameters_durum.r")  
      source("LINTUL2/Components/LINTUL_get_weather_durum.r")  
      source("LINTUL2/Components/LINTUL2_functions.r")  
      source("LINTUL2/Components/lINTUL2_model.r")  
      require('deSolve')    # used for solving ODEs
      
      # Load weather data for the selected year
      wdata <- get_weather(directory = wdirectory, country = country, station = station, year = substr(toString(YR), 2, 4))
      
      # LINTUL without irrigation: water limited production
      LintulParms <- LINTUL2_PARAMETERS(irri = FALSE, soiltype = SOILTYPE, crop = "springwheat")
      results_wlim <- ode(LINTUL2_iniSTATES(LintulParms), 
                          seq(STTIME, FINTIM, by = DELT), 
                          LINTUL2, 
                          parms = LintulParms,
                          WDATA = wdata,
                          method = "euler")
      
      # Write the results to a file
      write.csv(data.frame(results_wlim), file = paste0("LINTUL2/Resultscolder/Location_", station, "_results_for_", YR, ".csv"))
    }
  }
}

run_simulation_durum_1905_colder <- function(station_number) {
  print("The LINTUL2 model takes about 15 minutes to run for two locations")
  # Load necessary libraries
  library(deSolve)
  
  # General settings
  wdirectory <- "./LINTUL2/weathercolder/"
  country <- "ITA"
  SOILTYPE <- "sand"  # only sand or clay can be selected at this moment
  STTIME <- 30        # d     :     start time of simulation
  FINTIM <- 300       # d     :     finish time 
  DELT <- 1           # d     :     time step 
  # Define a vector of years from 1850 to 2014
  years <- 1901:1905
  
  # Loop through stations
  for (station in station_number) {
    # Loop through the years
    for (YR in years) {
      # Load necessary scripts and libraries
      source("LINTUL2/Components/LINTUL_parameters_durum.r")  
      source("LINTUL2/Components/LINTUL_get_weather_durum.r")  
      source("LINTUL2/Components/LINTUL2_functions.r")  
      source("LINTUL2/Components/lINTUL2_model.r")  
      require('deSolve')    # used for solving ODEs
      
      # Load weather data for the selected year
      wdata <- get_weather(directory = wdirectory, country = country, station = station, year = substr(toString(YR), 2, 4))
      
      # LINTUL without irrigation: water limited production
      LintulParms <- LINTUL2_PARAMETERS(irri = FALSE, soiltype = SOILTYPE, crop = "springwheat")
      results_wlim <- ode(LINTUL2_iniSTATES(LintulParms), 
                          seq(STTIME, FINTIM, by = DELT), 
                          LINTUL2, 
                          parms = LintulParms,
                          WDATA = wdata,
                          method = "euler")
      
      # Write the results to a file
      write.csv(data.frame(results_wlim), file = paste0("LINTUL2/Resultscolder/Location_", station, "_results_for_", YR, ".csv"))
    }
  }
}

run_simulation_durum_2013_colder <- function(station_number) {
  print("The LINTUL2 model takes about 15 minutes to run for two locations")
  # Load necessary libraries
  library(deSolve)
  
  # General settings
  wdirectory <- "./LINTUL2/weathercolder/"
  country <- "ITA"
  SOILTYPE <- "sand"  # only sand or clay can be selected at this moment
  STTIME <- 30        # d     :     start time of simulation
  FINTIM <- 300       # d     :     finish time 
  DELT <- 1           # d     :     time step 
  # Define a vector of years from 1850 to 2014
  years <- 1907:2013
  
  # Loop through stations
  for (station in station_number) {
    # Loop through the years
    for (YR in years) {
      # Load necessary scripts and libraries
      source("LINTUL2/Components/LINTUL_parameters_durum.r")  
      source("LINTUL2/Components/LINTUL_get_weather_durum.r")  
      source("LINTUL2/Components/LINTUL2_functions.r")  
      source("LINTUL2/Components/lINTUL2_model.r")  
      require('deSolve')    # used for solving ODEs
      
      # Load weather data for the selected year
      wdata <- get_weather(directory = wdirectory, country = country, station = station, year = substr(toString(YR), 2, 4))
      
      # LINTUL without irrigation: water limited production
      LintulParms <- LINTUL2_PARAMETERS(irri = FALSE, soiltype = SOILTYPE, crop = "springwheat")
      results_wlim <- ode(LINTUL2_iniSTATES(LintulParms), 
                          seq(STTIME, FINTIM, by = DELT), 
                          LINTUL2, 
                          parms = LintulParms,
                          WDATA = wdata,
                          method = "euler")
      
      # Write the results to a file
      write.csv(data.frame(results_wlim), file = paste0("LINTUL2/Resultscolder/Location_", station, "_results_for_", YR, ".csv"))
    }
  }
}



#### Dummy simulation #####


run_simulation_dummy_temp <- function(station_number) {
  print("The LINTUL2 model takes about 1 minute to run for the dummy data")
  # Load necessary libraries
  library(deSolve)
  
  # General settings
  wdirectory <- "./LINTUL2/weatherdummy/"
  country <- "dummychangetemp"
  SOILTYPE <- "sand"  # only sand or clay can be selected at this moment
  STTIME <- 30        # d     :     start time of simulation
  FINTIM <- 300       # d     :     finish time 
  DELT <- 1           # d     :     time step 
  # Define a vector of years 
  years <- 1240:1260
  
  # Loop through stations
  for (station in station_number) {
    # Loop through the years
    for (YR in years) {
      # Load necessary scripts and libraries
      source("LINTUL2/Components/LINTUL_parameters_dummy.r")  
      source("LINTUL2/Components/LINTUL_get_weather_dummy.r")  
      source("LINTUL2/Components/LINTUL2_functions.r")  
      source("LINTUL2/Components/lINTUL2_model.r")  
      require('deSolve')    # used for solving ODEs
      
      # Load weather data for the selected year
      wdata <- get_weather(directory = wdirectory, country = country, station = station, year = substr(toString(YR), 2, 4))
      
      # LINTUL without irrigation: water limited production
      LintulParms <- LINTUL2_PARAMETERS(irri = FALSE, soiltype = SOILTYPE, crop = "springwheat")
      results_wlim <- ode(LINTUL2_iniSTATES(LintulParms), 
                          seq(STTIME, FINTIM, by = DELT), 
                          LINTUL2, 
                          parms = LintulParms,
                          WDATA = wdata,
                          method = "euler")
      
      # Write the results to a file
      write.csv(data.frame(results_wlim), file = paste0("LINTUL2/Resultsdummy/Location_", station, "_results_for_", YR, ".csv"))
    }
  }
}

run_simulation_dummy_precip <- function(station_number) {
  print("The LINTUL2 model takes about 1 minute to run for the dummy data")
  # Load necessary libraries
  library(deSolve)
  
  # General settings
  wdirectory <- "./LINTUL2/weatherdummy/"
  country <- "dummychangeprecip"
  SOILTYPE <- "sand"  # only sand or clay can be selected at this moment
  STTIME <- 30        # d     :     start time of simulation
  FINTIM <- 300       # d     :     finish time 
  DELT <- 1           # d     :     time step 
  # Define a vector of years 
  years <- 1140:1160
  
  # Loop through stations
  for (station in station_number) {
    # Loop through the years
    for (YR in years) {
      # Load necessary scripts and libraries
      source("LINTUL2/Components/LINTUL_parameters_dummy.r")  
      source("LINTUL2/Components/LINTUL_get_weather_dummy.r")  
      source("LINTUL2/Components/LINTUL2_functions.r")  
      source("LINTUL2/Components/lINTUL2_model.r")  
      require('deSolve')    # used for solving ODEs
      
      # Load weather data for the selected year
      wdata <- get_weather(directory = wdirectory, country = country, station = station, year = substr(toString(YR), 2, 4))
      
      # LINTUL without irrigation: water limited production
      LintulParms <- LINTUL2_PARAMETERS(irri = FALSE, soiltype = SOILTYPE, crop = "springwheat")
      results_wlim <- ode(LINTUL2_iniSTATES(LintulParms), 
                          seq(STTIME, FINTIM, by = DELT), 
                          LINTUL2, 
                          parms = LintulParms,
                          WDATA = wdata,
                          method = "euler")
      
      # Write the results to a file
      write.csv(data.frame(results_wlim), file = paste0("LINTUL2/Resultsdummy/Location_", station, "_results_for_", YR, ".csv"))
    }
  }
}