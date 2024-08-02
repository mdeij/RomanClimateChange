plot_LINTULtemp_dummyyields <- function(LINTUL_dummy_temp_yields) {
  # Load the ggplot2 package
  library(ggplot2)
  
  # Give the adapted temperature values in column names
  col_names <- c("8.3", "9.3", "10.3", "11.3", "12.3", "13.3", "14.3", "15.3", "16.3", "17.3", "18.3", "19.3", "20.3", "21.3", "22.3", "23.3", "24.3", "25.3", "26.3", "27.3", "28.3")
  
  # Create a scatter plot using ggplot
  my_plot <- ggplot(data = LINTUL_dummy_temp_yields, aes(x = factor(col_names), y = Location1_WSOTHA)) +
    geom_point() +
    labs(x = "Temperature per year", y = "Yield") +
    ggtitle("Variable temperature yield LINTUL")
  
  # Save the plot as a PNG file
  ggsave("data/output/plots/dummy_LINTUL_temperature.png", plot = my_plot, width = 10, height = 6, units = "in", dpi = 300)
  
}


plot_LINTULprecip_dummyyields <- function(LINTUL_dummy_precip_yields) {
  # Load the ggplot2 package
  library(ggplot2)
  
  # Give the adapted temperature values in column names
  col_names <- seq(0.0, 2.0, 0.1)
  
  # Create a scatter plot using ggplot
  my_plot <- ggplot(data = LINTUL_dummy_precip_yields, aes(x = factor(col_names), y = Location1_WSOTHA)) +
    geom_point() +
    labs(x = "Precipitation per year", y = "Yield") +
    ggtitle("Variable precipitation yield LINTUL")
  
  # Save the plot as a PNG file
  ggsave("data/output/plots/dummy_LINTUL_precipitation.png", plot = my_plot, width = 10, height = 6, units = "in", dpi = 300)
  
}


plot_STICStemp_dummyyields <- function(STICS_dummy_yields) {
  data <- STICS_dummy_yields
  
  # Create a new column 'precip_mm_per_day' based on the year
  data$precip_mm_per_day <- (data$year - 1140) * 0.10
  
  # Plot the data
  my_plot <- ggplot(data, aes(x = precip_mm_per_day, y = precip)) +
    geom_point(size = 5, color = "black") +
    ggtitle("Yield estimates for STICS with modified precipitation") +
    xlab("Precipitation (mm per day)") +
    ylab("Yield in ton/hectare") +
    ylim(2, 4) +  # Set the y-axis limits
    theme_minimal() +
    theme(
      text = element_text(size = 18),  # Set the overall text size
      plot.title = element_text(size = 18),  # Set the title text size
      axis.text = element_text(size = 14)  # Set the axis text size
    )
  
  # Save the plot as a PNG file
  ggsave("data/output/plots/dummy_STICS_precipitation.jpg", plot = my_plot, width = 10, height = 6, units = "in", dpi = 300)
  
}

plot_STICSprecip_dummyyields <- function(STICS_dummy_yields) {
  data <- STICS_dummy_yields
  
  # Create a new column 'temp_C' based on the year. Assuming 1240 is 8.3 degrees, 1241 is 9.3 degrees, and so on.
  data$temp_C <- 8.3 + (data$year.1 - 1240)
  
  # Plot the data
  my_plot <- ggplot(data, aes(x = temp_C, y = temp)) +
    geom_point(size = 5, color = "black") +
    ggtitle("Yield estimates for STICS with modified temperature") +
    xlab("Temperature in degrees Celsius") +
    ylab("Yield in ton/hectare") +
    ylim(0, 10) +  # Set the y-axis limits
    theme_minimal() +
    theme(
      text = element_text(size = 18),  # Set the overall text size
      plot.title = element_text(size = 18),  # Set the title text size
      axis.text = element_text(size = 14)  # Set the axis text size
    )
  
  # Save the plot as a PNG file
  ggsave("data/output/plots/dummy_STICS_temperature.jpg", plot = my_plot, width = 10, height = 6, units = "in", dpi = 300)
  
}