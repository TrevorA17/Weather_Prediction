# Load necessary library
library(readr)

# Load dataset
weather_data <- read.csv("data/seattle-weather.csv", colClasses = c(
  precipitation = "numeric",
  temp_max = "numeric",
  temp_min = "numeric",
  wind = "numeric",
  weather = "factor"
))

# Check for missing values in each column
missing_values_per_column <- colSums(is.na(weather_data))
print(missing_values_per_column)

# Check for any missing values in the entire dataset
total_missing_values <- sum(is.na(weather_data))
print(total_missing_values)

# Percentage of missing values in each column
missing_values_percentage <- colSums(is.na(weather_data)) / nrow(weather_data) * 100
print(missing_values_percentage)
