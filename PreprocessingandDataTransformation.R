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
