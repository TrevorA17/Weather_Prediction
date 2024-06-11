# Load necessary library
library(readr)
library(caret)
library(boot)

# Load dataset
weather_data <- read.csv("data/seattle-weather.csv", colClasses = c(
  precipitation = "numeric",
  temp_max = "numeric",
  temp_min = "numeric",
  wind = "numeric",
  weather = "factor"
))

# Set seed for reproducibility
set.seed(123)

# Split data into training (70%) and testing (30%) sets
train_index <- createDataPartition(weather_data$weather, p = 0.7, list = FALSE)
train_data <- weather_data[train_index, ]
test_data <- weather_data[-train_index, ]

# Display the dimensions of the training and testing sets
dim(train_data)
dim(test_data)
