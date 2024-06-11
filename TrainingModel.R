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

# Set seed for reproducibility
set.seed(123)

# Define a function for bootstrapping
boot_fn <- function(data, indices) {
  d <- data[indices, ]  # Select bootstrap sample
  fit <- lm(precipitation ~ temp_max + temp_min + wind, data = d)
  return(coef(fit))
}

# Perform bootstrapping with 1000 replicates
bootstrap_results <- boot(data = train_data, statistic = boot_fn, R = 1000)

# Print bootstrap results
print(bootstrap_results)

# Set seed for reproducibility
set.seed(123)

# Define a train control object for k-fold cross-validation
train_control <- trainControl(method = "cv", number = 10)

# Train a model using cross-validation
model <- train(precipitation ~ temp_max + temp_min + wind, data = train_data,
               method = "lm", trControl = train_control)

# Print cross-validation results
print(model)
