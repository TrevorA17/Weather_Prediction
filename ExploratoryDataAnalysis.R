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

# Display the structure of the dataset
str(weather_data)

# View the first few rows of the dataset
head(weather_data)

# View the dataset in a separate viewer window
View(weather_data)

# Measures of Frequency

# Frequency distribution of the 'weather' column
weather_frequency <- table(weather_data$weather)
print(weather_frequency)

# Proportions of the 'weather' column
weather_proportions <- prop.table(weather_frequency)
print(weather_proportions)

# Measures of Central Tendency

# Mean
mean_precipitation <- mean(weather_data$precipitation, na.rm = TRUE)
mean_temp_max <- mean(weather_data$temp_max, na.rm = TRUE)
mean_temp_min <- mean(weather_data$temp_min, na.rm = TRUE)
mean_wind <- mean(weather_data$wind, na.rm = TRUE)

# Median
median_precipitation <- median(weather_data$precipitation, na.rm = TRUE)
median_temp_max <- median(weather_data$temp_max, na.rm = TRUE)
median_temp_min <- median(weather_data$temp_min, na.rm = TRUE)
median_wind <- median(weather_data$wind, na.rm = TRUE)

# Mode function (there can be more than one mode)
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

mode_precipitation <- get_mode(weather_data$precipitation)
mode_temp_max <- get_mode(weather_data$temp_max)
mode_temp_min <- get_mode(weather_data$temp_min)
mode_wind <- get_mode(weather_data$wind)

# Print the results
list(
  mean_precipitation = mean_precipitation,
  median_precipitation = median_precipitation,
  mode_precipitation = mode_precipitation,
  mean_temp_max = mean_temp_max,
  median_temp_max = median_temp_max,
  mode_temp_max = mode_temp_max,
  mean_temp_min = mean_temp_min,
  median_temp_min = median_temp_min,
  mode_temp_min = mode_temp_min,
  mean_wind = mean_wind,
  median_wind = median_wind,
  mode_wind = mode_wind
)

# Measures of Distribution
library(e1071)

# Range
range_precipitation <- range(weather_data$precipitation, na.rm = TRUE)
range_temp_max <- range(weather_data$temp_max, na.rm = TRUE)
range_temp_min <- range(weather_data$temp_min, na.rm = TRUE)
range_wind <- range(weather_data$wind, na.rm = TRUE)

# Variance
variance_precipitation <- var(weather_data$precipitation, na.rm = TRUE)
variance_temp_max <- var(weather_data$temp_max, na.rm = TRUE)
variance_temp_min <- var(weather_data$temp_min, na.rm = TRUE)
variance_wind <- var(weather_data$wind, na.rm = TRUE)

# Standard Deviation
sd_precipitation <- sd(weather_data$precipitation, na.rm = TRUE)
sd_temp_max <- sd(weather_data$temp_max, na.rm = TRUE)
sd_temp_min <- sd(weather_data$temp_min, na.rm = TRUE)
sd_wind <- sd(weather_data$wind, na.rm = TRUE)

# Skewness
skewness_precipitation <- skewness(weather_data$precipitation, na.rm = TRUE)
skewness_temp_max <- skewness(weather_data$temp_max, na.rm = TRUE)
skewness_temp_min <- skewness(weather_data$temp_min, na.rm = TRUE)
skewness_wind <- skewness(weather_data$wind, na.rm = TRUE)

# Kurtosis
kurtosis_precipitation <- kurtosis(weather_data$precipitation, na.rm = TRUE)
kurtosis_temp_max <- kurtosis(weather_data$temp_max, na.rm = TRUE)
kurtosis_temp_min <- kurtosis(weather_data$temp_min, na.rm = TRUE)
kurtosis_wind <- kurtosis(weather_data$wind, na.rm = TRUE)

# Print the results
list(
  range_precipitation = range_precipitation,
  variance_precipitation = variance_precipitation,
  sd_precipitation = sd_precipitation,
  skewness_precipitation = skewness_precipitation,
  kurtosis_precipitation = kurtosis_precipitation,
  range_temp_max = range_temp_max,
  variance_temp_max = variance_temp_max,
  sd_temp_max = sd_temp_max,
  skewness_temp_max = skewness_temp_max,
  kurtosis_temp_max = kurtosis_temp_max,
  range_temp_min = range_temp_min,
  variance_temp_min = variance_temp_min,
  sd_temp_min = sd_temp_min,
  skewness_temp_min = skewness_temp_min,
  kurtosis_temp_min = kurtosis_temp_min,
  range_wind = range_wind,
  variance_wind = variance_wind,
  sd_wind = sd_wind,
  skewness_wind = skewness_wind,
  kurtosis_wind = kurtosis_wind
)

# Measures of Relationship

# Correlation matrix for numeric columns
correlation_matrix <- cor(weather_data[, c("precipitation", "temp_max", "temp_min", "wind")], use = "complete.obs")
print(correlation_matrix)

# Scatter plot matrix to visualize relationships
pairs(weather_data[, c("precipitation", "temp_max", "temp_min", "wind")], 
      main = "Scatter Plot Matrix",
      col = weather_data$weather)
