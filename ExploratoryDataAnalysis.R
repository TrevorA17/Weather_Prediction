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

# Perform ANOVA for precipitation across different weather types
anova_precipitation <- aov(precipitation ~ weather, data = weather_data)
summary(anova_precipitation)

# Perform ANOVA for temp_max across different weather types
anova_temp_max <- aov(temp_max ~ weather, data = weather_data)
summary(anova_temp_max)

# Perform ANOVA for temp_min across different weather types
anova_temp_min <- aov(temp_min ~ weather, data = weather_data)
summary(anova_temp_min)

# Perform ANOVA for wind across different weather types
anova_wind <- aov(wind ~ weather, data = weather_data)
summary(anova_wind)

# Post-hoc test for precipitation
tukey_precipitation <- TukeyHSD(anova_precipitation)
print(tukey_precipitation)

# Post-hoc test for temp_max
tukey_temp_max <- TukeyHSD(anova_temp_max)
print(tukey_temp_max)

# Post-hoc test for temp_min
tukey_temp_min <- TukeyHSD(anova_temp_min)
print(tukey_temp_min)

# Post-hoc test for wind
tukey_wind <- TukeyHSD(anova_wind)
print(tukey_wind)

library(ggplot2)
# Boxplot for precipitation
ggplot(weather_data, aes(x = weather, y = precipitation)) +
  geom_boxplot() +
  labs(title = "Precipitation by Weather Type", x = "Weather", y = "Precipitation")

# Boxplot for temp_max
ggplot(weather_data, aes(x = weather, y = temp_max)) +
  geom_boxplot() +
  labs(title = "Max Temperature by Weather Type", x = "Weather", y = "Max Temperature")

# Boxplot for temp_min
ggplot(weather_data, aes(x = weather, y = temp_min)) +
  geom_boxplot() +
  labs(title = "Min Temperature by Weather Type", x = "Weather", y = "Min Temperature")

# Boxplot for wind
ggplot(weather_data, aes(x = weather, y = wind)) +
  geom_boxplot() +
  labs(title = "Wind Speed by Weather Type", x = "Weather", y = "Wind Speed")

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Histogram for precipitation
ggplot(weather_data, aes(x = precipitation)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Histogram of Precipitation", x = "Precipitation", y = "Frequency")

# Histogram for temp_max
ggplot(weather_data, aes(x = temp_max)) +
  geom_histogram(binwidth = 1, fill = "red", color = "black") +
  labs(title = "Histogram of Max Temperature", x = "Max Temperature", y = "Frequency")

# Histogram for temp_min
ggplot(weather_data, aes(x = temp_min)) +
  geom_histogram(binwidth = 1, fill = "green", color = "black") +
  labs(title = "Histogram of Min Temperature", x = "Min Temperature", y = "Frequency")

# Histogram for wind
ggplot(weather_data, aes(x = wind)) +
  geom_histogram(binwidth = 1, fill = "purple", color = "black") +
  labs(title = "Histogram of Wind Speed", x = "Wind Speed", y = "Frequency")

# Boxplot for precipitation
ggplot(weather_data, aes(x = "", y = precipitation)) +
  geom_boxplot(fill = "blue") +
  labs(title = "Boxplot of Precipitation", x = "", y = "Precipitation")

# Boxplot for temp_max
ggplot(weather_data, aes(x = "", y = temp_max)) +
  geom_boxplot(fill = "red") +
  labs(title = "Boxplot of Max Temperature", x = "", y = "Max Temperature")

# Boxplot for temp_min
ggplot(weather_data, aes(x = "", y = temp_min)) +
  geom_boxplot(fill = "green") +
  labs(title = "Boxplot of Min Temperature", x = "", y = "Min Temperature")

# Boxplot for wind
ggplot(weather_data, aes(x = "", y = wind)) +
  geom_boxplot(fill = "purple") +
  labs(title = "Boxplot of Wind Speed", x = "", y = "Wind Speed")

# Scatter plot for temp_max vs temp_min
ggplot(weather_data, aes(x = temp_min, y = temp_max, color = weather)) +
  geom_point() +
  labs(title = "Scatter Plot of Max Temp vs Min Temp", x = "Min Temperature", y = "Max Temperature")

# Scatter plot for temp_max vs wind
ggplot(weather_data, aes(x = wind, y = temp_max, color = weather)) +
  geom_point() +
  labs(title = "Scatter Plot of Max Temp vs Wind", x = "Wind Speed", y = "Max Temperature")

# Scatter plot for precipitation vs wind
ggplot(weather_data, aes(x = wind, y = precipitation, color = weather)) +
  geom_point() +
  labs(title = "Scatter Plot of Precipitation vs Wind", x = "Wind Speed", y = "Precipitation")

# Install and load GGally if not already installed
if (!require(GGally)) install.packages("GGally")
library(GGally)

# Pair plot for all numeric variables
ggpairs(weather_data, columns = c("precipitation", "temp_max", "temp_min", "wind"), 
        mapping = aes(color = weather),
        title = "Pair Plot of Weather Data")

