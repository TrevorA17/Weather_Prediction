# Saving the classification model
saveRDS(dt_model, "./models/saved_dt_model.rds")

# Load the saved model
loaded_dt_model <- readRDS("./models/saved_dt_model.rds")

# Prepare new data for prediction
new_data <- data.frame(
  precipitation = 0.8,
  temp_max = 11.7,
  temp_min = 7.2,
  wind = 2.3
)

# Use the loaded model to make predictions
predictions_loaded_model <- predict(loaded_dt_model, newdata = new_data)

# Print predictions
print(predictions_loaded_model)
