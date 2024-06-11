# Load the saved decision tree model
loaded_dt_model <- readRDS("./models/saved_dt_model.rds")

#* @apiTitle Weather Prediction Model API
#* @apiDescription Used to predict weather.

#* @param precipitation Precipitation
#* @param temp_max Maximum temperature
#* @param temp_min Minimum temperature
#* @param wind Wind speed

#* @post /predict_weather
predict_weather <- function(precipitation, temp_max, temp_min, wind) {
 
  # Prepare the input data for prediction
  new_data <- data.frame(
    precipitation = as.numeric(precipitation),
    temp_max = as.numeric(temp_max),
    temp_min = as.numeric(temp_min),
    wind = as.numeric(wind)
  )
  
  # Use the loaded model to make predictions
  predictions <- predict(loaded_dt_model, newdata = new_data)
  
  # Return the predictions
  return(predictions)
}
