# Load necessary libraries
library(plumber)
library(prophet)
library(ggplot2)
library(readr)
library(dplyr)
library(lubridate)

# Load and preprocess the data (only done once at startup)
bitcoin_data <- read_csv("gemini_BTCUSD_2020_1min.csv")

# Ensure the Date column is in datetime format using lubridate
bitcoin_data$Date <- mdy_hm(bitcoin_data$Date)  # Parse Month-Day-Year Hour:Minute format

# Extract only the date part (ignoring time)
bitcoin_data$Date <- floor_date(bitcoin_data$Date, unit = "day")

# Calculate the daily average of the 'Close' prices
daily_data <- bitcoin_data |>
  group_by(Date) |>
  summarise(Close = mean(Close))

# Prepare the data for Prophet
prophet_data <- data.frame(ds = daily_data$Date, y = daily_data$Close)

# Train the Prophet model (only done once at startup)
model <- prophet(prophet_data)

#* Predict closing prices for a given date
#* @param date A future date (format: YYYY-MM-DD) to predict closing price for
#* @get /predict
function(date) {
  # Convert the input date to a Date object
  input_date <- ymd(date)
  
  # Get the last available date in the dataset
  last_date <- ymd(max(prophet_data$ds))
  
  # Validate the input date
  if (is.na(input_date)) {
    return(list(
      error = "Invalid date format. Please provide a date in YYYY-MM-DD format."
    ))
  }
  
  if (input_date <= last_date) {
    return(list(
      error = "Please provide a future date beyond the last available date in the dataset.",
      last_available_date = as.character(last_date)
    ))
  }
  
  # Calculate the number of days to forecast
  days_to_forecast <- input_date - last_date
  
  # Check if the number of days to forecast is valid
  if (days_to_forecast < 0) {
    return(list(
      error = "The calculated forecast period is negative. Please check your input."
    ))
  }
  
  # Generate future predictions
  future <- make_future_dataframe(model, periods = days_to_forecast)
  forecast <- predict(model, future)
  
  # Find the prediction for the input date
  forecast_for_date <- forecast |>
    filter(ds == input_date) |>
    select(ds, yhat, yhat_lower, yhat_upper)
  
  # Return the prediction for the given date
  if (nrow(forecast_for_date) == 0) {
    return(list(
      error = "Unable to generate forecast for the given date. Please try a valid future date."
    ))
  } else {
    return(forecast_for_date)
  }
}
