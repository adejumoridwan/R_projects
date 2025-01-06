# Load necessary libraries
library(prophet)
library(ggplot2)
library(readr)
library(prophet)
library(dplyr)
library(lubridate)  # For working with dates

bitcoin_data <- read_csv("gemini_BTCUSD_2020_1min.csv")



# Ensure the Date column is in datetime format using lubridate
bitcoin_data$Date <- mdy_hm(bitcoin_data$Date)  # Parse Month-Day-Year Hour:Minute format

# Extract only the date part (ignoring time)
bitcoin_data$Date <- floor_date(bitcoin_data$Date, unit = "day")

# Calculate the daily average of the 'Close' prices
daily_data <- bitcoin_data %>%
  group_by(Date) %>%
  summarise(Close = mean(Close))

# Prepare the data for Prophet
prophet_data <- data.frame(ds = daily_data$Date, y = daily_data$Close)

# Initialize and fit the Prophet model
model <- prophet(prophet_data)

# Make future predictions for the next 30 days
future <- make_future_dataframe(model, periods = 30)
forecast <- predict(model, future)

# Plot the forecast
plot(model, forecast) +
  ggtitle("Bitcoin Daily Average Closing Price Forecast") +
  xlab("Date") +
  ylab("Price")

# Plot forecast components (trend, weekly, yearly seasonality)
prophet_plot_components(model, forecast)

# Save the forecasted values
write.csv(forecast, "bitcoin_daily_forecast.csv", row.names = FALSE)

# Print the next 30 days forecast
tail(forecast[c("ds", "yhat", "yhat_lower", "yhat_upper")], 30)
