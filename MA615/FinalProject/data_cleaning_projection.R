# data_cleaning_projection.R

# Load necessary libraries
library(forecast)
library(ggplot2)
library(dplyr)

# Function to load and clean data
load_and_clean_data <- function(files) {
  all_data <- do.call(rbind, lapply(files, function(file) {
    df <- read.csv(file)
    df$Date <- as.Date(df$X)
    return(df)
  }))
  
  # Filter and aggregate data
  data_filtered <- all_data %>%
    filter(format(Date, "%Y") %in% 2020:2024) %>%
    select(Date, Tx) %>%
    na.omit()
  
  data_filtered$Month <- format(data_filtered$Date, "%Y-%m")
  monthly_data <- data_filtered %>%
    group_by(Month) %>%
    summarize(AvgTemperature = mean(Tx, na.rm = TRUE))
  
  return(ts(monthly_data$AvgTemperature, start = c(2020, 1), frequency = 12))
}

# Function to forecast and return data
forecast_temperature <- function(monthly_ts) {
  arima_model <- auto.arima(monthly_ts)
  forecast(arima_model, h = 12)
}
