# Load necessary libraries
library(tidyverse)
library(lubridate)
library(forecast)
library(tseries)
library(caret)

# Load the dataset
file_path <- "C:\\Users\\HP\\Desktop\\ROHAN A6\\Amazon.csv"
data <- read_csv(file_path)

# Display the first few rows to understand the structure
head(data)

# Converting 'Date' column to Date format and sorting
data$Date <- dmy(data$Date)
data <- data %>% arrange(Date)

# Checking for missing values
sum(is.na(data))

# Handling missing values using interpolation
data <- data %>% mutate(across(where(is.numeric), ~ ifelse(is.na(.), zoo::na.approx(.), .)))

# Splitting data into train and test sets
train_data <- data %>% filter(Date < '2023-01-01')
test_data <- data %>% filter(Date >= '2023-01-01')

# Resampling the data to monthly frequency and decomposing into components
monthly_data <- train_data %>% 
  group_by(Date = floor_date(Date, "month")) %>% 
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

# Decomposing the data into trend, seasonal, and residual components
decomp_add <- decompose(ts(monthly_data$Price, frequency = 12), type = "additive")
decomp_mult <- decompose(ts(monthly_data$Price, frequency = 12), type = "multiplicative")

# Plotting the decomposed components
plot(decomp_add)
plot(decomp_mult)

# Holt-Winters model
hw_model <- HoltWinters(ts(train_data$Price, frequency = 12))
hw_forecast <- forecast(hw_model, h = 12)
plot(hw_forecast)

# ARIMA model on daily data
arima_model <- auto.arima(ts(train_data$Price, frequency = 365))
checkresiduals(arima_model)
arima_forecast <- forecast(arima_model, h = 90)
plot(arima_forecast)

# SARIMA model on daily data
sarima_model <- auto.arima(ts(train_data$Price, frequency = 365), seasonal = TRUE)
checkresiduals(sarima_model)
sarima_forecast <- forecast(sarima_model, h = 90)
plot(sarima_forecast)

# ARIMA model on monthly data
arima_monthly_model <- auto.arima(ts(monthly_data$Price, frequency = 12))
checkresiduals(arima_monthly_model)
arima_monthly_forecast <- forecast(arima_monthly_model, h = 12)
plot(arima_monthly_forecast)

# Multivariate Forecasting with Machine Learning Models
# Assuming we have additional features in the dataset (e.g., 'Open', 'High', 'Low', 'Vol.', 'Change %')

# Preparing data for LSTM
# ... (LSTM model setup using libraries like keras and tensorflow, if required)

# Random Forest and Decision Tree
rf_model <- train(Price ~ ., data = train_data, method = "rf")
rf_predictions <- predict(rf_model, newdata = test_data)
plot(test_data$Date, test_data$Price, type = 'l', col = 'blue')
lines(test_data$Date, rf_predictions, col = 'red')

dt_model <- train(Price ~ ., data = train_data, method = "rpart")
dt_predictions <- predict(dt_model, newdata = test_data)
plot(test_data$Date, test_data$Price, type = 'l', col = 'blue')
lines(test_data$Date, dt_predictions, col = 'green')

# Additional steps may include hyperparameter tuning and model evaluation
