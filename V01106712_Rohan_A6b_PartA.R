# Install necessary packages
install.packages("tseries")
install.packages("rugarch")

# Load the libraries
library(tseries)
library(rugarch)

# Load the data
data <- read.csv("C:\\Users\\HP\\Desktop\\ROHAN A6\\Amazon.csv")

# Convert the Date column to Date type
data$Date <- as.Date(data$Date, format="%m/%d/%Y")

# Sort the data by Date
data <- data[order(data$Date), ]

# Calculate daily returns
data$Returns <- diff(log(data$Price)) * 100

# Drop missing values
data <- na.omit(data)

# Check for ARCH effects using the Lagrange Multiplier (LM) test
arch_test <- arch.test(ts(data$Returns), lags = 1)
print(arch_test)

# Fit a GARCH(1,1) model
spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                   mean.model = list(armaOrder = c(1, 0)))
fit <- ugarchfit(spec = spec, data = data$Returns)

# Print model summary
print(fit)

# Forecasting the next 3 months (approximately 63 trading days)
forecast_horizon <- 63
forecast <- ugarchforecast(fit, n.ahead = forecast_horizon)
volatility_forecast <- sigma(forecast)

# Print the forecasted volatility
volatility_forecast
