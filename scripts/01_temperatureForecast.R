library(tidyverse)
library(forecast)

# TODO: code these models into Stan to propagate uncertainty other than random error
# TODO: use particle filter for data assimilation

# Read in the target data
targets <- read_csv("https://data.ecoforecast.org/targets/aquatics/aquatics-targets.csv.gz")

# Filter to include only data from the POSE stream site
targets_POSE <- targets %>% 
  filter(siteID == "POSE")

# Convert temperature data into a time series data with NAs for each data were data are missing
dates <- seq(min(targets_POSE$time), max(targets_POSE$time), by = "days")

temperature_POSE_ts <- tibble(time = dates) %>% 
  left_join(targets_POSE, by = "time") %>%
  pull(temperature) %>%
  ts(start = c(2016, 9, 14), frequency = 365.25)

# Determine order of Fourier term order (K) that minimizes AICc for an auto-fit ARIMA model of temperature.
# Note: Fourier terms are used to represent annual seasonality

AICc <- tibble(K = 1:10, 
               AICc = NA)

for (i in 1:nrow(AICc)){
  fit <- auto.arima(temperature_POSE_ts,
                    seasonal = FALSE,
                    xreg = fourier(temperature_POSE_ts, K = i))
  AICc$AICc[i] <- fit$aicc
}

# Plot AICc vs. K
ggplot(AICc, aes(x = K, y = AICc)) +
  geom_point()

K <- which.min(AICc$AICc)

# Fit an ARIMA model to the temperature data that minimizes AICc. Model structure
# is determined automaticaly by the auto.arima function.
fit_temp <- auto.arima(temperature_POSE_ts, 
                  seasonal = FALSE, 
                  xreg = fourier(temperature_POSE_ts, K = K))

# Check residuals 
checkresiduals(fit_temp) 
# TODO: Portmonteau test suggests there is still autocorrelation of residuals... 

# Plot data and model predictions together
plot(dates, fit_temp$fitted, pch = 19)
points(dates, tibble(time = dates) %>%
           left_join(targets_POSE, by = "time") %>%
           pull(temperature), col = "blue", pch = 4)

# Use calibrated model to forecast temperature 7 days into the future
tempForecast <- fit_temp %>% 
  forecast(h = 7, xreg = fourier(temperature_POSE_ts, K = 4, h = 7), 
           bootstrap = TRUE)

# Plot forecast with previous data
autoplot(tempForecast)

# Plot forecast only with 95% prediction interval
tibble(time = seq(max(targets_POSE$time)+1, max(targets_POSE$time)+7, by = "days"), 
       tempMean = tempForecast$mean, 
       upper = as.numeric(tempForecast$upper[,2]), 
       lower = as.numeric(tempForecast$lower[,2])) %>% 
  ggplot(aes(x = time)) + 
  geom_ribbon(aes(x = time, ymax = upper, ymin = lower))+
  geom_line(aes(y = tempMean))

# Generate prediction intervals by bootstrapping residuals 
# (note: this method only accounts for process error and not uncertainty in parameter estimates)
nSamples <- 10000
h <- 7
samples <- tibble(bootstrappedResiduals = 
                      map(1:nSamples, 
                          function(x) sample(fit_temp$residuals[!is.na(fit_temp$residuals)], 
                                             size = h, 
                                             replace = TRUE)
                          )
                    )

forecast_temp <- function(data, model, residualErrors, h, K){
  forecast <- numeric(h)
  for(i in 1:h){
    forecast[i] <- predict(model, newxreg = fourier(data, K = K, h = 1))$pred[1] + residualErrors[i]
    data <- ts(c(data, forecast[i]), 
               start = start(data), 
               frequency = frequency(data))
  }
  return(forecast)
}

samples <- samples %>%
  mutate(temp_forecasts = map(bootstrappedResiduals,
                              forecast_temp, 
                              data = temperature_POSE_ts,
                              model = fit_temp,
                              K = K, 
                              h = h))

