library(tidyverse)
library(forecast)

targets_POSE <- targets %>% 
  filter(siteID == "POSE")

dates <- seq(min(targets_POSE$time), max(targets_POSE$time), by = "days")
temperature_POSE_ts <- tibble(time = dates) %>% 
  left_join(targets_POSE, by = "time") %>%
  pull(temperature) %>%
  ts(start = c(2016, 9, 14), frequency = 365.25)

AICc <- tibble(K = 1:10, 
               AICc = NA)
for (i in 1:nrow(AICc)){
  fit <- auto.arima(temperature_POSE_ts,
                    seasonal = FALSE,
                    xreg = fourier(temperature_POSE_ts, K = i))
  AICc$AICc[i] <- fit$aicc
}

ggplot(AICc, aes(x = K, y = AICc)) + 
  geom_point()

K <- which.min(AICc$AICc)

fit_temp <- auto.arima(temperature_POSE_ts, 
                  seasonal = FALSE, 
                  xreg = fourier(temperature_POSE_ts, K = K))

plot(dates, fit_temp$fitted, pch = 19)
points(dates, tibble(time = dates) %>% 
           left_join(targets_POSE, by = "time") %>%
           pull(temperature), col = "blue", pch = 4)

tempForecast <- fit_temp %>% 
  forecast(h = 7, xreg = fourier(temperature_POSE_ts, K = 4, h = 7), 
           bootstrap = TRUE)
autoplot(tempForecast)
tibble(time = seq(max(targets_POSE$time)+1, max(targets_POSE$time)+7, by = "days"), 
       tempMean = tempForecast$mean, 
       upper = as.numeric(tempForecast$upper[,2]), 
       lower = as.numeric(tempForecast$lower[,2])) %>% 
  ggplot(aes(x = time)) + 
  geom_ribbon(aes(x = time, ymax = upper, ymin = lower))+
  geom_line(aes(y = tempMean))

# Create forecast ensemble by bootstrapping residuals
nEnsembles <- 10000
h <- 7
ensembles <- tibble(bootstrappedResiduals = 
                      map(1:nEnsembles, 
                          function(x) sample(fit_temp$residuals[!is.na(fit_temp$residuals)], 
                                             size = h, 
                                             replace = TRUE)
                          )
                    )

generateEnsembleMember <- function(data, model, residuals, h, K){
  forecast <- numeric(h)
  for(i in 1:h){
    forecast[i] <- predict(model, newxreg = fourier(data, K = K, h = 1))$pred[1] + residuals[i]
    data <- ts(c(data, forecast[i]), 
               start = start(data), 
               frequency = frequency(data))
  }
  return(forecast)
}

ensembles <- ensembles %>%
  mutate(temp_forecasts = map(bootstrappedResiduals,
                              generateEnsembleMember, 
                              data = temperature_POSE_ts,
                              model = fit_temp,
                              K = K, 
                              h = h))

# Old, experimental EDM code: abandoned because EDM is not likely to be effective for 
# data with strong seasonality (Change et al., 2017), and because forecast
# skill was maximized for theta=0 (non-linear case), suggesting an AR model 
# is probably more appropriate.
# library(rEDM)
# 
# dailyTempPOSE <- tibble(
#   date = seq(min(temp_TSW$date, na.rm = TRUE), max(temp_TSW$date, na.rm = TRUE), by = 1)
# ) %>%
#   left_join(temp_TSW, by = "date") %>%
#   select(date, temperature)
# 
# lib <- c(1, floor(length(dailyTempPOSE$temperature)*0.70)) # training data
# pred <- c(floor(length(dailyTempPOSE$temperature)*0.70) +1, length(dailyTempPOSE$temperature)) #test data
# 
# # Find optimal embedding dimension
# simplex_output <- simplex(time_series = dailyTempPOSE$temperature,
#                           lib = lib,
#                           pred = pred)
# plot(simplex_output$E, simplex_output$rho, type = 'l',
#      xlab = "Embedding Dimension (E)", ylab = "Forecast Skill (rho)")
# E <- simplex_output$E[which.max(simplex_output$rho)] # embedding dimension that produces maximum skill = 4
# 
# smap_output <- s_map(time_series = dailyTempPOSE$temperature, lib = lib, pred = pred, E = E)
# plot(smap_output$theta, smap_output$rho, type = "l",
#      xlab = "Nonlinearity (theta)", ylab = "Forecast Skill (rho)")
