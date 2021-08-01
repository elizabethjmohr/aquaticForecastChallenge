source("scripts/01_temperatureForecast.R")

# Generate time series object of POSE dissolved oxygen data
oxygen_POSE_ts <- tibble(time = dates) %>% 
  left_join(targets_POSE, by = "time") %>%
  pull(oxygen) %>%
  ts(start = c(2016, 9, 14), frequency = 365.25)

# Fit an ARIMA model to the oxygen data with temperature as a covariate
fit_DO <- auto.arima(oxygen_POSE_ts, xreg = temperature_POSE_ts)

# Check residuals
checkresiduals(fit_DO)
# TODO: residuals are definitely autocorrelated... 

# Generate prediction intervals by bootstrapping residuals and using 
# temperature forecast ensemble (Note: this only accounts for uncertainty associated with 
# process error and uncertainty associated with the temperature covariate)
forecast_DO <- function(model, tempForecast, residuals){
  forecast <- map2_dbl(
    tempForecast, 
    residuals,
    function(temp, residual, model) predict(model, newxreg = temp)$pred[1] + residual, 
    model = model
  )
  return(forecast)
}

samples <- samples %>% 
  mutate(
    bootstrappedDOResiduals = 
      map(1:nSamples, 
          function(x) sample(fit_DO$residuals[!is.na(fit_DO$residuals)], 
                             size = h, 
                             replace = TRUE)
      ),
    oxygen_forecasts = 
      map2(
        temp_forecasts, 
        bootstrappedDOResiduals,
        forecast_DO,
        model = fit_DO
      )
    )

# Put forecasts into correct format for submission
forecastDates <- seq(from = as.Date("2021-07-01"), as.Date("2021-07-07"), by = "days" )

samplesLong <- tibble(
  time = rep(forecastDates, times = nSamples), 
  siteID = "POSE", 
  ensemble = rep(1:nSamples, each = h), 
  forecast = 1,
  data_assimilation = 0, 
  oxygen = unlist(samples$oxygen_forecasts),
  temp = unlist(samples$temp_forecasts)
)

# Write out forecasts into a csv file
write.csv(x = samplesLong, 
          file = "./submissions/aquatics-2021-07-01-MSU_ARIMA.csv", 
          row.names = FALSE)

# Plot ensembles
ggplot(samplesLong, aes(x = time, 
                        y = temp, 
                        group = factor(ensemble))) +
  geom_point()

ggplot(samplesLong, aes(x = time, 
                        y = oxygen, 
                        group = factor(ensemble))) +
  geom_point()

samplesLong %>% 
  filter(ensemble %in% 1:10) %>% 
  ggplot(aes(x = time,
             y = temp, 
             group = factor(ensemble), 
             color = factor(ensemble))) + 
  geom_line()

# Plot median forecasts with 95% Prediction intervals
samplesLong %>% 
  group_by(time) %>%
  summarize(median = median(temp), 
            lower = stats::quantile(temp, 0.025),
            upper = stats::quantile(temp, 0.975)) %>%
  ggplot(aes(x = time)) + 
  geom_ribbon(aes(x = time, ymin = lower, ymax = upper), fill = "grey80")+
  geom_line(aes(y = median))

samplesLong %>% 
  group_by(time) %>%
  summarize(median = median(oxygen), 
            lower = stats::quantile(oxygen, 0.025),
            upper = stats::quantile(oxygen, 0.975)) %>%
  ggplot(aes(x = time)) + 
  geom_ribbon(aes(x = time, ymin = lower, ymax = upper), fill = "grey80")+
  geom_line(aes(y = median))

