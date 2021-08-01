
oxygen_POSE_ts <- tibble(time = dates) %>% 
  left_join(targets_POSE, by = "time") %>%
  pull(oxygen) %>%
  ts(start = c(2016, 9, 14), frequency = 365.25)

fit_DO <- auto.arima(oxygen_POSE_ts, xreg = temperature_POSE_ts)

predictDO <- function(model, tempForecast, residuals){
  forecast <- map2_dbl(
    tempForecast, 
    residuals,
    function(temp, residual, model) predict(model, newxreg = temp)$pred[1] + residual, 
    model = model
  )
  return(forecast)
}

ensembles <- ensembles %>% 
  mutate(
    bootstrappedDOResiduals = 
      map(1:nEnsembles, 
          function(x) sample(fit_DO$residuals[!is.na(fit_DO$residuals)], 
                             size = h, 
                             replace = TRUE)
      ),
    oxygen_forecasts = 
      map2(
        temp_forecasts, 
        bootstrappedDOResiduals,
        predictDO,
        model = fit_DO
      )
    )

forecastDates <- seq(from = as.Date("2021-07-01"), as.Date("2021-07-07"), by = "days" )

ensemblesLong <- tibble(
  time = rep(forecastDates, times = nEnsembles), 
  siteID = "POSE", 
  ensemble = rep(1:nEnsembles, each = h), 
  forecast = 1,
  data_assimilation = 0, 
  oxygen = unlist(ensembles$oxygen_forecasts),
  temp = unlist(ensembles$temp_forecasts)
)

write.csv(x = ensemblesLong, 
          file = "./submissions/aquatics-2021-07-01-MSU_ARIMA.csv", 
          row.names = FALSE)

ggplot(ensemblesLong, aes(x = time, 
                          y = temp, 
                          group = factor(ensemble))) +
  geom_point()

ggplot(ensemblesLong, aes(x = time, 
                          y = oxygen, 
                          group = factor(ensemble))) +
  geom_point()

ensemblesLong %>% 
  filter(ensemble %in% 1:10) %>% 
  ggplot(aes(x = time,
             y = temp, 
             group = factor(ensemble), 
             color = factor(ensemble))) + 
  geom_line()

ensemblesLong %>% 
  group_by(time) %>%
  summarize(median = median(temp), 
            lower = stats::quantile(temp, 0.025),
            upper = stats::quantile(temp, 0.975)) %>%
  ggplot(aes(x = time)) + 
  geom_ribbon(aes(x = time, ymin = lower, ymax = upper), fill = "grey80")+
  geom_line(aes(y = median))

ensemblesLong %>% 
  group_by(time) %>%
  summarize(median = median(oxygen), 
            lower = stats::quantile(oxygen, 0.025),
            upper = stats::quantile(oxygen, 0.975)) %>%
  ggplot(aes(x = time)) + 
  geom_ribbon(aes(x = time, ymin = lower, ymax = upper), fill = "grey80")+
  geom_line(aes(y = median))

