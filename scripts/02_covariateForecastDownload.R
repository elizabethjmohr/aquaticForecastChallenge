library(tidyverse)
library(neon4cast)

# Get NOAA's Global Ensemble Forecasting System forecast
download_noaa(siteID = "POSE", 
              interval = "1hr",
              date = "2021-07-01",
              dir = "./data")

# Read files and put into data frame
noaaEnsembles1hr <- stack_noaa(
  dir = "./data",
  forecast_date = "2021-07-01"
) %>%
  filter(interval == "1hr")

# Extract shortwave radiation data (W/m^2)
shortWaveForecast <- noaaEnsembles1hr %>%
  select(ensemble, time, shortwave = surface_downwelling_shortwave_flux_in_air)

# Plot ensembles
ggplot(shortWaveForecast, aes(x = time, y = shortwave, color = ensemble)) +
  geom_line() + 
  theme_bw()