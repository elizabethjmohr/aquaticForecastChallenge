library(tidyverse)
library(neonstore)
library(lubridate)
library(here)
source(here("functions", "noaa.R"))

foreacastStartDate <- "2021-08-01"

#### Target Data ####
targets <- read_csv("https://data.ecoforecast.org/targets/aquatics/aquatics-targets.csv.gz") 
# Water temperature and DO

#### Covariate Data ####
# Download NEON covariate data and summarize by day
neon_download("DP1.00002.001", site = "POSE",  dir = "./data") # Air temperature

# Read air temperature data files, summarize by day, and join to target
airTemp <- neon_read(product = "DP1.00002.001", 
                       table ="SAAT_30min", 
                       site = "POSE", 
                       dir = "./data") %>%
  filter(finalQF == 0) %>%
  mutate(time = as_datetime(startDateTime) %>% force_tz("America/New_York"),
         date = as_date(time)) %>%
  group_by(date) %>%
  summarize(airTemp= mean(tempSingleMean, na.rm = TRUE)) %>%
  ungroup() %>%
  select(date, airTemp) 

POSE <- targets %>%
  filter(siteID == "POSE") %>%
  left_join(airTemp, by = c("time" = "date")) %>%
  select(time, oxygen, temperature, airTemp, oxygen_sd, temperature_sd)

ggplot(POSE, aes(x = airTemp, y = temperature)) + 
  geom_point()

#### Covariate Forecast ####
# Get NOAA's Global Ensemble Forecasting System forecast
download_noaa(siteID = "POSE", interval = "1hr", date = foreacastStartDate, dir = "./data")

# Read files and put into data frame
noaaAirTempEnsForecast <- stack_noaa(dir = "./data", forecast_date = foreacastStartDate) %>%
  select(time, ensemble, air_temperature) %>%
  mutate(date = as_date(time)) %>%
  group_by(date) %>%
  summarize(airTemp = mean(air_temperature, na.rm = TRUE)) %>%
  ungroup()

# Plot ensembles
ggplot(noaaAirTempEnsForecast, aes(x = time, y = air_temperature, color = ensemble)) +
  geom_line() + 
  theme_bw() 
