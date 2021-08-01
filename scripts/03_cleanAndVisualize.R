library(tidyverse)
library(lubridate)
library(neonstore)

#### Dissolved Oxygen ####

# Read water quality table, clean DO data, and aggregate into 10 minute intervals
DO_POSE <- neon_read(product = "DP1.20288.001", 
                             table = "waq_instantaneous", 
                             site = "POSE", 
                             dir = "./data") %>%
  filter(dissolvedOxygenFinalQF == 0, 
         sensorDepth > 0|is.na(sensorDepth)) %>%
  mutate(dateTime = as_datetime(startDateTime),
         date = as_date(dateTime),
         hour = hour(dateTime), 
         interval_10min = floor(minute(dateTime)/10)) %>%
  group_by(interval_10min, hour, date, horizontalPosition) %>%
  summarize(DO.obs = mean(dissolvedOxygen, na.rm = TRUE), 
            count = sum(!is.na(dissolvedOxygen)),
            DO.sd = mean(dissolvedOxygenExpUncert, na.rm = TRUE) /sqrt(count),.groups = "drop") %>%
  mutate(depth = 1,
         hourHalf = if_else(interval_10min %in% c(0,1,2), "First", "Second")) %>% #TODO: account for depth 
  select(horizontalPosition, date, hour, hourHalf, interval_10min, DO.obs, DO.sd, depth) 

# Plot dissolved oxygen data
# ggplot(DO_POSE, aes(x = dateTime, y = DO.obs, color = horizontalPosition)) + 
#   geom_point() +
#   facet_wrap(~siteID)+
#   xlab("Time") + 
#   ylab("Dissolved oxgyen concentration (mg/L)") + 
#   theme_bw() + 
#   ggtitle("Dissolved Oxygen at POSE")

#### Temperature ####

# Read in and clean POSE temperature data
temp_POSE <- neon_read(product = "DP1.20053.001", 
                       table ="TSW_30min", 
                       site = "POSE", 
                       dir = "./data") %>%
  filter(finalQF == 0) %>%
  mutate(dateTime = as_datetime(startDateTime) %>% force_tz("America/New_York"),
         date = as_date(dateTime),
         hour = hour(dateTime),
         hourHalf = if_else(minute(dateTime)<30, "First", "Second")) %>% # helps join with data at finer resolution
  group_by(date, hour, hourHalf, horizontalPosition) %>%
  summarize(temp.water = mean(surfWaterTempMean, na.rm = TRUE)) %>%
  select(horizontalPosition, date, hour, hourHalf, temp.water) 

# Plot POSE temperature data
# ggplot(temp_POSE, aes(x = date, y = temp.water)) + 
#   geom_point() +
#   xlab("Time") + 
#   ylab("Temperature (degrees C)") + 
#   theme_bw() + 
#   ggtitle("Temperature at POSE")

#### Discharge ####
# Load, clean, and aggregate POSE discharge data 
discharge_POSE <- neon_read(product = "DP4.00130.001",
                            table ="continuousDischarge", 
                            site = "POSE", 
                            dir = "./data")%>%
  filter(dischargeFinalQF == 0) %>%
  mutate(dateTime = as_datetime(endDate) %>% force_tz("America/New_York"),
         date = as_date(dateTime),
         hour = hour(dateTime),
         interval_10min = floor(minute(dateTime)/10)) %>%
  group_by(interval_10min, hour, date) %>%
  summarize(discharge_LperSec = mean(maxpostDischarge, na.rm = TRUE), 
            count = sum(!is.na(maxpostDischarge))) %>%
  select(date, hour, interval_10min, discharge_LperSec) 

#### PAR ####
# Load, clean, and aggregete POSE PAR data
# Note: units are micromoles per square meter per second
PAR_POSE <- neon_read(product = "DP1.20042.001",
                            table ="PARWS_1min", 
                            site = "POSE", 
                            dir = "./data")%>%
  filter(PARFinalQF == 0) %>%
  mutate(dateTime = as_datetime(startDateTime) %>% force_tz("America/New_York"),
         date = as_date(dateTime),
         hour = hour(dateTime),
         interval_10min = floor(minute(dateTime)/10)) %>%
  group_by(interval_10min, hour, date) %>%
  summarize(PARMean = mean(PARMean, na.rm = TRUE), 
            count = sum(!is.na(PARMean))) %>%
  select(date, hour, interval_10min, light = PARMean)


#### Barometric Pressure ####
# Load, clean, and aggregate barometric pressure data
# note: units of kiloPascals
barometric_POSE <- neon_read(product = "DP1.00004.001",
                            table ="BP_1min", 
                            site = "POSE", 
                            dir = "./data")%>%
  filter(corPresFinalQF == 0) %>%
  mutate(dateTime = as_datetime(startDateTime) %>% force_tz("America/New_York"),
         date = as_date(startDateTime),
         hour = hour(startDateTime),
         interval_10min = floor(minute(dateTime)/10)) %>%
  group_by(interval_10min, hour, date) %>%
  summarize(pressure = mean(staPresMean, na.rm = TRUE), 
            count = sum(!is.na(staPresMean))) %>%
  select(date, hour, interval_10min, pressure)

#### Depth ####
# Load, clean, and aggregate POSE surface water elevation data
depth_POSE <- neon_read(product = "DP1.20016.001",
                            table ="EOS_5_min",
                            site = "POSE",
                            dir = "./data")%>%
  filter(sWatElevFinalQF == 0,
         horizontalPosition %in% c("101", "102")) %>%
  mutate(dateTime = as_datetime(startDateTime) %>% force_tz("America/New_York"),
         date = as_date(dateTime),
         hour = hour(dateTime),
         interval_10min = floor(minute(dateTime)/10)) %>%
  group_by(interval_10min, hour, date, horizontalPosition) %>%
  summarize(depth = mean(surfacewaterElevMean, na.rm = TRUE)) %>%
  select(date, hour, interval_10min, depth, horizontalPosition)


#### Shortwave Radiation ####
# Load, clean, and aggregate POSE shortwave radiation data
shortwave_POSE <- neon_read(product = "DP1.00023.001",
                            table ="SLRNR_30min",
                            site = "POSE",
                            dir = "./data")%>%
  filter(inSWFinalQF == 0) %>%
  mutate(dateTime = as_datetime(startDateTime) %>% force_tz("America/New_York"),
         date = as_date(dateTime),
         hour = hour(dateTime),
         hourHalf = if_else(minute(dateTime)<30, "First", "Second")) %>% # helps join with data at finer resolution
  group_by(date, hour, hourHalf) %>%
  summarize(shortwave = mean(inSWMean, na.rm = TRUE)) %>%
  select(date, hour, hourHalf, shortwave)
