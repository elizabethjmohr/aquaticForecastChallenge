library(tidyverse)
library(lubridate)
library(unitted)

# This script prepares DO data for estimating metabolism using StreamMetabolizer.
# It is currently not necessary to run it with the current forecast method.

# Join barometric pressure and discharge data to DO data 
# (these variables were measured at only one location at the POSE site)
# Join PAR and temperature data to POSE 
# (these variables were measured at two locations: 101 and 102)
# Calculate saturated DO concentration and solar time
data <- DO_POSE %>%
  left_join(barometric_POSE, by = c("date", "hour", "interval_10min")) %>%
  left_join(discharge_POSE, by = c("date", "hour", "interval_10min"))%>%
  left_join(depth_POSE, by = c("date", "hour", "interval_10min", "horizontalPosition"))%>%
  left_join(PAR_POSE, by = c("date", "hour", "interval_10min")) %>%
  left_join(temp_POSE, by = c("date", "hour", "hourHalf", "horizontalPosition")) %>%
  left_join(shortwave_POSE, by = c("date", "hour", "hourHalf")) %>%
  mutate(dateTime = make_datetime(year = year(date), 
                                  month = month(date),
                                  day = day(date), 
                                  hour = hour,
                                  min = (interval_10min*10 + 5)) %>% force_tz("America/New_York")) %>%
  mutate(pressure_mbar = pressure*10) %>%
  mutate(DO.sat = calc_DO_sat(temp.water, pressure.air = pressure_mbar)) %>%
  mutate(solar.time = calc_solar_time(dateTime, longitude = -78.147258)) %>% 
  filter(year(solar.time) >= "2019")

# Separate data by sensor location
DO_POSE_101 <- data %>%
  filter(horizontalPosition == "101") %>%
  mutate(solar.time = u(solar.time, "")) %>%
  select("solar.time", "DO.obs", "DO.sat", "depth", "temp.water", "light") %>%
  arrange(solar.time) %>%
  data.frame() %>%
  u(c("", "mgO2 L^-1", "mgO2 L^-1","m", "degC", " umol m^-2 s^-1"))

DO_POSE_102 <- data%>%
  filter(horizontalPosition == "102")%>%
  mutate(solar.time = u(solar.time, "")) %>%
  select("solar.time", "DO.obs", "DO.sat", "depth", "temp.water", "light") %>%
  arrange(solar.time) %>%
  filter(!is.na(solar.time)) %>%
  data.frame() %>%
  u(c("", "mgO2 L^-1", "mgO2 L^-1","m", "degC", " umol m^-2 s^-1"))

ggplot(DO_POSE_101, 
       aes(x = solar.time, y = DO.obs)) + 
  geom_point()

ggplot(DO_POSE_102, 
       aes(x = solar.time, y = DO.obs)) + 
  geom_point()


