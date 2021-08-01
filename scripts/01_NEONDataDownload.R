library(tidyverse)
library(neonstore)

# Specify NEON sites of interest
# sites <- c("BARC", "POSE")

# Download NEON data
neon_download("DP1.20288.001", site = "POSE", dir = "./data") # Water quality
neon_download("DP1.20264.001", site = "POSE", dir = "./data") # Temperature at specific depth in surface water
neon_download("DP1.20053.001", site = "POSE", dir = "./data") # Temperature at water surface 
neon_download("DP4.00130.001", site = "POSE",  dir = "./data") # Discharge
neon_download("DP1.20042.001", site = "POSE",  dir = "./data") # Photosynthetically active radiation at water surface
neon_download("DP1.00004.001", site = "POSE", dir = "./data") # Barometric pressure
neon_download("DP1.00023.001", site = "POSE",  dir = "./data") # Shortwave and longwave radiation (net radiometer)
neon_download("DP1.20016.001", site = "POSE", dir = "./data") # Elevation of surface water

# Download targets
targets <- read_csv("https://data.ecoforecast.org/targets/aquatics/aquatics-targets.csv.gz")

