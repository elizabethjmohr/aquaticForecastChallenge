library(neon4cast)

##---- Reset these for each submission ----##
foreacastStartDate <- "2021-08-01"
teamName <- "MSU"
##-----------------------------------------##

# Make file path names 
forecastPath <- paste0("./submissions/aquatics-", forecastStartDate ,"-", teamName, ".csv")
ymlPath <- paste0("./submissions/aquatics-", teamName, ".yml") 
xmlPath <- paste0("./submissions/aquatics-", forecastStartDate, "-", teamName, ".xml")

# Create a new metadata file
create_model_metadata(forecastPath)

# Generate metadata in EML
write_metadata_eml(forecast_file = forecastPath,
                   metadata_yaml = ymlPath, 
                   forecast_issue_time = Sys.Date(), 
                   forecast_iteration_id = "1")

# Submit forecast and metadata
Sys.setenv("AWS_DEFAULT_REGION" = "data",
           "AWS_S3_ENDPOINT" = "ecoforecast.org")
neon4cast::submit(forecast_file = forecastPath,
                  metadata = xmlPath)
