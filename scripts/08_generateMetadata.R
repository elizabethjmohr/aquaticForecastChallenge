library(neon4cast)
create_model_metadata("./submissions/aquatics-2021-07-01-MSU_ARIMA.csv")
write_metadata_eml(forecast_file = "./submissions/aquatics-2021-07-01-MSU_ARIMA.csv",
                   metadata_yaml = "./submissions/aquatics-MSU_ARIMA.yml", 
                   forecast_issue_time = Sys.Date(), 
                   forecast_iteration_id = "1")

Sys.setenv("AWS_DEFAULT_REGION" = "data",
           "AWS_S3_ENDPOINT" = "ecoforecast.org")
neon4cast::submit(forecast_file = "./submissions/aquatics-2021-07-01-MSU_ARIMA.csv",
                  metadata = "./submissions/aquatics-2021-07-01-MSU_ARIMA.xml")
