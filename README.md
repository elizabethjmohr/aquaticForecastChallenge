# NEON Ecological Forecast Challenge - Aquatics theme

Herein lies my attempt to forecast daily temperature and dissolved oxygen 7 days into the future in Posey Creek (a site within the National Ecological Observatory Network, or NEON). This work is motivated by the [Ecological Forecasting Initiative Research Coordination Network NEON Forecasting Challenge](https://ecoforecast.org/efi-rcn-forecast-challenges/).

## Temperature forecast
Temperature is modeled with seasonal and nonseasonal components, similar to [Hague and Patterson, 2014](https://afspubs.onlinelibrary.wiley.com/doi/full/10.1080/02755947.2013.847879). The seasonal component is represented by six Fourier terms (three harmonics), and the non seasonal component is represented using two terms that account for the relationship between air temperature and water temperature and for serial autocorrelation. Specifically, the first non-seasonal term is calculated using a linear relationship between the air and water temperature residuals after accounting for seasonal effects, and the second non-seasonal term represents first-order autocorrelation in residuals after accounting for seasonality and air temperature. Temperature forecasts are generated using noaa ensemble air temperature forecasts.

## Oxgyen forecast
Dissolved oxygen is modeled as being linearly related with water temperature. Forecasts are generated using the water temperature forecasts generated as described above.


