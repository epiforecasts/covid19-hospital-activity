library(tidyverse)
library(covid19.nhs.data)
library(covidregionaldata)

source(here::here("R", "timeseries_fns.R"))


# Forecast dates ----------------------------------------------------------

# Date(s) to forecast from (defined as first day of forecast)

forecast_dates <- as.Date("2021-01-01")


# Load raw data -----------------------------------------------------------

raw_adm <- covid19.nhs.data::get_admissions()
raw_case <- covidregionaldata::get_regional_data("UK", include_level_2_regions = TRUE)


# Reshape data ------------------------------------------------------------

adm <- raw_adm %>%
  dplyr::filter(trust_code %in% covid19.nhs.data::trust_ltla_mapping$trust_code) %>%
  dplyr::select(id = trust_code, date, new_adm = admissions) 



# Update all forecasts ----------------------------------------------------

## Autoregressive time series forecasts
source(here::here("forecasts", "admissions_arima.R"))
source(here::here("forecasts", "admissions_ets.R"))
source(here::here("forecasts", "admissions_ts_ensemble.R"))


## Time series forecasts with predictor(s)



## EpiNow2 forecast_secondary






