library(tidyverse)
library(covid19.nhs.data)
library(covidregionaldata)
library(future, quietly = TRUE)

source(here::here("R", "timeseries_fns.R"))
source(here::here("R", "utils.R"))

future::plan("multisession",  gc = TRUE, earlySignal = TRUE)
options(mc.cores = 4)

# Forecast dates ----------------------------------------------------------

# Date(s) to forecast from (defined as first day of forecast)

forecast_date <- as.Date("2021-01-01")


# Load raw data -----------------------------------------------------------

raw_adm <- covid19.nhs.data::get_admissions()
raw_case <- covidregionaldata::get_regional_data("UK", include_level_2_regions = TRUE)


# Reshape data ------------------------------------------------------------

case <- raw_case %>%
  dplyr::filter(date >= min(raw_adm$date)) %>%
  dplyr::mutate(utla_code = ifelse(utla_code == "E10000002", "E06000060", utla_code)) %>%
  dplyr::left_join(covid19.nhs.data::trust_utla_mapping, by = c("utla_code" = "geo_code")) %>%
  dplyr::mutate(trust_new_case = p_geo * cases_new) %>%
  dplyr::group_by(id = trust_code, date) %>%
  dplyr::summarise(new_case = round(sum(trust_new_case, na.rm = TRUE)),
                   .groups = "drop")

adm <- raw_adm %>%
  dplyr::filter(trust_code %in% covid19.nhs.data::trust_ltla_mapping$trust_code) %>%
  dplyr::select(id = trust_code, date, new_adm = admissions) %>%
  dplyr::left_join(case, by = c("id", "date"))



# Update all forecasts ----------------------------------------------------

## Autoregressive time series forecasts
source(here::here("forecasts", "admissions_arima.R"))
source(here::here("forecasts", "admissions_ets.R"))
source(here::here("forecasts", "admissions_ts_ensemble.R"))


## Time series forecasts with predictor(s)



## EpiNow2 forecast_secondary
source(here::here("forecasts", "admissions_secondary.R"))






