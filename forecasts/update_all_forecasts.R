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

forecast_dates <- as.character(seq.Date(from = as.Date("2020-08-02")+63, by = "week", length = 17))


# Load raw data -----------------------------------------------------------

raw_hosp <- covid19.nhs.data::download_trust_data() %>%
  dplyr::filter(data %in% c("Hosp ads & diag", "New hosp cases", "All beds COVID")) %>%
  dplyr::select(id = org_code, date, data, value) %>%
  dplyr::mutate(data = dplyr::case_when(data == "Hosp ads & diag" ~ "all_adm",
                                        data == "All beds COVID" ~ "bed_occ",
                                        data == "New hosp cases" ~ "new_adm"),
                id = ifelse(id %in% c("RD3", "RDZ"), "R0D", id)) %>%
  dplyr::group_by(id, date, data) %>%
  dplyr::summarise(value = sum(value, na.rm = TRUE),
                   .groups = "drop") %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(id_cols = -c(data, value), names_from = data)
                  
raw_case <- covidregionaldata::get_regional_data("UK", include_level_2_regions = TRUE)


# Reshape data ------------------------------------------------------------

case <- raw_case %>%
  dplyr::filter(date >= min(raw_hosp$date) - 14) %>%
  dplyr::mutate(utla_code = ifelse(utla_code == "E10000002", "E06000060", utla_code)) %>%
  dplyr::left_join(covid19.nhs.data::trust_utla_mapping, by = c("utla_code" = "geo_code")) %>%
  dplyr::mutate(trust_new_case = p_geo * cases_new) %>%
  dplyr::group_by(id = trust_code, date) %>%
  dplyr::summarise(new_case = round(sum(trust_new_case, na.rm = TRUE)),
                   .groups = "drop")

hosp <- raw_hosp %>%
  dplyr::filter(id %in% covid19.nhs.data::trust_ltla_mapping$trust_code) %>%
  dplyr::right_join(case, by = c("id", "date")) %>%
  dplyr::arrange(id, date) %>%
  dplyr::group_by(id) %>%
  tidyr::fill(bed_occ) %>%
  dplyr::filter(date >= min(raw_hosp$date),
                date <= max(raw_hosp$date),
                !is.na(id),
                id != "RPY") %>%
  dplyr::ungroup()



# Update all forecasts ----------------------------------------------------

## Baseline forecasts
source(here::here("forecasts", "forecast_snaive.R"))


## Autoregressive time series 
source(here::here("forecasts", "forecast_arima.R"))
source(here::here("forecasts", "forecast_ets.R"))
source(here::here("forecasts", "forecast_tbats.R"))
source(here::here("forecasts", "forecast_tsensemble.R"))


## Regression + ARIMA errors
source(here::here("forecasts", "forecast_admissions_arima_xreg.R"))
source(here::here("forecasts", "forecast_occupancy_arima_xreg.R"))


## EpiNow2 forecast_secondary
source(here::here("forecasts", "forecast_admissions_secondary.R"))
source(here::here("forecasts", "forecast_occupancy_secondary.R"))






