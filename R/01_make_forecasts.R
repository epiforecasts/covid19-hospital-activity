library(tidyverse)
library(covid19.nhs.data)
library(covidregionaldata)
library(future, quietly = TRUE)

source(here::here("R", "load_data_fns.R"))
source(here::here("R", "forecast_fns.R"))
source(here::here("R", "utils.R"))

devtools::source_gist("https://gist.github.com/seabbs/4dad3958ca8d83daca8f02b143d152e6")


# Set-up ------------------------------------------------------------------

future::plan("multisession",  gc = TRUE, earlySignal = TRUE)
options(mc.cores = 4)

# Forecast dates to forecast from (defined as first day of forecast)
forecast_dates <- as.character(seq.Date(from = as.Date("2020-08-02") + 63,
                                        by = "week",
                                        length = 30))

# Load observed UTLA-level cases
case_dat <- load_case_data() %>%
  dplyr::left_join(covid19.nhs.data::utla_names, by = c("id" = "geo_code"))

# Load combined Trust-level data (admissions + cases)
dat <- load_combined_data()


# Update individual model forecasts ---------------------------------------

for(forecast_date in forecast_dates){
  
  forecast_date <- as.Date(forecast_date)
  
  # Load case forecasts
  case_forecast <- load_case_forecasts(obs_case_data = case_dat,
                                       forecast_date = forecast_date,
                                       level = "trust",
                                       replace_flag = TRUE)
  
  # Baseline
  source(here::here("R", "run_baseline.R"))
  
  # Individual ts (ARIMA, ETS)
  source(here::here("R", "run_arima.R"))
  source(here::here("R", "run_ets.R"))
  
  # TS ensembles
  source(here::here("R", "run_tsensemble.R"))
  
  # ARIMA regression
  source(here::here("R", "run_arimareg.R"))
  
  # Convolution
  source(here::here("R", "run_convolution.R"))
  
}


# Make ensemble forecasts -------------------------------------------------

# Define constiuent models
ensemble_observed_models <- c("ts_ensemble_aez",
                              "arima_case7_observed_raw",
                              "convolution_observed")
ensemble_forecast_models <- c("ts_ensemble_aez",
                              "arima_case7_forecast_raw",
                              "convolution_rt")

# Load constiuent model forecasts
summary_dir <- here::here("data", "out", "admissions_forecast")
summary_files <- list.files(summary_dir)[grepl("admissions", list.files(summary_dir))]

forecast_summary <- purrr::map_df(.x = summary_files, .f = ~{
  
  out <- readRDS(file = here::here(summary_dir, .x))
  
}) %>%
  dplyr::bind_rows()

# Make ensemble
ensemble_summary <- ensemble_forecast(model_forecasts = forecast_summary,
                                      models = ensemble_observed_models) %>%
  dplyr::mutate(model = paste0(model, "_observed")) %>%
  dplyr::bind_rows(ensemble_forecast(model_forecasts = forecast_summary,
                                     models = ensemble_forecast_models) %>%
                     dplyr::mutate(model = paste0(model, "_forecast")))

forecast_name <- "admissions_ensemble.rds"
saveRDS(object = ensemble_summary, file = here::here("data", "out", "admissions_forecast", "summary", forecast_name))
