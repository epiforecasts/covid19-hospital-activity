library(tidyverse)
library(covid19.nhs.data)
library(covidregionaldata)
library(future, quietly = TRUE)

source(here::here("R", "load_data_fns.R"))
source(here::here("R", "forecast_fns.R"))
source(here::here("R", "regional_secondary_fns.R"))
source(here::here("R", "utils.R"))

# Set-up ------------------------------------------------------------------

future::plan("multisession",  gc = TRUE, earlySignal = TRUE)
options(mc.cores = 4)

# Forecast dates to forecast from (defined as first day of forecast)
forecast_dates <- as.character(seq.Date(from = as.Date("2020-10-04"),
                                        to = as.Date("2021-04-25"),
                                        by = "week"))

# Load observed UTLA-level cases
case_dat <- load_case_data() %>%
  dplyr::left_join(covid19.nhs.data::utla_names, by = c("id" = "geo_code"))

# Load combined Trust-level data (admissions + cases)
dat <- load_combined_data(add_private = TRUE)

# List of Trust mergers
trust_mergers <- read_xlsx_quietly(path = here::here("data", "raw", "trust_mergers.xlsx"))$result %>%
  dplyr::mutate(from_date = as.Date(from_date))


# Update individual model forecasts ---------------------------------------

for(forecast_date in forecast_dates){
  
  forecast_date <- as.Date(forecast_date)
  
  # Load case forecasts
  case_forecast <- load_case_forecasts(obs_case_data = case_dat,
                                       forecast_date = forecast_date,
                                       forecast_path = here::here("data",
                                                                  "out",
                                                                  "epinow2_case_forecast"),
                                       level = "trust",
                                       replace_flag = TRUE,
                                       replace_model = "ae")
  
  # Manually 
  if(forecast_date + 14 > as.Date("2021-02-01")){
    
    dat <- dat %>%
      dplyr::filter(id != "RT3")
    
    if(forecast_date + 14 > as.Date("2021-04-01")){
      
      dat <- dat %>%
        dplyr::filter(id != "RXH")
      
    }
    
  }
  
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

# Define constituent models
ensemble_observed_models <- c("tsensemble_aez",
                              "arimareg_7_observed",
                              "convolution_observed_noprior")
ensemble_forecast_models <- c("tsensemble_aez",
                              "arimareg_7_forecast",
                              "convolution_forecast_noprior")

# Load constituent model forecasts
summary_dir <- here::here("data", "out", "admissions_forecast", "summary")
summary_files <- list.files(summary_dir)

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

# Save individual ensemble forecast summaries
for(f in forecast_dates){
  
  forecast_name <- paste0("fullensemble_", f, ".rds")
  saveRDS(object = ensemble_summary %>%
            dplyr::filter(forecast_from == as.Date(f)),
          file = here::here("data", "out", "admissions_forecast", "summary", forecast_name))
  
}

