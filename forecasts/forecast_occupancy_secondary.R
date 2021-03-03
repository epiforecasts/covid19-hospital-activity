
devtools::source_gist("https://gist.github.com/seabbs/4dad3958ca8d83daca8f02b143d152e6")
samples_dir <- here::here("forecasts", "samples")

plan(multisession)

for(forecast_date in forecast_dates){
  
  forecast_date <- as.Date(forecast_date)
  
  # Reshape observed data
  df_observed <- hosp %>%
    dplyr::filter(date >= forecast_date - 42,
                  date <= forecast_date) %>%
    dplyr::select(region = id, date, primary = all_adm, secondary = bed_occ) %>%
    na.omit()
  dt_observed <- data.table::data.table(df_observed)
  
  
  # Time series ensemble forecast
  adm_forecast_ensemble <- readRDS(file = here::here(samples_dir,
                                                     paste0("admissions_ensemble_aet_trust_", forecast_date, ".rds")))
  df_forecast_ensemble <- adm_forecast_ensemble %>%
    dplyr::mutate(date = forecast_from + horizon) %>%
    dplyr::select(region = id, date, sample, cases = value) %>%
    na.omit()
  dt_forecast_ensemble <- data.table::data.table(df_forecast_ensemble)
  
  ## Run regional_secondary with ensemble admissions forecast
  secondary_ensemble_forecast <- regional_secondary(reports = dt_observed,
                                                    case_forecast = dt_forecast_ensemble,
                                                    return_fit = FALSE,
                                                    return_plots = FALSE,
                                                    secondary = secondary_opts(type = "prevalence"),
                                                    obs = EpiNow2::obs_opts(week_effect = FALSE),
                                                    control = list(adapt_delta = 0.99, max_treedepth = 15),
                                                    verbose = TRUE)
  
  ## Reshape samples and summary
  secondary_ensemble_samples <- secondary_ensemble_forecast$samples %>%
    dplyr::mutate(value = round(value)) %>%
    dplyr::filter(date > forecast_date) %>%
    dplyr::mutate(forecast_from = forecast_date,
                  horizon = as.integer(date - forecast_from),
                  model = "epinow2_secondary_ensemble") %>%
    dplyr::select(id = region, sample, horizon, value, forecast_from, model)
  
  secondary_ensemble_summary <- timeseries_summary(samples = secondary_ensemble_samples)
  
  
  # ARIMA + cases forecast
  adm_forecast_arima <- readRDS(file = here::here(samples_dir,
                                                      paste0("admissions_arimacase4_", forecast_date, ".rds")))
  df_forecast_arima <- adm_forecast_arima %>%
    dplyr::filter(model == "arima_case4_forecast_raw") %>%
    dplyr::mutate(date = forecast_from + horizon) %>%
    dplyr::select(region = id, date, sample, cases = value) %>%
    na.omit()
  dt_forecast_arima <- data.table::data.table(df_forecast_arima)
  
  ## Run regional_secondary with ensemble admissions forecast
  secondary_arima_forecast <- regional_secondary(reports = dt_observed,
                                                 case_forecast = dt_forecast_arima,
                                                 return_fit = FALSE,
                                                 return_plots = FALSE,
                                                 secondary = secondary_opts(type = "prevalence"),
                                                 obs = EpiNow2::obs_opts(week_effect = FALSE),
                                                 control = list(adapt_delta = 0.99, max_treedepth = 15),
                                                 verbose = TRUE)
  
  ## Reshape samples and summary
  secondary_arima_samples <- secondary_arima_forecast$samples %>%
    dplyr::mutate(value = round(value)) %>%
    dplyr::filter(date > forecast_date) %>%
    dplyr::mutate(forecast_from = forecast_date,
                  horizon = as.integer(date - forecast_from),
                  model = "epinow2_secondary_arima") %>%
    dplyr::select(id = region, sample, horizon, value, forecast_from, model)
  
  secondary_arima_summary <- timeseries_summary(samples = secondary_arima_samples)
  
   
  
  # Save results
  secondary_samples <- secondary_ensemble_samples %>%
    dplyr::bind_rows(secondary_arima_samples)
  secondary_summary <- secondary_ensemble_summary %>%
    dplyr::bind_rows(secondary_arima_summary)
  forecast_name <- paste0("occupancy_secondary_", forecast_date, ".rds")
  saveRDS(object = secondary_samples, file = here::here("forecasts", "samples", forecast_name))
  saveRDS(object = secondary_summary, file = here::here("forecasts", "summary", forecast_name))
  
  
  
}
