
# Trust-level forecasts ---------------------------------------------------

for(forecast_date in forecast_dates){
  
  forecast_date <- as.Date(forecast_date)
  
  
  # Admissions
  
  ## AEZ ensemble
  ensemble_samples <- timeseries_samples(data = hosp, yvar = "all_adm", models = "aez", horizon = 14, samples = 1000,
                                         train_from = forecast_date - 42, forecast_from = forecast_date) %>%
    dplyr::mutate(model = "ts_ensemble_aez")
  ensemble_summary <- timeseries_summary(samples = ensemble_samples)

  forecast_name <- paste0("admissions_ensemble_aez_trust_", forecast_date, ".rds")
  saveRDS(object = ensemble_samples, file = here::here("forecasts", "samples", forecast_name))
  saveRDS(object = ensemble_summary, file = here::here("forecasts", "summary", forecast_name))
  
  ## AET ensemble
  ensemble_samples <- timeseries_samples(data = hosp, yvar = "all_adm", models = "aet", horizon = 14, samples = 1000,
                                         train_from = forecast_date - 42, forecast_from = forecast_date) %>%
    dplyr::mutate(model = "ts_ensemble_aet")
  ensemble_summary <- timeseries_summary(samples = ensemble_samples)
  
  forecast_name <- paste0("admissions_ensemble_aet_trust_", forecast_date, ".rds")
  saveRDS(object = ensemble_samples, file = here::here("forecasts", "samples", forecast_name))
  saveRDS(object = ensemble_summary, file = here::here("forecasts", "summary", forecast_name))
  
  ## AETZ ensemble
  ensemble_samples <- timeseries_samples(data = hosp, yvar = "all_adm", models = "aetz", horizon = 14, samples = 1000,
                                         train_from = forecast_date - 42, forecast_from = forecast_date) %>%
    dplyr::mutate(model = "ts_ensemble_aetz")
  ensemble_summary <- timeseries_summary(samples = ensemble_samples)
  
  forecast_name <- paste0("admissions_ensemble_aetz_trust_", forecast_date, ".rds")
  saveRDS(object = ensemble_samples, file = here::here("forecasts", "samples", forecast_name))
  saveRDS(object = ensemble_summary, file = here::here("forecasts", "summary", forecast_name))
  
  
  # Bed occupancy
  
  ## AEZ ensemble
  ensemble_samples <- timeseries_samples(data = hosp, yvar = "bed_occ", models = "aez", horizon = 14, samples = 1000,
                                         train_from = forecast_date - 42, forecast_from = forecast_date) %>%
    dplyr::mutate(model = "ts_ensemble_aez")
  ensemble_summary <- timeseries_summary(samples = ensemble_samples)
  
  forecast_name <- paste0("occupancy_ensemble_aez_", forecast_date, ".rds")
  saveRDS(object = ensemble_samples, file = here::here("forecasts", "samples", forecast_name))
  saveRDS(object = ensemble_summary, file = here::here("forecasts", "summary", forecast_name))
  
  ## AET ensemble
  ensemble_samples <- timeseries_samples(data = hosp, yvar = "bed_occ", models = "aet", horizon = 14, samples = 1000,
                                         train_from = forecast_date - 42, forecast_from = forecast_date) %>%
    dplyr::mutate(model = "ts_ensemble_aet")
  ensemble_summary <- timeseries_summary(samples = ensemble_samples)
  
  forecast_name <- paste0("occupancy_ensemble_aet_", forecast_date, ".rds")
  saveRDS(object = ensemble_samples, file = here::here("forecasts", "samples", forecast_name))
  saveRDS(object = ensemble_summary, file = here::here("forecasts", "summary", forecast_name))
  
  ## AETZ ensemble
  ensemble_samples <- timeseries_samples(data = hosp, yvar = "bed_occ", models = "aetz", horizon = 14, samples = 1000,
                                         train_from = forecast_date - 42, forecast_from = forecast_date) %>%
    dplyr::mutate(model = "ts_ensemble_aetz")
  ensemble_summary <- timeseries_summary(samples = ensemble_samples)
  
  forecast_name <- paste0("occupancy_ensemble_aetz_", forecast_date, ".rds")
  saveRDS(object = ensemble_samples, file = here::here("forecasts", "samples", forecast_name))
  saveRDS(object = ensemble_summary, file = here::here("forecasts", "summary", forecast_name))
  
}
