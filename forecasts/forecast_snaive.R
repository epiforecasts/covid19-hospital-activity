
# Trust-level forecasts ---------------------------------------------------

for(forecast_date in forecast_dates){
  
  forecast_date <- as.Date(forecast_date)
  
  ## Make admissions forecast
  forecast_out <- full_snaive(data = hosp, yvar = "all_adm", horizon = 14, samples = 1000,
                              train_from = forecast_date - 42, forecast_from = forecast_date)
  snaive_samples <- forecast_out$samples
  snaive_summary <- forecast_out$summary
  
  ## Save admissions results
  forecast_name <- paste0("admissions_snaive_trust_", forecast_date, ".rds")
  saveRDS(object = snaive_samples, file = here::here("forecasts", "samples", forecast_name))
  saveRDS(object = snaive_summary, file = here::here("forecasts", "summary", forecast_name))
  
  
  ## Make occupancy forecast
  forecast_out <- full_snaive(data = hosp, yvar = "bed_occ", horizon = 14, samples = 1000,
                              train_from = forecast_date - 42, forecast_from = forecast_date)
  snaive_samples <- forecast_out$samples
  snaive_summary <- forecast_out$summary

  ## Save occupancy results
  forecast_name <- paste0("occupancy_snaive_trust_", forecast_date, ".rds")
  saveRDS(object = snaive_samples, file = here::here("forecasts", "samples", forecast_name))
  saveRDS(object = snaive_summary, file = here::here("forecasts", "summary", forecast_name))
  
}
