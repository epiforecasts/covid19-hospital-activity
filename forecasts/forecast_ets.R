
# Trust-level forecasts ---------------------------------------------------

for(forecast_date in forecast_dates){
  
  forecast_date <- as.Date(forecast_date)
  
  # Admissions
  ets_samples <- timeseries_samples(data = hosp, yvar = "all_adm", models = "e", horizon = 14, samples = 1000,
                                    train_from = forecast_date - 42, forecast_from = forecast_date) %>%
    dplyr::mutate(model = "ets")
  ets_summary <- timeseries_summary(samples = ets_samples) 
  
  forecast_name <- paste0("admissions_ets_", forecast_date, ".rds")
  saveRDS(object = ets_samples, file = here::here("forecasts", "samples", forecast_name))
  saveRDS(object = ets_summary, file = here::here("forecasts", "summary", forecast_name))
  
  # Bed occupancy
  ets_samples <- timeseries_samples(data = hosp, yvar = "bed_occ", models = "e", horizon = 14, samples = 1000,
                                    train_from = forecast_date - 42, forecast_from = forecast_date) %>%
    dplyr::mutate(model = "ets")
  ets_summary <- timeseries_summary(samples = ets_samples) 
  
  forecast_name <- paste0("occupancy_ets_", forecast_date, ".rds")
  saveRDS(object = ets_samples, file = here::here("forecasts", "samples", forecast_name))
  saveRDS(object = ets_summary, file = here::here("forecasts", "summary", forecast_name))
  
}


