
# Trust-level forecasts ---------------------------------------------------

for(forecast_date in forecast_dates){
  
  forecast_date <- as.Date(forecast_date)
  
  # Admissions
  tbats_samples <- timeseries_samples(data = hosp, yvar = "all_adm", models = "t", horizon = 14, samples = 1000,
                                      train_from = forecast_date - 42, forecast_from = forecast_date) %>%
    dplyr::mutate(model = "tbats")
  tbats_summary <- timeseries_summary(samples = tbats_samples) 

  forecast_name <- paste0("admissions_tbats_", forecast_date, ".rds")
  saveRDS(object = tbats_samples, file = here::here("forecasts", "samples", forecast_name))
  saveRDS(object = tbats_summary, file = here::here("forecasts", "summary", forecast_name))
  
  # Bed occupancy
  tbats_samples <- timeseries_samples(data = hosp, yvar = "bed_occ", models = "t", horizon = 14, samples = 1000,
                                      train_from = forecast_date - 42, forecast_from = forecast_date) %>%
    dplyr::mutate(model = "tbats")
  tbats_summary <- timeseries_summary(samples = tbats_samples) 
  
  forecast_name <- paste0("occupancy_tbats_", forecast_date, ".rds")
  saveRDS(object = tbats_samples, file = here::here("forecasts", "samples", forecast_name))
  saveRDS(object = tbats_summary, file = here::here("forecasts", "summary", forecast_name))
  
}
