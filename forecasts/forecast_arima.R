
# Trust-level forecasts ---------------------------------------------------

for(forecast_date in forecast_dates){
  
  forecast_date <- as.Date(forecast_date)
  
  # Admissions
  arima_samples <- timeseries_samples(data = hosp, yvar = "all_adm", models = "a", horizon = 14, samples = 1000,
                                      train_from = forecast_date - 42, forecast_from = forecast_date) %>%
    dplyr::mutate(model = "arima")
  arima_summary <- timeseries_summary(samples = arima_samples) 
  
  forecast_name <- paste0("admissions_arima_", forecast_date, ".rds")
  saveRDS(object = arima_samples, file = here::here("forecasts", "samples", forecast_name))
  saveRDS(object = arima_summary, file = here::here("forecasts", "summary", forecast_name))
  
  # Bed occupancy
  arima_samples <- timeseries_samples(data = hosp, yvar = "bed_occ", models = "a", horizon = 14, samples = 1000,
                                      train_from = forecast_date - 42, forecast_from = forecast_date) %>%
    dplyr::mutate(model = "arima")
  arima_summary <- timeseries_summary(samples = arima_samples) 
  
  forecast_name <- paste0("occupancy_arima_", forecast_date, ".rds")
  saveRDS(object = arima_samples, file = here::here("forecasts", "samples", forecast_name))
  saveRDS(object = arima_summary, file = here::here("forecasts", "summary", forecast_name))
  
}
