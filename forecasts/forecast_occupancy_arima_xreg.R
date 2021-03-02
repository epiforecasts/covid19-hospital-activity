
for(forecast_date in forecast_dates){
  
  forecast_date <- as.Date(forecast_date)
  
  # No lag -----------------------------------------------------------------
  
  arima_adm0_samples <- timeseries_samples(data = hosp, yvar = "bed_occ", xvars = "all_adm",
                                            horizon = 14, samples = 1000, models = "a",
                                            train_from = forecast_date - 42, forecast_from = forecast_date) %>%
    dplyr::mutate(model = "arima_adm0_observed_raw") %>%
    # dplyr::bind_rows(timeseries_samples(data = hosp, yvar = "bed_occ", xvars = "xxx",
    #                                     horizon = 14, samples = 1000, models = "a",
    #                                     train_from = forecast_date - 42, forecast_from = forecast_date) %>%
    #                    dplyr::mutate(model = "arima_adm0_forecast_raw")) %>%
    dplyr::bind_rows(timeseries_samples(data = hosp, yvar = "bed_occ", xvars = "all_adm",
                                        horizon = 14, samples = 1000, models = "a",
                                        train_from = forecast_date - 42, forecast_from = forecast_date) %>%
                       dplyr::mutate(model = "arima_adm0_observed_roll")) 
    # dplyr::bind_rows(timeseries_samples(data = hosp, yvar = "bed_occ", xvars = "xxx",
    #                                     horizon = 14, samples = 1000, models = "a",
    #                                     train_from = forecast_date - 42, forecast_from = forecast_date) %>%
    #                    dplyr::mutate(model = "arima_adm0_forecast_roll"))
  arima_adm0_summary <- timeseries_summary(samples = arima_adm0_samples)
  
  forecast_name <- paste0("occupancy_arima_adm0_", forecast_date, ".rds")
  saveRDS(object = arima_adm0_samples, file = here::here("forecasts", "samples", forecast_name))
  saveRDS(object = arima_adm0_summary, file = here::here("forecasts", "summary", forecast_name))
  
}
