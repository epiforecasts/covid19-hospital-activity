# Run forecast
arima_samples <- timeseries_samples(data = dat, yvar = "all_adm", models = "a", horizon = 14, samples = 1000,
                                    train_from = forecast_date - 42, forecast_from = forecast_date) %>%
  dplyr::mutate(model = "arima")
arima_summary <- forecast_summary(samples = arima_samples) 
# Save forecast
arima_name <- paste0("admissions_arima_", forecast_date, ".rds")
saveRDS(object = arima_samples, file = here::here("data", "out", "admissions_forecast", "samples", arima_name))
saveRDS(object = arima_summary, file = here::here("data", "out", "admissions_forecast", "summary", arima_name))