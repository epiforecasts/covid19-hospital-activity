# Run forecast
ets_samples <- timeseries_samples(data = dat, yvar = "all_adm", models = "e", horizon = 14, samples = 1000,
                                  train_from = forecast_date - 42, forecast_from = forecast_date) %>%
  dplyr::mutate(model = "ets")
ets_summary <- forecast_summary(samples = ets_samples) 
# Save forecast
ets_name <- paste0("ets_", forecast_date, ".rds")
saveRDS(object = ets_samples, file = here::here("data", "out", "admissions_forecast", "samples", ets_name))
saveRDS(object = ets_summary, file = here::here("data", "out", "admissions_forecast", "summary", ets_name))