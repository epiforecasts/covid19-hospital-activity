# Run forecast
baseline_out <- forecast_baseline(data = dat, yvar = "all_adm", horizon = 14, samples = 1000,
                                  train_from = forecast_date - 42, forecast_from = forecast_date)
baseline_samples <- baseline_out$samples
baseline_summary <- baseline_out$summary
# Save forecast
baseline_name <- paste0("baseline_", forecast_date, ".rds")
saveRDS(object = baseline_samples, file = here::here("data", "out", "admissions_forecast", "samples", baseline_name))
saveRDS(object = baseline_summary, file = here::here("data", "out", "admissions_forecast", "summary", baseline_name))