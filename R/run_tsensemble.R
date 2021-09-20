# Run forecast(s)

## ARIMA+ETS (ae)
ae_samples <- timeseries_samples(data = dat, yvar = "all_adm", models = "ae", horizon = 14, samples = 1000,
                                  train_from = forecast_date - 42, forecast_from = forecast_date) %>%
  dplyr::mutate(model = "tsensemble_ae")
ae_summary <- forecast_summary(samples = ae_samples)

## ARIMA+ETS+baseline (aez)
aez_samples <- timeseries_samples(data = dat, yvar = "all_adm", models = "aez", horizon = 14, samples = 1000,
                                  train_from = forecast_date - 42, forecast_from = forecast_date) %>%
  dplyr::mutate(model = "tsensemble_aez")
aez_summary <- forecast_summary(samples = aez_samples)

# Save forecast(s)
tsensemble_summary <- ae_summary %>%
  dplyr::bind_rows(aez_summary)
tsensemble_samples <- ae_samples %>%
  dplyr::bind_rows(aez_samples)

tsensemble_name <- paste0("tsensemble_", forecast_date, ".rds")
saveRDS(object = tsensemble_samples, file = here::here("data", "out", "admissions_forecast", "samples", tsensemble_name))
saveRDS(object = tsensemble_summary, file = here::here("data", "out", "admissions_forecast", "summary", tsensemble_name))



