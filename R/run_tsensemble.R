# Run forecast (AEZ)
aez_samples <- timeseries_samples(data = dat, yvar = "all_adm", models = "aez", horizon = 14, samples = 1000,
                                  train_from = forecast_date - 42, forecast_from = forecast_date) %>%
  dplyr::mutate(model = "ts_ensemble_aez")
aez_summary <- forecast_summary(samples = aez_samples)
# Save forecast
aez_name <- paste0("admissions_ensemble_aez_trust_", forecast_date, ".rds")
saveRDS(object = aez_samples, file = here::here("data", "out", "admissions_forecast", "samples", aez_name))
saveRDS(object = aez_summary, file = here::here("data", "out", "admissions_forecast", "summary", aez_name))


# Run forecast (AET)
aet_samples <- timeseries_samples(data = dat, yvar = "all_adm", models = "aet", horizon = 14, samples = 1000,
                                  train_from = forecast_date - 42, forecast_from = forecast_date) %>%
  dplyr::mutate(model = "ts_ensemble_aet")
aet_summary <- forecast_summary(samples = aet_samples)
# Save forecast
aet_name <- paste0("admissions_ensemble_aet_trust_", forecast_date, ".rds")
saveRDS(object = aet_samples, file = here::here("data", "out", "admissions_forecast", "samples", aet_name))
saveRDS(object = aet_summary, file = here::here("data", "out", "admissions_forecast", "summary", aet_name))


# Run forecast (AETZ)
aetz_samples <- timeseries_samples(data = dat, yvar = "all_adm", models = "aetz", horizon = 14, samples = 1000,
                                   train_from = forecast_date - 42, forecast_from = forecast_date) %>%
  dplyr::mutate(model = "ts_ensemble_aetz")
aetz_summary <- forecast_summary(samples = aetz_samples)
# Save forecast
aetz_name <- paste0("admissions_ensemble_aetz_trust_", forecast_date, ".rds")
saveRDS(object = aetz_samples, file = here::here("data", "out", "admissions_forecast", "samples", aetz_name))
saveRDS(object = aetz_summary, file = here::here("data", "out", "admissions_forecast", "summary", aetz_name))