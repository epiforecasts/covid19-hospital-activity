
# Trust-level forecasts ---------------------------------------------------

ensemble_samples <- timeseries_samples(data = adm, yvar = "new_adm", models = "aez", horizon = 14, samples = 1000,
                                       train_from = forecast_date - 42, forecast_from = forecast_date)

ensemble_summary <- timeseries_summary(samples = ensemble_samples) %>%
  dplyr::mutate(model = "Ensemble (ARIMA + ETS)")
