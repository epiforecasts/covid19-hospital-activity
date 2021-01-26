
# Trust-level forecasts ---------------------------------------------------

ets_samples <- timeseries_samples(data = adm, yvar = "new_adm", models = "e", horizon = 14, samples = 1000,
                                  train_from = forecast_date - 42, forecast_from = forecast_date)

ets_summary <- timeseries_summary(samples = ets_samples) %>%
  dplyr::mutate(model = "ETS")
