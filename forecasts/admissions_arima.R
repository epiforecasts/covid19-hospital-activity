
# Trust-level forecasts ---------------------------------------------------

arima_samples <- timeseries_samples(data = adm, yvar = "new_adm", models = "a", horizon = 14, samples = 1000,
                                    train_from = forecast_date - 42, forecast_from = forecast_date)

arima_summary <- timeseries_summary(samples = arima_samples) %>%
  dplyr::mutate(model = "ARIMA (AR)")
