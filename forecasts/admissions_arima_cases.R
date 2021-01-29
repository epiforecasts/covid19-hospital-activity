
# Trust-level forecasts ---------------------------------------------------

arima_case_samples <- timeseries_samples(data = adm, yvar = "new_adm", xvars = "new_case", models = "a", horizon = 14, samples = 1000,
                                    train_from = forecast_date - 42, forecast_from = forecast_date)

arima_case_summary <- timeseries_summary(samples = forecast) %>%
  dplyr::mutate(model = "ARIMA (+cases)")