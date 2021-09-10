
# Load case forecasts -----------------------------------------------------

dat_in <- dat %>%
  dplyr::select(id, date, adm = all_adm, case_raw = cases) %>%
  dplyr::left_join(case_forecast$summary, by = c("id", "date")) %>%
  dplyr::mutate(case_forecast = ifelse(date <= forecast_date, case_raw, case_forecast))

# No lag -----------------------------------------------------------------

arima_case0_samples <- timeseries_samples(data = dat_in, yvar = "adm", xvars = "case_raw",
                                          horizon = 14, samples = 1000, models = "a",
                                          train_from = forecast_date - 42, forecast_from = forecast_date) %>%
  dplyr::mutate(model = "arima_case0_observed_raw") %>%
  dplyr::bind_rows(timeseries_samples(data = dat_in, yvar = "adm", xvars = "case_forecast",
                                      horizon = 14, samples = 1000, models = "a",
                                      train_from = forecast_date - 42, forecast_from = forecast_date) %>%
                     dplyr::mutate(model = "arima_case0_forecast_raw"))
arima_case0_summary <- forecast_summary(samples = arima_case0_samples)

arima_case0_name <- paste0("admissions_arimacase0_", forecast_date, ".rds")
saveRDS(object = arima_case0_samples, file = here::here("data", "out", "admissions_forecast", "samples", arima_case0_name))
saveRDS(object = arima_case0_summary, file = here::here("data", "out", "admissions_forecast", "summary", arima_case0_name))


# Lag 4 days --------------------------------------------------------------

dat_in4 <- dat_in %>%
  dplyr::group_by(id) %>%
  dplyr::mutate(across(contains("case"), ~lag(.x, 4)))

arima_case4_samples <- timeseries_samples(data = dat_in4, yvar = "adm", xvars = "case_raw",
                                          horizon = 14, samples = 1000, models = "a",
                                          train_from = forecast_date - 42, forecast_from = forecast_date) %>%
  dplyr::mutate(model = "arima_case4_observed_raw") %>%
  dplyr::bind_rows(timeseries_samples(data = dat_in4, yvar = "adm", xvars = "case_forecast",
                                      horizon = 14, samples = 1000, models = "a",
                                      train_from = forecast_date - 42, forecast_from = forecast_date) %>%
                     dplyr::mutate(model = "arima_case4_forecast_raw"))
arima_case4_summary <- forecast_summary(samples = arima_case4_samples)

arima_case4_name <- paste0("admissions_arimacase4_", forecast_date, ".rds")
saveRDS(object = arima_case4_samples, file = here::here("data", "out", "admissions_forecast", "samples", arima_case4_name))
saveRDS(object = arima_case4_summary, file = here::here("data", "out", "admissions_forecast", "summary", arima_case4_name))


# Lag 7 days --------------------------------------------------------------

dat_in7 <- dat_in %>%
  dplyr::group_by(id) %>%
  dplyr::mutate(across(contains("case"), ~lag(.x, 7)))

arima_case7_samples <- timeseries_samples(data = dat_in7, yvar = "adm", xvars = "case_raw",
                                          horizon = 14, samples = 1000, models = "a",
                                          train_from = forecast_date - 42, forecast_from = forecast_date) %>%
  dplyr::mutate(model = "arima_case7_observed_raw") %>%
  dplyr::bind_rows(timeseries_samples(data = dat_in7, yvar = "adm", xvars = "case_forecast",
                                      horizon = 14, samples = 1000, models = "a",
                                      train_from = forecast_date - 42, forecast_from = forecast_date) %>%
                     dplyr::mutate(model = "arima_case7_forecast_raw"))
arima_case7_summary <- forecast_summary(samples = arima_case7_samples)

arima_case7_name <- paste0("admissions_arimacase7_", forecast_date, ".rds")
saveRDS(object = arima_case7_samples, file = here::here("data", "out", "admissions_forecast", "samples", arima_case7_name))
saveRDS(object = arima_case7_summary, file = here::here("data", "out", "admissions_forecast", "summary", arima_case7_name))


# Lag 10 days -------------------------------------------------------------

dat_in10 <- dat_in %>%
  dplyr::group_by(id) %>%
  dplyr::mutate(across(contains("case"), ~lag(.x, 10)))

arima_case10_samples <- timeseries_samples(data = dat_in10, yvar = "adm", xvars = "case_raw",
                                           horizon = 14, samples = 1000, models = "a",
                                           train_from = forecast_date - 42, forecast_from = forecast_date) %>%
  dplyr::mutate(model = "arima_case10_observed_raw") %>%
  dplyr::bind_rows(timeseries_samples(data = dat_in10, yvar = "adm", xvars = "case_forecast",
                                      horizon = 14, samples = 1000, models = "a",
                                      train_from = forecast_date - 42, forecast_from = forecast_date) %>%
                     dplyr::mutate(model = "arima_case10_forecast_raw"))
arima_case10_summary <- forecast_summary(samples = arima_case10_samples)

arima_case10_name <- paste0("admissions_arimacase10_", forecast_date, ".rds")
saveRDS(object = arima_case10_samples, file = here::here("data", "out", "admissions_forecast", "samples", arima_case10_name))
saveRDS(object = arima_case10_summary, file = here::here("data", "out", "admissions_forecast", "summary", arima_case10_name))
