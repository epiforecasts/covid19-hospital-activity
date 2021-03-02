
for(forecast_date in forecast_dates){
  
  forecast_date <- as.Date(forecast_date)
  
  case_forecast_file <- paste0("epinow_utla_", forecast_date, ".rds")
  case_forecast <- readRDS(file = here::here("data", "utla_case_forecast", case_forecast_file)) %>%
    dplyr::mutate(region = ifelse(region == "Hackney and City of London", "Hackney", region),
                  region = ifelse(region == "Cornwall and Isles of Scilly", "Cornwall", region)) %>%
    dplyr::left_join(covid19.nhs.data::utla_names, by = c("region" = "geo_name")) %>%
    dplyr::select(geo_code, date, case_forecast = median) %>%
    dplyr::left_join(covid19.nhs.data::trust_utla_mapping, by = "geo_code") %>%
    dplyr::mutate(case_forecast_trust = p_geo * case_forecast) %>%
    dplyr::group_by(id = trust_code, date) %>%
    dplyr::summarise(case_forecast = round(sum(case_forecast_trust, na.rm = TRUE)),
                     .groups = "drop")
  
  hosp_in <- hosp %>%
    dplyr::select(id, date, adm = all_adm, case_raw = new_case) %>%
    dplyr::left_join(case_forecast, by = c("id", "date")) %>%
    dplyr::mutate(case_forecast = ifelse(date <= forecast_date, case_raw, case_forecast)) %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(case_raw_roll7 = zoo::rollmean(x = case_raw, k = 7, align = "right", fill = NA),
                  case_forecast_roll7 = zoo::rollmean(x = case_forecast, k = 7, align = "right", fill = NA))
  
  # No lag -----------------------------------------------------------------
  
  arima_case0_samples <- timeseries_samples(data = hosp_in, yvar = "adm", xvars = "case_raw",
                                            horizon = 14, samples = 1000, models = "a",
                                            train_from = forecast_date - 42, forecast_from = forecast_date) %>%
    dplyr::mutate(model = "arima_case0_observed_raw") %>%
    dplyr::bind_rows(timeseries_samples(data = hosp_in, yvar = "adm", xvars = "case_forecast",
                                        horizon = 14, samples = 1000, models = "a",
                                        train_from = forecast_date - 42, forecast_from = forecast_date) %>%
                       dplyr::mutate(model = "arima_case0_forecast_raw")) %>%
    dplyr::bind_rows(timeseries_samples(data = hosp_in, yvar = "adm", xvars = "case_raw_roll7",
                                        horizon = 14, samples = 1000, models = "a",
                                        train_from = forecast_date - 42, forecast_from = forecast_date) %>%
                       dplyr::mutate(model = "arima_case0_observed_roll")) %>%
    dplyr::bind_rows(timeseries_samples(data = hosp_in, yvar = "adm", xvars = "case_forecast_roll7",
                                        horizon = 14, samples = 1000, models = "a",
                                        train_from = forecast_date - 42, forecast_from = forecast_date) %>%
                       dplyr::mutate(model = "arima_case0_forecast_roll"))
  arima_case0_summary <- timeseries_summary(samples = arima_case0_samples)
  
  forecast_name <- paste0("admissions_arimacase0_", forecast_date, ".rds")
  saveRDS(object = arima_case0_samples, file = here::here("forecasts", "samples", forecast_name))
  saveRDS(object = arima_case0_summary, file = here::here("forecasts", "summary", forecast_name))
  
  
  # Lag 4 days --------------------------------------------------------------
  
  hosp_in4 <- hosp_in %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(across(contains("case"), ~lag(.x, 4)))
  
  arima_case4_samples <- timeseries_samples(data = hosp_in4, yvar = "adm", xvars = "case_raw",
                                            horizon = 14, samples = 1000, models = "a",
                                            train_from = forecast_date - 42, forecast_from = forecast_date) %>%
    dplyr::mutate(model = "arima_case4_observed_raw") %>%
    dplyr::bind_rows(timeseries_samples(data = hosp_in4, yvar = "adm", xvars = "case_forecast",
                                        horizon = 14, samples = 1000, models = "a",
                                        train_from = forecast_date - 42, forecast_from = forecast_date) %>%
                       dplyr::mutate(model = "arima_case4_forecast_raw")) %>%
    dplyr::bind_rows(timeseries_samples(data = hosp_in4, yvar = "adm", xvars = "case_raw_roll7",
                                        horizon = 14, samples = 1000, models = "a",
                                        train_from = forecast_date - 42, forecast_from = forecast_date) %>%
                       dplyr::mutate(model = "arima_case4_observed_roll")) %>%
    dplyr::bind_rows(timeseries_samples(data = hosp_in4, yvar = "adm", xvars = "case_forecast_roll7",
                                        horizon = 14, samples = 1000, models = "a",
                                        train_from = forecast_date - 42, forecast_from = forecast_date) %>%
                       dplyr::mutate(model = "arima_case4_forecast_roll"))
  arima_case4_summary <- timeseries_summary(samples = arima_case4_samples)
  
  forecast_name <- paste0("admissions_arimacase4_", forecast_date, ".rds")
  saveRDS(object = arima_case4_samples, file = here::here("forecasts", "samples", forecast_name))
  saveRDS(object = arima_case4_summary, file = here::here("forecasts", "summary", forecast_name))
  

  # Lag 7 days --------------------------------------------------------------
  
  hosp_in7 <- hosp_in %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(across(contains("case"), ~lag(.x, 7)))
  
  arima_case7_samples <- timeseries_samples(data = hosp_in7, yvar = "adm", xvars = "case_raw",
                                            horizon = 14, samples = 1000, models = "a",
                                            train_from = forecast_date - 42, forecast_from = forecast_date) %>%
    dplyr::mutate(model = "arima_case7_observed_raw") %>%
    dplyr::bind_rows(timeseries_samples(data = hosp_in7, yvar = "adm", xvars = "case_forecast",
                                        horizon = 14, samples = 1000, models = "a",
                                        train_from = forecast_date - 42, forecast_from = forecast_date) %>%
                       dplyr::mutate(model = "arima_case7_forecast_raw")) %>%
    dplyr::bind_rows(timeseries_samples(data = hosp_in7, yvar = "adm", xvars = "case_raw_roll7",
                                        horizon = 14, samples = 1000, models = "a",
                                        train_from = forecast_date - 42, forecast_from = forecast_date) %>%
                       dplyr::mutate(model = "arima_case7_observed_roll")) %>%
    dplyr::bind_rows(timeseries_samples(data = hosp_in7, yvar = "adm", xvars = "case_forecast_roll7",
                                        horizon = 14, samples = 1000, models = "a",
                                        train_from = forecast_date - 42, forecast_from = forecast_date) %>%
                       dplyr::mutate(model = "arima_case7_forecast_roll"))
  arima_case7_summary <- timeseries_summary(samples = arima_case7_samples)
  
  forecast_name <- paste0("admissions_arimacase7_", forecast_date, ".rds")
  saveRDS(object = arima_case7_samples, file = here::here("forecasts", "samples", forecast_name))
  saveRDS(object = arima_case7_summary, file = here::here("forecasts", "summary", forecast_name))
  
}
