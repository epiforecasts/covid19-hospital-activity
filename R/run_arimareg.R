
# Reshape forecast data ---------------------------------------------------

dat_in <- dat %>%
  dplyr::select(id, date, adm = all_adm, case_raw = cases) %>%
  dplyr::left_join(case_forecast$summary, by = c("id", "date")) %>%
  dplyr::mutate(case_forecast = ifelse(date <= forecast_date, case_raw, case_forecast))


# Function for general lag ------------------------------------------------

run_arimareg_x <- function(lag = 7, forecast_date, dat_in){
  
  dat_in_lag <- dat_in %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(across(contains("case"), ~lag(.x, lag)))
  
  model_name <- paste0("arimareg_", lag)
  
  arima_samples <- timeseries_samples(data = dat_in_lag, yvar = "adm", xvars = "case_raw",
                                      horizon = 14, samples = 1000, models = "a",
                                      train_from = forecast_date - 42, forecast_from = forecast_date) %>%
    dplyr::mutate(model = paste0(model_name, "_observed")) %>%
    dplyr::bind_rows(timeseries_samples(data = dat_in_lag, yvar = "adm", xvars = "case_forecast",
                                        horizon = 14, samples = 1000, models = "a",
                                        train_from = forecast_date - 42, forecast_from = forecast_date) %>%
                       dplyr::mutate(model = paste0(model_name, "_forecast")))
  arima_summary <- forecast_summary(samples = arima_samples)
  
  return(list(samples = arima_samples,
              summary = arima_summary))
  
}

# Lag 0 -------------------------------------------------------------------

arimareg0 <- run_arimareg_x(dat_in = dat_in,
                            lag = 0,
                            forecast_date = forecast_date)

# Lag 4 -------------------------------------------------------------------

arimareg4 <- run_arimareg_x(dat_in = dat_in,
                            lag = 4,
                            forecast_date = forecast_date)

# Lag 5 -------------------------------------------------------------------

arimareg5 <- run_arimareg_x(dat_in = dat_in,
                            lag = 5,
                            forecast_date = forecast_date)

# Lag 6 -------------------------------------------------------------------

arimareg6 <- run_arimareg_x(dat_in = dat_in,
                            lag = 6,
                            forecast_date = forecast_date)

# Lag 7 -------------------------------------------------------------------

arimareg7 <- run_arimareg_x(dat_in = dat_in,
                            lag = 7,
                            forecast_date = forecast_date)

# Lag 10 ------------------------------------------------------------------

arimareg10 <- run_arimareg_x(dat_in = dat_in,
                            lag = 10,
                            forecast_date = forecast_date)



# Save all outputs --------------------------------------------------------

arimareg_samples <- arimareg0$samples %>%
  dplyr::bind_rows(arimareg4$samples) %>%
  dplyr::bind_rows(arimareg5$samples) %>%
  dplyr::bind_rows(arimareg6$samples) %>%
  dplyr::bind_rows(arimareg7$samples) %>%
  dplyr::bind_rows(arimareg10$samples)

arimareg_summary <- arimareg0$summary %>%
  dplyr::bind_rows(arimareg4$summary) %>%
  dplyr::bind_rows(arimareg5$summary) %>%
  dplyr::bind_rows(arimareg6$summary) %>%
  dplyr::bind_rows(arimareg7$summary) %>%
  dplyr::bind_rows(arimareg10$summary)

arimareg_name <- paste0("arimareg_", forecast_date, ".rds")

saveRDS(object = arimareg_samples, file = here::here("data", "out", "admissions_forecast", "samples", arimareg_name))
saveRDS(object = arimareg_summary, file = here::here("data", "out", "admissions_forecast", "summary", arimareg_name))

