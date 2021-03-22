
devtools::source_gist("https://gist.github.com/seabbs/4dad3958ca8d83daca8f02b143d152e6")
samples_dir <- here::here("forecasts", "samples")

offline_hosp <- readRDS(file = here::here("data", "raw", "offline_hosp.rds"))
offline_mapping <- readRDS(file = here::here("data", "raw", "offline_mapping.rds"))

for(forecast_date in forecast_dates){
  
  forecast_date <- as.Date(forecast_date)
  
  # Reshape observed data
  df_observed <- offline_hosp %>%
    dplyr::filter(date >= forecast_date - (21 + 42),
                  date <= forecast_date) %>%
    dplyr::select(region = id, date, primary = all_adm, secondary = bed_occ) %>%
    na.omit()
  dt_observed <- data.table::data.table(df_observed)
  
  
  # Observed data
  df_forecast_in <- offline_hosp %>%
    dplyr::filter(date > forecast_date,
                  date <= forecast_date + 14) %>%
    dplyr::select(region = id, date, cases = all_adm)
  df_forecast <- purrr::map_df(.x = 1:1000, .f = ~ {
    out <- df_forecast_in %>%
      dplyr::mutate(sample = as.integer(.x))
    return(out)
  }) %>%
    dplyr::bind_rows()
  dt_forecast <- data.table::data.table(df_forecast)
  
  ## Run regional_secondary with observed cases
  convolution_forecast_observed <- regional_secondary(reports = dt_observed,
                                                      case_forecast = dt_forecast,
                                                      delays = delay_opts(list(mean = 2.5, mean_sd = 0.4, 
                                                                               sd = 0.47, sd_sd = 0.2, max = 30)),
                                                      secondary = secondary_opts(type = "prevalence"),
                                                      obs = EpiNow2::obs_opts(week_effect = FALSE),
                                                      burn_in = 21,
                                                      control = list(adapt_delta = 0.99, max_treedepth = 15),
                                                      return_fit = FALSE,
                                                      return_plots = FALSE,
                                                      verbose = TRUE)
  
  ## Reshape samples and summary
  convolution_samples_observed <- convolution_forecast_observed$samples %>%
    dplyr::mutate(value = round(value)) %>%
    dplyr::filter(date > forecast_date) %>%
    dplyr::mutate(forecast_from = forecast_date,
                  horizon = as.integer(date - forecast_from),
                  model = "convolution_observed") %>%
    dplyr::select(id = region, sample, horizon, value, forecast_from, model)
  
  convolution_summary_observed <- forecast_summary(samples = convolution_samples_observed)
  
  
  # Time series ensemble forecast
  adm_forecast_ensemble <- readRDS(file = here::here(samples_dir,
                                                     paste0("admissions_ensemble_aet_", forecast_date, ".rds")))
  df_forecast_ensemble <- adm_forecast_ensemble %>%
    dplyr::mutate(date = forecast_from + horizon) %>%
    dplyr::select(region = id, date, sample, cases = value) %>%
    na.omit()
  dt_forecast_ensemble <- data.table::data.table(df_forecast_ensemble)
  
  ## Run regional_secondary with ensemble admissions forecast
  convolution_forecast_ensemble <- regional_secondary(reports = dt_observed,
                                                      case_forecast = dt_forecast_ensemble,
                                                      delays = delay_opts(list(mean = 2.5, mean_sd = 0.4, 
                                                                               sd = 0.47, sd_sd = 0.2, max = 30)),
                                                      secondary = secondary_opts(type = "prevalence"),
                                                      obs = EpiNow2::obs_opts(week_effect = FALSE),
                                                      burn_in = 21,
                                                      control = list(adapt_delta = 0.99, max_treedepth = 15),
                                                      return_fit = FALSE,
                                                      return_plots = FALSE,
                                                      verbose = TRUE)
  
  ## Reshape samples and summary
  convolution_samples_ensemble <- convolution_forecast_ensemble$samples %>%
    dplyr::mutate(value = round(value)) %>%
    dplyr::filter(date > forecast_date) %>%
    dplyr::mutate(forecast_from = forecast_date,
                  horizon = as.integer(date - forecast_from),
                  model = "convolution_ensemble") %>%
    dplyr::select(id = region, sample, horizon, value, forecast_from, model)
  
  convolution_summary_ensemble <- forecast_summary(samples = convolution_samples_ensemble)
  
  
  # ARIMA + cases forecast
  adm_forecast_arima <- readRDS(file = here::here(samples_dir,
                                                      paste0("admissions_arimacase4_", forecast_date, ".rds")))
  df_forecast_arima <- adm_forecast_arima %>%
    dplyr::filter(model == "arima_case4_forecast_raw") %>%
    dplyr::mutate(date = forecast_from + horizon) %>%
    dplyr::select(region = id, date, sample, cases = value) %>%
    na.omit()
  dt_forecast_arima <- data.table::data.table(df_forecast_arima)
  
  ## Run regional_secondary with ensemble admissions forecast
  convolution_forecast_arima <- regional_secondary(reports = dt_observed,
                                                   case_forecast = dt_forecast_arima,
                                                   delays = delay_opts(list(mean = 2.5, mean_sd = 0.4, 
                                                                            sd = 0.47, sd_sd = 0.2, max = 30)),
                                                   secondary = secondary_opts(type = "prevalence"),
                                                   obs = EpiNow2::obs_opts(week_effect = FALSE),
                                                   burn_in = 21,
                                                   control = list(adapt_delta = 0.99, max_treedepth = 15),
                                                   return_fit = FALSE,
                                                   return_plots = FALSE,
                                                   verbose = TRUE)
  
  ## Reshape samples and summary
  convolution_samples_arima <- convolution_forecast_arima$samples %>%
    dplyr::mutate(value = round(value)) %>%
    dplyr::filter(date > forecast_date) %>%
    dplyr::mutate(forecast_from = forecast_date,
                  horizon = as.integer(date - forecast_from),
                  model = "convolution_arima") %>%
    dplyr::select(id = region, sample, horizon, value, forecast_from, model)
  
  convolution_summary_arima <- forecast_summary(samples = convolution_samples_arima)
  
   
  
  # Save results
  convolution_samples <- convolution_samples_observed %>%
    dplyr::bind_rows(convolution_samples_ensemble) %>%
    dplyr::bind_rows(convolution_samples_arima)
  convolution_summary <- convolution_summary_observed %>%
    dplyr::bind_rows(convolution_summary_ensemble) %>%
    dplyr::bind_rows(convolution_summary_arima)
  forecast_name <- paste0("occupancy_convolution_", forecast_date, ".rds")
  saveRDS(object = convolution_samples, file = here::here("forecasts", "samples", forecast_name))
  saveRDS(object = convolution_summary, file = here::here("forecasts", "summary", forecast_name))
  
  
  
}
