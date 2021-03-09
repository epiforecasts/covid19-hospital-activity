
devtools::source_gist("https://gist.github.com/seabbs/4dad3958ca8d83daca8f02b143d152e6")

offline_hosp <- readRDS(file = here::here("data", "raw", "offline_hosp.rds"))
offline_mapping <- readRDS(file = here::here("data", "raw", "offline_mapping.rds"))

for(forecast_date in forecast_dates){
  
  forecast_date <- as.Date(forecast_date)
  
  # Reshape observed data
  df_observed <- offline_hosp %>%
    dplyr::filter(date >= forecast_date - (42+21),
                  date <= forecast_date) %>%
    dplyr::select(region = id, date, primary = new_case, secondary = all_adm) %>%
    na.omit()
  dt_observed <- data.table::data.table(df_observed)
  
  
  # Observed data
  df_forecast_in <- offline_hosp %>%
    dplyr::filter(date > forecast_date,
                  date <= forecast_date + 14) %>%
    dplyr::select(region = id, date, cases = new_case)
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
                                                      secondary = secondary_opts(type = "incidence"),
                                                      obs = EpiNow2::obs_opts(week_effect = FALSE,
                                                                              scale = list(mean = 0.2, sd = 0.1)),
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
  
  
  
  # Rt case forecast
  case_forecast_file <- paste0("epinow_utla_", forecast_date, ".rds")
  case_forecast_summary <- readRDS(file = here::here("data", "utla_case_forecast", case_forecast_file))
  case_forecast_samples <- epinow_samples(df = case_forecast_summary)

  df_forecast <- case_forecast_samples %>%
    dplyr::left_join(offline_mapping, by = c("id" = "geo_code")) %>%
    # dplyr::left_join(covid19.nhs.data::trust_utla_mapping, by = c("id" = "geo_code")) %>%
    dplyr::mutate(trust_value = p_geo*value) %>%
    dplyr::group_by(forecast_from, trust_code, date, sample) %>%
    dplyr::summarise(value = round(sum(trust_value, na.rm = TRUE)),
                     value = ifelse(is.na(value), 0, value),
                     .groups = "drop") %>%
    dplyr::filter(!is.na(trust_code)) %>%
    dplyr::select(region = trust_code, date, sample, cases = value)
  dt_forecast <- data.table::data.table(df_forecast)
  
  ## Run regional_secondary with EpiNow2 UTLA case forecast
  convolution_forecast_rt <- regional_secondary(reports = dt_observed,
                                           case_forecast = dt_forecast,
                                           secondary = secondary_opts(type = "incidence"),
                                           obs = EpiNow2::obs_opts(week_effect = FALSE,
                                                                   scale = list(mean = 0.2, sd = 0.1)),
                                           burn_in = 21,
                                           control = list(adapt_delta = 0.99, max_treedepth = 15),
                                           return_fit = FALSE,
                                           return_plots = FALSE,
                                           verbose = TRUE)
  
  ## Reshape samples and summary
  convolution_samples_rt <- convolution_forecast_rt$samples %>%
    dplyr::mutate(value = round(value)) %>%
    dplyr::filter(date > forecast_date) %>%
    dplyr::mutate(forecast_from = forecast_date,
                  horizon = as.integer(date - forecast_from),
                  model = "convolution_rt") %>%
    dplyr::select(id = region, sample, horizon, value, forecast_from, model)
  
  convolution_summary_rt <- forecast_summary(samples = convolution_samples_rt)
  
  
  # Save results
  convolution_samples <- convolution_samples_observed %>%
    dplyr::bind_rows(convolution_samples_rt)
  convolution_summary <- convolution_summary_observed %>%
    dplyr::bind_rows(convolution_summary_rt)
  forecast_name <- paste0("admissions_convolution_", forecast_date, ".rds")
  saveRDS(object = convolution_samples, file = here::here("forecasts", "samples", forecast_name))
  saveRDS(object = convolution_summary, file = here::here("forecasts", "summary", forecast_name))
  
  
}
