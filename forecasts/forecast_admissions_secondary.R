
devtools::source_gist("https://gist.github.com/seabbs/4dad3958ca8d83daca8f02b143d152e6")

plan(multisession)

for(forecast_date in forecast_dates){
  
  forecast_date <- as.Date(forecast_date)
  
  # Reshape observed data
  df_observed <- hosp %>%
    dplyr::filter(date >= forecast_date - 42,
                  date <= forecast_date) %>%
    dplyr::select(region = id, date, primary = new_case, secondary = all_adm) %>%
    na.omit()
  dt_observed <- data.table::data.table(df_observed)
  
  
  # Time series ensemble forecast
  case_forecast_file <- paste0("epinow_utla_", forecast_date, ".rds")
  case_forecast_summary <- readRDS(file = here::here("data", "utla_case_forecast", case_forecast_file))
  case_forecast_samples <- epinow_samples(df = case_forecast_summary)

  df_forecast <- case_forecast_samples %>%
    dplyr::left_join(covid19.nhs.data::trust_utla_mapping, by = c("id" = "geo_code")) %>%
    dplyr::mutate(trust_value = p_geo*value) %>%
    dplyr::group_by(forecast_from, trust_code, date, sample) %>%
    dplyr::summarise(value = round(sum(trust_value, na.rm = TRUE)),
                     value = ifelse(is.na(value), 0, value),
                     .groups = "drop") %>%
    dplyr::filter(!is.na(trust_code)) %>%
    dplyr::select(region = trust_code, date, sample, cases = value)
  dt_forecast <- data.table::data.table(df_forecast)
  
  ## Run regional_secondary with EpiNow2 UTLA case forecast
  secondary_forecast <- regional_secondary(reports = dt_observed,
                                           case_forecast = dt_forecast,
                                           return_fit = FALSE,
                                           return_plots = FALSE,
                                           secondary = secondary_opts(type = "incidence"),
                                           obs = EpiNow2::obs_opts(week_effect = FALSE,
                                                                   scale = list(mean = 0.2, sd = 0.1)),
                                           control = list(adapt_delta = 0.99, max_treedepth = 15),
                                           verbose = TRUE)
  
  ## Reshape samples and summary
  secondary_samples <- secondary_forecast$samples %>%
    dplyr::mutate(value = round(value)) %>%
    dplyr::filter(date > forecast_date) %>%
    dplyr::mutate(forecast_from = forecast_date,
                  horizon = as.integer(date - forecast_from),
                  model = "epinow2_secondary") %>%
    dplyr::select(id = region, sample, horizon, value, forecast_from, model)
  
  secondary_summary <- timeseries_summary(samples = secondary_samples)
  
  
  # Save results
  forecast_name <- paste0("admissions_secondary_", forecast_date, ".rds")
  saveRDS(object = secondary_samples, file = here::here("forecasts", "samples", forecast_name))
  saveRDS(object = secondary_summary, file = here::here("forecasts", "summary", forecast_name))
  
  
}
