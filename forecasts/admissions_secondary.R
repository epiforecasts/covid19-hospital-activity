
devtools::source_gist("https://gist.github.com/seabbs/4dad3958ca8d83daca8f02b143d152e6")

# Trust-level forecasts ---------------------------------------------------

## Load UTLA-level EpiNow2 case forecasts
case_forecast_file <- paste0("epinow_utla_", forecast_date, ".rds")
case_forecast_summary <- readRDS(file = here::here("data", "utla_case_forecast", case_forecast_file))
case_forecast_samples <- epinow_samples(df = case_forecast_summary)

## Map to Trust-level case forecasts
case_forecast_samples <- case_forecast_samples %>%
  dplyr::left_join(covid19.nhs.data::trust_utla_mapping, by = c("id" = "geo_code")) %>%
  dplyr::mutate(trust_value = p_geo*value) %>%
  dplyr::group_by(forecast_from, trust_code, date, sample) %>%
  dplyr::summarise(value = round(sum(trust_value, na.rm = TRUE)),
                   value = ifelse(is.na(value), 0, value),
                   .groups = "drop") %>%
  dplyr::filter(!is.na(trust_code))

## Reshape observed/future data to pass into `forecast_secondary()`
df_observed <- adm %>%
  dplyr::filter(date >= forecast_date - 42,
                date <= forecast_date) %>%
  dplyr::rename(region = id, primary = new_case, secondary = new_adm) %>%
  na.omit()
dt_observed <- data.table::data.table(df_observed)

df_forecast <- case_forecast_samples %>%
  dplyr::filter(date > forecast_date,
                sample <= 100,
                date <= forecast_date + 14) %>%
  dplyr::select(region = trust_code, date, sample, cases = value) %>%
  na.omit()
dt_forecast <- data.table::data.table(df_forecast)

## Fit and forecast secondary model
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
  dplyr::mutate(forecast_from = forecast_date)

secondary_summary <- secondary_forecast$summarised %>%
  dplyr::filter(date > forecast_date) %>%
  dplyr::rename(id = region,
                date_horizon = date,
                lower_0 = median) %>%
  dplyr::mutate(forecast_from = forecast_date,
                horizon = as.integer(date_horizon - forecast_from)) %>%
  dplyr::select(-c(primary, secondary, mean, sd)) %>%
  tidyr::pivot_longer(cols = -c(forecast_from, id, horizon, date_horizon), names_to = "quantile_label") %>%
  dplyr::mutate(value = round(value),
                quantile = ifelse(grepl("lower_", quantile_label),
                                  str_remove(quantile_label, "lower_"),
                                  str_remove(quantile_label, "upper_")),
                quantile = as.integer(quantile)/100)



