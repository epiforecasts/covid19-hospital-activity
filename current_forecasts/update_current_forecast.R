library(tidyverse)
library(covid19.nhs.data)
library(covidregionaldata)
library(future, quietly = TRUE)

source(here::here("R", "load_data.R"))
source(here::here("R", "timeseries_fns.R"))
source(here::here("R", "forecast_fns.R"))
source(here::here("R", "utils.R"))

# Forecast dates ----------------------------------------------------------

# Forecasts made from last Sunday before current date 

today_date <- as.Date("2021-05-09")
forecast_date <- lubridate::floor_date(today_date, unit = "week", week_start = 7)


# Load raw observed data --------------------------------------------------

# Admissions (wrapper for covid19.nhs.data::download_trust_data() )
admissions <- load_hospital_data(keep_data = "all_adm", add_private = TRUE)

# Observed cases
cases_utla_obs <- covidregionaldata::get_regional_data("UK", include_level_2_regions = TRUE)
saveRDS(object = cases_utla_obs,
        file = here::here("data", "raw", "uk_utla_case.rds"))

# Forecast cases (Rt)
file_dir <- here::here("current_forecasts", "cases_utla")
recent_file_name <- paste0("cases_by_report_", forecast_date, ".csv")
cases_utla_rt <- readr::read_csv(file = here::here(file_dir, recent_file_name))


# Reshape data ------------------------------------------------------------

# Format UTLA observed cases
cases_utla_obs_in <- cases_utla_obs %>%
  dplyr::filter(grepl("E", utla_code),
                date >= as.Date("2020-08-01"),
                date <= forecast_date) %>%
  dplyr::mutate(utla_code = ifelse(utla_code == "E10000002", "E06000060", utla_code)) %>%
  dplyr::group_by(id = utla_code, date) %>%
  dplyr::summarise(cases = sum(cases_new, na.rm = TRUE))

# Map observed cases from UTLA to Trust
cases_trust_obs <- cases_utla_obs_in %>%
  dplyr::left_join(covid19.nhs.data::trust_utla_mapping, by = c("id" = "geo_code")) %>%
  dplyr::mutate(trust_value = p_geo*cases) %>%
  dplyr::group_by(trust_code, date) %>%
  dplyr::summarise(value = round(sum(trust_value, na.rm = TRUE)),
                   value = ifelse(is.na(value), 0, value),
                   .groups = "drop") %>%
  dplyr::filter(!is.na(trust_code)) %>%
  dplyr::select(id = trust_code, date, cases = value)

# Format UTLA forecast cases
cases_utla_rt_in <- cases_utla_rt %>%
  dplyr::filter(date > forecast_date) %>%
  dplyr::mutate(region = ifelse(region == "Hackney and City of London", "Hackney", region),
                region = ifelse(region == "Cornwall and Isles of Scilly", "Cornwall", region)) %>%
  dplyr::left_join(covid19.nhs.data::utla_names, by = c("region" = "geo_name")) %>%
  dplyr::mutate(geo_code = ifelse(geo_code == "E10000002", "E06000060", geo_code)) %>%
  dplyr::filter(!is.na(geo_code),
                grepl("E", geo_code)) %>%
  dplyr::rename(id = geo_code)

# Map forecasts cases (median) from UTLA to Trust
cases_trust_rt_median <- cases_utla_rt_in %>%
  dplyr::select(id, date, cases = median) %>%
  dplyr::left_join(covid19.nhs.data::trust_utla_mapping, by = c("id" = "geo_code")) %>%
  dplyr::mutate(trust_value = p_geo*cases) %>%
  dplyr::group_by(trust_code, date) %>%
  dplyr::summarise(value = round(sum(trust_value, na.rm = TRUE)),
                   value = ifelse(is.na(value), 0, value),
                   .groups = "drop") %>%
  dplyr::filter(!is.na(trust_code)) %>%
  dplyr::select(id = trust_code, date, cases = value)

cases_trust <- cases_trust_obs %>%
  dplyr::bind_rows(cases_trust_rt_median) %>%
  dplyr::arrange(id, date) %>%
  dplyr::group_by(id) %>%
  dplyr::mutate(cases_lag4 = lag(cases, 4),
                cases_lag7 = lag(cases, 7))

# Combine hospital and case data
combined_trust <- admissions %>%
  dplyr::right_join(cases_trust, by = c("id", "date")) %>%
  dplyr::filter(date >= as.Date("2020-08-01"),
                !is.na(id),
                id != "RPY")

#
cases_trust_rt_samples <- epinow_samples(df = cases_utla_rt_in) %>%
  dplyr::select(id, date, sample, cases = value) %>%
  dplyr::left_join(covid19.nhs.data::trust_utla_mapping, by = c("id" = "geo_code")) %>%
  dplyr::mutate(trust_value = p_geo*cases) %>%
  dplyr::group_by(trust_code, date, sample) %>%
  dplyr::summarise(value = round(sum(trust_value, na.rm = TRUE)),
                   value = ifelse(is.na(value), 0, value),
                   .groups = "drop") %>%
  dplyr::filter(!is.na(trust_code)) %>%
  dplyr::select(region = trust_code, date, sample, cases = value)


# Drop Trusts missing data ------------------------------------------------

drop_trusts <- combined_trust %>%
  dplyr::filter(date > forecast_date - 42,
                date <= forecast_date) %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(total_adm = sum(all_adm),
                   total_case = sum(cases)) %>%
  dplyr::filter(is.na(total_adm) | total_adm == 0 | is.na(total_case) | total_case == 0) %>%
  dplyr::pull(id)

message(paste0(c("Dropping the following Trusts for missing data or no recent activity:", drop_trusts), collapse = " "))

combined_trust <- combined_trust %>%
  filter(!id %in% drop_trusts)


# Update all forecasts ----------------------------------------------------

## Baseline forecast
baseline_out <- full_snaive(data = combined_trust, yvar = "all_adm",
                            horizon = 14, samples = 1000,
                            train_from = forecast_date - 42,
                            forecast_from = forecast_date)
baseline_summary <- baseline_out$summary


## Autoregressive time series 
tsensemble_samples <- timeseries_samples(data = combined_trust, yvar = "all_adm",
                                         horizon = 14, samples = 1000, models = "aez", 
                                         train_from = forecast_date - 42,
                                         forecast_from = forecast_date) %>%
  dplyr::mutate(model = "ts_ensemble")
tsensemble_summary <- forecast_summary(samples = tsensemble_samples)
tsensemble_summary_long <- forecast_summary(samples = tsensemble_samples,
                                            quantiles = seq(0.01, 0.99, 0.01))


## Regression + ARIMA errors
arimareg_samples <- timeseries_samples(data = combined_trust, yvar = "all_adm", xvars = "cases_lag7",
                                       horizon = 14, samples = 1000, models = "a", 
                                       train_from = forecast_date - 42,
                                       forecast_from = forecast_date) %>%
  dplyr::mutate(model = "regression_arima")
arimareg_summary <- forecast_summary(samples = arimareg_samples)
arimareg_summary_long <- forecast_summary(samples = arimareg_samples,
                                          quantiles = seq(0.01, 0.99, 0.01))


## Convolution
future::plan("multisession",  gc = TRUE, earlySignal = TRUE)
devtools::source_gist("https://gist.github.com/seabbs/4dad3958ca8d83daca8f02b143d152e6")

convolution_obs <- combined_trust %>%
  dplyr::filter(date >= forecast_date - (42+21),
                date <= forecast_date) %>%
  dplyr::select(region = id, date, primary = cases, secondary = all_adm) %>%
  na.omit()

convolution_forecast_rt <- regional_secondary(reports = convolution_obs,
                                              case_forecast = cases_trust_rt_samples,
                                              secondary = secondary_opts(type = "incidence"),
                                              obs = EpiNow2::obs_opts(week_effect = FALSE,
                                                                      scale = list(mean = 0.2, sd = 0.1)),
                                              burn_in = 21,
                                              control = list(adapt_delta = 0.99, max_treedepth = 15),
                                              return_fit = FALSE,
                                              return_plots = FALSE,
                                              verbose = TRUE)

convolution_samples <- convolution_forecast_rt$samples %>%
  dplyr::mutate(value = round(value)) %>%
  dplyr::filter(date > forecast_date) %>%
  dplyr::mutate(forecast_from = forecast_date,
                horizon = as.integer(date - forecast_from),
                model = "convolution_rt") %>%
  dplyr::select(id = region, sample, horizon, value, forecast_from, model)

convolution_summary <- forecast_summary(samples = convolution_samples)
convolution_summary_long <- forecast_summary(samples = convolution_samples,
                                             quantiles = seq(0.01, 0.99, 0.01))



# Mean ensemble -----------------------------------------------------------

## Summary
models_summary <- baseline_summary %>%
  dplyr::bind_rows(tsensemble_summary) %>%
  dplyr::bind_rows(arimareg_summary) %>%
  dplyr::bind_rows(convolution_summary)
ensemble_summary <- ensemble_forecast(model_forecasts = models_summary,
                                      models = setdiff(unique(models_summary$model), "snaive"))

forecast_out <- models_summary %>%
  dplyr::bind_rows(ensemble_summary)
saveRDS(object = forecast_out, file = here::here("current_forecasts",
                                                 "admissions_trust",
                                                 paste0("admissions_", forecast_date, ".rds")))
saveRDS(object = forecast_out, file = here::here("current_forecasts",
                                                 "admissions_trust",
                                                 "admissions_latest.rds"))

## Long summary
models_summary_long <- tsensemble_summary_long %>%
  dplyr::bind_rows(arimareg_summary_long) %>%
  dplyr::bind_rows(convolution_summary_long)
ensemble_summary_long <- ensemble_forecast(model_forecasts = models_summary_long,
                                      models = unique(models_summary_long$model))

saveRDS(object = ensemble_summary_long, file = here::here("current_forecasts",
                                                          "admissions_trust",
                                                          paste0("admissions_long_", forecast_date, ".rds")))
  

