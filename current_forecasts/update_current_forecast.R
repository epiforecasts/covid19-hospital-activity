library(tidyverse)
library(covid19.nhs.data)
library(covidregionaldata)
library(future, quietly = TRUE)

source(here::here("R", "load_data_fns.R"))
source(here::here("R", "forecast_fns.R"))
source(here::here("R", "utils.R"))

# Forecast date -----------------------------------------------------------

today_date <- as.Date("2021-05-30")
forecast_date <- lubridate::floor_date(today_date, unit = "week", week_start = 7)



# Check UTLA-level case forecasts -----------------------------------------

# Visually check current_case_forecast.pdf for flagged UTLAs

source("current_forecasts/current_case_forecast.R")

flag_trusts <- covid19.nhs.data::trust_utla_mapping %>%
  covid19.nhs.data::get_names() %>%
  dplyr::filter(geo_name %in% flag_utlas$id_name,
                p_geo > 0.2) %>%
  dplyr::group_by(trust_code, trust_name) %>%
  dplyr::summarise(n = n(),
                   utla_names = paste(geo_name, collapse = ", ")) %>%
  dplyr::mutate(label = ifelse(n == 1,
                               paste0(trust_name, " (UTLA: ", utla_names, ")" ),
                               paste0(trust_name, " (UTLAs: ", utla_names, ")" ))) %>%
  dplyr::ungroup()

# Load raw observed data --------------------------------------------------

# Combined data
raw_obs <- load_combined_data(add_private = TRUE)
obs <- raw_obs %>%
  dplyr::select(id, date, all_adm, cases) %>%
  dplyr::filter(date >= as.Date("2020-08-02"),
                date <= forecast_date)

# Forecast cases (Rt)
file_name <- paste0("cases_by_report_", forecast_date, ".csv")
case_forecast_utla_raw <- readr::read_csv(file = here::here("current_forecasts", "cases_utla", file_name))


# Reshape data ------------------------------------------------------------

# Format UTLA forecast cases
case_forecast_utla <- case_forecast_utla_raw %>%
  dplyr::filter(date > forecast_date) %>%
  dplyr::mutate(region = ifelse(region == "Hackney and City of London", "Hackney", region),
                region = ifelse(region == "Cornwall and Isles of Scilly", "Cornwall", region)) %>%
  dplyr::left_join(covid19.nhs.data::utla_names, by = c("region" = "geo_name")) %>%
  dplyr::mutate(geo_code = ifelse(geo_code == "E10000002", "E06000060", geo_code)) %>%
  dplyr::filter(!is.na(geo_code),
                grepl("E", geo_code)) %>%
  dplyr::rename(id = geo_code)

# Map forecasts cases (median) from UTLA to Trust
case_forecast_trust <- case_forecast_utla %>%
  dplyr::select(id, date, cases = median) %>%
  dplyr::left_join(covid19.nhs.data::trust_utla_mapping, by = c("id" = "geo_code")) %>%
  dplyr::mutate(trust_value = p_geo*cases) %>%
  dplyr::group_by(trust_code, date) %>%
  dplyr::summarise(value = round(sum(trust_value, na.rm = TRUE)),
                   value = ifelse(is.na(value), 0, value),
                   .groups = "drop") %>%
  dplyr::filter(!is.na(trust_code)) %>%
  dplyr::select(id = trust_code, date, cases = value)

combined_trust <- obs %>%
  dplyr::bind_rows(case_forecast_trust) %>%
  dplyr::filter(id != "RPY") %>%
  dplyr::group_by(id) %>%
  dplyr::mutate(cases_lag7 = lag(cases, 7))

#
case_forecast_trust_samples <- epinow_samples(df = case_forecast_utla) %>%
  dplyr::select(id, date, sample, cases = value) %>%
  dplyr::left_join(covid19.nhs.data::trust_utla_mapping, by = c("id" = "geo_code")) %>%
  dplyr::mutate(trust_value = p_geo*cases) %>%
  dplyr::group_by(trust_code, date, sample) %>%
  dplyr::summarise(value = round(sum(trust_value, na.rm = TRUE)),
                   value = ifelse(is.na(value), 0, value),
                   .groups = "drop") %>%
  dplyr::filter(!is.na(trust_code),
                trust_code != "RPY") %>%
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
baseline_out <- forecast_baseline(data = combined_trust, yvar = "all_adm",
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
                                              case_forecast = case_forecast_trust_samples %>%
                                                dplyr::filter(!region %in% drop_trusts),
                                              secondary = secondary_opts(type = "incidence"),
                                              obs = EpiNow2::obs_opts(week_effect = FALSE,
                                                                      scale = list(mean = 0.2, sd = 0.1)),
                                              burn_in = 21,
                                              control = list(adapt_delta = 0.99, max_treedepth = 15),
                                              return_fit = TRUE,
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

# Case-hospitalisation ratio
convolution_chr <- convolution_forecast_rt$summarised_posterior %>%
  dplyr::rename(id = region) %>%
  dplyr::select(-contains("_95"))
# TEMP: fix for column names
setnames(convolution_chr, 
         old = paste0("lower_", seq(10, 90, 10)),
         new = paste0("lower_", seq(90, 10, -10))
)
saveRDS(convolution_chr, file = here::here("current_forecasts",
                                           "chr",
                                           paste0("chr_", forecast_date, ".rds")))




# Ensemble ----------------------------------------------------------------

# Combined model summaries
models_summary <- baseline_summary %>%
  dplyr::bind_rows(tsensemble_summary) %>%
  dplyr::bind_rows(arimareg_summary) %>%
  dplyr::bind_rows(convolution_summary)
# Make ensemble; TEMP: drop convolution model from flagged Trusts
ensemble_summary <- ensemble_forecast(model_forecasts = models_summary %>% dplyr::filter(!id %in% flag_trusts$trust_code),
                                      models = setdiff(unique(models_summary$model), "snaive")) %>%
  dplyr::mutate(ensemble_models = paste(setdiff(unique(models_summary$model), "snaive"),
                                        collapse = ", "))
ensemble_summary_flag <- ensemble_forecast(model_forecasts = models_summary %>% dplyr::filter(id %in% flag_trusts$trust_code),
                                           models = setdiff(unique(models_summary$model), c("snaive", "convolution_rt"))) %>%
  dplyr::mutate(ensemble_models = paste(setdiff(unique(models_summary$model), c("snaive", "convolution_rt")),
                                        collapse = ", "))
# Add ensemble to models summary
forecast_out <- models_summary %>%
  dplyr::bind_rows(ensemble_summary) %>%
  dplyr::bind_rows(ensemble_summary_flag)
saveRDS(object = forecast_out, file = here::here("current_forecasts",
                                                 "admissions_trust",
                                                 paste0("admissions_", forecast_date, ".rds")))
saveRDS(object = forecast_out, file = here::here("current_forecasts",
                                                 "admissions_trust",
                                                 "admissions_latest.rds"))

# As above, but for long summary 
models_summary_long <- tsensemble_summary_long %>%
  dplyr::bind_rows(arimareg_summary_long) %>%
  dplyr::bind_rows(convolution_summary_long)
#
ensemble_summary_long <- ensemble_forecast(model_forecasts = models_summary_long %>% dplyr::filter(!id %in% flag_trusts$trust_code),
                                           models = unique(models_summary_long$model)) %>%
  dplyr::mutate(ensemble_models = paste(setdiff(unique(models_summary$model), c("snaive")),
                                        collapse = ", "))
ensemble_summary_long_flag <- ensemble_forecast(model_forecasts = models_summary_long %>% dplyr::filter(id %in% flag_trusts$trust_code),
                                                models = setdiff(unique(models_summary_long$model), c("snaive", "convolution_rt"))) %>%
  dplyr::mutate(ensemble_models = paste(setdiff(unique(models_summary_long$model), c("snaive", "convolution_rt")),
                                        collapse = ", "))
#
saveRDS(object = ensemble_summary_long %>%
          dplyr::bind_rows(ensemble_summary_long_flag),
        file = here::here("current_forecasts",
                          "admissions_trust",
                          paste0("admissions_long_", forecast_date, ".rds")))
  

