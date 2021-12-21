library(tidyverse)
library(covid19.nhs.data)
library(covidregionaldata)
library(future, quietly = TRUE)

source(here::here("R", "load_data_fns.R"))
source(here::here("R", "forecast_fns.R"))
source(here::here("R", "regional_secondary_fns.R"))
source(here::here("R", "utils.R"))


# Set up ------------------------------------------------------------------

forecast_date <- as.Date("2021-12-15")


# Load and check case forecasts -------------------------------------------

case_dat <- load_case_data() %>%
  dplyr::left_join(covid19.nhs.data::utla_names, by = c("id" = "geo_code"))

case_forecast <- load_case_forecasts(obs_case_data = case_dat,
                                     forecast_date = forecast_date,
                                     forecast_path = here::here("current_forecasts",
                                                                "data",
                                                                "cases_utla"),
                                     level = "trust",
                                     replace_flag = TRUE,
                                     replace_model = "ae")

case_forecast_samples <- case_forecast$samples

saveRDS(object = case_forecast$flag,
        file = here::here("current_forecasts", "data", "flagged_utlas.rds"))


# Create input data -------------------------------------------------------

# Load 'observed' Trust-level data (admissions + cases)
dat <- load_combined_data(add_private = TRUE)

# Trust mergers
trust_mergers <- readxl::read_xlsx(path = here::here("data", "raw", "trust_mergers.xlsx")) %>%
  mutate(from_date = as.Date(from_date))

dat_in <- dat %>%
  dplyr::bind_rows(case_forecast$summary %>%
                     dplyr::rename(cases = case_forecast)) %>%
  # dplyr::filter(date >= as.Date("2020-08-02"),
  #               !is.na(id),
  #               !is.na(nhs_region)) %>%
  dplyr::filter(date >= as.Date("2020-08-02"),
                !is.na(id),
                !id %in% trust_mergers$trust_code_old,
                id != "RPY") %>%
  dplyr::group_by(id) %>%
  dplyr::mutate(cases_lag7 = lag(cases, 7)) %>%
  dplyr::select(-bed_occ)


# Update all forecasts ----------------------------------------------------

## Baseline forecast
baseline_out <- forecast_baseline(data = dat_in, yvar = "all_adm",
                                  horizon = 14, samples = 1000,
                                  train_from = forecast_date - 42,
                                  forecast_from = forecast_date)
baseline_summary <- baseline_out$summary


## Autoregressive time series 
tsensemble_samples <- timeseries_samples(data = dat_in, yvar = "all_adm",
                                         horizon = 14, samples = 1000, models = "aez", 
                                         train_from = forecast_date - 42,
                                         forecast_from = forecast_date) %>%
  dplyr::mutate(model = "ts_ensemble")
tsensemble_summary <- forecast_summary(samples = tsensemble_samples)
tsensemble_summary_long <- forecast_summary(samples = tsensemble_samples,
                                            quantiles = seq(0.01, 0.99, 0.01))


## Regression + ARIMA errors
arimareg_samples <- timeseries_samples(data = dat_in, yvar = "all_adm", xvars = "cases_lag7",
                                       horizon = 14, samples = 1000, models = "a", 
                                       train_from = forecast_date - 42,
                                       forecast_from = forecast_date) %>%
  dplyr::mutate(model = "regression_arima")
arimareg_summary <- forecast_summary(samples = arimareg_samples)
arimareg_summary_long <- forecast_summary(samples = arimareg_samples,
                                          quantiles = seq(0.01, 0.99, 0.01))


## Convolution
future::plan("multisession",  gc = TRUE, earlySignal = TRUE)

convolution_obs <- dat_in %>%
  dplyr::filter(date >= forecast_date - (42+21),
                date <= forecast_date) %>%
  dplyr::select(region = id, date, primary = cases, secondary = all_adm) %>%
  na.omit()

convolution_forecast_rt <- regional_secondary(reports = convolution_obs,
                                              case_forecast = case_forecast_samples,
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
                                           "data",
                                           "chr",
                                           paste0("chr_", forecast_date, ".rds")))




# Ensemble ----------------------------------------------------------------

# Combined model summaries
models_summary <- baseline_summary %>%
  dplyr::bind_rows(tsensemble_summary) %>%
  dplyr::bind_rows(arimareg_summary) %>%
  dplyr::bind_rows(convolution_summary)
# Make ensemble
ensemble_summary <- ensemble_forecast(model_forecasts = models_summary,
                                      models = setdiff(unique(models_summary$model), "snaive"))
# Add ensemble to models summary
forecast_out <- models_summary %>%
  dplyr::bind_rows(ensemble_summary)
saveRDS(object = forecast_out, file = here::here("current_forecasts",
                                                 "data",
                                                 "admissions_trust",
                                                 paste0("admissions_", forecast_date, ".rds")))
saveRDS(object = forecast_out, file = here::here("current_forecasts",
                                                 "data",
                                                 "admissions_trust",
                                                 "admissions_latest.rds"))

# As above, but for long summary (more quantiles) 
models_summary_long <- tsensemble_summary_long %>%
  dplyr::bind_rows(arimareg_summary_long) %>%
  dplyr::bind_rows(convolution_summary_long)

ensemble_summary_long <- ensemble_forecast(model_forecasts = models_summary_long,
                                           models = unique(models_summary_long$model))

forecast_out_long <- models_summary_long %>%
  dplyr::bind_rows(ensemble_summary_long)
saveRDS(object = forecast_out_long,
        file = here::here("current_forecasts",
                          "data",
                          "admissions_trust",
                          paste0("admissions_long_", forecast_date, ".rds")))



# Regional and national forecasts -----------------------------------------

## Regional forecasts
region_samples <- tsensemble_samples %>%
  dplyr::bind_rows(arimareg_samples) %>%
  dplyr::bind_rows(convolution_samples) %>%
  dplyr::left_join(dat %>%
                     filter(!is.na(nhs_region)) %>%
                     select(nhs_region, id) %>%
                     unique(),
                   by = "id") %>%
  dplyr::group_by(nhs_region, sample, horizon, forecast_from, model) %>% 
  dplyr::summarise(value = sum(value, na.rm = TRUE)) %>%
  dplyr::rename(id = nhs_region)

region_summary <- forecast_summary(samples = region_samples)
region_ensemble <- ensemble_forecast(model_forecasts = region_summary,
                                     models = unique(region_summary$model))


## National forecasts
england_samples <- tsensemble_samples %>%
  dplyr::bind_rows(arimareg_samples) %>%
  dplyr::bind_rows(convolution_samples) %>%
  dplyr::group_by(sample, horizon, forecast_from, model) %>% 
  dplyr::summarise(value = sum(value, na.rm = TRUE)) %>%
  dplyr::mutate(id = "England")

england_summary <- forecast_summary(samples = england_samples)
england_ensemble <- ensemble_forecast(model_forecasts = england_summary,
                                     models = unique(england_summary$model))

## Combine and save forecasts
both_ensemble <- region_ensemble %>%
  dplyr::bind_rows(region_summary) %>%
  dplyr::bind_rows(england_ensemble) %>%
  dplyr::bind_rows(england_summary)
saveRDS(object = both_ensemble,
        file = here::here("current_forecasts",
                          "data",
                          "admissions_region",
                          paste0("admissions_", forecast_date, ".rds")))

