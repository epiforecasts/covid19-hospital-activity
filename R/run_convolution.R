
# Models ------------------------------------------------------------------

# 1. observed future cases, CHR prior scale = list(mean = 0.2, sd = 0.1)
# 2. observed future cases, regional CHR prior
# 3. forecast future cases, CHR prior scale = list(mean = 0.2, sd = 0.1)
# 4. observed future cases, regional CHR prior

# Estimate priors ---------------------------------------------------------

# Reshape observed data
df_observed <- dat %>%
  dplyr::filter(date >= forecast_date - (12*21),
                date <= forecast_date) %>%
  dplyr::group_by(region = nhs_region, date) %>%
  dplyr::summarise(primary = sum(cases, na.rm = TRUE),
                   secondary = sum(all_adm, na.rm = TRUE),
                   .groups = "drop") %>%
  na.omit()
dt_observed <- data.table::data.table(df_observed)

fit <- regional_secondary(reports = dt_observed,
                          secondary = EpiNow2::secondary_opts(type = "incidence"),
                          delays = delay_opts(list(
                            mean = 2.5, mean_sd = 0.5,
                            sd = 0.47, sd_sd = 0.25, max = 30
                          )),
                          obs = EpiNow2::obs_opts(week_effect = FALSE,
                                                  scale = list(mean = 0.2, sd = 0.1)),
                          posterior_params = c("delay", "frac_obs", "rep_phi"),
                          burn_in = 14)

priors <- fit$summarised_posterior %>%
  dplyr::right_join(dat %>%
                      dplyr::group_by(nhs_region, id) %>%
                      dplyr::summarise(.groups = "drop") %>%
                      na.omit(),
                    by = c("region" = "nhs_region")) %>%
  dplyr::select(-region) %>%
  dplyr::select(region = id, variable, mean, se_mean, sd, contains("lower"), median, contains("upper"))



# Set-up ------------------------------------------------------------------

# Reshape observed data
df_observed <- dat %>%
  dplyr::filter(date >= forecast_date - 6*7,
                date <= forecast_date) %>%
  dplyr::select(region = id, date, primary = cases, secondary = all_adm) %>%
  na.omit()
dt_observed <- data.table::data.table(df_observed)

# Future case values (observed)
df_future_in <- dat %>%
  dplyr::filter(date > forecast_date,
                date <= forecast_date + 14) %>%
  dplyr::select(region = id, date, cases)
df_future_obs <- purrr::map_df(.x = 1:1000, .f = ~ {
  out <- df_future_in %>%
    dplyr::mutate(sample = as.integer(.x))
  return(out)
}) %>%
  dplyr::bind_rows()
dt_future_obs <- data.table::data.table(df_future_obs)

# Future case values (forecast)
df_future_forecast <- case_forecast$samples
dt_future_forecast <- data.table::data.table(df_future_forecast)



# Observed cases, no `priors` ---------------------------------------------

convolution_forecast <- regional_secondary(reports = dt_observed,
                                           case_forecast = dt_future_obs,
                                           secondary = secondary_opts(type = "incidence"),
                                           delays = delay_opts(list(
                                             mean = 2.5, mean_sd = 0.5,
                                             sd = 0.47, sd_sd = 0.25, max = 30
                                           )),
                                           obs = EpiNow2::obs_opts(week_effect = FALSE,
                                                                   scale = list(mean = 0.2, sd = 0.1)),
                                           burn_in = 14,
                                           control = list(adapt_delta = 0.99, max_treedepth = 15),
                                           return_fit = FALSE,
                                           return_plots = FALSE,
                                           verbose = TRUE)

# Model samples and summary
convolution_model_samples <- convolution_forecast$samples %>%
  dplyr::mutate(value = round(value)) %>%
  dplyr::filter(date > forecast_date) %>%
  dplyr::mutate(forecast_from = forecast_date,
                horizon = as.integer(date - forecast_from),
                model = "convolution_observed_noprior") %>%
  dplyr::select(id = region, sample, horizon, value, forecast_from, model)
convolution_model_summary <- forecast_summary(samples = convolution_model_samples)

# Full output
convolution_samples <- convolution_model_samples
convolution_summary <- convolution_model_summary

# Observed cases, regional `priors` ---------------------------------------

convolution_forecast <- regional_secondary(reports = dt_observed,
                                           case_forecast = dt_future_obs,
                                           priors = priors,
                                           secondary = secondary_opts(type = "incidence"),
                                           delays = delay_opts(list(
                                             mean = 2.5, mean_sd = 0.5,
                                             sd = 0.47, sd_sd = 0.25, max = 30
                                           )),
                                           obs = EpiNow2::obs_opts(week_effect = FALSE),
                                           burn_in = 14,
                                           control = list(adapt_delta = 0.99, max_treedepth = 15),
                                           return_fit = FALSE,
                                           return_plots = FALSE,
                                           verbose = TRUE)

# Model samples and summary
convolution_model_samples <- convolution_forecast$samples %>%
  dplyr::mutate(value = round(value)) %>%
  dplyr::filter(date > forecast_date) %>%
  dplyr::mutate(forecast_from = forecast_date,
                horizon = as.integer(date - forecast_from),
                model = "convolution_observed_regprior") %>%
  dplyr::select(id = region, sample, horizon, value, forecast_from, model)
convolution_model_summary <- forecast_summary(samples = convolution_model_samples)

# Full output
convolution_samples <- convolution_samples %>%
  dplyr::bind_rows(convolution_model_samples)
convolution_summary <- convolution_summary %>%
  dplyr::bind_rows(convolution_model_summary)


# Forecast cases, no `priors` ---------------------------------------------

convolution_forecast <- regional_secondary(reports = dt_observed,
                                           case_forecast = dt_future_forecast,
                                           secondary = secondary_opts(type = "incidence"),
                                           delays = delay_opts(list(
                                             mean = 2.5, mean_sd = 0.5,
                                             sd = 0.47, sd_sd = 0.25, max = 30
                                           )),
                                           obs = EpiNow2::obs_opts(week_effect = FALSE,
                                                                   scale = list(mean = 0.2, sd = 0.1)),
                                           burn_in = 14,
                                           control = list(adapt_delta = 0.99, max_treedepth = 15),
                                           return_fit = FALSE,
                                           return_plots = FALSE,
                                           verbose = TRUE)

# Model samples and summary
convolution_model_samples <- convolution_forecast$samples %>%
  dplyr::mutate(value = round(value)) %>%
  dplyr::filter(date > forecast_date) %>%
  dplyr::mutate(forecast_from = forecast_date,
                horizon = as.integer(date - forecast_from),
                model = "convolution_forecast_noprior") %>%
  dplyr::select(id = region, sample, horizon, value, forecast_from, model)
convolution_model_summary <- forecast_summary(samples = convolution_model_samples)

# Full output
convolution_samples <- convolution_samples %>%
  dplyr::bind_rows(convolution_model_samples)
convolution_summary <- convolution_summary %>%
  dplyr::bind_rows(convolution_model_summary)



# Forecast cases, regional `priors` ---------------------------------------

convolution_forecast <- regional_secondary(reports = dt_observed,
                                           case_forecast = dt_future_forecast,
                                           priors = priors,
                                           secondary = secondary_opts(type = "incidence"),
                                           delays = delay_opts(list(
                                             mean = 2.5, mean_sd = 0.5,
                                             sd = 0.47, sd_sd = 0.25, max = 30
                                           )),
                                           obs = EpiNow2::obs_opts(week_effect = FALSE),
                                           burn_in = 14,
                                           control = list(adapt_delta = 0.99, max_treedepth = 15),
                                           return_fit = FALSE,
                                           return_plots = FALSE,
                                           verbose = TRUE)

# Model samples and summary
convolution_model_samples <- convolution_forecast$samples %>%
  dplyr::mutate(value = round(value)) %>%
  dplyr::filter(date > forecast_date) %>%
  dplyr::mutate(forecast_from = forecast_date,
                horizon = as.integer(date - forecast_from),
                model = "convolution_forecast_regprior") %>%
  dplyr::select(id = region, sample, horizon, value, forecast_from, model)
convolution_model_summary <- forecast_summary(samples = convolution_model_samples)

# Full output
convolution_samples <- convolution_samples %>%
  dplyr::bind_rows(convolution_model_samples)
convolution_summary <- convolution_summary %>%
  dplyr::bind_rows(convolution_model_summary)

# Save results ------------------------------------------------------------

forecast_name <- paste0("convolution_", forecast_date, ".rds")
saveRDS(object = convolution_samples, file = here::here("data", "out", "admissions_forecast", "samples", forecast_name))
saveRDS(object = convolution_summary, file = here::here("data", "out", "admissions_forecast", "summary", forecast_name))
