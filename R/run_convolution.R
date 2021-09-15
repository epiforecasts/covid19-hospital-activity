
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


# Observed cases ----------------------------------------------------------

# Reshape observed data
df_observed <- dat %>%
  dplyr::filter(date >= forecast_date - 6*7,
                date <= forecast_date) %>%
  dplyr::select(region = id, date, primary = cases, secondary = all_adm) %>%
  na.omit()
dt_observed <- data.table::data.table(df_observed)

# Future case values (observed values)
df_forecast_in <- dat %>%
  dplyr::filter(date > forecast_date,
                date <= forecast_date + 14) %>%
  dplyr::select(region = id, date, cases)
df_forecast <- purrr::map_df(.x = 1:1000, .f = ~ {
  out <- df_forecast_in %>%
    dplyr::mutate(sample = as.integer(.x))
  return(out)
}) %>%
  dplyr::bind_rows()
dt_forecast <- data.table::data.table(df_forecast)


# Run forecast
convolution_forecast_observed <- regional_secondary(reports = dt_observed,
                                                    case_forecast = dt_forecast,
                                                    priors = priors,
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

# Reshape forecast samples
convolution_samples_observed <- convolution_forecast_observed$samples %>%
  dplyr::mutate(value = round(value)) %>%
  dplyr::filter(date > forecast_date) %>%
  dplyr::mutate(forecast_from = forecast_date,
                horizon = as.integer(date - forecast_from),
                model = "convolution_observed") %>%
  dplyr::select(id = region, sample, horizon, value, forecast_from, model)
# Make forecast summary
convolution_summary_observed <- forecast_summary(samples = convolution_samples_observed)


# Forecast cases ----------------------------------------------------------

# Convert forecast from UTLA to Trust
df_forecast <- case_forecast$samples
dt_forecast <- data.table::data.table(df_forecast)

# Run forecast
convolution_forecast_rt <- regional_secondary(reports = dt_observed,
                                              case_forecast = dt_forecast,
                                              secondary = secondary_opts(type = "incidence"),
                                              delays = delay_opts(list(
                                                mean = 2.5, mean_sd = 0.5,
                                                sd = 0.47, sd_sd = 0.25, max = 30
                                              )),
                                              obs = EpiNow2::obs_opts(week_effect = FALSE,
                                                                      scale = list(mean = 0.2, sd = 0.1)),
                                              burn_in = 21,
                                              control = list(adapt_delta = 0.99, max_treedepth = 15),
                                              return_fit = FALSE,
                                              return_plots = FALSE,
                                              verbose = TRUE)

# Reshape forecast samples
convolution_samples_rt <- convolution_forecast_rt$samples %>%
  dplyr::mutate(value = round(value)) %>%
  dplyr::filter(date > forecast_date) %>%
  dplyr::mutate(forecast_from = forecast_date,
                horizon = as.integer(date - forecast_from),
                model = "convolution_forecast") %>%
  dplyr::select(id = region, sample, horizon, value, forecast_from, model)
# Make forecast summary
convolution_summary_rt <- forecast_summary(samples = convolution_samples_rt)


# Save forecast -----------------------------------------------------------

convolution_samples <- convolution_samples_observed %>%
  dplyr::bind_rows(convolution_samples_rt)
convolution_summary <- convolution_summary_observed %>%
  dplyr::bind_rows(convolution_summary_rt)
forecast_name <- paste0("convolution_", forecast_date, ".rds")
saveRDS(object = convolution_samples, file = here::here("data", "out", "admissions_forecast", "samples", forecast_name))
saveRDS(object = convolution_summary, file = here::here("data", "out", "admissions_forecast", "summary", forecast_name))
