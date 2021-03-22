
# Admissions --------------------------------------------------------------

summary_dir <- here::here("forecasts", "summary")
summary_files <- list.files(summary_dir)[grepl("admissions", list.files(summary_dir))]

forecast_summary <- purrr::map_df(.x = summary_files, .f = ~{
  
  out <- readRDS(file = here::here(summary_dir, .x))
  
}) %>%
  dplyr::bind_rows()

ensemble_models <- c("ts_ensemble_aetz", "arima_case7_observed_raw", "convolution_observed")
ensemble_summary <- ensemble_forecast(model_forecasts = forecast_summary,
                                      models = ensemble_models)
forecast_name <- "admissions_ensemble.rds"
saveRDS(object = ensemble_summary, file = here::here("forecasts", "summary", forecast_name))
