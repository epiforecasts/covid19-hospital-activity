
library(tidyverse)
library(scoringutils)

source("R", "load_data_fns.R")

# Set-up ------------------------------------------------------------------

# Define observed (truth) data
raw_hosp <- load_hospital_data()
observed <- raw_hosp %>%
  dplyr::select(id, date, true_value = all_adm)

# Load quantile forecasts
summary_dir <- here::here("data", "out", "admissions_forecast", "summary")
summary_files <- list.files(summary_dir)[grepl("admissions", list.files(summary_dir))]

forecast_summary <- purrr::map_df(.x = summary_files, .f = ~{
  
  out <- readRDS(file = here::here(summary_dir, .x))
  
}) %>%
  dplyr::bind_rows() %>%
  dplyr::rename(prediction = value) %>%
  dplyr::mutate(range = round(abs(1 - 2 * quantile) * 100),
                boundary = stringr::str_sub(quantile_label, 1, 5),
                model = case_when(model == "ts_ensemble_aez" ~ "ts_ensemble",
                                  model == "arima_case7_forecast_raw" ~ "regression_arima",
                                  model == "median_ensemble_forecast" ~ "median_ensemble",
                                  model == "mean_ensemble_forecast" ~ "mean_ensemble",
                                  TRUE ~ model)) %>%
  dplyr::select(forecast_from, model, id, horizon, date = date_horizon, range, boundary, prediction)

# Reshape quantile forecasts, include observed (truth) data
score_summary_in <- forecast_summary %>%
  dplyr::left_join(observed, by = c("id", "date")) %>%
  dplyr::filter(!is.na(true_value))
score_summary_in <- data.table::setDT(score_summary_in)


# Score forecasts ---------------------------------------------------------

# All individual scores
score_summary_out <- scoringutils::eval_forecasts(score_summary_in,
                                                  by = c("forecast_from", "id", "model", "horizon"))

# WIS scores
## Overall
wis_overall_out <- scoringutils::eval_forecasts(score_summary_in,
                                                by = c("forecast_from", "id", "model", "horizon"),
                                                summarise_by = c("model"))
## By horizon
wis_horizon_out <- scoringutils::eval_forecasts(score_summary_in,
                                                by = c("forecast_from", "id", "model", "horizon"),
                                                summarise_by = c("horizon", "model"))
## By horizon and forecast date (forecast_from)
wis_time_out <- scoringutils::eval_forecasts(score_summary_in,
                                                by = c("forecast_from", "id", "model", "horizon"),
                                                summarise_by = c("forecast_from", "horizon", "model"))
## By horizon and Trust
wis_trust_out <- scoringutils::eval_forecasts(score_summary_in,
                                              by = c("forecast_from", "id", "model", "horizon"),
                                              summarise_by = c("id", "horizon", "model"))

# Save scores -------------------------------------------------------------

scores_dir <- here::here("data", "out", "evaluation")

saveRDS(object = score_summary_out,
        file = here::here(scores_dir, "full_scoring.rds"))
saveRDS(object = wis_overall_out,
        file = here::here(scores_dir, "wis_overall.rds"))
saveRDS(object = wis_horizon_out,
        file = here::here(scores_dir, "wis_horizon.rds"))
saveRDS(object = wis_time_out,
        file = here::here(scores_dir, "wis_time.rds"))
saveRDS(object = wis_trust_out,
        file = here::here(scores_dir, "wis_trust.rds"))
