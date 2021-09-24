
library(tidyverse)
library(scoringutils)

source(here::here("R", "utils.R"))
source(here::here("R", "load_data_fns.R"))

# Set-up ------------------------------------------------------------------

# Define observed (truth) data
raw_hosp <- load_hospital_data()
observed <- raw_hosp %>%
  dplyr::select(id, date, true_value = all_adm)

# Load quantile forecasts
summary_dir <- here::here("data", "out", "admissions_forecast", "summary")
summary_files <- list.files(summary_dir)

forecast_summary <- purrr::map_df(.x = summary_files, .f = ~{
  
  out <- readRDS(file = here::here(summary_dir, .x))
  
}) %>%
  dplyr::bind_rows() %>%
  dplyr::rename(prediction = value) %>%
  dplyr::mutate(range = round(abs(1 - 2 * quantile) * 100),
                boundary = stringr::str_sub(quantile_label, 1, 5),
                ) %>%
  dplyr::select(forecast_from, model, id, horizon, date = date_horizon, range, boundary, prediction)

# Reshape quantile forecasts, include observed (truth) data
score_summary_in <- forecast_summary %>%
  dplyr::left_join(observed, by = c("id", "date")) %>%
  dplyr::filter(!is.na(true_value))
score_summary_in <- data.table::setDT(score_summary_in)


# Score forecasts ---------------------------------------------------------

# Empirical coverage
empirical_coverage <- forecast_summary %>%
  dplyr::left_join(observed %>% dplyr::select(id, date, true_value), by = c("id", "date")) %>%
  dplyr::filter(horizon %in% 1:14,
                !is.na(true_value)) %>%
  dplyr::mutate(range = paste0(boundary, "_", range)) %>%
  dplyr::select(-boundary) %>%
  tidyr::pivot_wider(id_cols = c(forecast_from, id, true_value, model, horizon, date),
                     names_from = range,
                     values_from = prediction) %>%
  dplyr::group_by(model, horizon) %>%
  dplyr::summarise(empirical50 = length(true_value[which(true_value > lower_50 & true_value < upper_50)])/n(),
                   empirical90 = length(true_value[which(true_value > lower_90 & true_value < upper_90)])/n()) %>%
  tidyr::pivot_longer(cols = contains("empirical")) %>%
  dplyr::mutate(truth = ifelse(name == "empirical50", 0.5, 0.9),
                name = ifelse(name == "empirical50", "50% prediction\ninterval", "90% prediction\ninterval"))

# All individual scores
score_all_out <- scoringutils::eval_forecasts(score_summary_in,
                                                  by = c("forecast_from", "id", "model", "horizon"))

# WIS scores
## Overall
score_summary_overall <- scoringutils::eval_forecasts(score_summary_in,
                                                by = c("forecast_from", "id", "model", "horizon"),
                                                summarise_by = c("model"))
## By horizon
score_summary_horizon <- scoringutils::eval_forecasts(score_summary_in,
                                                by = c("forecast_from", "id", "model", "horizon"),
                                                summarise_by = c("horizon", "model"))
## By horizon and forecast date (forecast_from)
score_summary_time <- scoringutils::eval_forecasts(score_summary_in,
                                                by = c("forecast_from", "id", "model", "horizon"),
                                                summarise_by = c("forecast_from", "horizon", "model"))
## By horizon and Trust
score_summary_location <- scoringutils::eval_forecasts(score_summary_in,
                                              by = c("forecast_from", "id", "model", "horizon"),
                                              summarise_by = c("id", "horizon", "model"))

# Save scores -------------------------------------------------------------

scores_dir <- here::here("data", "out", "evaluation")

saveRDS(object = empirical_coverage,
        file = here::here(scores_dir, "empirical_coverage.rds"))
saveRDS(object = score_all_out,
        file = here::here(scores_dir, "scores_full.rds"))
saveRDS(object = score_summary_overall,
        file = here::here(scores_dir, "scores_summary_overall.rds"))
saveRDS(object = score_summary_horizon,
        file = here::here(scores_dir, "scores_summary_horizon.rds"))
saveRDS(object = score_summary_time,
        file = here::here(scores_dir, "scores_summary_time.rds"))
saveRDS(object = score_summary_location,
        file = here::here(scores_dir, "scores_summary_location.rds"))
