
library(tidyverse)
library(scoringutils)

# Observed data -----------------------------------------------------------

raw_hosp <- covid19.nhs.data::download_trust_data() %>%
  dplyr::filter(data %in% c("Hosp ads & diag", "New hosp cases", "All beds COVID")) %>%
  dplyr::mutate(data = dplyr::case_when(data == "Hosp ads & diag" ~ "all_adm",
                                        data == "All beds COVID" ~ "bed_occ",
                                        data == "New hosp cases" ~ "new_adm")) %>%
  dplyr::select(id = org_code, date, data, value) %>%
  tidyr::pivot_wider(id_cols = -c(data, value), names_from = data)

observed <- raw_hosp %>%
  dplyr::filter(id %in% covid19.nhs.data::trust_ltla_mapping$trust_code) %>%
  dplyr::select(id, date, true_value = all_adm)


# Summary -----------------------------------------------------------------

summary_dir <- here::here("forecasts", "summary")
summary_files <- list.files(summary_dir)[grepl("admissions", list.files(summary_dir))]

forecast_summary <- purrr::map_df(.x = summary_files, .f = ~{
  
  out <- readRDS(file = here::here(summary_dir, .x))
  
}) %>%
  dplyr::bind_rows() %>%
  dplyr::rename(prediction = value) %>%
  dplyr::mutate(range = round(abs(1 - 2 * quantile) * 100),
                boundary = stringr::str_sub(quantile_label, 1, 5)) %>%
  dplyr::select(forecast_from, model, id, horizon, date = date_horizon, range, boundary, prediction)

# All scoring
score_summary_in <- forecast_summary %>%
  dplyr::left_join(observed, by = c("id", "date")) %>%
  dplyr::filter(horizon %in% c(1, 7, 14),
                !is.na(true_value))
score_summary_in <- data.table::setDT(score_summary_in)

score_summary_out <- scoringutils::eval_forecasts(score_summary_in,
                                                  by = c("forecast_from", "id", "model", "horizon")) %>%
  dplyr::select(-c(bias, sharpness))
                                

# Samples -----------------------------------------------------------------

samples_dir <- here::here("forecasts", "samples")
samples_files <- list.files(samples_dir)[grepl("admissions", list.files(samples_dir))]

score_samples_out <- purrr::map_df(.x = samples_files, .f = ~{
  
  score_samples_in <- readRDS(file = here::here(samples_dir, .x)) %>%
    dplyr::rename(prediction = value) %>%
    dplyr::mutate(date = forecast_from + horizon) %>%
    dplyr::left_join(observed, by = c("id", "date")) %>%
    dplyr::filter(horizon %in% c(1, 7, 14),
                  !is.na(true_value)) %>%
    dplyr::mutate(prediction = as.integer(round(prediction)),
                  true_value = as.integer(true_value))
  score_samples_in <- data.table::setDT(score_samples_in)
  
  out <- scoringutils::eval_forecasts(score_samples_in,
                                                    by = c("forecast_from", "model", "id", "horizon")) %>%
    dplyr::select(forecast_from, model, id, horizon, bias, sharpness, crps, dss) %>%
    dplyr::left_join(score_samples_in %>%
                       dplyr::mutate(ae = abs(prediction - true_value),
                                     ape = ae/true_value) %>%
                       dplyr::group_by(forecast_from, id, model, horizon) %>%
                       dplyr::summarise(mae = mean(ae, na.rm = TRUE),
                                        mape = mean(ape, na.rm = TRUE),
                                        .groups = "drop"),
                     by = c("forecast_from", "model", "id", "horizon"))
  
  return(out)
    
}) %>%
  dplyr::bind_rows()



# Combined scores ---------------------------------------------------------

score_out <- score_summary_out %>%
  dplyr::left_join(score_samples_out,
                   by = c("forecast_from", "model", "id", "horizon")) 

saveRDS(object = score_out, file = here::here("evaluation", "admissions_scores.rds"))

