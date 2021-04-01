
library(tidyverse)
library(ggrepel)
library(gganimate)

source("R/load_data.R")
source("R/forecast_fns.R")



# Output: scatter plot of admissions vs. prob of increase for historical forecasts

# Need
  # (1) 7-day average admissions by week
  # (2) 7- and 14-day-ahead point forecasts
  # (3) probability of increase (based on samples)



# (1) 7-day average observed admissions ------------------------------------


# Load observed admissions data and reshape
admissions <- load_hospital_data(keep_data = c("all_adm")) %>%
  dplyr::rename(adm = all_adm) %>%
  dplyr::left_join(covid19.nhs.data::trust_names, by = c("id" = "trust_code")) %>%
  dplyr::mutate(trust_name = stringr::str_wrap(trust_name, width = 40))

# Average admissions per week
trusts_observed7 <- admissions %>%
  dplyr::mutate(week_ending = lubridate::ceiling_date(date - 1, unit = "week", week_start = 7)) %>%
  dplyr::group_by(id, week_ending) %>%
  dplyr::summarise(observed = sum(adm, na.rm = TRUE)/7,
                   .groups = "drop_last")



# (2) point forecasts -----------------------------------------------------


# Load ensemble forecast summary and reshape
ensemble_summary <- readRDS(file = here::here("forecasts", "summary", "admissions_ensemble.rds")) %>%
  dplyr::filter(model == "mean_ensemble_forecast") %>%
  dplyr::select(-quantile) %>%
  tidyr::pivot_wider(id_cols = -c(quantile_label, value), names_from = quantile_label) %>%
  dplyr::select(forecast_from, model, id, date = date_horizon, horizon,
                median = lower_0, lower_90, lower_50, upper_50, upper_90) %>%
  dplyr::left_join(covid19.nhs.data::trust_names, by = c("id" = "trust_code")) %>%
  dplyr::mutate(trust_name = stringr::str_wrap(trust_name, width = 40))

# Point (median) forecast for 7 and 14 days ahead
ensemble_point <- ensemble_summary %>%
  dplyr::filter(horizon %in% c(7, 14)) %>%
  dplyr::select(forecast_from, id, horizon, median) %>%
  dplyr::mutate(horizon = paste0("forecast_", horizon)) %>%
  tidyr::pivot_wider(id_cols = -c(horizon, median), names_from = horizon, values_from = median) %>%
  dplyr::left_join(trusts_observed7, by = c("id", "forecast_from" = "week_ending")) %>%
  dplyr::left_join(covid19.nhs.data::trust_names, by = c("id" = "trust_code")) %>%
  dplyr::select(forecast_from, id, trust_name, observed, forecast_7, forecast_14)



# (3) probability of increase ---------------------------------------------

# Make forecast summaries using all quantiles
ensemble_files <- c("admissions_ensemble_aez", "admissions_arimacase7", "admissions_convolution")
ensemble_models <- c("ts_ensemble_aez", "arima_case7_observed_raw", "convolution_rt")

samples_dir <- here::here("forecasts", "samples")
samples_files <- list.files(samples_dir)[which(grepl(ensemble_files[1], list.files(samples_dir)) |
                                                 grepl(ensemble_files[2], list.files(samples_dir)) |
                                                 grepl(ensemble_files[3], list.files(samples_dir)))]
samples_files <- samples_files[which(!grepl("trust", samples_files))]

models_summary_long <- purrr::map_df(.x = samples_files, .f = ~ {
  
  # Load model samples
  raw_samples <- readRDS(file = here::here(samples_dir, .x)) %>%
    dplyr::filter(model %in% ensemble_models)
  
  # Summarise samples with all quantiles
  out <- forecast_summary(samples = raw_samples,
                          quantiles = seq(0.01, 0.99, 0.01))
  
  return(out)
  
}) %>%
  dplyr::bind_rows()

# Make ensemble forecast with all quantiles
ensemble_summary_long <- ensemble_forecast(model_forecasts = models_summary_long, models = ensemble_models)

# Calculate (approximate) probability of increase based on quantiles
ensemble_prob <- ensemble_summary_long %>%
  dplyr::filter(model == "mean_ensemble",
                horizon %in% c(7, 14)) %>%
  dplyr::left_join(trusts_observed7, by = c("id", "forecast_from" = "week_ending")) %>%
  dplyr::mutate(observed = round(observed)) %>%
  dplyr::group_by(forecast_from, id, horizon) %>%
  dplyr::filter(value > observed) %>%
  dplyr::group_by(forecast_from, id, horizon) %>%
  dplyr::filter(quantile == min(quantile)) %>%
  dplyr::mutate(horizon = paste0("p_", horizon),
                p_increase = 1-quantile) %>%
  dplyr::select(forecast_from, id, horizon, p_increase) %>%
  unique() %>%
  tidyr::pivot_wider(id_cols = -c(horizon, p_increase), names_from = horizon, values_from = p_increase)



# Putting (1), (2) and (3) together ---------------------------------------

current_forecast_tb <- ensemble_point %>%
  dplyr::left_join(ensemble_prob, by = c("id", "forecast_from")) %>%
  dplyr::select(forecast_from, id, trust_name, observed, forecast_7, p_7, forecast_14, p_14) %>%
  dplyr::mutate(observed = round(observed, 1),
                p_7 = round(p_7, 2),
                p_14 = round(p_14, 2))


# Plotting ----------------------------------------------------------------

# Create data to be plotted (adding forecast change and labels)
scatter_data <- current_forecast_tb %>%
  dplyr::group_by(forecast_from) %>%
  dplyr::mutate(change_7 = forecast_7 - observed,
                change_14 = forecast_14 - observed,
                change_14 = ifelse(abs(change_14) > quantile(abs(change_14), 0.99, na.rm = TRUE) & abs(change_14) > 3*max(observed), NA, change_14),
                label = ifelse(observed > quantile(observed, 0.95, na.rm = TRUE) |
                                 p_14 > quantile(p_14, 0.95, na.rm = TRUE) |
                                 change_14 > quantile(change_14, 0.95, na.rm = TRUE),
                               trust_name, NA),
                label = stringr::str_wrap(label, width = 25)) %>%
  dplyr::ungroup()


# GIF
scatter_data %>%
  ggplot(aes(x = p_14, y = observed, col = change_14)) +
  geom_point(size = 3) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 120), breaks = seq(0, 200, 10)) +
  scale_color_fermenter(n.breaks = 9, palette = "RdBu", limits = c(-20, 20)) +
  labs(x = "Probability of increase (14 days ahead)", y = "Current admissions (7-day average)",
       title = 'Forecast date: {closest_state}',
       col = "Forecast\nchange") +
  theme_bw() +
  transition_states(forecast_from,
                    transition_length = 2,
                    state_length = 1) + 
  ease_aes('cubic-in-out')

anim_save("scatter_plot_historical.gif", path = here::here("current_forecasts", "plots"))


# Individual plots for each forecast date

for(d in as.list(unique(scatter_data$forecast_from))){
  
  scatter_data %>%
    dplyr::filter(forecast_from == d) %>%
    # dplyr::mutate(label = ifelse(id %in% top_admissions_50$id[1:10], trust_name, NA),
    #               label = stringr::str_wrap(label, width = 25)) %>%
    ggplot(aes(x = p_14, y = observed, col = change_14)) +
    geom_point(size = 3) +
    geom_text_repel(aes(label = label), size = 3, col = "grey20",
                    point.padding = 0.2, min.segment.length = 0.1) +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(0, 120), breaks = seq(0, 200, 10)) +
    scale_color_fermenter(n.breaks = 9, palette = "RdBu", limits = c(-20, 20)) +
    labs(x = "Probability of increase (14 days ahead)", y = "Current admissions (7-day average)",
         title = paste0("Forecast date: ", d),
         col = "Forecast\nchange") +
    theme_bw()

  ggsave(filename = paste0("scatter_", d, ".png"), path = here::here("current_forecasts", "plots"),
         width = 16, height = 9, units = "in")
  
}





