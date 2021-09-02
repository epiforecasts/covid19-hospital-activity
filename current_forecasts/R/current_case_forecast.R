
library(tidyverse)
library(covidregionaldata)

source("R/load_data_fns.R")

# Summary -----------------------------------------------------------------

# Evaluate and visualise current UTLA-level case forecasts;
# ran through update_current_forecast.R

# Load data ---------------------------------------------------------------

case_obs_raw <- load_case_data() %>%
  dplyr::left_join(covid19.nhs.data::utla_names, by = c("id" = "geo_code"))

case_obs <- case_obs_raw %>%
  dplyr::filter(date <= forecast_date)

file_name <- paste0("cases_by_report_", forecast_date, ".csv")
case_forecast <- readr::read_csv(file = here::here("current_forecasts", "data", "cases_utla", file_name)) %>%
  dplyr::filter(date > forecast_date) %>%
  dplyr::mutate(region = ifelse(region == "Hackney and City of London", "Hackney", region),
                region = ifelse(region == "Cornwall and Isles of Scilly", "Cornwall", region)) %>%
  dplyr::left_join(covid19.nhs.data::utla_names, by = c("region" = "geo_name")) %>%
  dplyr::mutate(geo_code = ifelse(geo_code == "E10000002", "E06000060", geo_code)) %>%
  dplyr::filter(!is.na(geo_code),
                grepl("E", geo_code)) %>%
  dplyr::rename(id = geo_code,
                geo_name = region)



# Flag UTLAs --------------------------------------------------------------

case_summary <- case_forecast %>%
  dplyr::select(id, date, median, upper_90) %>%
  dplyr::left_join(case_obs %>%
                     dplyr::filter(date == max(date)) %>%
                     dplyr::select(id, id_name = geo_name, last_case = cases),
                   by = "id") %>%
  dplyr::select(id, id_name, date, median, upper_90, last_case)

flag_utlas <- case_summary %>%
  dplyr::filter(last_case > 10,
                (median > 50*last_case | upper_90 > 500*last_case))


# Run time series case forecasts ------------------------------------------

if(nrow(flag_utlas) > 0){
  
  flag_utlas <- flag_utlas %>%
    dplyr::group_by(id, id_name) %>%
    dplyr::filter(date == max(date, na.rm = TRUE)) %>%
    dplyr::mutate(flag = TRUE) %>%
    dplyr::ungroup()
  
  dat_in <- case_obs %>%
    dplyr::filter(id %in% flag_utlas$id) %>%
    dplyr::select(id, date, cases)
  
  case_tsensemble_samples <- timeseries_samples(data = dat_in, yvar = "cases",
                                                horizon = 14, samples = 1000, models = "aez", 
                                                train_from = forecast_date - 42,
                                                forecast_from = forecast_date) %>%
    dplyr::mutate(model = "ts_ensemble")
  case_tsensemble_summary <- forecast_summary(samples = case_tsensemble_samples)
  
  plot_case_forecast <- case_forecast %>%
    dplyr::left_join(flag_utlas %>% select(id, flag), by = "id")
  
  plot_ts <- case_tsensemble_summary %>%
    dplyr::ungroup() %>%
    dplyr::select(id, date = date_horizon, name = quantile_label, value) %>%
    tidyr::pivot_wider(id_cols = c(id, date)) %>%
    dplyr::select(-upper_0) %>%
    dplyr::rename(median = lower_0) %>%
    dplyr::left_join(covid19.nhs.data::utla_names, by = c("id" = "geo_code")) %>%
    dplyr::mutate(model = "Time series\nensemble") %>%
    dplyr::bind_rows(plot_case_forecast %>%
                       dplyr::filter(is.na(flag)) %>%
                       dplyr::select(id, geo_name, date, median, lower_90, lower_50, upper_50, upper_90) %>%
                       dplyr::mutate(model = "Rt"))
  
} else {
  
  plot_case_forecast <- case_forecast
  
  plot_ts <- plot_case_forecast %>%
    dplyr::select(id, geo_name, date, median, lower_90, lower_50, upper_50, upper_90) %>%
    dplyr::mutate(model = "Rt")
  
}

saveRDS(object = flag_utlas,
        file = here::here("current_forecasts", "data", "flag_utla.rds"))


# Vis current case forecast -----------------------------------------------

# Observed cases
g <- case_obs %>%
  dplyr::filter(grepl("E", id),
                date > forecast_date - 42) %>%
  ggplot() +
  geom_line(aes(x = date, y = cases)) +
  facet_wrap(. ~ geo_name, scales = "free_y") +
  theme_bw() +
  labs(x = "Date", y = "Daily Covid-19 cases",
       title = "Current UTLA-level Covid-19 case forecasts",
       subtitle = paste0("Forecast date: ", format.Date(forecast_date, format = "%d %B %Y")),
       caption = "Median + 50% prediction interval") +
  theme(strip.text.x = element_text(size = 6))

# Forecast cases (Rt or time series ensemble)
g <- g +
  geom_line(data = plot_ts,
            aes(x = date, y = median, col = model)) +
  geom_ribbon(data = plot_ts,
              aes(x = date, y = median, ymin = lower_50, ymax = upper_50, fill = model), alpha = 0.4) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1")

ggsave(plot = g,
       filename = here::here("current_forecasts", "current_case_forecast.pdf"),
       device = "pdf",
       width = 16, height = 12, units = "in")


# Flagged UTLAs only
g_flag <- case_obs %>%
  dplyr::filter(id %in% flag_utlas$id,
                grepl("E", id),
                date > forecast_date - 42) %>%
  ggplot() +
  geom_line(aes(x = date, y = cases)) +
  facet_wrap(. ~ geo_name, scales = "free_y") +
  theme_bw() +
  labs(x = "Date", y = "Daily Covid-19 cases",
       title = "Current UTLA-level Covid-19 case forecasts",
       subtitle = paste0("Forecast date: ", format.Date(forecast_date, format = "%d %B %Y")),
       caption = "Median + 50% prediction interval") +
  theme(strip.text.x = element_text(size = 6))

# Add Rt forecasts
g_flag <- g_flag +
  geom_line(data = plot_case_forecast %>% dplyr::filter(flag),
            aes(x = date, y = median, col = flag)) +
  geom_ribbon(data = plot_case_forecast %>% dplyr::filter(flag),
              aes(x = date, y = median, ymin = lower_50, ymax = upper_50, fill = flag), alpha = 0.4) +
  theme(legend.position = "none")

# ggsave(plot = g_rt,
#        filename = here::here("current_forecasts", "current_case_forecast.pdf"),
#        device = "pdf",
#        width = 16, height = 12, units = "in")
