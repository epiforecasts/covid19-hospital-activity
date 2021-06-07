
library(tidyverse)
library(covidregionaldata)

source("R/load_data_fns.R")

# Summary -----------------------------------------------------------------

# Evaluate and visualise current UTLA-level case forecasts;
# ran through update_current_forecast.R

# Load data ---------------------------------------------------------------

case_obs <- load_case_data() %>%
  dplyr::left_join(covid19.nhs.data::utla_names, by = c("id" = "geo_code")) %>%
  dplyr::filter(date <= forecast_date)

file_name <- paste0("cases_by_report_", forecast_date, ".csv")
case_forecast <- readr::read_csv(file = here::here("current_forecasts", "cases_utla", file_name)) %>%
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
                median > 20*last_case,
                upper_90 > 200*last_case) %>%
  dplyr::group_by(id, id_name) %>%
  dplyr::filter(date == max(date)) %>%
  dplyr::mutate(flag = TRUE) %>%
  dplyr::ungroup()

saveRDS(object = flag_utlas,
        file = here::here("current_forecasts", "flag_utla.rds"))


# Vis current case forecast -----------------------------------------------

plot_case_forecast <- case_forecast %>%
  dplyr::left_join(flag_utlas %>% select(id, flag), by = "id")

g <- case_obs %>%
  dplyr::filter(grepl("E", id),
                date > forecast_date - 42) %>%
  ggplot() +
  geom_line(aes(x = date, y = cases)) +
  facet_wrap(. ~ geo_name, scales = "free_y") +
  theme_bw() +
  labs(x = "Date", y = "Daily Covid-19 cases",
       title = "Current UTLA-level Covid-19 case forecasts",
       subtitle = format.Date(forecast_date, format = "%d %B %Y")) +
  theme(strip.text.x = element_text(size = 6))

g <- g +
  geom_line(data = plot_case_forecast,
            aes(x = date, y = median, col = flag)) +
  geom_ribbon(data = plot_case_forecast,
              aes(x = date, y = median, ymin = lower_50, ymax = upper_50, fill = flag), alpha = 0.4)

ggsave(filename = here::here("current_forecasts", "current_case_forecast.pdf"),
       device = "pdf",
       width = 16, height = 12, units = "in")
