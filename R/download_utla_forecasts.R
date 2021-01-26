library(tidyverse)



# New (daily) forecasts ---------------------------------------------------

utla_forecast <- readr::read_csv(file = "https://raw.githubusercontent.com/epiforecasts/covid-rt-estimates/master/subnational/united-kingdom-local/cases/summary/cases_by_report.csv") %>%
  dplyr::filter(type == "forecast")

forecast_date <- utla_forecast %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(n = n(),
                   .groups = "drop") %>%
  dplyr::filter(n > max(n)/2) %>%
  pull(date) %>%
  min() - 1

utla_forecast <- utla_forecast %>%
  dplyr::filter(date > forecast_date)

file_name <- paste0("epinow_utla_", forecast_date, ".rds")

saveRDS(object = utla_forecast,
        file = here::here("data", "utla_case_forecast", file_name))





# Download past forecasts (if any are missed) -----------------------------

commit_ids <- c("69bb0150f3958d652300a5363f41cb240a565030",
                "c72b41c681238805fe85095ad7d556cc9660b102")

for(c in commit_ids){
  
  url <- paste0("https://raw.githubusercontent.com/epiforecasts/covid-rt-estimates/",
                c,
                "/subnational/united-kingdom-local/cases/summary/cases_by_report.csv")
  
  utla_forecast <- readr::read_csv(file = url) %>%
    dplyr::filter(type == "forecast")
  
  forecast_date <- utla_forecast %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(n = n(),
                     .groups = "drop") %>%
    dplyr::filter(n > max(n)/2) %>%
    pull(date) %>%
    min() - 1
  
  utla_forecast <- utla_forecast %>%
    dplyr::filter(date > forecast_date)
  
  file_name <- paste0("epinow_utla_", forecast_date, ".rds")
  
  saveRDS(object = utla_forecast,
          file = here::here("data", "utla_case_forecast", file_name))
  
}
