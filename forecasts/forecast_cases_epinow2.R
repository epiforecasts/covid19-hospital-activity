
library(tidyverse)
library(EpiNow2)

# Define forecast dates ---------------------------------------------------

# forecast_dates <- as.character(seq.Date(from = as.Date("2020-08-02") + 56 + 7,
#                                         by = "week",
#                                         length = 11))

forecast_dates <- "2021-01-31"


# Download raw UTLA-level case data ---------------------------------------

# raw_case <- covidregionaldata::get_regional_data("UK", include_level_2_regions = TRUE)
raw_case <- readRDS(file = here::here("data", "raw", "uk_utla_case.rds"))



# Format for EpiNow2 input ------------------------------------------------

reported_cases <- raw_case %>%
  dplyr::filter(stringr::str_sub(utla_code, 1, 1) == "E",
                date <= max(as.Date(forecast_dates))) %>%
  dplyr::select(authority, date, confirm = cases_new) %>%
  dplyr::rename(region = authority)



# Set up ------------------------------------------------------------------

n_cores <- EpiNow2::setup_future(reported_cases = reported_cases)
options(mc.cores = n_cores)

## COVID-19 distributions and delays
generation_time <- get_generation_time(disease = "SARS-CoV-2", source = "ganyani")
incubation_period <- get_incubation_period(disease = "SARS-CoV-2", source = "lauer")
reporting_delay <- readRDS(file = here::here("data", "raw", "onset_to_admission_delay.rds"))



# Run EpiNow2 over all forecast dates -------------------------------------

for(forecast_date in forecast_dates){
  
  forecast_date <- as.Date(forecast_date)
  
  ## 16-week training window
  reported_cases_in <- reported_cases %>%
    dplyr::filter(date > forecast_date - 16*7,
                  date <= forecast_date)
  reported_cases_in <- data.table::data.table(reported_cases_in)
  
  ## Run EpiNow2 on UTLAs
  out <- regional_epinow(reported_cases = reported_cases_in, 
                         generation_time = generation_time,
                         delays = delay_opts(incubation_period, reporting_delay),
                         rt = rt_opts(prior = list(mean = 2, sd = 0.2)),
                         horizon = 14,
                         return_output = TRUE,
                         output = c("regions"),
                         verbose = interactive())
  
  out_summary <- regional_summary(regional_output = out$regional,
                                  reported_cases = reported_cases_in)
  
  ## Save reported cases forecasts
  out_reported <- out_summary$results$estimates$summarised %>%
    dplyr::filter(variable == "reported_cases",
                  type == "forecast") %>%
    dplyr::select(-c(variable, strat)) %>%
    dplyr::mutate(across(where(is.double), round))
  
  file_name <- paste0("epinow_utla_", forecast_date, ".rds")
  saveRDS(object = out_reported, file = here::here("data", "utla_case_forecast", file_name))
  
}



