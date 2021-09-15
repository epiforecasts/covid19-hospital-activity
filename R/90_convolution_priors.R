library(tidyverse)
library(covid19.nhs.data)
library(covidregionaldata)
library(future, quietly = TRUE)

source(here::here("R", "load_data_fns.R"))
source(here::here("R", "forecast_fns.R"))
source(here::here("R", "regional_secondary_fns.R"))
source(here::here("R", "utils.R"))

# Set-up ------------------------------------------------------------------

future::plan("multisession",  gc = TRUE, earlySignal = TRUE)
options(mc.cores = 4)

# Forecast dates to forecast from (defined as first day of forecast)
forecast_dates <- as.character(seq.Date(from = as.Date("2020-10-04"),
                                        to = as.Date("2021-04-25"),
                                        by = "week"))

# Load combined Trust-level data (admissions + cases)
dat <- load_combined_data(add_private = TRUE)

chr <- purrr::map_df(.x = forecast_dates,
                     .f = ~ {
                       
                       forecast_date <- as.Date(.x)
                       
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
                                                 secondary = secondary_opts(type = "incidence"),
                                                 delays = delay_opts(list(
                                                   mean = 2.5, mean_sd = 0.5,
                                                   sd = 0.47, sd_sd = 0.25, max = 30
                                                   )),
                                                 obs = EpiNow2::obs_opts(week_effect = FALSE,
                                                                         scale = list(mean = 0.2, sd = 0.1)),
                                                 burn_in = 14)
                       
                       out <- fit$summarised_posterior %>%
                         dplyr::mutate(date = forecast_date)
                       
                       return(out)
                       
                     }) %>%
  dplyr::bind_rows()

saveRDS(object = chr, file = here::here("data", "out", "admissions_forecast", "regional_chr.rds"))
