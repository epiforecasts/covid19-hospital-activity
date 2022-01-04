library(tidyverse)
library(covid19.nhs.data)
library(covidregionaldata)
library(forecast)
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

# Load observed UTLA-level cases
case_dat <- load_case_data() %>%
  dplyr::left_join(covid19.nhs.data::utla_names, by = c("id" = "geo_code"))

# Load combined Trust-level data (admissions + cases)
dat <- load_combined_data(add_private = TRUE)

# List of Trust mergers
trust_mergers <- read_xlsx_quietly(path = here::here("data", "raw", "trust_mergers.xlsx"))$result %>%
  dplyr::mutate(from_date = as.Date(from_date))


# Update individual model forecasts ---------------------------------------

model_summary <- tibble()

for(forecast_date in forecast_dates){
  
  forecast_date <- as.Date(forecast_date)
  
  # Manually 
  if(forecast_date + 14 > as.Date("2021-02-01")){
    
    dat <- dat %>%
      dplyr::filter(id != "RT3")
    
    if(forecast_date + 14 > as.Date("2021-04-01")){
      
      dat <- dat %>%
        dplyr::filter(id != "RXH")
      
    }
    
  }
  
  model_sum_out <- purrr::map_df(.x = unique(dat$id),
                                 .f = ~ {
                                   
                                   # forecast_date <- as.Date("2021-01-03")
                                   # .x <- "RXR"
                                   
                                   dat_int <- dat %>%
                                     filter(date <= forecast_date,
                                            date > forecast_date - 42,
                                            id == .x) %>%
                                     pull(all_adm)
                                   
                                   if(sum(dat_int, na.rm = TRUE) > 0) {
                                     
                                     # ARIMA, not seasonal
                                     arima_fit <- auto.arima(dat_int, trace = FALSE)
                                     arima_out0 <- tibble(
                                       id = .x,
                                       model_ver = "non_seasonal",
                                       model = "arima",
                                       parameter = c(names(arimaorder(arima_fit))),
                                       value = c(arimaorder(arima_fit))
                                     ) %>%
                                       complete(id,
                                                model_ver,
                                                model,
                                                parameter = c("p", "d", "q"),
                                                fill = list(value = 0))
                                     if("intercept" %in% names(arima_fit$coef)){
                                       arima_out0 <- arima_out0 %>%
                                         bind_rows(tibble(
                                           id = .x,
                                           model_ver = "non_seasonal",
                                           model = "arima",
                                           parameter = "intercept",
                                           value = arima_fit$coef["intercept"]
                                         ))
                                     }
                                     if("drift" %in% names(arima_fit$coef)){
                                       arima_out0 <- arima_out0 %>%
                                         bind_rows(tibble(
                                           id = .x,
                                           model_ver = "non_seasonal",
                                           model = "arima",
                                           parameter = "drift",
                                           value = arima_fit$coef["drift"]
                                         ))
                                     }
                                     
                                     # ETS, not seasonal
                                     ets_fit <- ets(dat_int, trace = FALSE)
                                     ets_out0 <- tibble(
                                       id = .x,
                                       model_ver = "non_seasonal",
                                       model = "ets",
                                       parameter = c("error", "trend", "season"),
                                       value = ets_fit$components[1:3]
                                     ) %>%
                                       mutate(value = case_when(value == "N" ~ 0,
                                                                value == "A" ~ 1,
                                                                value == "M" ~ 2))
                                     
                                     
                                     ##### Seasonal models #####
                                     
                                     dat_int <- dat_int %>%
                                       ts(frequency = 7)
                                     
                                     # ARIMA, seasonal
                                     arima_fit <- auto.arima(dat_int, trace = FALSE)
                                     arima_out <- tibble(
                                       id = .x,
                                       model_ver = "seasonal",
                                       model = "arima",
                                       parameter = c(names(arimaorder(arima_fit))),
                                       value = c(arimaorder(arima_fit))
                                     ) %>%
                                       complete(id,
                                                model_ver,
                                                model,
                                                parameter = c("p", "d", "q", "P", "D", "Q"),
                                                fill = list(value = 0)) %>%
                                       mutate(parameter = case_when(parameter %in% c("P", "D", "Q") ~ tolower(paste0(parameter, "_season")),
                                                                    TRUE ~ parameter)) %>%
                                       filter(parameter != "Frequency")
                                     if("intercept" %in% names(arima_fit$coef)){
                                       arima_out <- arima_out %>%
                                         bind_rows(tibble(
                                           id = .x,
                                           model_ver = "seasonal",
                                           model = "arima",
                                           parameter = "intercept",
                                           value = arima_fit$coef["intercept"]
                                         ))
                                     }
                                     if("drift" %in% names(arima_fit$coef)){
                                       arima_out <- arima_out %>%
                                         bind_rows(tibble(
                                           id = .x,
                                           model_ver = "seasonal",
                                           model = "arima",
                                           parameter = "drift",
                                           value = arima_fit$coef["drift"]
                                         ))
                                     }
                                     
                                     # ETS, seasonal
                                     ets_fit <- ets(dat_int, trace = FALSE)
                                     ets_out <- tibble(
                                       id = .x,
                                       model_ver = "seasonal",
                                       model = "ets",
                                       parameter = c("error", "trend", "season"),
                                       value = ets_fit$components[1:3]
                                     ) %>%
                                       mutate(value = case_when(value == "N" ~ 0,
                                                                value == "A" ~ 1,
                                                                value == "M" ~ 2))
                                     
                                     # seastests::isSeasonal
                                     seas_out <- tibble(
                                       id = .x,
                                       model_ver = "seasonal",
                                       parameter = "season_test",
                                       value = seastests::isSeasonal(dat_int)
                                     )
                                     
                                     out <- arima_out0 %>%
                                       bind_rows(ets_out0) %>%
                                       bind_rows(arima_out) %>%
                                       bind_rows(ets_out) %>%
                                       bind_rows(seas_out)
                                     
                                   } else {
                                     
                                     out <- tibble()
                                     
                                   }
                                   
                                   return(out)
                                   
                                 }) %>%
    bind_rows() %>%
    mutate(forecast_date = forecast_date)
  
  model_summary <- model_summary %>%
    bind_rows(model_sum_out)
  
}


# Save results ------------------------------------------------------------

saveRDS(object = model_summary,
        file = here::here("data", "out", "evaluation", "timeseries_fit_summary.rds"))

