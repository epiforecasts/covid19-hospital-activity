
# Download and format admissions data -------------------------------------

load_hospital_data <- function(format = TRUE, keep_data = c("all_adm", "bed_occ"), add_private = FALSE){
  
  raw_data <- covid19.nhs.data::download_trust_data()
  trust_mergers <- read_xlsx_quietly(path = here::here("data", "raw", "trust_mergers.xlsx"))$result %>%
    dplyr::mutate(from_date = as.Date(from_date))
  
  if(format){
    
    out <- raw_data %>%
      dplyr::filter(data %in% c("Hosp ads & diag",
                                "New hosp cases",
                                "All beds COVID",
                                "Adult G&A Beds Occupied COVID",
                                "Adult G&A Bed Occupied NonCOVID",
                                "Adult G&A Beds Unoccupied")) %>%
      dplyr::select(nhs_region, id = org_code, date, data, value) %>%
      dplyr::mutate(data = dplyr::case_when(data == "Hosp ads & diag" ~ "all_adm",
                                            data == "All beds COVID" ~ "bed_occ",
                                            data == "New hosp cases" ~ "new_adm",
                                            data == "Adult G&A Beds Occupied COVID" ~ "ga_covid",
                                            data == "Adult G&A Bed Occupied NonCOVID" ~ "ga_other",
                                            data == "Adult G&A Beds Unoccupied" ~ "ga_unocc")) %>%
      dplyr::filter(data %in% keep_data,
                    id %in% covid19.nhs.data::trust_ltla_mapping$trust_code) %>%
      dplyr::left_join(trust_mergers, by = c("id" = "trust_code_old")) %>% 
      dplyr::mutate(id = ifelse(!is.na(trust_code_new) & date >= from_date, trust_code_new, id)) %>%
      dplyr::group_by(nhs_region, id, date, data) %>%
      dplyr::summarise(value = sum(value, na.rm = TRUE),
                       .groups = "drop") %>%
      dplyr::ungroup() %>%
      tidyr::pivot_wider(id_cols = -c(data, value), names_from = data)
    
  } else {
    
    out <- raw_data
    
  }
  
  if(add_private){
    
    minmax_date <- out %>%
      pivot_longer(cols = -c(nhs_region, id, date)) %>%
      filter(!is.na(value)) %>%
      group_by(name) %>%
      summarise(max_date = max(date),
                .groups = "drop") %>%
      filter(max_date == min(max_date)) %>%
      pull(max_date)
    
    out_private <- readRDS(file = here::here("data", "private", "hospitalisations_trusts.rds")) %>%
      dplyr::select(id = org_code, date, all_adm = hospitalisations) %>%
      dplyr::filter(date > minmax_date | date < min(out$date)) %>%
      dplyr::mutate(all_adm = ifelse(is.na(all_adm), 0, all_adm)) %>%
      tidyr::complete(id = unique(out$id), date) %>%
      dplyr::right_join(out %>% dplyr::select(nhs_region, id) %>% unique() %>% na.omit(), by = "id") %>%
      dplyr::left_join(trust_mergers, by = c("id" = "trust_code_old")) %>% 
      dplyr::mutate(id = ifelse(!is.na(trust_code_new) & date >= from_date, trust_code_new, id)) %>%
      dplyr::group_by(nhs_region, id, date) %>%
      dplyr::summarise(all_adm = sum(all_adm, na.rm = TRUE),
                       .groups = "drop") %>%
      dplyr::ungroup()
    
    out <- out %>%
      dplyr::filter(date <= minmax_date) %>%
      dplyr::bind_rows(out_private)
    
  }
  
  out <- out %>%
    dplyr::filter(! id %in% trust_mergers$trust_code_old[which(trust_mergers$from_date < max(out$date))])
  
  return(out)
  
}

# Load UTLA-level case data -----------------------------------------------
# Wrapper around covidregionaldata::get_regional_data(); England only

load_case_data <- function(){
  
  raw_case <- covidregionaldata::get_regional_data(country = "UK", level = "2") %>%
    dplyr::filter(grepl("E", local_authority_code)) %>%
    dplyr::mutate(local_authority_code = ifelse(local_authority_code == "E10000002", "E06000060", local_authority_code)) %>%
    dplyr::group_by(id = local_authority_code, date) %>%
    dplyr::summarise(cases = sum(cases_new, na.rm = TRUE)) %>%
    dplyr::ungroup()
  
  return(raw_case)
  
}

# Load combined data (by Trust) -------------------------------------------

load_combined_data <- function(add_private = FALSE){
  
  adm <- load_hospital_data(add_private = add_private)
  case <- load_case_data()
  trust_mergers <- read_xlsx_quietly(path = here::here("data", "raw", "trust_mergers.xlsx"))$result %>%
    dplyr::mutate(from_date = as.Date(from_date))
  
  case_trust <- case %>%
    dplyr::left_join(covid19.nhs.data::trust_utla_mapping, by = c("id" = "geo_code")) %>%
    dplyr::mutate(trust_case = p_geo*cases) %>%
    dplyr::left_join(trust_mergers, by = c("trust_code" = "trust_code_old")) %>% 
    dplyr::mutate(trust_code = ifelse(!is.na(trust_code_new) & date >= from_date, trust_code_new, trust_code)) %>%
    dplyr::group_by(trust_code, date) %>%
    dplyr::summarise(trust_case = round(sum(trust_case, na.rm = TRUE)),
                     trust_case = ifelse(is.na(trust_case), 0, trust_case),
                     .groups = "drop") %>%
    dplyr::filter(!is.na(trust_code)) %>%
    dplyr::select(id = trust_code, date, cases = trust_case)
  
  df <- case_trust %>%
    dplyr::full_join(adm, by = c("id", "date")) %>%
    dplyr::filter(id != "RPY")
  
  return(df)
  
}

# Load population data ----------------------------------------------------
## Downloaded and saved offline from
## https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland

load_population_data <- function(level = "utla"){
  
  file_path <- here::here("data", "raw", "ukpopestimatesmid2020on2021geography.xls")
  raw_pop_est <- read_xls_quietly(path = file_path, sheet = "MYE2 - Persons")$result
  colnames(raw_pop_est) <- raw_pop_est[7,]
  raw_pop_est <- raw_pop_est[8:nrow(raw_pop_est),] %>%
    janitor::clean_names()
  
  pop_est <- raw_pop_est %>%
    dplyr::select(id = code, id_name = name, population = all_ages) %>%
    dplyr::mutate(population = as.integer(population))
  
  return(pop_est)
  
}


# Load case forecast data -------------------------------------------------
## forecast_date (str): YYYY-MM-DD string, a Sunday between 2020-10-04 and 2021-04-25
## level (str): level to return case forecasts at ("trust" or "utla")
## replace_flag (bool): flag and replace 'bad' Rt forecasts with ARIMA-ETS ensemble

load_case_forecasts <- function(obs_case_data, forecast_date, forecast_path, level = "trust", replace_flag = TRUE, replace_model = "ae"){
  
  trust_mergers <- read_xlsx_quietly(path = here::here("data", "raw", "trust_mergers.xlsx"))$result %>%
    dplyr::mutate(from_date = as.Date(from_date))
  
  # Load Rt forecast
  case_forecast_file <- paste0("cases_by_report_", forecast_date, ".csv")
  case_forecast <- read_csv_quietly(file = here::here(forecast_path, case_forecast_file))$result %>%
    dplyr::filter(date > forecast_date) %>%
    dplyr::mutate(region = ifelse(region == "Hackney and City of London", "Hackney", region),
                  region = ifelse(region == "Cornwall and Isles of Scilly", "Cornwall", region)) %>%
    dplyr::left_join(covid19.nhs.data::utla_names, by = c("region" = "geo_name")) %>%
    dplyr::select(id = geo_code, id_name = region, date, median, mean, sd, contains("lower"), contains("upper"))
  
  case_forecast_samples <- epinow_samples(df = case_forecast)
  
  # Check for flagged UTLAs 
  flagged_utlas <- check_case_forecasts(obs_data = obs_case_data,
                                        forecast_date = forecast_date,
                                        forecast_path = forecast_path)
  ## Replace flagged forecasts with ARIMA+ETS ensemble
  if(replace_flag & nrow(flagged_utlas) > 0){
    
    message("Replace flagged Rt forecasts with timeseries forecast...")
    # Make timeseries forecasts
    case_dat_in <- obs_case_data %>%
      dplyr::filter(id %in% flagged_utlas$id) %>%
      dplyr::select(id, date, cases)
    case_tsensemble_samples <- timeseries_samples(data = case_dat_in, yvar = "cases",
                                                  horizon = 14, samples = 1000, models = replace_model, 
                                                  train_from = forecast_date - 42,
                                                  forecast_from = forecast_date) %>%
      dplyr::mutate(model = "ts_ensemble")
    case_tsensemble_summary <- forecast_summary(samples = case_tsensemble_samples)
    
    # Replace in case_forecast and case_forecast_samples
    case_forecast <- case_forecast %>%
      dplyr::filter(!id %in% flagged_utlas$id) %>%
      dplyr::bind_rows(case_tsensemble_summary %>%
                         dplyr::ungroup() %>%
                         dplyr::select(-c(model, forecast_from, quantile)) %>%
                         tidyr::pivot_wider(id_cols = -c(quantile_label, value), names_from = quantile_label) %>%
                         dplyr::select(id, date = date_horizon, median = lower_0, lower_90, lower_50, upper_50, upper_90))
    case_forecast_samples <- case_forecast_samples %>%
      dplyr::filter(!id %in% flagged_utlas$id) %>%
      dplyr::bind_rows(case_tsensemble_samples %>%
                         dplyr::mutate(date = forecast_from + horizon,
                                       value = round(value)) %>%
                         dplyr::select(forecast_from, id, date, sample, value))
    
  }
  
  message("Plotting UTLA case forecasts...")
  g <- case_forecast %>%
    dplyr::filter(!is.na(id),
                  grepl("E", id)) %>%
    dplyr::mutate(model = ifelse(id %in% flagged_utlas$id, "ARIMA+ETS", "Rt")) %>%
    ggplot(aes(x = date, y = median)) +
    geom_line(data = obs_case_data %>%
                dplyr::ungroup() %>%
                dplyr::filter(date > forecast_date - 21,
                              date <= forecast_date,
                              id %in% case_forecast$id,
                              grepl("E", id)),
              aes(x = date, y = cases)) +
    geom_line(aes(col = model), lwd = 0.8) +
    geom_ribbon(aes(ymin = lower_90, ymax = upper_90, fill = model), col = NA, alpha = 0.4) +
    facet_wrap(. ~ id, scales = "free_y") +
    scale_x_date(breaks = c(forecast_date - 14, forecast_date, forecast_date + 14), date_labels = "%d%b") +
    scale_y_continuous(limits = c(0, NA)) +
    labs(title = "UTLA case forecasts",
         subtitle = paste0("Forecast from ", format.Date(forecast_date, "%d %B %Y")),
         x = "Date", y = "Cases", col = "Model", fill = "Model") +
    theme_bw() +
    theme(legend.position = "none", strip.text.x = element_text(size = 8))
  
  # Optionally map from UTLA to Trust
  ## NB formatting to use in run_arimareg.R and run_convolution.R
  if(level == "trust"){
    
    case_forecast <- case_forecast %>%
      dplyr::select(geo_code = id, date, case_forecast = median) %>%
      dplyr::left_join(covid19.nhs.data::trust_utla_mapping, by = "geo_code") %>%
      dplyr::mutate(case_forecast_trust = p_geo * case_forecast) %>%
      dplyr::left_join(trust_mergers, by = c("trust_code" = "trust_code_old")) %>% 
      dplyr::mutate(trust_code = ifelse(!is.na(trust_code_new) & date >= from_date, trust_code_new, trust_code)) %>%
      dplyr::group_by(id = trust_code, date) %>%
      dplyr::summarise(case_forecast = round(sum(case_forecast_trust, na.rm = TRUE)),
                       .groups = "drop")
    
    case_forecast_samples <- case_forecast_samples %>%
      dplyr::left_join(covid19.nhs.data::trust_utla_mapping, by = c("id" = "geo_code")) %>%
      dplyr::mutate(trust_value = p_geo*value) %>%
      dplyr::left_join(trust_mergers, by = c("trust_code" = "trust_code_old")) %>% 
      dplyr::mutate(trust_code = ifelse(!is.na(trust_code_new) & date >= from_date, trust_code_new, trust_code)) %>%
      dplyr::group_by(forecast_from, trust_code, date, sample) %>%
      dplyr::summarise(value = round(sum(trust_value, na.rm = TRUE)),
                       value = ifelse(is.na(value), 0, value),
                       .groups = "drop") %>%
      dplyr::filter(!is.na(trust_code)) %>%
      dplyr::select(region = trust_code, date, sample, cases = value)
    
  }
  
  return(list(summary = case_forecast,
              samples = case_forecast_samples,
              flag = flagged_utlas,
              plot = g))
  
}

