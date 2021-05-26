
# Download and format admissions data -------------------------------------

load_hospital_data <- function(format = TRUE, keep_data = c("all_adm", "bed_occ"), add_private = FALSE){
  
  raw_data <- covid19.nhs.data::download_trust_data()
  
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
                                            data == "Adult G&A Beds Unoccupied" ~ "ga_unocc"),
                    id = ifelse(id %in% c("RD3", "RDZ"), "R0D", id)) %>%
      dplyr::filter(data %in% keep_data,
                    id %in% covid19.nhs.data::trust_ltla_mapping$trust_code) %>%
      dplyr::group_by(nhs_region, id, date, data) %>%
      dplyr::summarise(value = sum(value, na.rm = TRUE),
                       .groups = "drop") %>%
      dplyr::ungroup() %>%
      tidyr::pivot_wider(id_cols = -c(data, value), names_from = data)
    
  } else {
    
    out <- raw_data
    
  }
  
  if(add_private){
    
    out_private <- readRDS(file = here::here("data", "private", "hospitalisations_trusts.rds")) %>%
      dplyr::select(id = org_code, date, all_adm = hospitalisations) %>%
      dplyr::filter(date > max(out$date)) %>%
      dplyr::mutate(all_adm = ifelse(is.na(all_adm), 0, all_adm)) %>%
      tidyr::complete(id = unique(out$id), date) %>%
      dplyr::right_join(out %>% dplyr::select(nhs_region, id) %>% unique() %>% na.omit(), by = "id")
    
    out <- out %>%
      dplyr::bind_rows(out_private)
    
  }
  
  return(out)
  
}

# Load UTLA-level case data -----------------------------------------------
# Wrapper around covidregionaldata::get_regional_data(); England only

load_case_data <- function(){
  
  raw_case <- covidregionaldata::get_regional_data("UK", include_level_2_regions = TRUE) %>%
    dplyr::filter(grepl("E", utla_code)) %>%
    dplyr::mutate(utla_code = ifelse(utla_code == "E10000002", "E06000060", utla_code)) %>%
    dplyr::group_by(id = utla_code, date) %>%
    dplyr::summarise(cases = sum(cases_new, na.rm = TRUE)) %>%
    dplyr::ungroup()
  
  return(raw_case)
  
}

# Load combined data (by Trust) -------------------------------------------

load_combined_data <- function(add_private = FALSE){
  
  adm <- load_hospital_data(add_private = add_private)
  case <- load_case_data()
  
  case_trust <- case %>%
    dplyr::left_join(covid19.nhs.data::trust_utla_mapping, by = c("id" = "geo_code")) %>%
    dplyr::mutate(trust_case = p_geo*cases) %>%
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
