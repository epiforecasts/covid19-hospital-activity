

# Download and format admissions data -------------------------------------

load_hospital_data <- function(format = TRUE, keep_data = c("all_adm", "bed_occ")){
  
  raw_data <- covid19.nhs.data::download_trust_data()
  
  if(format){
    
    out <- raw_data %>%
      dplyr::filter(data %in% c("Hosp ads & diag", "New hosp cases", "All beds COVID")) %>%
      dplyr::select(id = org_code, date, data, value) %>%
      dplyr::mutate(data = dplyr::case_when(data == "Hosp ads & diag" ~ "all_adm",
                                            data == "All beds COVID" ~ "bed_occ",
                                            data == "New hosp cases" ~ "new_adm"),
                    id = ifelse(id %in% c("RD3", "RDZ"), "R0D", id)) %>%
      dplyr::filter(data %in% keep_data,
                    id %in% covid19.nhs.data::trust_ltla_mapping$trust_code) %>%
      dplyr::group_by(id, date, data) %>%
      dplyr::summarise(value = sum(value, na.rm = TRUE),
                       .groups = "drop") %>%
      dplyr::ungroup() %>%
      tidyr::pivot_wider(id_cols = -c(data, value), names_from = data)
    
  } else {
    
    out <- raw_data
    
  }
  
  return(out)
  
}
