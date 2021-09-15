
# Quiet loading functions -------------------------------------------------

read_xls_quietly <- purrr::quietly(readxl::read_xls)
read_xlsx_quietly <- purrr::quietly(readxl::read_xlsx)
read_csv_quietly <- purrr::quietly(readr::read_csv)

## To only allow integer breaks on plots (https://www.r-bloggers.com/setting-axes-to-integer-values-in-ggplot2/)
integer_breaks <- function(n = 5, ...){
  
  fxn <- function(x){
    
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
    
  }
  
  return(fxn)
  
}

## Get coordinates from given postcode (wrapper around PostcodesioR::postcode_query())
extract_postcode <- function(outcode = NULL){
  
  if(is.na(outcode)){
    
    out <- NA
    
  } else {
    
    query_res <- PostcodesioR::postcode_lookup(postcode = outcode)
    query_cod <- query_res$admin_district
    
    if(is.null(query_cod) | length(query_cod) == 0){
      
      out <- list(longitude = NA,
                  latitude = NA)
      
    } else {
      
      out <- list(longitude = query_res$longitude,
                  latitude = query_res$latitude)
      
    }
    
  }
  
  return(out)
  
}

## Generate samples from EpiNow2 forecast summary (df has columns "id", "date", "mean", "sd")
epinow_samples <- function(df = NULL, n = 1000){
  
  if(!("id" %in% colnames(df))){
    
    df <- df %>%
      dplyr::mutate(region = ifelse(grepl("Cornwall", region), "Cornwall", region)) %>%
      dplyr::left_join(covid19.nhs.data::utla_names, by = c("region" = "geo_name")) %>%
      dplyr::rename(id = geo_code) %>%
      dplyr::filter(!is.na(id))
    
  }
  
  forecast_date <- df %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(n = n(),
                     .groups = "drop") %>%
    dplyr::filter(n > max(n)/2) %>%
    dplyr::pull(date) %>%
    min() - 1
  
  grid <- expand_grid(ids = unique(df$id), dates = unique(df$date))
  ids <- grid %>% pull(ids)
  dates <- grid %>% pull(dates)
  
  samples <- purrr::map2_df(.x = ids, .y = dates,
                            .f = ~{
                              mu = df %>%
                                dplyr::filter(id == .x,
                                              date == .y) %>%
                                dplyr::pull(mean)
                              
                              sigma = df %>%
                                dplyr::filter(id == .x,
                                              date == .y) %>%
                                pull(sd)
                                           
                              out <- tibble::tibble(value = MASS::rnegbin(n = 1000, mu = mu, theta = (mu^2)/(sigma^2 - mu))) %>%
                                dplyr::mutate(id = .x,
                                              date = .y,
                                              forecast_from = as.Date(forecast_date),
                                              sample = 1:n) %>%
                                dplyr::select(forecast_from, id, date, sample, value)
                              
                              return(out)
                              
                              }) %>%
    dplyr::bind_rows()
  
  return(samples)
  
  
}

## Function to return clusters, based on correlation and hclust function
return_clusters <- function(df, k = 6, exclude_ids = NULL){
  
  mat_in <- df %>%
    dplyr::filter(!id %in% exclude_ids) %>%
    tidyr::pivot_wider(id_cols = date, names_from = id) %>%
    dplyr::select(-date)
  
  mat_out <- cor(mat_in, use = "pairwise.complete.obs", method = "pearson")
  
  # Using 1-correlation as distance, construct tree
  c_dist <- as.dist(1 - mat_out)
  out_tree <- hclust(c_dist, method = "complete")
  g <- plot(out_tree, cex = 0.6)
  
  out_dend <- as.dendrogram(out_tree)
  
  clusters <- dendextend::cutree(out_dend, k = k)
  out_clusters <- tibble::tibble(id = names(clusters), cluster = clusters)
  
  return(list(tree_plot = g, clusters = out_clusters))
  
}


# Check UTLA-level Rt case forecasts --------------------------------------

check_case_forecasts <- function(obs_data, 
                                 forecast_date,
                                 forecast_path = here::here("data", "out", "epinow2_case_forecast")){
  
  message("Checking UTLA-level case forecasts...")
  
  # Load and reshape data
  pop_est <- load_population_data()
  
  recent_cases <- obs_data %>%
    dplyr::filter(date == forecast_date) %>%
    dplyr::left_join(pop_est %>% dplyr::select(-id_name), by = "id") %>%
    dplyr::mutate(forecast_from = forecast_date) %>%
    dplyr::select(id, id_name = geo_name, population, forecast_from, last_case = cases)
  
  case_forecast_file <- paste0("cases_by_report_", forecast_date, ".csv")
  case_forecast <- read_csv_quietly(file = here::here(forecast_path, case_forecast_file))$result %>%
    dplyr::mutate(region = ifelse(region == "Hackney and City of London", "Hackney", region),
                  region = ifelse(region == "Cornwall and Isles of Scilly", "Cornwall", region)) %>%
    dplyr::left_join(covid19.nhs.data::utla_names, by = c("region" = "geo_name")) %>%
    dplyr::select(id = geo_code, id_name = region, date, median, mean, sd, contains("lower"), contains("upper"))
  
  data_in <- case_forecast %>%
    dplyr::left_join(recent_cases %>% dplyr::select(-id_name), by = "id")
  
  # Flag UTLAS
  ## (1) missing case forecasts
  check_missing <- recent_cases %>%
    dplyr::filter(id %in% setdiff(recent_cases$id, unique(case_forecast$id))) %>%
    dplyr::mutate(flag = "missing")
  ## (2) forecast 90% CI exceeds UTLA population
  check_population <- data_in %>%
    dplyr::filter(upper_90 > population) %>%
    dplyr::mutate(flag = "population")
  ## (3) uncertainty
  check_uncertainty <- data_in %>%
    dplyr::filter(!id %in% check_population$id,
                  (last_case > 0 & upper_90 > 1e3*last_case)
    ) %>%
    dplyr::mutate(flag = "uncertainty")
  
  flag_utlas <- check_missing %>%
    dplyr::bind_rows(check_population) %>%
    dplyr::bind_rows(check_uncertainty)
  
  return(flag_utlas)
  
}
