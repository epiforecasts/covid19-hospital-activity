
## To only allow integer breaks on plots (https://www.r-bloggers.com/setting-axes-to-integer-values-in-ggplot2/)
integer_breaks <- function(n = 5, ...){
  
  fxn <- function(x){
    
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
    
  }
  
  return(fxn)
  
}

## Generate samples from EpiNow2 forecast summary (df has columns "region", "date", "mean", "sd")
epinow_samples <- function(df = NULL, n = 1000){
  
  if(!("id" %in% colnames(case_forecast_summary))){
    
    df <- df %>%
      dplyr::mutate(region = ifelse(grepl("Cornwall", region), "Cornwall", region)) %>%
      dplyr::left_join(readRDS(here::here("data", "raw", "offline_utla_names.rds")), by = c("region" = "geo_name")) %>%
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
