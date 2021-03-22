
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

## Generate samples from EpiNow2 forecast summary (df has columns "region", "date", "mean", "sd")
epinow_samples <- function(df = NULL, n = 1000){
  
  if(!("id" %in% colnames(df))){
    
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

## Function to return clusters, based on correlation and hclust function
return_clusters <- function(df, k = 6, exclude_ids = NULL){
  
  mat_in <- df %>%
    dplyr::filter(!id %in% small_trust,
                  !id %in% exclude_ids) %>%
    tidyr::pivot_wider(id_cols = date, names_from = id) %>%
    dplyr::select(-date)
  
  mat_out <- cor(mat_in, use = "pairwise.complete.obs", method = "kendall")
  
  # Using 1-correlation as distance, construct tree
  c_dist <- as.dist(1 - mat_out)
  out_tree <- hclust(c_dist, method = "complete")
  g <- plot(out_tree, cex = 0.6)
  
  out_dend <- as.dendrogram(out_tree)
  
  clusters <- dendextend::cutree(out_dend, k = k)
  out_clusters <- tibble::tibble(id = names(clusters), cluster = clusters)
  
  return(list(tree_plot = g, clusters = out_clusters))
  
}

## function to identify local peak(s) in weekly data
find_local_peaks <- function(df = NULL,
                             cutoff_date = "2020-12-09",
                             threshold_max = 0.95,
                             threshold_size = 70,
                             threshold_ratio = 2){
  
  ## Get dates of peaks before/after cutoff_date
  peaks <- df %>%
    dplyr::group_by(id, early_max = date < as.Date(cutoff_date)) %>%
    dplyr::filter(adm >= threshold_max*max(adm, na.rm = TRUE)) %>%
    dplyr::group_by(id, early_max) %>%
    dplyr::summarise(date = min(date, na.rm = TRUE),
                     .groups = "drop_last") %>%
    dplyr::arrange(id, -early_max) %>%
    dplyr::mutate(early_max = ifelse(early_max, "first_peak", "second_peak")) %>%
    tidyr::pivot_wider(id_cols = id, names_from = early_max, values_from = date)
  
  ## Get peak details (value, and minimum between them)
  peaks_details <- df %>%
    dplyr::left_join(peaks, by = "id") %>%
    dplyr::filter(date >= first_peak,
                  date <= second_peak) %>%
    dplyr::group_by(id, first_peak, second_peak) %>%
    dplyr::summarise(localmin_dat = date[which.min(adm)],
                     first_peak_val = adm[which(first_peak == date)],
                     second_peak_val = adm[which(second_peak == date)],
                     localmin_val = min(adm),
                     .groups = "drop_last") %>%
    dplyr::select(id, first_peak, localmin_dat, second_peak, first_peak_val, localmin_val, second_peak_val)
  
  
  exclude_trusts <- peaks_details %>%
    dplyr::filter(first_peak_val < threshold_size | second_peak_val < threshold_size) %>%
    dplyr::pull(id)
  
  ## One peak
  one_peak <- peaks_details %>%
    dplyr::filter(!id %in% exclude_trusts,
                  first_peak == localmin_dat | second_peak == localmin_dat) %>%
    dplyr::pull(id)
  
  ## Two distinct peaks
  two_peaks <- peaks_details %>%
    dplyr::filter(!id %in% c(exclude_trusts, one_peak),
                  first_peak_val/localmin_val >= threshold_ratio,
                  second_peak_val/localmin_val >= threshold_ratio) %>%
    dplyr::pull(id)
  
  ## Two indistinct peaks
  two_peaks_indistinct <- adm_localmax_det %>%
    dplyr::filter(!id %in% c(exclude_trusts, one_peak, two_peaks),
                  first_peak_val/localmin_val < threshold_ratio | second_peak_val/localmin_val < threshold_ratio) %>%
    dplyr::pull(id)
  
  peaks_out <- peaks %>%
    tidyr::pivot_longer(cols = -id, values_to = "date") %>%
    dplyr::left_join(df %>% dplyr::rename(value = adm), by = c("id", "date")) %>%
    dplyr::mutate(group = case_when(id %in% one_peak ~ "one",
                                    id %in% two_peaks ~ "two",
                                    id %in% two_peaks_indistinct ~ "two_unclear")) %>%
    dplyr::filter(!(group == "one" & name == "first_peak")) %>%
    dplyr::select(nhs_region, id, group, name, date, value)
  
  return(peaks_out)
  
}
