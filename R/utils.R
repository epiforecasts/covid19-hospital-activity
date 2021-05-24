
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
