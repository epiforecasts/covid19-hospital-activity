
# Calculate correlation ---------------------------------------------------

# df (data.frame): with columns id, date and admissions and "var"
# var (string): column name to get correlation with admissions
# region (string): name of region (id) 
# lag_days (int): lag between admissions and var
# date_end (string YYYY-MM-DD): end of 6-week window

calc_cor_int <- function(df = NULL, var = NULL, region = NULL, lag_days = NULL, date_end = NULL){
  
  df <- df %>%
    dplyr::filter(id == region) %>%
    na.omit()
  df <- df %>%
    dplyr::bind_cols(lag(df[var], n = lag_days) %>%
                       dplyr::rename(lag = var)) %>%
    dplyr::filter(date > as.Date(date_end) - 42,
                  date <= as.Date(date_end))
  
  out <- cor(df$admissions, df$lag)
  
  return(out)
  
}


# Correlation pipeline ----------------------------------------------------

# Wrapper to run calc_cor_int for multiple trusts/lags

calc_cor <- function(df = NULL, var = NULL, trusts = NULL, lags = 0:20, date_end = NULL){
  
  code_col <- colnames(df)[grepl("id", colnames(df))]
  
  ## Use all Trusts if none are specified
  if(is.null(trusts)){
    
    trusts <- unique(pull(df, code_col))
    
  }
  grid <- expand.grid(trusts = trusts, lags = lags)
  trusts <- grid$trusts
  lags <- grid$lags
  
  
  ## Calculate lagged correlation over trusts and lags
  out <- purrr::map2(trusts, lags,
                     .f = ~{
                       corr <- calc_cor_int(df = df,
                                            var = var,
                                            date_end = date_end,
                                            region = .x,
                                            lag_days = .y)
                       tibble::tibble(id = .x, lag_days = .y, corr)
                     }) %>%
    dplyr::bind_rows()
  
  return(out)
  
}
