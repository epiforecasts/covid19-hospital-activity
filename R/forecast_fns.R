
# Baseline model, summary only --------------------------------------------

# Internal function
# Returns forecast summary (median, and 50% and 90% CIs) only

# Based around forecast::snaive with some modifications

forecast_baseline_int <- function(y = NULL, horizon = NULL){
  
  f <- forecast::snaive(y = y,
                        h = horizon,
                        drift = TRUE,
                        level = c(50, 90))
  out <- tibble::tibble(q_0.5 = as.double(f$mean),
                        q_0.25 = f$lower[,1],
                        q_0.05 = f$lower[,2],
                        q_0.75 = f$upper[,1],
                        q_0.95 = f$upper[,2]) %>%
    dplyr::mutate(horizon = 1:horizon) %>%
    dplyr::mutate_at(vars(contains("q_")), ~ ifelse(.x < 0, 0, .x)) %>%
    tidyr::pivot_longer(cols = c(contains("q_")), names_to = "quantile") %>%
    dplyr::mutate(quantile = as.double(stringr::str_remove(quantile, "q_")),
                  quantile_label = paste0(ifelse(quantile <= 0.5, "lower", "upper"), "_", abs(1 - 2 * quantile) * 100))
  
  out <- out %>%
    dplyr::bind_rows(out %>%
                       dplyr::filter(quantile_label == "lower_0") %>%
                       dplyr::mutate(quantile_label = "upper_0"))
  
  return(out)
  
}


# Baseline model, samples and summary -------------------------------------

# Wrapper around forecast_baseline_int
# Returns forecast summary (median, and 50% and 90% CIs) and samples

forecast_baseline <- function(data = NULL, yvar = NULL,
                              horizon = 14, samples = 1000, train_from = NULL, forecast_from = NULL){
  
  data <- data %>%
    dplyr::mutate(forecast = date > forecast_from) %>%
    dplyr::filter(date > as.Date(train_from),
                  date <= as.Date(forecast_from) + horizon)
  
  baseline_summary <- data %>%
    dplyr::group_by(id) %>%
    dplyr::group_modify( ~ forecast_baseline_int(y = .x %>% filter(!forecast) %>% pull(yvar),
                                                 horizon = 14)) %>%
    dplyr::mutate(model = "baseline",
                  forecast_from = as.Date(forecast_from),
                  date_horizon = forecast_from + horizon)
  
  baseline_samples <- baseline_summary %>%
    dplyr::filter(quantile_label %in% c("upper_0", "upper_90")) %>%
    dplyr::select(model, forecast_from, id, horizon, quantile, value) %>%
    tidyr::pivot_wider(id_cols = -c(quantile, value), names_from = quantile) %>%
    dplyr::rename(mean = `0.5`) %>%
    dplyr::mutate(sd = (`0.95` - mean)/qnorm(0.95)) %>%
    dplyr::group_by(model, forecast_from, id, horizon) %>%
    dplyr::group_modify(.f = ~ { out <- tibble::tibble(sample = 1:samples,
                                                       value = rnorm(n = samples, mean = .x$mean, sd = .x$sd))}
    ) %>%
    dplyr::mutate(value = ifelse(value < 0, 0, value))
  
  return(list(samples = baseline_samples, summary = baseline_summary))
  
}

# ARIMA with xreg wrapper -------------------------------------------------

## Based on EpiSoon::forecast_model, with some modifications
## Used internally in timeseries_samples

## Returns ARIMA + xreg forecast samples

forecast_model_xreg <- function(y = NULL, samples = NULL, horizon = NULL,
                                model_params = NULL, forecast_params = NULL,
                                ...) {
  
  # convert to timeseries object
  timeseries <- stats::ts(y)
  
  # fit and forecast
  fit <- suppressMessages(
    suppressWarnings(
      do.call(forecast::auto.arima,
              c(list(y = timeseries), model_params))
    )
  )
  prediction <- do.call(forecast::forecast,
                        c(list(object = fit, h = horizon), forecast_params))
  
  # extract samples and tidy format
  sample_from_model <- prediction
  
  if (samples == 1) {
    sample_from_model <- data.frame(t(as.data.frame(sample_from_model$mean)))
    rownames(sample_from_model) <- NULL
  } else {
    mean <- as.numeric(prediction$mean)
    upper <- prediction$upper[, ncol(prediction$upper)]
    lower <-  prediction$lower[, ncol(prediction$lower)]
    sd <- (upper - lower) / 3.92
    sample_from_model <- purrr::map2(mean, sd,
                                     ~ rnorm(samples, mean = .x,  sd = .y))
    
    sample_from_model <- suppressMessages(dplyr::bind_cols(sample_from_model))
  }
  
  return(sample_from_model)
  
}


# Fit time series model(s) ------------------------------------------------
## Based on `forecast` and `forecastHybrid` wrappers in `EpiSoon`
## Fits models:
##  Autoregressive time series: ARIMA (models = "a"), ETS ("e"), TBATS ("t"), naive ("z")
##  Dynamic regression: ARIMA + xreg (models = "a", xreg = c("predictor1", "predictor2"))

# Parameters:
#   data (data.frame): columns "id", "date", yvar, xvars
#   yvar (string): name of column to forecast
#   xvars (string): name(s) of predictors (used in ARIMA regression model only)
#   horizon (integer): length of horizon (days) for forecasting. Default value 14
#   samples (integer): number of samples to take for timeseries model(s). Default value 1000
#   train_from (date or string "YYYY-MM-DD"): date to begin training window from
#   forecast_from (date or string "YYYY-MM-DD"): date to begin forecasting from
#   models (string): model(s) to forecast with; if multiple models are given, an mean-weighted ensemble forecast is returned

## Returns forecast samples

timeseries_samples <- function(data = NULL, yvar = NULL, xvars = NULL,
                               horizon = 14, samples = 1000, train_from = NULL, forecast_from = NULL, models = NULL){
  
  data <- data %>%
    dplyr::mutate(forecast = date > forecast_from) %>%
    dplyr::filter(date > as.Date(train_from),
                  date <= as.Date(forecast_from) + horizon) 
  
  
  if(stringr::str_length(models) > 1){
    
    # time series ensemble model
    forecast <- data %>%
      dplyr::group_by(id) %>%
      dplyr::group_modify(~ EpiSoon::forecastHybrid_model(y = .x %>% filter(!forecast) %>% pull(yvar),
                                                          samples = samples, 
                                                          horizon = horizon,
                                                          model_params = list(models = models, weights = "equal"),
                                                          forecast_params = list(PI.combination = "mean"))) %>%
      dplyr::mutate(sample = rep(1:samples)) %>%
      tidyr::pivot_longer(cols = starts_with("..."), names_to = "horizon") %>%
      dplyr::mutate(horizon = as.numeric(str_remove_all(horizon, "...")) - 1,
                    value = ifelse(value < 0, 0, value),
                    forecast_from = forecast_from)
    
  } else if (models == "a") {
    
    if(is.null(xvars)){
      
      # ARIMA model
      forecast <- data %>%
        dplyr::group_by(id) %>%
        dplyr::group_modify(~ EpiSoon::forecast_model(y = .x %>% filter(!forecast) %>% pull(yvar),
                                                      samples = samples, 
                                                      horizon = horizon,
                                                      model = forecast::auto.arima)) %>%
        dplyr::mutate(sample = rep(1:samples)) %>%
        tidyr::pivot_longer(cols = starts_with("..."), names_to = "horizon") %>%
        dplyr::mutate(horizon = as.numeric(str_remove_all(horizon, "...")) - 1,
                      value = ifelse(value < 0, 0, value),
                      forecast_from = forecast_from)
      
    } else {
      
      # ARIMA regression model
      forecast <- data %>%
        dplyr::group_by(id) %>%
        dplyr::group_modify( ~ forecast_model_xreg(y = .x %>% filter(!forecast) %>% pull(yvar),
                                                   samples = samples, 
                                                   horizon = horizon,
                                                   model_params = list(
                                                     xreg = as.matrix(.x %>%
                                                                        filter(!forecast) %>%
                                                                        select(xvars))
                                                   ),
                                                   forecast_params = list(
                                                     xreg = as.matrix(.x %>%
                                                                        filter(forecast) %>%
                                                                        select(xvars))
                                                   )
        )) %>%
        dplyr::mutate(sample = rep(1:samples)) %>%
        tidyr::pivot_longer(cols = starts_with("..."), names_to = "horizon") %>%
        dplyr::mutate(horizon = as.numeric(str_remove_all(horizon, "...")) - 1,
                      value = ifelse(value < 0, 0, value),
                      forecast_from = forecast_from)
      
    }
    
  } else if(models == "e") {
    
    # ETS model
    forecast <- data %>%
      dplyr::group_by(id) %>%
      dplyr::group_modify(~ EpiSoon::forecast_model(y = .x %>% filter(!forecast) %>% pull(yvar),
                                                    samples = samples, 
                                                    horizon = horizon,
                                                    model = forecast::ets)) %>%
      dplyr::mutate(sample = rep(1:samples)) %>%
      tidyr::pivot_longer(cols = starts_with("..."), names_to = "horizon") %>%
      dplyr::mutate(horizon = as.numeric(str_remove_all(horizon, "...")) - 1,
                    value = ifelse(value < 0, 0, value),
                    forecast_from = forecast_from)
    
  } else if(models == "t") {
    
    # TBATS model
    forecast <- data %>%
      dplyr::group_by(id) %>%
      dplyr::group_modify(~ EpiSoon::forecast_model(y = .x %>% filter(!forecast) %>% pull(yvar),
                                                    samples = samples, 
                                                    horizon = horizon,
                                                    model = forecast::tbats)) %>%
      dplyr::mutate(sample = rep(1:samples)) %>%
      tidyr::pivot_longer(cols = starts_with("..."), names_to = "horizon") %>%
      dplyr::mutate(horizon = as.numeric(str_remove_all(horizon, "...")) - 1,
                    value = ifelse(value < 0, 0, value),
                    forecast_from = forecast_from)
    
  } else {
    
    message("Check models used.")
    forecast <- NA
    
  }
  
  return(forecast)
  
}

# Summarise forecast samples ----------------------------------------------

# Returns data.frame with quantile forecasts
# by model, id, horizon and forecast date (forecast_from)

# Parameters:
# samples (data.frame): with columns "model", "id", "horizon", "forecast_from", "value" (e.g. output from timeseries_samples)
# quantiles : vector of quantiles to output. Default value c(0.05, 0.25, 0.5, 0.75, 0.95)

forecast_summary <- function(samples = NULL, quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95)){
  
  out <- samples %>%
    group_by(model, id, horizon, forecast_from) %>%
    dplyr::group_modify( ~ quantile(.x$value, probs = quantiles, na.rm = T) %>%
                           tibble::enframe(name = "quantile", value = "value")) %>%
    dplyr::mutate(date_horizon = forecast_from + horizon,
                  value = round(value),
                  quantile = as.numeric(str_remove(quantile, "%"))/100,
                  quantile_range = abs(1 - 2 * quantile) * 100,
                  quantile_boundary = ifelse(quantile <= 0.5, "lower", "upper"),
                  quantile_label = paste0(quantile_boundary, "_", quantile_range)) %>%
    dplyr::select(model, forecast_from, id, horizon, date_horizon, quantile, quantile_label, value)
  
  out <- out %>%
    dplyr::bind_rows(out %>%
                       dplyr::filter(quantile_label == "lower_0") %>%
                       dplyr::mutate(quantile_label = "upper_0"))
  
  return(out)
  
}

# Make ensemble forecast --------------------------------------------------

# Parameters
# model_forecasts (data.frame): individual forecast summaries - 
# with columns "model", "forecast_from", "id", "horizon", "date_horizon", "quantile", "quantile_label" and "value"
# models (string): names of 2+ models (as appear in model_forecats$model) to use in ensemble

ensemble_forecast <- function(model_forecasts, models){
  
  mean_ensemble <- model_forecasts %>%
    dplyr::filter(model %in% models) %>%
    dplyr::group_by(forecast_from, id, horizon, date_horizon, quantile, quantile_label) %>%
    dplyr::summarise(value =  round(mean(value, na.rm = TRUE)),
                     .groups = "drop") %>%
    dplyr::mutate(model = "mean_ensemble")
  
  median_ensemble <- model_forecasts %>%
    dplyr::filter(model %in% models) %>%
    dplyr::group_by(forecast_from, id, horizon, date_horizon, quantile, quantile_label) %>%
    dplyr::summarise(value =  round(median(value, na.rm = TRUE)),
                     .groups = "drop") %>%
    dplyr::mutate(model = "median_ensemble")
  
  out <- dplyr::bind_rows(mean_ensemble, median_ensemble)
  
  return(out)
  
}




# Ensemble samples --------------------------------------------------------

ensemble_samples <- function(model_samples, models){
  
  forecast_from_int <- unique(model_samples$forecast_from)
  
  ensemble_mean_sd <- model_samples %>%
    dplyr::ungroup() %>%
    dplyr::group_by(id, horizon) %>%
    dplyr::summarise(mu = mean(value, na.rm = TRUE),
                     sigma = sd(value, na.rm = TRUE)) %>%
    dplyr::mutate(theta = (mu^2)/(sigma^2 - mu),
                  theta = ifelse(theta < 0, 1, theta)) %>%
    na.omit()
  
  grid <- expand_grid(ids = unique(ensemble_mean_sd$id), horizons = 1:14)
  ids <- grid %>% pull(ids)
  horizons <- grid %>% pull(horizons)
  
  samples <- purrr::map2_df(.x = ids, .y = horizons,
                            .f = ~{
                              mu <- ensemble_mean_sd %>%
                                dplyr::filter(id == .x,
                                              horizon == .y) %>%
                                dplyr::pull(mu)
                              
                              theta <- ensemble_mean_sd %>%
                                dplyr::filter(id == .x,
                                              horizon == .y) %>%
                                pull(theta)
                              
                              out <- tibble::tibble(value = MASS::rnegbin(n = 1000, mu = mu, theta = theta)) %>%
                                dplyr::mutate(id = .x,
                                              horizon = .y,
                                              forecast_from = as.Date(forecast_from_int),
                                              sample = 1:1000) %>%
                                dplyr::select(forecast_from, id, horizon, sample, value)
                              
                              return(out)
                              
                            }) %>%
    dplyr::bind_rows()
  
  return(samples)
  
}
