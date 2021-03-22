
# Summarise forecast samples ----------------------------------------------

# Parameters:
#   samples (data.frame): with columns "model", "id", "horizon", "forecast_from", "value" (e.g. output from timeseries_samples)
#   quantiles : vector of quantiles to output. Default value c(0.05, 0.25, 0.5, 0.75, 0.95)

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
#   model_forecasts (data.frame): individual forecast summaries, 
      # with columns "model", "forecast_from", "id", "horizon", "date_horizon", "quantile", "quantile_label" and "value"
#   models (string): names of 2+ models (as appear in model_forecats$model) to use in ensemble

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




