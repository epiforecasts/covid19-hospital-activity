
# Plot observed data ------------------------------------------------------

# Returns ggplot of standardised data (trend + raw (optional) -
# facetted by variable

# Parameters:
# observed (data.frame): with cols "id", "date", "name" (of variables(s)) and "value"
# trusts (string vec): 3-letter codes of Trust(s) to plot
# scale_date_breaks (string): to use in scale_x_date() date_breaks
# plot_vars (string vec): name(s) of variables to plot (from "name")
# plot_raw (bool): include raw data?

plot_observed <- function(observed,
                          trusts = "REM",
                          scale_date_breaks = "6 weeks",
                          plot_vars = NULL,
                          plot_raw = FALSE){
  
  if(is.null(plot_vars)){
    
    plot_vars <- unique(observed$name)
    
  }
  
  dat_in <- observed %>%
    dplyr::filter(id %in% trusts,
                  name %in% plot_vars) %>% 
    dplyr::group_by(id, name) %>%
    dplyr::mutate(value_trend = (stl(ts(value, freq = 7), s.window = 7)$time.series)[,2],
                  value_std = value/sd(value, na.rm = TRUE),
                  value_trend_std = (stl(ts(value_std, freq = 13), s.window = 7)$time.series)[,2]) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(name = ordered(name, plot_vars))
  
  g <- dat_in %>%
    ggplot() +
    geom_line(aes(x = date, y = value_trend_std, col = name), lwd = 0.6) +
    facet_wrap(. ~ id, scales = "free_y") +
    scale_x_date(breaks = scale_date_breaks, date_labels = "%d %b") +
    scale_color_brewer(palette = "Set1") +
    labs(x = "Date", y = "Value (standardised)",
         col = "Data") +
    theme_bw() +
    theme(legend.position = "top")
  
  if(plot_raw){
    
    g <- g + 
      geom_line(data = dat_in, aes(x = date, y = value_std, col = name), alpha = 0.4)
    
  }
  
  return(g)
  
}


# Plot forecasts (bars) ---------------------------------------------------

# Returns plot of forecast for given Trust(s)/model(s)
# Multiple forecast dates and fixed horizon

# Parameters:
# observed (data.frame): with cols "id", "date", "name" (of variables(s)) and "value"
# forecast (data.frame):
# h (int): forecast horizon to plot
# models (string): name(s) of models to show forecasts for
# trusts (string vec): 3-letter codes of Trust(s) to plot
# trust_facet_var (string): variable name to facet (wrap) Trusts on
# facet_models (bool): facet (grid) by model?
# use_dates (date vec): forecast date(s) to show forecasts for

plot_forecast_bars <- function(observed, forecast,
                               h = 7,
                               trusts = NULL,
                               models = NULL,
                               trust_facet_var = "trust_name",
                               facet_models = TRUE ,
                               use_dates = NULL){
  
  # Filter observed data
  observed_plt <- observed %>%
    dplyr::filter(id %in% trusts)
  
  # Filter forecast data
  forecast_plt <- forecast %>%
    dplyr::filter(id %in% trusts,
                  horizon == h)
  if(!is.null(models)){
    forecast_plt <- forecast_plt %>%
      dplyr::filter(model %in% models) %>%
      dplyr::mutate(model = ordered(model, models))
  }
  if(!is.null(use_dates)){
    forecast_plt <- forecast_plt %>%
      dplyr::filter(forecast_from %in% use_dates)
  }
  
  # Plot forecast data
  g <- ggplot() +
    geom_point(data = forecast_plt,
               aes(x = date, y = median, col = model),
               size = 2, shape = 1, stroke = 1) +
    geom_errorbar(data = forecast_plt,
                  aes(x = date, y = median, col = model, ymin = lower_90, ymax = upper_90),
                  alpha = 0.5, width = 0, lwd = 2) +
    geom_errorbar(data = forecast_plt,
                  aes(x = date, y = median, col = model, ymin = lower_50, ymax = upper_50),
                  alpha = 1, width = 0, lwd = 2) +
    facet_wrap(as.formula(paste("~", trust_facet_var)), scales = "free_y") +
    scale_color_brewer(palette = "Set2") +
    labs(x = "Date", y = "Admissions",
         title = paste0(h, "-days ahead forecasts"),
         col = "Forecast model") +
    theme_bw() +
    theme(legend.position = "top")
  
  # Add observed data
  g <- g +
    geom_line(data = observed_plt,
              aes(x = date, y = observed),
              lwd = 0.5) 
  
  # Optional faceting by model
  if(facet_models){
    
    g <- g +
      facet_grid(as.formula(paste("model ~", trust_facet_var)), scales = "free_y") +
      theme(legend.position = "none")
    
  }
  
  return(g)
  
  
}



# Plot forecasts (ribbons) ------------------------------------------------

# Returns plot of forecast for given Trust(s)/model(s)
# Multiple forecast dates and all horizons

# Parameters:
# observed (data.frame): with cols "id", "date", "name" (of variables(s)) and "value"
# forecast (data.frame):
# models (string): name(s) of models to show forecasts for
# trusts (string vec): 3-letter codes of Trust(s) to plot
# trust_facet_var (string): variable name to facet (wrap) Trusts on
# facet_models (bool): facet (grid) by model?
# use_dates (date vec): forecast date(s) to show forecasts for - works best for one

plot_forecast_ribbons <- function(observed, forecast,
                                  trusts = NULL,
                                  models = NULL,
                                  trust_facet_var = "trust_name",
                                  facet_models = TRUE,
                                  facet_col = 5,
                                  facet_row = 5,
                                  forecast_dates = NULL){
  
  # Filter observed data
  observed_plt <- observed %>%
    dplyr::filter(id %in% trusts)
  
  # Filter forecast data
  forecast_plt <- forecast %>%
    dplyr::filter(id %in% trusts) %>%
    dplyr::mutate(forecast_from = as.character(forecast_from)) %>%
    dplyr::ungroup()
  if(!is.null(models)){
    forecast_plt <- forecast_plt %>%
      dplyr::filter(model %in% models) %>%
      dplyr::mutate(model = ordered(model, models),
                    id = ordered(id, trusts))
  }
  if(!is.null(forecast_dates)){
    forecast_plt <- forecast_plt %>%
      dplyr::filter(forecast_from %in% as.character(forecast_dates))
  } else {
    all_dates <- unique(forecast_plt$forecast_from)
    forecast_plt <- forecast_plt %>%
      dplyr::filter(forecast_from %in% all_dates[seq(1, length(all_dates), 1)])
  }
  
  # Plot forecast data
  g <- ggplot() +
    geom_line(data = forecast_plt,
               aes(x = date, y = median, col = model),
               lwd = 0.5) +
    geom_ribbon(data = forecast_plt,
                  aes(x = date, y = median, fill = model, ymin = lower_90, ymax = upper_90),
                  alpha = 0.4) +
    geom_ribbon(data = forecast_plt,
                aes(x = date, y = median, fill = model, ymin = lower_50, ymax = upper_50),
                alpha = 0.4) +
    facet_wrap(as.formula(paste("~", trust_facet_var)), ncol = facet_col, nrow = facet_row, scales = "free_y") +
    scale_y_continuous(limits = c(0, NA)) +
    scale_color_brewer(palette = "Set2") +
    scale_fill_brewer(palette = "Set2") +
    labs(x = "Date", y = "Admissions",
         col = "Forecast model", fill = "Forecast model") +
    theme_bw() +
    theme(legend.position = "top")
  
  # Add observed data
  g <- g +
    geom_line(data = observed_plt,
              aes(x = date, y = observed),
              lwd = 0.5) 
  
  # Optional faceting by model
  if(facet_models){
    
    g <- g +
      facet_grid(as.formula(paste("model", "~", trust_facet_var)), scales = "free_y") +
      theme(legend.position = "none")
    
  }
  
  return(g)
  
}
