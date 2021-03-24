
# Plot observed data ------------------------------------------------------

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
                  value_trend_std = (stl(ts(value_std, freq = 7), s.window = 7)$time.series)[,2]) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(name = ordered(name, plot_vars))
  
  g <- dat_in %>%
    ggplot(aes(x = date, y = value_trend_std, col = name)) +
    geom_line(lwd = 0.6) +
    facet_wrap(. ~ id) +
    scale_x_date(breaks = scale_date_breaks, date_labels = "%d %b") +
    scale_color_brewer(palette = "Set1") +
    labs(x = "Date", y = "Value (std)",
         col = "Data") +
    theme_bw() +
    theme(legend.position = "top")
  
  if(plot_raw){
    
    g <- g + 
      geom_line(data = dat_in, aes(y = value_std), alpha = 0.4)
    
  }
  
  return(g)
  
}


# Plot forecasts (bars) ---------------------------------------------------

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

plot_forecast_ribbons <- function(observed, forecast,
                                  trusts = NULL,
                                  models = NULL,
                                  trust_facet_var = "trust_name",
                                  facet_models = TRUE,
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
      dplyr::filter(forecast_from %in% forecast_dates)
  } else {
    all_dates <- unique(forecast_plt$forecast_from)
    forecast_plt <- forecast_plt %>%
      dplyr::filter(forecast_from %in% all_dates[seq(1, length(all_dates), 1)])
  }
  
  # Plot forecast data
  g <- ggplot() +
    geom_line(data = forecast_plt,
               aes(x = date, y = median, col = model, group = forecast_from),
               lwd = 0.5) +
    geom_ribbon(data = forecast_plt,
                  aes(x = date, y = median, fill = model, group = forecast_from, ymin = lower_90, ymax = upper_90),
                  alpha = 0.4) +
    geom_ribbon(data = forecast_plt,
                aes(x = date, y = median, fill = model, group = forecast_from, ymin = lower_50, ymax = upper_50),
                alpha = 0.4) +
    facet_wrap(as.formula(paste("~", trust_facet_var)), scales = "free_y") +
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
