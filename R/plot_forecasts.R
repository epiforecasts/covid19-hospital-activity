

# Plot forecasts (bars) ---------------------------------------------------

observed <- raw_hosp %>%
  dplyr::select(id, date, observed = all_adm)

forecast <- adm_forecasts %>%
  dplyr::select(forecast_from, model, id, date, horizon,
                median = lower_0, lower_90, lower_50, upper_50, upper_90)

plot_forecast_bars <- function(observed, forecast,
                               trusts = NULL,
                               h = 7,
                               models = NULL,
                               facet_models = TRUE ,
                               forecast_dates = NULL){
  
  # Filter observed data
  observed_plt <- observed %>%
    dplyr::filter(id %in% trusts)
  
  # Filter forecast data
  forecast_plt <- forecast %>%
    dplyr::filter(id %in% trusts,
                  horizon == h)
  if(!is.null(models)){
    forecast_plt <- forecast_plt %>%
      dplyr::filter(model %in% models)
  }
  if(!is.null(forecast_dates)){
    forecast_plt <- forecast_plt %>%
      dplyr::filter(forecast_from %in% forecast_dates)
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
    facet_grid(rows = vars(id), scales = "free_y") +
    scale_x_date(limits = c(as.Date("2020-08-02"), as.Date("2021-02-14")),
                 breaks = seq.Date(from = as.Date("2020-08-16"),
                                   to = as.Date("2021-02-14"),
                                   by = "4 weeks"),
                 date_labels = "%d %b %y") +
    scale_color_brewer(palette = "Set2") +
    labs(x = "Date", y = "Admissions",
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
      facet_grid(rows = vars(id), cols = vars(model), scales = "free_y") +
      theme(legend.position = "none")
    
  }
  
  return(g)
  
  
}



# Plot forecasts (ribbons) ------------------------------------------------

plot_forecast_ribbons <- function(observed, forecast,
                                  trusts = NULL,
                                  models = NULL,
                                  facet_models = TRUE,
                                  forecast_dates = NULL){
  
  # Filter observed data
  observed_plt <- observed %>%
    dplyr::filter(id %in% trusts)
  
  # Filter forecast data
  forecast_plt <- forecast %>%
    dplyr::filter(id %in% trusts)
  if(!is.null(models)){
    forecast_plt <- forecast_plt %>%
      dplyr::filter(model %in% models)
  }
  if(!is.null(forecast_dates)){
    forecast_plt <- forecast_plt %>%
      dplyr::filter(forecast_from %in% forecast_dates)
  } else {
    all_dates <- unique(forecast_plt$forecast_from)
    forecast_plt <- forecast_plt %>%
      dplyr::filter(forecast_from %in% all_dates[seq(1, length(all_dates), 2)])
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
    facet_grid(rows = vars(id), scales = "free_y") +
    scale_x_date(limits = c(as.Date("2020-08-02"), as.Date("2021-02-14")),
                 breaks = seq.Date(from = as.Date("2020-08-16"),
                                   to = as.Date("2021-02-14"),
                                   by = "4 weeks"),
                 date_labels = "%d %b %y") +
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
      facet_grid(rows = vars(id), cols = vars(model), scales = "free_y") +
      theme(legend.position = "none")
    
  }
  
  return(g)
  
}
