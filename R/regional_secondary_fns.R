# Copy of https://github.com/seabbs/regional-secondary/blob/master/regional-secondary.R
# Saved 2021-09-14

# load required packages
library(EpiNow2)
library(future.apply)
library(purrr)
library(data.table)

# extract priors from a posterior and update fitting args
extract_secondary_priors <- function(posterior) {
  posterior <- as.data.table(posterior)
  posterior <- posterior[, .(variable, mean, sd)]
  return(posterior)
}

update_secondary_args <- function(args, posterior) {
  if (!missing(posterior)) {
    if (!is.null(posterior)) {
      # pull out just the columns of interes
      posterior <- extract_secondary_priors(posterior)
      # replace scaling if present in the posterior
      scale <- posterior[grepl("frac_obs", variable)]
      if (nrow(scale) > 0) {
        args$obs$scale$mean <- as.array(signif(scale$mean, 3))
        args$obs$scale$sd <- as.array(signif(scale$sd, 3))
      }
      # replace delay parameters if present
      delay_mean <- posterior[grepl("delay_mean", variable)]
      delay_sd <- posterior[grepl("delay_sd", variable)]
      if (nrow(delay_mean) > 0) {
        args$delays$delay_mean_mean <- as.array(signif(delay_mean$mean, 3))
        args$delays$delay_mean_sd <- as.array(signif(delay_mean$sd, 3))
        args$delays$delay_sd_mean <- as.array(signif(delay_sd$mean, 3))
        args$delays$delay_sd_sd <- as.array(signif(delay_sd$sd, 3))
      }
      # replace phi prior if present
      phi <- posterior[grepl("rep_phi", variable)]
      if (nrow(phi) > 0) {
        args$obs$phi <- c(signif(phi$mean, 3), signif(phi$sd, 3))
      }
    }
  }
  return(args)
}

# handle replacing priors for the convolution model and taking only a single
# window
estimate_region <- function(obs, burn_in = 14, prior = NULL,
                            prior_scale = 1, fit_args = list(),
                            verbose = FALSE) {
  # update args to use posterior priors
  if (!is.null(prior)) {
    if (nrow(prior) > 0) {
      if (verbose) {
        message("Replacing specified priors with those from the passed in
         prior dataframe")
      }
      prior <- prior[, sd := sd * prior_scale]
      fit_args <- update_secondary_args(fit_args, posterior = prior)
    }
  }
  # estimate relationship fitting to just the last month of data
  estimates <- do.call(estimate_secondary, c(
    list(
      reports = obs,
      verbose = FALSE,
      burn_in = burn_in
    ),
    fit_args
  ))
  
  out <- list()
  out$estimate_secondary <- estimates
  out$summarised_posterior <- extract_stan_param(
    estimates$fit,
    CrIs = c(seq(0.1, 0.9, 0.1), 0.95)
  )
  return(out)
}

# inner function for forecasting a single region
forecast_region <- function(target_region, reports, case_forecast,
                            verbose = TRUE, return_fit = TRUE,
                            return_plots = TRUE, window = NULL,
                            prior_scale = 1, burn_in = 14, priors, ...) {
  if (verbose) {
    message("Processing: ", target_region)
  }
  # filter for target region
  target_obs <- reports[region == target_region][, region := NULL]
  
  # update args to use posterior priors
  fit_args <- list(...)
  prior <- NULL
  if (!missing(priors)) {
    if (!is.null(priors)) {
      prior <- priors[region == target_region]
    }
  }
  # set burn in if using window
  if (!is.null(window)) {
    adj_burn_in <- 
      as.integer(max(target_obs$date) - min(target_obs$date)) - window
    burn_in <- ifelse(adj_burn_in < burn_in, burn_in, adj_burn_in)
  }
  prior_estimates <- list()
  prior_estimates[[1]] <- estimate_region(
    target_obs,
    burn_in = burn_in[1],
    prior_scale = prior_scale[1],
    prior = prior,
    fit_args = fit_args
  )
  
  if (length(burn_in) > 1) {
    if (length(prior_scale) == 1) {
      prior_scale <- rep(prior_scale, length(burn_in))
    }
    if (length(burn_in) != length(prior_scale)) {
      stop("A prior_scale must be given for each window to fit")
    }
    for (i in 2:length(burn_in)) {
      prior_estimates[[i]] <- estimate_region(
        target_obs,
        burn_in = burn_in[i],
        prior_scale = prior_scale[i],
        prior = prior_estimates[[i - 1]]$summarised_posterior,
        fit_args = fit_args
      )
    }
  }
  estimates <- prior_estimates[[length(burn_in)]]
  
  out <- estimates
  if (length(burn_in) > 1) {
    out$prior_fits <- prior_estimates[1:(length(burn_in) - 1)]
  }
  if (return_plots) {
    out$plots$fit <- plot(estimates$estimate_secondary)
  }
  
  # forecast using model fit
  if (!missing(case_forecast)) {
    if (!is.null(case_forecast)) {
      if (nrow(case_forecast) > 0) {
        pred_cases <- case_forecast[region == target_region]
        pred_cases <- pred_cases[, .(date, sample, value = cases)]
        pred_cases <- pred_cases[date > max(target_obs$date)]
        
        deaths_forecast <- forecast_secondary(
          estimates$estimate_secondary, pred_cases,
          samples = 1000
        )
        if (return_plots) {
          out$plots$forecast <- plot(
            deaths_forecast,
            from = max(target_obs$date) - 7
          )
        }
        
        # link in previous observations to forecast
        obs_as_samples <- target_obs[
          ,
          .(date,
            value = secondary,
            sample = list(unique(deaths_forecast$samples$sample))
          )
        ]
        obs_as_samples <- obs_as_samples[,
                                         .(sample = as.numeric(unlist(sample))),
                                         by = c("date", "value")
        ]
        deaths_forecast$samples <- rbindlist(list(
          obs_as_samples,
          deaths_forecast$samples
        ), use.names = TRUE)
        
        # return samples + summary
        out$samples <- deaths_forecast$samples
        out$summarised <- deaths_forecast$predictions
      }
    }
  }
  if (!return_fit) {
    out$estimate_secondary <- NULL
    out$prior_fits <- NULL
  }
  if (verbose) {
    message("Completed processing: ", target_region)
  }
  return(out)
}

# extract a summary of the posteriors for the convolution model parameters
summarised_secondary_posteriors <- function(secondary_list,
                                            params = c(
                                              "delay", "frac_obs",
                                              "rep_phi"
                                            )) {
  summarised_posterior <- map(
    secondary_list,
    ~ .$summarised_posterior
  )
  summarised_posterior <- rbindlist(summarised_posterior, idcol = "region")
  if (length(params) > 0) {
    summarised_posterior <- map(
      params,
      ~ summarised_posterior[grepl(., variable)]
    )
    summarised_posterior <- rbindlist(summarised_posterior)
    setorder(summarised_posterior, region, variable)
  }
  return(summarised_posterior)
}

# wrapper for forecasting across regions
# additional arguments are passed to estimate_secondary
regional_secondary <- function(reports, case_forecast = NULL,
                               verbose = interactive(),
                               return_fit = TRUE, return_plots = TRUE,
                               posterior_params = c("delay", "frac_obs",
                                                    "rep_phi"),
                               priors = NULL, window = NULL, ...) {
  
  # Convert to data.table
  reports <- as.data.table(reports)
  case_forecast <- as.data.table(case_forecast)
  
  # run the forecast safely in case of failure
  safe_forecast_region <- safely(forecast_region)
  
  # forecast all regions
  forecasts <- future_lapply(unique(reports$region),
                             safe_forecast_region,
                             reports = reports,
                             case_forecast = case_forecast,
                             priors = priors,
                             window = window,
                             verbose = verbose,
                             return_fit = return_fit,
                             return_plots = return_plots,
                             future.seed = TRUE,
                             future.scheduling = Inf,
                             ...
  )
  # pick out error messages
  errors <- map(forecasts, ~ .[[2]])
  names(errors) <- unique(reports$region)
  # pick out results and name
  forecasts <- map(forecasts, ~ .[[1]])
  names(forecasts) <- unique(reports$region)
  
  failed_regions <- compact(errors)
  if (length(failed_regions) > 0) {
    message("Following regions failed: ")
    print(failed_regions)
  }
  
  # format output
  out <- list()
  out$region <- forecasts
  if (!all(map_lgl(forecasts, is.null))) {
    out$samples <- rbindlist(map(forecasts, ~ .$samples), idcol = "region")
    out$summarised <- rbindlist(map(forecasts, ~ .$summarised),
                                idcol = "region"
    )
    out$summarised_posterior <- summarised_secondary_posteriors(
      forecasts,
      params = posterior_params
    )
  }
  out$errors <- errors
  return(out)
}