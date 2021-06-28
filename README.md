# covid19-hospital-activity

This repo contains forecasts of Trust-level Covid-19 hospital admissions in England. It is split into two (related) parts:

1. Forecasts and analyses for the manuscript "*Short-term forecasting of Covid-19 hospital admissions in NHS Trusts in England, August 2020 - January 2021*".
2. [current_forecasts/](https://github.com/epiforecasts/covid19-hospital-activity/tree/main/current_forecasts), ongoing forecasts from 14 March 2021, updated weekly.

## *Short-term forecasting of Covid-19 hospital admissions in NHS Trusts in England, August 2020 - January 2021*

Forecasts and related analyses are made with the following scripts:

* [R/00_trust_characteristics.R](https://github.com/epiforecasts/covid19-hospital-activity/blob/main/R/00_trust_characteristics.R)
* [R/01_make_forecasts.R](https://github.com/epiforecasts/covid19-hospital-activity/blob/main/R/01_make_forecasts.R)
* [R/02_eval_forecasts.R](https://github.com/epiforecasts/covid19-hospital-activity/blob/main/R/02_eval_forecasts.R)

Figures and any in-text values are made by running the following reports:

* [reports/01_introduction.Rmd](https://github.com/epiforecasts/covid19-hospital-activity/blob/main/reports/01_introduction.Rmd)
* [reports/02_forecast_evaluation.Rmd](https://github.com/epiforecasts/covid19-hospital-activity/blob/main/reports/02_forecast_evaluation.Rmd)
* [reports/90_estimate_lag.Rmd](https://github.com/epiforecasts/covid19-hospital-activity/blob/main/reports/90_estimate_lag.Rmd)

### Making admissions forecasts

[R/01_make_forecasts.R](https://github.com/epiforecasts/covid19-hospital-activity/blob/main/R/01_make_forecasts.R) makes the forecasts through the following steps:

1. Define the forecast dates, and load the Trust-level admissions (observed) and case (estimated) data.
2. Run the individual forecasting models by sourcing scripts of the form `R/run_xxx.R` (apart from `R/run_epinow2_case.R`).
3. Save forecast (samples and quantile forecasts comprising the median, and 50% and 90% prediction intervals) in [data/out/samples/](https://github.com/epiforecasts/covid19-hospital-activity/tree/main/data/out/admissions_forecast/samples) and [data/out/summary/](https://github.com/epiforecasts/covid19-hospital-activity/tree/main/data/out/admissions_forecast/summary), respectively.
4. Make and save ensemble forecasts from a defined subset of the individual models.

### Evaluating forecasts

[R/02_eval_forecasts.R](https://github.com/epiforecasts/covid19-hospital-activity/blob/main/R/02_eval_forecasts.R) evaluates the forecasts, using the [`scoringutils`](https://github.com/epiforecasts/scoringutils) package.


## Current forecasts

We continue to make Trust-level forecasts of hospital admissions. This work is contained in the folder [current_forecasts/](https://github.com/epiforecasts/covid19-hospital-activity/tree/main/current_forecasts):

* **[README.md](https://github.com/epiforecasts/covid19-hospital-activity/tree/main/current_forecasts/README.md): Full instructions to update the current forecasts.**
* [update_current_forecast.R](https://github.com/epiforecasts/covid19-hospital-activity/tree/main/current_forecasts/update_current_forecast.R): Runs the full forecast pipeline for the current forecasts; need to manually update the forecast date and to have added the relevant UTLA case forecasts to [cases_utla/](https://github.com/epiforecasts/covid19-hospital-activity/tree/main/current_forecasts/cases_utla).
* [current_forecast.Rmd](https://github.com/epiforecasts/covid19-hospital-activity/blob/main/current_forecasts/current_forecast.Rmd): report summarising forecasts for a given forecast date.
* [admissions_trust/](https://github.com/epiforecasts/covid19-hospital-activity/tree/main/current_forecasts/admissions_trust): quantile forecasts of Trust-level hospital admissions labelled by forecast date. Standard version (e.g. admissions_2021-03-14.rds) contains quantiles 0.05, 0.25, 0.5, 0.75 and 0.95; the long version (e.g. admissions_long_2021-03-14.rds) contains all quantiles 0.01 through 0.99 (inclusive).
* [cases_utla/](https://github.com/epiforecasts/covid19-hospital-activity/tree/main/current_forecasts/cases_utla): quantile forecasts of UTLA-level case forecasts; see [epiforecasts/covid-rt-estimates](https://github.com/epiforecasts/covid-rt-estimates) for details.

### Updates

* **07 June 2021:** forecasts for several Trusts have been temporarily dropped from the 2021-05-30 report due to issues with the underlying case forecasts in one or more associated UTLAs.
