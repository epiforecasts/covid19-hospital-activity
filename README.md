# covid19-hospital-activity

Forecasts are produced with [forecasts/update_all_forecasts.R](https://github.com/sophiemeakin/covid19-hospital-activity/blob/main/forecasts/update_all_forecasts.R), which does the following:

1. load and combine the raw hospital activity (Trust) and case (UTLA) data
2. call the individual hospital activity forecasting models (all other scripts in the [forecasts/](https://github.com/sophiemeakin/covid19-hospital-activity/blob/main/forecasts) folder

Forecast samples and summaries (median plus 50% and 90% CIs) are saved in [forecasts/samples/](https://github.com/sophiemeakin/covid19-hospital-activity/blob/main/forecasts/samples) and [forecasts/summary/](https://github.com/sophiemeakin/covid19-hospital-activity/blob/main/forecasts/summary), respectively.
