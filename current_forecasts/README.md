# Intrstruction to update the current forecasts

Forecasts from a Sunday forecast date (e.g. 2021-06-13) can be updated from the following Friday (e.g. 2021-06-18) when UTLA-level case forecasts have been updated, with the following steps:

1. Download the UTLA-level case forecasts from [epiforecasts/covid-rt-estimates](https://github.com/epiforecasts/covid-rt-estimates/blob/master/subnational/united-kingdom-local/cases/summary/cases_by_report.csv).
2. Save as "cases_by_report_YYYY-MM-DD.csv" in [current_forecasts/data/cases_utla/](https://github.com/epiforecasts/covid19-hospital-activity/tree/main/current_forecasts/data/cases_utla), where YYYY-MM-DD is the forecast date.
3. Open `update_current_forecast.R` and change `forecast_date` to the current forecast date.
4. Run `update_current_forecast.R`, which will: update the case forecast visualisation (`current_case_forecast.pdf`); make and save all individual and ensemble model forecasts (`current_forecasts/data/admissions_trust/admissions_YYYY-MM-DD.rds`); save current CHR estimates from the convolution model (`current_forecasts/data/chr/chr_YYYY-MM-DD.rds`).

The report is `current_forecasts/reports/current_admissions_forecast.Rmd`.