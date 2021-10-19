
library(tidyverse); library(covid19.nhs.data)
source(here("R", "load_data_fns.R"))
source("R/utils.R")

# Load raw data
all_data <- load_hospital_data(keep_data = c("all_adm", "bed_occ", "ga_covid", "ga_other", "ga_unocc")) %>%
  dplyr::filter(date < as.Date("2021-05-01"))

# Weekly data
weekly_data <- all_data %>%
  dplyr::mutate(week = lubridate::ceiling_date(date, unit = "week", week_start = 1) - 1) %>%
  dplyr::filter(week > as.Date("2020-08-02")) %>%
  dplyr::group_by(nhs_region, id, week) %>%
  dplyr::summarise(all_adm = sum(all_adm, na.rm = TRUE),
                   bed_occ = mean(bed_occ, na.rm = TRUE),
                   .groups = "drop")

# Raw postcode data
# (NHS Trusts and Sites https://data.england.nhs.uk/dataset/a3387fe0-ad58-48d6-99ce-451a2f5eab3e)
raw_data <- readr::read_csv(file = here::here("data", "raw", "etr.csv"),
                            col_names = FALSE) %>%
  dplyr::select(id = X1, postcode = X10) %>%
  dplyr::filter(id %in% covid19.nhs.data::trust_ltla_mapping$trust_code) %>%
  dplyr::mutate(postcode = ifelse(postcode == "CB23 3RE", "CB2 0AY", postcode)) %>%
  dplyr::bind_rows(tibble(id = "R0A", postcode = "M13 9WL"))

# Trust location (long-lat) -----------------------------------------------

extract_postcode_safely <- purrr::safely(extract_postcode, otherwise = NA)

loc <- purrr::map_df(.x = raw_data$id, .f = ~{
  
  res <- extract_postcode_safely(outcode = raw_data$postcode[which(raw_data$id == .x)])$result
  
  out <- tibble::tibble(id = .x,
                        postcode = raw_data$postcode[which(raw_data$id == .x)],
                        lng = res$longitude,
                        lat = res$latitude)
  
  return(out)
  
})

saveRDS(object = loc, file = here::here("data", "out", "trust_characteristics", "trust_locations.rds"))

# Trust size --------------------------------------------------------------

trust_size <- all_data %>%
  dplyr::group_by(id, date) %>%
  dplyr::summarise(total_bed = sum(c(ga_covid, ga_other, ga_unocc), na.rm = FALSE)) %>%
  dplyr::filter(date >= as.Date("2020-11-17")) %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(total_bed = round(mean(total_bed, na.rm = TRUE))) %>%
  dplyr::ungroup()

saveRDS(object = trust_size,
        file = here::here("data", "out", "trust_characteristics", "trust_size.rds"))

# Total Covid-19 admissions -----------------------------------------------

trust_adms <- all_data %>%
  dplyr::select(id, all_adm) %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(total_adm = sum(all_adm, na.rm = TRUE))

saveRDS(object = trust_adms,
        file = here::here("data", "out", "trust_characteristics", "trust_total.rds"))

# Mapping size ------------------------------------------------------------

trust_mapping <- covid19.nhs.data::trust_utla_mapping %>%
  dplyr::group_by(id = trust_code) %>%
  dplyr::summarise(n  = length(p_geo),
                   n_05 = length(p_geo[which(p_geo > 0.05)]),
                   n_10 = length(p_geo[which(p_geo > 0.1)]),
                   n_20 = length(p_geo[which(p_geo > 0.2)]))

saveRDS(object = trust_mapping,
        file = here::here("data", "out", "trust_characteristics", "trust_mapping.rds"))

# Clustering --------------------------------------------------------------

# Daily data
dat_daily_in <- all_data %>%
  dplyr::select(id, date, value = all_adm) %>%
  dplyr::group_by(id) %>%
  dplyr::mutate(value = (stl(ts(value, freq = 7), s.window = 7)$time.series)[,2])

trust_cluster_daily <- purrr::map_df(.x = 3:10, .f = ~ {
  
  out <- return_clusters(df = dat_daily_in,
                         k = .x,
                         exclude_ids = trust_adms$id[which(trust_adms$total_adm < 1000)])$clusters %>%
    dplyr::mutate(k = .x)
  
}) %>%
  dplyr::bind_rows()

saveRDS(object = trust_cluster_daily,
        file = here::here("data", "out", "trust_characteristics", "trust_cluster_daily.rds"))

# Weekly data
dat_weekly_in <- weekly_data %>%
  dplyr::select(id, date = week, value = all_adm)

trust_cluster_weekly <- purrr::map_df(.x = 3:10, .f = ~ {
  
  out <- return_clusters(df = dat_weekly_in,
                         k = .x,
                         exclude_ids = trust_adms$id[which(trust_adms$total_adm < 1000)])$clusters %>%
    dplyr::mutate(k = .x)
  
}) %>%
  dplyr::bind_rows()

saveRDS(object = trust_cluster_weekly,
        file = here::here("data", "out", "trust_characteristics", "trust_cluster_weekly.rds"))




