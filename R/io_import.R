# R/io_import.R

read_jhu_timeseries <- function(path, value_name) {
  df <- read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)

  id_cols <- c("Province/State", "Country/Region", "Lat", "Long")
  date_cols <- setdiff(names(df), id_cols)

  long_df <- tidyr::pivot_longer(
    data = df,
    cols = tidyselect::all_of(date_cols),
    names_to = "date_chr",
    values_to = value_name
  )

  long_df$date <- as.Date(long_df$date_chr, format = "%m/%d/%y")

  long_df |>
    dplyr::rename(
      province_state = `Province/State`,
      country = `Country/Region`,
      lat = Lat,
      lon = Long
    ) |>
    dplyr::select(country, province_state, lat, lon, date, dplyr::all_of(value_name))
}

aggregate_one_metric_country <- function(path, value_name) {
  read_jhu_timeseries(path, value_name) |>
    dplyr::group_by(country, date) |>
    dplyr::summarise(
      !!value_name := sum(.data[[value_name]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(country, date)
}

aggregate_jhu_country <- function(confirmed_path, deaths_path, recovered_path) {
  confirmed_country <- aggregate_one_metric_country(confirmed_path, "confirmed")
  deaths_country    <- aggregate_one_metric_country(deaths_path, "deaths")
  recovered_country <- aggregate_one_metric_country(recovered_path, "recovered")

  country_daily <- confirmed_country |>
    dplyr::left_join(deaths_country, by = c("country", "date")) |>
    dplyr::left_join(recovered_country, by = c("country", "date")) |>
    dplyr::arrange(country, date)

  country_daily$deaths[is.na(country_daily$deaths)] <- 0
  country_daily$recovered[is.na(country_daily$recovered)] <- 0

  country_daily
}