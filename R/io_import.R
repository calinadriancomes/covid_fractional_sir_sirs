# R/io_import.R

# Parse JHU column dates robustly. The original JHU files use "m/d/yy", but
# files re-saved through spreadsheet software carry "m/d/yyyy". Parsing a
# 4-digit year with "%y" silently keeps only the first two digits (all years
# collapse into 2020), so the year token is handled explicitly here.
parse_jhu_date <- function(x) {
  parts <- strsplit(as.character(x), "/", fixed = TRUE)
  m <- as.integer(vapply(parts, `[`, character(1), 1))
  d <- as.integer(vapply(parts, `[`, character(1), 2))
  y_chr <- vapply(parts, `[`, character(1), 3)
  y <- as.integer(y_chr)
  y <- ifelse(nchar(y_chr) <= 2, 2000L + y, y)
  out <- as.Date(sprintf("%04d-%02d-%02d", y, m, d))
  if (anyNA(out)) {
    stop("Unparseable JHU date columns: ", paste(head(x[is.na(out)], 5), collapse = ", "))
  }
  out
}

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

  long_df$date <- parse_jhu_date(long_df$date_chr)

  n_unique_dates <- length(unique(long_df$date))
  if (n_unique_dates != length(date_cols)) {
    stop("Date parsing collapsed ", length(date_cols), " date columns into ",
         n_unique_dates, " unique dates. Check the header format of: ", path)
  }

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