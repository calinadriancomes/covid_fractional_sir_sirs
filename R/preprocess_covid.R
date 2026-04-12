# R/preprocess_covid.R

compute_daily_from_cumulative <- function(x) {
  dx <- c(NA, diff(x))
  dx <- ifelse(is.na(dx), NA, pmax(dx, 0))
  dx
}

build_country_sir_features <- function(df_country, population) {
  df_country <- df_country |>
    dplyr::arrange(date) |>
    dplyr::mutate(
      new_confirmed = compute_daily_from_cumulative(confirmed),
      new_deaths = compute_daily_from_cumulative(deaths),
      new_recovered = compute_daily_from_cumulative(recovered)
    )

  df_country <- df_country |>
    dplyr::mutate(
      active_cases = clip_nonnegative(confirmed - deaths - recovered),
      S = clip_nonnegative(population - active_cases - recovered - deaths),
      I = clip_nonnegative(active_cases),
      R_compartment = clip_nonnegative(recovered + deaths)
    )

  df_country <- df_country |>
    dplyr::mutate(
      incidence_rate = new_confirmed / population,
      mortality_rate = new_deaths / population,
      recovery_rate_daily = new_recovered / pmax(lag_vec(I, 1), 1),
      beta_naive = new_confirmed / pmax((lag_vec(S, 1) * lag_vec(I, 1)) / population, 1e-8),
      gamma_naive = (new_recovered + new_deaths) / pmax(lag_vec(I, 1), 1)
    )

  df_country
}

build_all_countries_model_input <- function(country_df, population_table = NULL, fallback_population = 1e7) {
  if (!is.null(population_table)) {
    country_df <- country_df |>
      dplyr::left_join(population_table, by = "country")
  } else {
    country_df$population <- fallback_population
  }

  country_df$population[is.na(country_df$population)] <- fallback_population

  out <- country_df |>
    dplyr::group_by(country) |>
    dplyr::group_modify(~ build_country_sir_features(.x, unique(.x$population)[1])) |>
    dplyr::ungroup()

  out
}

add_standard_ml_features <- function(df) {
  df |>
    dplyr::group_by(country) |>
    dplyr::arrange(date, .by_group = TRUE) |>
    dplyr::mutate(
      lag_new_confirmed_1 = lag_vec(new_confirmed, 1),
      lag_new_confirmed_3 = lag_vec(new_confirmed, 3),
      lag_new_confirmed_7 = lag_vec(new_confirmed, 7),
      lag_I_1 = lag_vec(I, 1),
      lag_I_7 = lag_vec(I, 7),
      ma_new_confirmed_3 = as.numeric(rolling_mean_simple(new_confirmed, 3)),
      ma_new_confirmed_7 = as.numeric(rolling_mean_simple(new_confirmed, 7)),
      ma_new_confirmed_14 = as.numeric(rolling_mean_simple(new_confirmed, 14)),
      growth_new_confirmed = growth_rate(new_confirmed),
      target_new_confirmed_tplus1 = lead_vec(new_confirmed, 1),
      target_new_confirmed_tplus7 = lead_vec(new_confirmed, 7)
    ) |>
    dplyr::ungroup()
}