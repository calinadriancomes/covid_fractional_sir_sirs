latest_dm_snapshot <- function(output_dir = file.path(PROJECT_DIR, 'outputs', 'diebold_mariano')) {
  by_country_path <- file.path(output_dir, 'diebold_mariano_by_country_latest.csv')
  summary_path <- file.path(output_dir, 'diebold_mariano_summary_latest.csv')
  fds_path <- file.path(output_dir, 'diebold_mariano_fractional_dominance_latest.csv')
  out <- list(by_country = NULL, summary = NULL, fractional_dominance = NULL)
  if (file.exists(by_country_path)) out$by_country <- utils::read.csv(by_country_path, stringsAsFactors = FALSE)
  if (file.exists(summary_path)) out$summary <- utils::read.csv(summary_path, stringsAsFactors = FALSE)
  if (file.exists(fds_path)) out$fractional_dominance <- utils::read.csv(fds_path, stringsAsFactors = FALSE)
  out
}

fractional_feature_registry <- function(df_frac) {
  frac_cols <- grep('^(fracdiff|fracmem)', names(df_frac), value = TRUE)
  if (length(frac_cols) == 0) return(data.frame())
  data.frame(feature = frac_cols, stringsAsFactors = FALSE) |>
    dplyr::mutate(
      family = ifelse(grepl('^fracdiff', feature), 'Fractional difference', 'Fractional memory'),
      variable = dplyr::case_when(
        grepl('new_confirmed', feature) ~ 'new_confirmed',
        grepl('_I_', feature) ~ 'I',
        TRUE ~ 'other'
      ),
      alpha = sub('.*_a', '', feature),
      alpha = gsub('_', '.', alpha)
    )
}

country_rank_table <- function(dm_by_country, family_name, metric = 'mean_loss_diff') {
  if (is.null(dm_by_country) || nrow(dm_by_country) == 0) return(data.frame())
  dm_by_country |>
    dplyr::filter(.data$family == family_name) |>
    dplyr::mutate(rank_metric = .data[[metric]]) |>
    dplyr::arrange(dplyr::desc(abs(rank_metric))) |>
    dplyr::mutate(rank = dplyr::row_number())
}

build_operator_kernel_df <- function(alpha = 0.6, n = 40) {
  k <- 1:n
  data.frame(
    k = rep(k, 3),
    value = c(k^(-alpha), exp(-alpha * k / max(k)), 1 / (1 + k)^alpha),
    operator = rep(c('Caputo / power-law', 'Atangana-Baleanu / non-singular', 'Riemann-Liouville / power-law'), each = n)
  )
}

build_output_registry <- function() {
  out_dir <- file.path(PROJECT_DIR, 'outputs', 'diebold_mariano')
  if (!dir.exists(out_dir)) return(data.frame())
  files <- list.files(out_dir, recursive = TRUE, full.names = TRUE)
  if (length(files) == 0) return(data.frame())
  info <- file.info(files)
  data.frame(
    file = basename(files),
    ext = tools::file_ext(files),
    size_kb = round(info$size / 1024, 2),
    stringsAsFactors = FALSE
  ) |>
    dplyr::mutate(group = dplyr::case_when(
      ext == 'csv' ~ 'CSV tables',
      ext == 'rds' ~ 'RDS objects',
      ext == 'png' ~ 'PNG figures',
      TRUE ~ 'Other'
    ))
}
