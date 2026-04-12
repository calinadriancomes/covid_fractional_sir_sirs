# R/diebold_mariano.R

infer_dm_horizon <- function(target_col = "target_new_confirmed_tplus7") {
  if (grepl("tplus7$", target_col)) return(7)
  if (grepl("tplus1$", target_col)) return(1)
  1
}

.run_dm_once <- function(e1, e2, h = 1, power = 2, varestimator = "acf") {
  warn_msg <- NULL
  dm_obj <- withCallingHandlers(
    tryCatch(
      forecast::dm.test(
        e1 = e1,
        e2 = e2,
        h = h,
        power = power,
        varestimator = varestimator
      ),
      error = function(e) e
    ),
    warning = function(w) {
      warn_msg <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )

  list(
    result = dm_obj,
    warning = warn_msg,
    varestimator = varestimator,
    h = h
  )
}

safe_dm_test <- function(actual,
                         pred_standard,
                         pred_fractional,
                         h = 1,
                         power = 2,
                         varestimator = "acf") {
  if (!requireNamespace("forecast", quietly = TRUE)) {
    stop("Package 'forecast' is required. Please install.packages('forecast').")
  }

  idx <- is.finite(actual) & is.finite(pred_standard) & is.finite(pred_fractional)
  actual <- actual[idx]
  pred_standard <- pred_standard[idx]
  pred_fractional <- pred_fractional[idx]

  if (length(actual) < max(8, h + 2)) {
    return(data.frame(
      n = length(actual),
      dm_statistic = NA_real_,
      dm_p_value = NA_real_,
      mean_loss_standard = NA_real_,
      mean_loss_fractional = NA_real_,
      mean_loss_diff = NA_real_,
      better_model = NA_character_,
      dm_varestimator = NA_character_,
      dm_note = "Insufficient aligned observations for DM test.",
      stringsAsFactors = FALSE
    ))
  }

  e1 <- actual - pred_standard
  e2 <- actual - pred_fractional

  loss1 <- abs(e1)^power
  loss2 <- abs(e2)^power

  dm_attempt <- .run_dm_once(
    e1 = e1,
    e2 = e2,
    h = h,
    power = power,
    varestimator = varestimator
  )

  dm_note <- dm_attempt$warning

  if (is.character(dm_note) && grepl("Variance is negative", dm_note, ignore.case = TRUE)) {
    dm_attempt <- .run_dm_once(
      e1 = e1,
      e2 = e2,
      h = h,
      power = power,
      varestimator = "bartlett"
    )
    dm_note <- "Negative long-run variance under acf estimator; switched to Bartlett estimator."
  }

  if (inherits(dm_attempt$result, "error")) {
    dm_note <- paste(
      na.omit(c(dm_note, paste0("DM test error: ", conditionMessage(dm_attempt$result)))),
      collapse = " | "
    )
    dm_obj <- NULL
  } else {
    dm_obj <- dm_attempt$result
  }

  mean_loss_standard <- mean(loss1, na.rm = TRUE)
  mean_loss_fractional <- mean(loss2, na.rm = TRUE)
  mean_loss_diff <- mean_loss_standard - mean_loss_fractional

  better_model <- if (isTRUE(mean_loss_diff > 0)) {
    "fractional"
  } else if (isTRUE(mean_loss_diff < 0)) {
    "standard"
  } else {
    "tie"
  }

  data.frame(
    n = length(actual),
    dm_statistic = if (!is.null(dm_obj)) as.numeric(dm_obj$statistic) else NA_real_,
    dm_p_value = if (!is.null(dm_obj)) dm_obj$p.value else NA_real_,
    mean_loss_standard = mean_loss_standard,
    mean_loss_fractional = mean_loss_fractional,
    mean_loss_diff = mean_loss_diff,
    better_model = better_model,
    dm_varestimator = dm_attempt$varestimator,
    dm_note = ifelse(is.null(dm_note) || identical(dm_note, ""), "OK", dm_note),
    stringsAsFactors = FALSE
  )
}

align_prediction_frames <- function(pred_standard, pred_fractional) {
  std <- pred_standard[, c("date", "actual", "predicted"), drop = FALSE]
  frac <- pred_fractional[, c("date", "actual", "predicted"), drop = FALSE]

  names(std)[names(std) == "predicted"] <- "predicted_standard"
  names(frac)[names(frac) == "predicted"] <- "predicted_fractional"
  names(frac)[names(frac) == "actual"] <- "actual_fractional"

  out <- merge(std, frac, by = "date", all = FALSE)

  if (nrow(out) == 0) {
    return(data.frame(
      date = as.Date(character()),
      actual = numeric(),
      predicted_standard = numeric(),
      predicted_fractional = numeric()
    ))
  }

  if ("actual_fractional" %in% names(out)) {
    out$actual <- ifelse(is.finite(out$actual), out$actual, out$actual_fractional)
    out$actual_fractional <- NULL
  }

  out <- out[order(out$date), c("date", "actual", "predicted_standard", "predicted_fractional")]
  rownames(out) <- NULL
  out
}

run_family_dm_test <- function(pred_standard,
                               pred_fractional,
                               family,
                               country,
                               h = 1,
                               power = 2) {
  aligned <- align_prediction_frames(pred_standard, pred_fractional)

  dm_tbl <- safe_dm_test(
    actual = aligned$actual,
    pred_standard = aligned$predicted_standard,
    pred_fractional = aligned$predicted_fractional,
    h = h,
    power = power
  )

  cbind(
    data.frame(
      country = country,
      family = family,
      stringsAsFactors = FALSE
    ),
    dm_tbl
  )
}

run_country_dm_suite <- function(df_standard,
                                 df_fractional,
                                 country_name,
                                 target_col = "target_new_confirmed_tplus7",
                                 train_ratio = 0.8,
                                 rf_ntree = 500,
                                 xgb_nrounds = 300,
                                 seed = 123) {
  h_dm <- infer_dm_horizon(target_col)

  rf_std <- fit_rf_model(
    df_country = df_standard,
    target_col = target_col,
    train_ratio = train_ratio,
    ntree = rf_ntree,
    seed = seed
  )

  rf_frac <- fit_rf_fractional_model(
    df_country = df_fractional,
    target_col = target_col,
    train_ratio = train_ratio,
    ntree = rf_ntree,
    seed = seed
  )

  xgb_std <- fit_xgb_model(
    df_country = df_standard,
    target_col = target_col,
    train_ratio = train_ratio,
    nrounds = xgb_nrounds,
    seed = seed
  )

  xgb_frac <- fit_xgb_fractional_model(
    df_country = df_fractional,
    target_col = target_col,
    train_ratio = train_ratio,
    nrounds = xgb_nrounds,
    seed = seed
  )

  arima_cmp <- compare_arima_family(
    df_country = df_standard,
    target_col = "new_confirmed",
    train_ratio = train_ratio
  )

  per_family <- dplyr::bind_rows(
    run_family_dm_test(
      rf_std$predictions_test,
      rf_frac$predictions_test,
      family = "Random Forest vs Fractional Random Forest",
      country = country_name,
      h = h_dm
    ),
    run_family_dm_test(
      xgb_std$predictions_test,
      xgb_frac$predictions_test,
      family = "XGBoost vs Fractional XGBoost",
      country = country_name,
      h = h_dm
    ),
    run_family_dm_test(
      arima_cmp$arima$predictions_test,
      arima_cmp$arfima$predictions_test,
      family = "ARIMA vs ARFIMA",
      country = country_name,
      h = 1
    )
  )

  list(
    country = country_name,
    target_col = target_col,
    h_dm = h_dm,
    tests = per_family,
    fits = list(
      rf = rf_std,
      rf_fractional = rf_frac,
      xgb = xgb_std,
      xgb_fractional = xgb_frac,
      arima = arima_cmp$arima,
      arfima = arima_cmp$arfima
    )
  )
}

run_cross_country_dm_analysis <- function(covid_data,
                                          covid_frac_data,
                                          target_col = "target_new_confirmed_tplus7",
                                          train_ratio = 0.8,
                                          min_obs = 80,
                                          seed = 123) {
  countries <- sort(intersect(unique(covid_data$country), unique(covid_frac_data$country)))

  all_results <- list()
  error_log <- list()

  for (ct in countries) {
    df_std <- covid_data[covid_data$country == ct, , drop = FALSE]
    df_std <- df_std[order(df_std$date), , drop = FALSE]

    df_frac <- covid_frac_data[covid_frac_data$country == ct, , drop = FALSE]
    df_frac <- df_frac[order(df_frac$date), , drop = FALSE]

    if (nrow(df_std) < min_obs || nrow(df_frac) < min_obs) {
      next
    }

    if (!(target_col %in% names(df_std)) || !(target_col %in% names(df_frac))) {
      next
    }

    res <- tryCatch(
      run_country_dm_suite(
        df_standard = df_std,
        df_fractional = df_frac,
        country_name = ct,
        target_col = target_col,
        train_ratio = train_ratio,
        seed = seed
      ),
      error = function(e) e
    )

    if (inherits(res, "error")) {
      error_log[[length(error_log) + 1]] <- data.frame(
        country = ct,
        error = conditionMessage(res),
        stringsAsFactors = FALSE
      )
    } else {
      all_results[[length(all_results) + 1]] <- res$tests
    }
  }

  tests_tbl <- if (length(all_results) > 0) dplyr::bind_rows(all_results) else data.frame()
  err_tbl <- if (length(error_log) > 0) dplyr::bind_rows(error_log) else data.frame()

  summary_tbl <- if (nrow(tests_tbl) > 0) {
    dplyr::bind_rows(lapply(split(tests_tbl, tests_tbl$family), function(df_one) {
      data.frame(
        family = unique(df_one$family),
        countries_tested = nrow(df_one),
        significant_dm_5pct = sum(is.finite(df_one$dm_p_value) & df_one$dm_p_value < 0.05, na.rm = TRUE),
        standard_better = sum(df_one$better_model == "standard", na.rm = TRUE),
        fractional_better = sum(df_one$better_model == "fractional", na.rm = TRUE),
        ties = sum(df_one$better_model == "tie", na.rm = TRUE),
        median_dm_p_value = stats::median(df_one$dm_p_value, na.rm = TRUE),
        mean_loss_diff = mean(df_one$mean_loss_diff, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    }))
  } else {
    data.frame()
  }

  if (nrow(summary_tbl) > 0) {
    summary_tbl$dominant_model <- ifelse(
      summary_tbl$mean_loss_diff > 0,
      "fractional",
      ifelse(summary_tbl$mean_loss_diff < 0, "standard", "tie")
    )
  }

  fds_tbl <- compute_fractional_dominance(tests_tbl)

  if (nrow(summary_tbl) > 0 && nrow(fds_tbl) > 0) {
    summary_tbl <- dplyr::left_join(summary_tbl, fds_tbl[, c("family", "fractional_dominance_score", "dominance_label")], by = "family")
  }

  list(
    by_country = tests_tbl,
    summary = summary_tbl,
    fractional_dominance = fds_tbl,
    errors = err_tbl
  )
}


compute_fractional_dominance <- function(tests_tbl) {
  if (is.null(tests_tbl) || nrow(tests_tbl) == 0) {
    return(data.frame())
  }

  dplyr::bind_rows(lapply(split(tests_tbl, tests_tbl$family), function(df_one) {
    tested_n <- sum(is.finite(df_one$dm_p_value))
    frac_sig_n <- sum(df_one$better_model == "fractional" & is.finite(df_one$dm_p_value) & df_one$dm_p_value < 0.05, na.rm = TRUE)
    std_sig_n <- sum(df_one$better_model == "standard" & is.finite(df_one$dm_p_value) & df_one$dm_p_value < 0.05, na.rm = TRUE)
    fds <- if (tested_n > 0) frac_sig_n / tested_n else NA_real_

    data.frame(
      family = unique(df_one$family),
      countries_tested = tested_n,
      significant_fractional_wins = frac_sig_n,
      significant_standard_wins = std_sig_n,
      fractional_dominance_score = fds,
      dominance_label = ifelse(is.na(fds), NA_character_, ifelse(fds > 0.5, "fractional", ifelse(fds < 0.5, "standard", "balanced"))),
      stringsAsFactors = FALSE
    )
  }))
}


normalize_country_for_world_map <- function(country) {
  out <- as.character(country)
  recode_idx <- !is.na(out)
  out[recode_idx] <- dplyr::recode(
    out[recode_idx],
    "United States of America" = "USA",
    "United States" = "USA",
    "Russian Federation" = "Russia",
    "Viet Nam" = "Vietnam",
    "Republic of Korea" = "South Korea",
    "Democratic People's Republic of Korea" = "North Korea",
    "Iran (Islamic Republic of)" = "Iran",
    "Syrian Arab Republic" = "Syria",
    "Lao People's Democratic Republic" = "Laos",
    "Republic of Moldova" = "Moldova",
    "United Republic of Tanzania" = "Tanzania",
    "Czechia" = "Czech Republic",
    "Brunei Darussalam" = "Brunei",
    "Cabo Verde" = "Cape Verde",
    "Timor-Leste" = "Timor-Leste",
    "Eswatini" = "Swaziland",
    "North Macedonia" = "Macedonia",
    "Türkiye" = "Turkey",
    "Micronesia (Federated States of)" = "Micronesia",
    "Bolivia (Plurinational State of)" = "Bolivia",
    "Venezuela (Bolivarian Republic of)" = "Venezuela",
    "Dominican Republic" = "Dominican Rep.",
    "Bosnia and Herzegovina" = "Bosnia and Herz.",
    "The Bahamas" = "Bahamas",
    "Bahamas" = "Bahamas",
    "Gambia" = "Gambia",
    "Côte d’Ivoire" = "Ivory Coast",
    "Cote d'Ivoire" = "Ivory Coast",
    "Curaçao" = "Curacao",
    "occupied Palestinian territory, including east Jerusalem" = "Palestine",
    "State of Palestine" = "Palestine"
  )
  out
}

compute_dm_map_data <- function(by_country_tbl, family = NULL) {
  if (is.null(by_country_tbl) || nrow(by_country_tbl) == 0) {
    return(data.frame())
  }

  df <- by_country_tbl
  if (!is.null(family) && length(family) == 1 && is.character(family) && nzchar(family)) {
    df <- df[df$family == family, , drop = FALSE]
  }
  if (nrow(df) == 0) return(data.frame())

  df$map_status <- ifelse(
    is.finite(df$dm_p_value) & df$dm_p_value < 0.05 & df$better_model == "fractional",
    "fractional better",
    ifelse(
      is.finite(df$dm_p_value) & df$dm_p_value < 0.05 & df$better_model == "standard",
      "classical better",
      "non-significant"
    )
  )
  df$region <- normalize_country_for_world_map(df$country)
  df[, c("country", "region", "family", "dm_p_value", "better_model", "map_status")]
}


compute_dm_status_counts <- function(by_country_tbl) {
  if (is.null(by_country_tbl) || nrow(by_country_tbl) == 0) return(data.frame())
  df <- by_country_tbl
  df$comparison_status <- ifelse(
    is.finite(df$dm_p_value) & df$dm_p_value < 0.05 & df$better_model == "fractional",
    "fractional better",
    ifelse(
      is.finite(df$dm_p_value) & df$dm_p_value < 0.05 & df$better_model == "standard",
      "classical better",
      "non-significant"
    )
  )
  out <- dplyr::bind_rows(lapply(split(df, df$family), function(df_one) {
    data.frame(
      family = unique(df_one$family),
      category = c("fractional better", "classical better", "non-significant"),
      countries = c(
        sum(df_one$comparison_status == "fractional better", na.rm = TRUE),
        sum(df_one$comparison_status == "classical better", na.rm = TRUE),
        sum(df_one$comparison_status == "non-significant", na.rm = TRUE)
      ),
      stringsAsFactors = FALSE
    )
  }))
  rownames(out) <- NULL
  out
}

compute_dm_heatmap_data <- function(by_country_tbl) {
  if (is.null(by_country_tbl) || nrow(by_country_tbl) == 0) return(data.frame())
  df <- by_country_tbl
  df$comparison_status <- ifelse(
    is.finite(df$dm_p_value) & df$dm_p_value < 0.05 & df$better_model == "fractional",
    "fractional better",
    ifelse(
      is.finite(df$dm_p_value) & df$dm_p_value < 0.05 & df$better_model == "standard",
      "classical better",
      "non-significant"
    )
  )
  score <- ifelse(df$comparison_status == "fractional better", 1L,
                  ifelse(df$comparison_status == "classical better", -1L, 0L))
  df$dominance_score <- score
  ord <- stats::aggregate(dominance_score ~ country, data = df, FUN = sum, na.rm = TRUE)
  ord <- ord[order(ord$dominance_score, ord$country, decreasing = TRUE), , drop = FALSE]
  df$country <- factor(df$country, levels = ord$country)
  df$family <- factor(df$family, levels = c(
    "ARIMA vs ARFIMA",
    "Random Forest vs Fractional Random Forest",
    "XGBoost vs Fractional XGBoost"
  ))
  df[, c("country", "family", "comparison_status", "dm_p_value", "better_model", "mean_loss_diff")]
}

compute_top_fractional_countries <- function(by_country_tbl, top_n = 20, significant_only = TRUE) {
  if (is.null(by_country_tbl) || nrow(by_country_tbl) == 0) return(data.frame())
  df <- by_country_tbl
  df <- df[df$better_model == "fractional", , drop = FALSE]
  if (significant_only) {
    df <- df[is.finite(df$dm_p_value) & df$dm_p_value < 0.05, , drop = FALSE]
  }
  if (nrow(df) == 0) return(data.frame())
  ord_abs <- abs(df$mean_loss_diff)
  ord_abs[!is.finite(ord_abs)] <- -Inf
  df <- df[order(df$dm_p_value, -ord_abs, df$country), , drop = FALSE]
  rownames(df) <- NULL
  utils::head(df[, c(
    "country", "family", "n", "dm_statistic", "dm_p_value",
    "mean_loss_standard", "mean_loss_fractional", "mean_loss_diff",
    "dm_varestimator", "dm_note"
  )], top_n)
}


compute_dm_country_stats <- function(by_country_tbl, selected_country) {
  if (is.null(by_country_tbl) || nrow(by_country_tbl) == 0 || is.null(selected_country) || !nzchar(selected_country)) {
    return(data.frame())
  }
  df <- by_country_tbl[by_country_tbl$country == selected_country, , drop = FALSE]
  if (nrow(df) == 0) return(data.frame())
  df$comparison_status <- ifelse(
    is.finite(df$dm_p_value) & df$dm_p_value < 0.05 & df$better_model == "fractional",
    "fractional better",
    ifelse(
      is.finite(df$dm_p_value) & df$dm_p_value < 0.05 & df$better_model == "standard",
      "classical better",
      "non-significant"
    )
  )
  df[, c("country", "family", "n", "dm_statistic", "dm_p_value", "mean_loss_diff", "better_model", "comparison_status", "dm_varestimator", "dm_note")]
}

compute_world_centroids <- function() {
  world <- ggplot2::map_data("world")
  stats::aggregate(cbind(long, lat) ~ region, data = world, FUN = mean)
}

compute_dm_map_markers <- function(by_country_tbl, family = NULL) {
  df <- compute_dm_map_data(by_country_tbl, family = family)
  if (is.null(df) || nrow(df) == 0) return(data.frame())
  cent <- compute_world_centroids()
  out <- merge(df, cent, by = "region", all.x = TRUE)
  out <- out[is.finite(out$long) & is.finite(out$lat), , drop = FALSE]
  rownames(out) <- NULL
  out
}


build_dm_heatmap_hover_table <- function(heatmap_tbl) {
  if (is.null(heatmap_tbl) || nrow(heatmap_tbl) == 0) return(data.frame())
  df <- heatmap_tbl
  df$tooltip <- paste0(
    "Country: ", df$country, "&#10;",
    "Family: ", df$family, "&#10;",
    "Outcome: ", df$comparison_status, "&#10;",
    "Better model: ", df$better_model, "&#10;",
    "DM statistic: ", fmt_num(df$dm_statistic, 3), "&#10;",
    "p-value: ", fmt_num(df$dm_p_value, 4), "&#10;",
    "Mean loss diff: ", fmt_num(df$mean_loss_diff, 2), "&#10;",
    "Mean loss standard: ", fmt_num(df$mean_loss_standard, 2), "&#10;",
    "Mean loss fractional: ", fmt_num(df$mean_loss_fractional, 2), "&#10;",
    "Variance estimator: ", df$dm_varestimator, "&#10;",
    "Note: ", df$dm_note
  )
  df$display_html <- paste0('<span title="', df$tooltip, '">', df$comparison_status, '</span>')
  wide <- tidyr::pivot_wider(
    df[, c("country", "family", "display_html")],
    names_from = family,
    values_from = display_html
  )
  rownames(wide) <- NULL
  wide
}

plot_dm_family_maps <- function(by_country_tbl) {
  if (is.null(by_country_tbl) || nrow(by_country_tbl) == 0) {
    return(ggplot() + theme_void() + labs(title = "No map data available"))
  }
  world <- ggplot2::map_data("world")
  map_df <- compute_dm_map_data(by_country_tbl)
  world2 <- dplyr::left_join(world, map_df, by = "region")
  world2$map_status[is.na(world2$map_status)] <- "non-significant"
  ggplot(world2, aes(long, lat, group = group, fill = map_status)) +
    geom_polygon(color = "white", linewidth = 0.12) +
    coord_quickmap() +
    facet_wrap(~family, ncol = 1) +
    scale_fill_manual(
      values = c(
        "fractional better" = "#2ca25f",
        "classical better" = "#de2d26",
        "non-significant" = "#d9d9d9"
      ),
      breaks = c("fractional better", "classical better", "non-significant"),
      drop = FALSE,
      name = "Country shading"
    ) +
    labs(
      title = "Global Diebold–Mariano dominance maps by model family",
      subtitle = "Green = fractional better, red = classical better, grey = non-significant"
    ) +
    theme_void(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      strip.text = element_text(face = "bold", colour = "#1f3c88"),
      legend.position = "bottom"
    )
}

save_dm_pngs <- function(dm_results, output_dir, prefix = "diebold_mariano") {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  pngs <- list()

  if (!is.null(dm_results$fractional_dominance) && nrow(dm_results$fractional_dominance) > 0) {
    p_fds <- ggplot(dm_results$fractional_dominance, aes(x = family, y = fractional_dominance_score)) +
      geom_col() +
      geom_hline(yintercept = 0.5, linetype = "dashed") +
      labs(title = "Fractional Dominance Score by model family", x = "Model family", y = "FDS") +
      ylim(0, 1) +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 20, hjust = 1))
    fp <- file.path(output_dir, paste0(prefix, "_fds_", timestamp, ".png"))
    ggplot2::ggsave(fp, p_fds, width = 9, height = 5, dpi = 300)
    pngs$fds <- fp
    ggplot2::ggsave(file.path(output_dir, paste0(prefix, "_fds_latest.png")), p_fds, width = 9, height = 5, dpi = 300)
  }

  if (!is.null(dm_results$by_country) && nrow(dm_results$by_country) > 0) {
    status_counts <- compute_dm_status_counts(dm_results$by_country)
    p_counts <- ggplot(status_counts, aes(x = family, y = countries, fill = category)) +
      geom_col(position = "stack") +
      scale_fill_manual(values = c("fractional better" = "#2ca25f", "classical better" = "#de2d26", "non-significant" = "#bdbdbd"), name = "DM outcome") +
      labs(title = "Country counts by family and Diebold–Mariano outcome", x = "Model family", y = "Number of countries") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 20, hjust = 1), legend.position = "bottom")
    fp = file.path(output_dir, paste0(prefix, "_family_counts_", timestamp, ".png"))
    ggplot2::ggsave(fp, p_counts, width = 10, height = 6, dpi = 300)
    pngs$family_counts <- fp
    ggplot2::ggsave(file.path(output_dir, paste0(prefix, "_family_counts_latest.png")), p_counts, width = 10, height = 6, dpi = 300)

    p_maps <- plot_dm_family_maps(dm_results$by_country)
    fp = file.path(output_dir, paste0(prefix, "_family_maps_", timestamp, ".png"))
    ggplot2::ggsave(fp, p_maps, width = 11, height = 14, dpi = 300)
    pngs$family_maps <- fp
    ggplot2::ggsave(file.path(output_dir, paste0(prefix, "_family_maps_latest.png")), p_maps, width = 11, height = 14, dpi = 300)

    fams <- unique(dm_results$by_country$family)
    for (fam in fams) {
      map_df <- compute_dm_map_data(dm_results$by_country, family = fam)
      if (nrow(map_df) > 0) {
        world <- ggplot2::map_data("world")
        world2 <- dplyr::left_join(world, map_df, by = "region")
        world2$map_status[is.na(world2$map_status)] <- "non-significant"
        pf <- ggplot(world2, aes(long, lat, group = group, fill = map_status)) +
          geom_polygon(color = "white", linewidth = 0.12) +
          coord_quickmap() +
          scale_fill_manual(values = c("fractional better" = "#2ca25f", "classical better" = "#de2d26", "non-significant" = "#d9d9d9"), drop = FALSE) +
          labs(title = paste("Global Diebold–Mariano map -", fam), subtitle = "Green = fractional better, red = classical better, grey = non-significant") +
          theme_void(base_size = 12) +
          theme(plot.title = element_text(face = "bold"), legend.position = "bottom")
        safe_name <- gsub("[^A-Za-z0-9]+", "_", fam)
        fp_f <- file.path(output_dir, paste0(prefix, "_map_", safe_name, "_", timestamp, ".png"))
        ggplot2::ggsave(fp_f, pf, width = 11, height = 6, dpi = 300)
        pngs[[paste0("map_", safe_name)]] <- fp_f
        ggplot2::ggsave(file.path(output_dir, paste0(prefix, "_map_", safe_name, "_latest.png")), pf, width = 11, height = 6, dpi = 300)
      }
    }
  }
  pngs
}
