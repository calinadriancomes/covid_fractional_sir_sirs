# R/fractional_features.R

fractional_binomial_weights <- function(alpha, k_max) {
  w <- numeric(k_max + 1)
  w[1] <- 1
  if (k_max >= 1) {
    for (k in 1:k_max) {
      w[k + 1] <- w[k] * ((k - 1 - alpha) / k)
    }
  }
  w
}

fractional_difference_gl <- function(x, alpha = 0.4, k_max = 30) {
  n <- length(x)
  w <- fractional_binomial_weights(alpha, k_max)
  out <- rep(NA_real_, n)

  for (t in seq_len(n)) {
    k_use <- min(k_max, t - 1)
    vals <- x[t:(t - k_use)]
    weights <- w[1:(k_use + 1)]
    out[t] <- sum(weights * vals, na.rm = FALSE)
  }

  out
}

fractional_memory_index <- function(x, alpha = 0.4, k_max = 14) {
  n <- length(x)
  weights <- 1 / ((1:k_max)^alpha)
  weights <- weights / sum(weights)
  out <- rep(NA_real_, n)

  for (t in seq_len(n)) {
    if (t <= k_max) next
    hist_vals <- x[(t - 1):(t - k_max)]
    out[t] <- sum(hist_vals * weights, na.rm = TRUE)
  }

  out
}

add_fractional_features <- function(df, alphas = c(0.2, 0.4, 0.6, 0.8), k_max = 30) {
  df |>
    dplyr::group_by(country) |>
    dplyr::arrange(date, .by_group = TRUE) |>
    dplyr::group_modify(function(.x, .y) {
      tmp <- .x

      for (a in alphas) {
        nm1 <- paste0("fracdiff_new_confirmed_a", gsub("\\.", "_", as.character(a)))
        nm2 <- paste0("fracdiff_I_a", gsub("\\.", "_", as.character(a)))
        nm3 <- paste0("fracmem_new_confirmed_a", gsub("\\.", "_", as.character(a)))

        tmp[[nm1]] <- fractional_difference_gl(tmp$new_confirmed, alpha = a, k_max = k_max)
        tmp[[nm2]] <- fractional_difference_gl(tmp$I, alpha = a, k_max = k_max)
        tmp[[nm3]] <- fractional_memory_index(tmp$new_confirmed, alpha = a, k_max = min(14, k_max))
      }

      tmp
    }) |>
    dplyr::ungroup()
}