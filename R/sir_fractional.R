# R/sir_fractional.R

fractional_kernel_weights <- function(alpha = 0.6, k_max = 14) {
  k <- 1:k_max
  w <- 1 / (k^alpha)
  w / sum(w)
}

fractional_memory_term <- function(I_hist, alpha = 0.6, k_max = 14) {
  n <- length(I_hist)
  out <- rep(NA_real_, n)
  w <- fractional_kernel_weights(alpha = alpha, k_max = k_max)

  for (t in seq_len(n)) {
    if (t <= k_max) next
    past_vals <- I_hist[(t - 1):(t - k_max)]
    out[t] <- sum(past_vals * w, na.rm = TRUE)
  }

  out
}

fit_fractional_sir_proxy <- function(df_country, alpha = 0.6, k_max = 14) {
  req_cols <- c("date", "S", "I", "R_compartment", "beta_naive", "gamma_naive")
  miss <- setdiff(req_cols, names(df_country))
  if (length(miss) > 0) {
    stop("Missing required columns in df_country: ", paste(miss, collapse = ", "))
  }

  df_country <- df_country[order(df_country$date), ]
  mem_I <- fractional_memory_term(df_country$I, alpha = alpha, k_max = k_max)

  beta_hat <- median(df_country$beta_naive[is.finite(df_country$beta_naive)], na.rm = TRUE)
  gamma_hat <- median(df_country$gamma_naive[is.finite(df_country$gamma_naive)], na.rm = TRUE)

  if (!is.finite(beta_hat)) beta_hat <- 0.2
  if (!is.finite(gamma_hat)) gamma_hat <- 0.1

  # A pragmatic memory coefficient estimated from relative scale
  lambda_hat <- median(mem_I / pmax(df_country$I, 1), na.rm = TRUE)
  if (!is.finite(lambda_hat)) lambda_hat <- 0.15
  lambda_hat <- min(max(lambda_hat, 0.01), 0.5)

  list(
    beta = beta_hat,
    gamma = gamma_hat,
    alpha = alpha,
    lambda = lambda_hat,
    k_max = k_max
  )
}

simulate_fractional_sir_proxy <- function(S0, I0, R0, N,
                                          beta, gamma,
                                          alpha = 0.6,
                                          lambda = 0.15,
                                          n_steps = 14,
                                          I_history = NULL,
                                          k_max = 14) {
  if (is.null(I_history)) {
    I_history <- rep(I0, k_max)
  }

  S <- numeric(n_steps)
  I <- numeric(n_steps)
  R <- numeric(n_steps)
  new_infections <- numeric(n_steps)
  mem_term_vec <- numeric(n_steps)

  S[1] <- S0
  I[1] <- I0
  R[1] <- R0

  I_buffer <- c(I_history, I0)
  w <- fractional_kernel_weights(alpha = alpha, k_max = k_max)

  for (t in seq_len(n_steps)) {
    recent_hist <- rev(tail(I_buffer, k_max))
    mem_term <- sum(recent_hist * w, na.rm = TRUE)
    mem_term_vec[t] <- mem_term

    effective_I <- (1 - lambda) * ifelse(t == 1, I0, I[t]) + lambda * mem_term
    inf_t <- beta * ifelse(t == 1, S0, S[t]) * effective_I / N
    rec_t <- gamma * effective_I

    if (t == 1) {
      new_infections[t] <- inf_t
    } else {
      S[t] <- max(S[t - 1] - inf_t, 0)
      I[t] <- max(I[t - 1] + inf_t - rec_t, 0)
      R[t] <- max(R[t - 1] + rec_t, 0)
      new_infections[t] <- inf_t
    }

    if (t == 1) {
      S[t] <- max(S0 - inf_t, 0)
      I[t] <- max(I0 + inf_t - rec_t, 0)
      R[t] <- max(R0 + rec_t, 0)
    }

    I_buffer <- c(I_buffer, I[t])
  }

  data.frame(
    step = seq_len(n_steps),
    S_frac = S,
    I_frac = I,
    R_frac = R,
    new_infections_frac = new_infections,
    memory_term = mem_term_vec
  )
}

run_fractional_sir_forecast <- function(df_country, horizon = 14, alpha = 0.6, k_max = 14) {
  df_country <- df_country[order(df_country$date), ]

  pars <- fit_fractional_sir_proxy(df_country, alpha = alpha, k_max = k_max)

  S0 <- tail(df_country$S, 1)
  I0 <- tail(df_country$I, 1)
  R0 <- tail(df_country$R_compartment, 1)
  N  <- S0 + I0 + R0

  I_history <- tail(df_country$I, k_max)
  if (length(I_history) < k_max) {
    I_history <- c(rep(I0, k_max - length(I_history)), I_history)
  }

  sim <- simulate_fractional_sir_proxy(
    S0 = S0,
    I0 = I0,
    R0 = R0,
    N = N,
    beta = pars$beta,
    gamma = pars$gamma,
    alpha = pars$alpha,
    lambda = pars$lambda,
    n_steps = horizon,
    I_history = I_history,
    k_max = k_max
  )

  sim$date <- seq(from = max(df_country$date) + 1, by = "day", length.out = horizon)
  sim$beta <- pars$beta
  sim$gamma <- pars$gamma
  sim$alpha <- pars$alpha
  sim$lambda <- pars$lambda

  sim
}