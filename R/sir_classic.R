# R/sir_classic.R

fit_sir_naive <- function(df_country) {
  req_cols <- c("date", "S", "I", "R_compartment", "beta_naive", "gamma_naive")
  miss <- setdiff(req_cols, names(df_country))
  if (length(miss) > 0) {
    stop("Missing required columns in df_country: ", paste(miss, collapse = ", "))
  }

  beta_hat <- median(df_country$beta_naive[is.finite(df_country$beta_naive)], na.rm = TRUE)
  gamma_hat <- median(df_country$gamma_naive[is.finite(df_country$gamma_naive)], na.rm = TRUE)

  if (!is.finite(beta_hat)) beta_hat <- 0.2
  if (!is.finite(gamma_hat)) gamma_hat <- 0.1

  list(
    beta = beta_hat,
    gamma = gamma_hat
  )
}

simulate_sir_discrete <- function(S0, I0, R0, N, beta, gamma, n_steps) {
  S <- numeric(n_steps)
  I <- numeric(n_steps)
  R <- numeric(n_steps)
  new_infections <- numeric(n_steps)

  S[1] <- S0
  I[1] <- I0
  R[1] <- R0
  new_infections[1] <- beta * S0 * I0 / N

  for (t in 2:n_steps) {
    inf_t <- beta * S[t - 1] * I[t - 1] / N
    rec_t <- gamma * I[t - 1]

    S[t] <- max(S[t - 1] - inf_t, 0)
    I[t] <- max(I[t - 1] + inf_t - rec_t, 0)
    R[t] <- max(R[t - 1] + rec_t, 0)
    new_infections[t] <- inf_t
  }

  data.frame(
    step = seq_len(n_steps),
    S_sim = S,
    I_sim = I,
    R_sim = R,
    new_infections_sim = new_infections
  )
}

run_sir_forecast <- function(df_country, horizon = 7) {
  df_country <- df_country[order(df_country$date), ]

  pars <- fit_sir_naive(df_country)

  S0 <- tail(df_country$S, 1)
  I0 <- tail(df_country$I, 1)
  R0 <- tail(df_country$R_compartment, 1)
  N  <- S0 + I0 + R0

  sim <- simulate_sir_discrete(
    S0 = S0,
    I0 = I0,
    R0 = R0,
    N = N,
    beta = pars$beta,
    gamma = pars$gamma,
    n_steps = horizon
  )

  sim$date <- seq(from = max(df_country$date) + 1, by = "day", length.out = horizon)
  sim$beta <- pars$beta
  sim$gamma <- pars$gamma

  sim
}