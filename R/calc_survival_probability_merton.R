#' Calculate survival probability
#'
#' Function calculates survival probability for a maturity based on a structural
#' Merton model.
#' For details on implementation please compare [CreditRisk::Merton()].
#' Unlike [CreditRisk::Merton()] this implementation:
#' 1. only holds functionality to calculate probability of survival
#' 1. can be called in vectorised fashion
#' 1. additionally checks that all input values are of the same length
#' 1. additionally checks input vectors for implausible values (`r` must be => 0
#' and all other args > 0)
#'
#' @param L Numeric vector, holding debt values at maturity.
#' @param V0 Numeric vector, holding company values at time t0.
#' @param sigma Numeric vector, holding volatility values.
#' @param r Numeric vector, holding risk free interest rates.
#' @param t Vector vector holding debt maturities.
#'
#' @return A vector holding survival probabilities,
calc_survival_probability_merton <- function(L, V0, sigma, r, t) {

  input_args <- list(L, V0, sigma, r, t)

  if (dplyr::n_distinct(purrr::map_int(input_args, length)) > 1) {
    stop("All input arugments need to have the same length.")
  }

  if (!all(unique(purrr::map_lgl(input_args, is.numeric)))) {
    stop("All input arguments need to be numeric.")
  }

  if (!all(r >= 0)) {
    stop("Argument r may not be negative.")
  }

  if (!all(unique(purrr::map_lgl(list(L, V0, sigma, t), function(x) {
    all(x > 0)
  })))) {

    stop(paste0("Unexpected non positive numbers detected on at least one of arguments L, V0, sigma, t."))
  }

  d1 <- (log(V0 / L) + (r + (sigma^2 / 2) * t)) / (sigma * sqrt(t))
  d2 <- d1 - sigma * sqrt(t)

  # Default Probability
  p_default <- stats::pnorm(-d2)

  # Survival Probability Q(tau > T)
  p_survival <- 1 - p_default

  return(p_survival)
}
