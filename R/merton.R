#' Calculate survival probability
#'
#' Function calculates survival probability for a maturity based on a structural
#' Merton model.
#' For details on implementation please compare [CreditRisk::Merton()].
#' Unlike [CreditRisk::Merton()] this implementation:
#' 1. only holds functionality to calculate probability of survival
#' 1. can be called in vectorised fashion
#' 1. has more robust input validation.
#'
#' @param L Numeric, holding dept value at maturity.
#' @param V0 Numeric, holding value at time t0.
#' @param sigma Numeric, holding volatility value.
#' @param r Numeric, holding risk free rate.
#' @param t Vector holding debt maturity.
#'
#' @return A tibble holding survival probability,
calc_survival_probabily_merton <- function(L, V0, sigma, r, t) {

  d1 <- (log(V0 / L) + (r + (sigma^2 / 2) * t)) / (sigma * sqrt(t))
  d2 <- d1 - sigma * sqrt(t)

  # Default Probability
  Q <- stats::pnorm(-d2)

  # Survival Probability Q(tau > T)
  p_survival <- 1 - Q

  survival <- tibble::tibble(Survival = p_survival)

  return(survival)
}
