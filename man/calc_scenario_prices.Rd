% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_scenario_prices.R
\name{calc_scenario_prices}
\alias{calc_scenario_prices}
\title{Calculate scenario prices}
\usage{
calc_scenario_prices(
  price_data,
  baseline_scenario,
  target_scenario,
  transition_scenario,
  start_year
)
}
\arguments{
\item{price_data}{A tibble holding price data.}

\item{baseline_scenario}{String holding name of the baseline scenario.}

\item{target_scenario}{String holding name of the target scenario.}

\item{transition_scenario}{Tibble with 1 row holding at least variables
\code{year_of_shock} and \code{duration_of_shock}.}

\item{start_year}{Start_year of analysis}
}
\value{
A tibble holding late_and_sudden_prices
}
\description{
Function generates prices for baseline and late and sudden shock scenario.
Price for baseline scenario correspond to prices of \code{baseline_scenario}.
Prices for the late sudden scenario also correspond to \code{baseline_scenario}
until the \code{year_of_shock}. From then on they linearly approach the price
level of the \code{shock_scenario} during the \code{duration_of_shock}.
}
