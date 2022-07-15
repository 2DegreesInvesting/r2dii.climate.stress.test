% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_lrisk.R
\name{run_lrisk}
\alias{run_lrisk}
\title{Run stress testing for provided asset type.}
\usage{
run_lrisk(
  asset_type,
  input_path_project_specific,
  input_path_project_agnostic,
  output_path,
  baseline_scenario = "WEO2020_SPS",
  shock_scenario = "WEO2020_SDS",
  lgd_senior_claims = 0.45,
  lgd_subordinated_claims = 0.75,
  risk_free_rate = 0.02,
  discount_rate = 0.07,
  growth_rate = 0.03,
  div_netprofit_prop_coef = 1,
  shock_year = 2030,
  fallback_term = 2,
  scenario_geography = "Global",
  use_company_terms = FALSE,
  settlement_factor = 1,
  exp_share_damages_paid = 0.027,
  scc = 40L,
  return_results = FALSE
)
}
\arguments{
\item{asset_type}{String holding asset_type. For accepted values compare
\code{stress_test_arguments}.}

\item{input_path_project_specific}{String holding path to project specific
data.}

\item{input_path_project_agnostic}{String holding path to project agnostic
data.}

\item{output_path}{String holding path to which output files are written.
NOTE: Results and logs per run are saved to a subdirectory of output_path
that will be generated automatically. The name of the subdirectory is the
timestamp of the run of the analysis.}

\item{baseline_scenario}{Holds the name of the baseline scenario to be used
in the stress test, for accepted value range check \code{stress_test_arguments}.}

\item{shock_scenario}{Holds the name of the shock scenario to be used in the
stress test, for accepted value range check \code{stress_test_arguments}.}

\item{lgd_senior_claims}{Numeric, holding the loss given default for senior
claims, for accepted value range check \code{stress_test_arguments}.}

\item{lgd_subordinated_claims}{Numeric, holding the loss given default for
subordinated claims, for accepted value range check
\code{stress_test_arguments}.}

\item{risk_free_rate}{Numeric that indicates the risk free rate of interest.
For accepted range compare \code{stress_test_arguments}.}

\item{discount_rate}{Numeric, that holds the discount rate of dividends per
year in the DCF. For accepted range compare \code{stress_test_arguments}.}

\item{growth_rate}{Numeric, that holds the terminal growth rate of profits
beyond the final year in the DCF. For accepted range compare
\code{stress_test_arguments}.}

\item{div_netprofit_prop_coef}{Numeric. A coefficient that determines how
strongly the future dividends propagate to the company value. For accepted
range compare \code{stress_test_arguments}.}

\item{shock_year}{Numeric, holding year the shock is applied. For accepted
range compare \code{stress_test_arguments}.}

\item{fallback_term}{Numeric. A coefficient that determines for which
maturity the expected loss should be calculated in the credit risk section
in case no company level term data are provided via \code{use_company_terms}.
For accepted range compare \code{stress_test_arguments}.}

\item{scenario_geography}{Character vector, indicating which geographical
region(s) (concerning asset location) results shall be calculated for. For
accepted values compare \code{stress_test_arguments}.}

\item{use_company_terms}{Boolean, indicating if term values for individual
companies are to be used. For accepted values compare
\code{stress_test_arguments}. Note that currently this functionality is not
available for asset_type bonds.}

\item{settlement_factor}{bla}

\item{exp_share_damages_paid}{bla}

\item{scc}{bla}

\item{return_results}{Boolean, indicating if results shall be exported.}
}
\description{
This function runs the transition risk stress test. It can be desirable to
understand sensitivities of the scenarios, in which case the user may pass a
vector of values to one (and only one) of the detail arguments. This will
result in running the analysis multiple times in a row with the argument
varied.
NOTE: argument \code{asset_type} and \code{fallback_term} cannot be iterated.
NOTE: if \code{return_results} is TRUE results will not be written to \verb{output path} but instead are returned.
}