% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_data.R
\name{check_level_availability}
\alias{check_level_availability}
\title{Check that required levels of a variable are available in data}
\usage{
check_level_availability(
  data,
  data_name,
  expected_levels_list,
  throw_error = TRUE
)
}
\arguments{
\item{data}{A tibble.}

\item{data_name}{Character, holding name of dataset.}

\item{expected_levels_list}{List holding variable names along with their
expected levels. Variables must be present in data.}

\item{throw_error}{Boolean indicating if to throw error. If FALSE warning is
signaled instead.}
}
\value{
Returns \code{data} invisibly.
}
\description{
Check that required levels of a variable are available in data
}
