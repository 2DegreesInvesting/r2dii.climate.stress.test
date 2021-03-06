% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wrangle_and_check.R
\name{cap_terms}
\alias{cap_terms}
\title{Cap terms}
\usage{
cap_terms(data)
}
\arguments{
\item{data}{A tibble holding at least column \code{term}.}
}
\value{
Tibble \code{data} with capped term.
}
\description{
Caps terms to maximum of 5 (years). The value 5 was chosen as for longer
time frames reliability of model deteriorates. Informative message on number
of affected companies is thrown.
}
