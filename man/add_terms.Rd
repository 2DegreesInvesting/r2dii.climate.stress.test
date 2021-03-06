% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_terms.R
\name{add_terms}
\alias{add_terms}
\title{Add company terms}
\usage{
add_terms(pacta_results, company_terms, fallback_term)
}
\arguments{
\item{pacta_results}{A tibble holding pacta_results.}

\item{company_terms}{A tibble holding company_terms.}

\item{fallback_term}{A numeric holding term value to use as fallback for
missing terms.}
}
\value{
\code{data} with added column terms.
}
\description{
Function adds terms as provided on company level in \code{company_terms}. If
\code{company_terms} is NULL or terms are missing they are substituted with the
\code{fallback_term}.
}
