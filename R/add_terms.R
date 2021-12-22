#' Add company terms
#'
#' Function adds terms as provided on company level in `company_terms`. If
#' `company_terms` is NULL or terms are missing they are substituted with the
#' `fallback_term`.
#'
#' @param pacta_results A tibble holding pacta_results.
#' @param company_terms A tibble holding company_terms.
#' @param fallback_term A numeric holding term value to use as fallback for
#'   missing terms.
#'
#' @return `data` with added column terms.
add_terms <- function(pacta_results, company_terms, fallback_term) {

  if (is.null(company_terms)) {
    pacta_results <- pacta_results %>%
      dplyr::mutate(term = fallback_term)

    return(pacta_results)
  }

}
