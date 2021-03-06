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
      dplyr::mutate(term = as.double(fallback_term))

    return(pacta_results)
  }

  # keeping track of rows here to alert user if rows were dropped during merge
  # as this would indicate incorrect company_terms data
  companies_before_merge <- unique(pacta_results$company_name)

  results_with_term <- pacta_results %>%
    dplyr::inner_join(company_terms, by = "company_name")

  companies_after_merge <- unique(pacta_results$company_name)

  if (length(companies_before_merge) > length(companies_after_merge)) {
    missing_companies <- paste0(setdiff(companies_before_merge, companies_after_merge), collapse = ", ")
    rlang::abort(c(
      "All companies considered in this analysis must be provided in company_terms.",
      x = glue::glue("Missing companies: {missing_companies}."),
      i = "Please check input data."
    ))
  }

  return(results_with_term)
}
