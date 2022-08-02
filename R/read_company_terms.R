#' Read in company terms
#'
#' Function reads in company terms and checks for existence of required columns.
#'
#' @inheritParams run_trisk
#' @param path Path to dir holding company terms data.
#'
#' @return A tibble holding company terms data or NULL
read_company_terms <- function(path, use_company_terms) {
  if (!use_company_terms) {
    return(NULL)
  }

  company_terms <- validate_file_exists(path) %>%
    readr::read_csv(
      col_types = readr::cols(
        company_name = "c",
        term = "d"
      )
    )

  validate_data_has_expected_cols(
    data = company_terms,
    expected_columns = c(
      "company_name", "term"
    )
  )
  return(company_terms)
}
