#' Read in company terms
#'
#' Function reads in company terms and checks for existence of required columns.
#'
#' @param path Path to dir holding company terms data.
#'
#' @return A tibble holding company terms data.
read_company_terms <- function(path) {
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
