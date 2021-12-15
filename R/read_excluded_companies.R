#' Read excluded company data
#'
#' @param path Path to company exclusion data.
#'
#' @return A tibble holding company exclusion data.
read_excluded_companies <- function(path) {
  excluded_companies <- validate_file_exists(path) %>%
    readr::read_csv(
      col_types = readr::cols(
        company_name = "c",
        technology = "c"
      )
    )

  return(excluded_companies)
}
