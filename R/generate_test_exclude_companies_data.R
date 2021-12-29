#' Generates a toy empty exclude-companies data set.
#'
#' @description
#' Does not take any inputs. Generates an data set that corresponds in structure
#' to the one used in the transition risk stress test. Company exclusion is not
#' a main test case, hence no entries are required.
#' @family example case functions
#' @return data frame
#' @export
generate_test_exclude_companies_data <- function() {
  test_exclude_companies_data <- tibble::tibble(
    company_name = character(),
    technology = character()
  )
}
