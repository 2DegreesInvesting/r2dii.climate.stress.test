#' Read in AR PAMS production data.
#'
#' @param path A string that points to the location of the file containing the
#'   AR PAMS dataset
#'
#' @family import functions
read_production_data <- function(path = NULL) {
  path %||% stop("Must provide 'path'")

  data <- validate_file_exists(path) %>%
    readr::read_csv(
      col_types = readr::cols_only(
        id = "d",
        company_name = "c",
        scenario_geography = "c",
        year = "d",
        ald_sector = "c",
        technology = "c",
        plan_tech_prod = "d",
        plan_emission_factor = "d",
        plan_sec_prod = "d"
      )
    )

  return(data)
}
