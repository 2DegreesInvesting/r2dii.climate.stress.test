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
        company_id = "d",
        company_name = "c",
        scenario_geography = "c",
        year = "d",
        ald_sector = "c",
        ald_business_unit = "c",
        ald_production_unit="c",
        emissions_factor_unit="c",
        plan_tech_prod = "d",
        plan_emission_factor = "d",
        plan_sec_prod = "d"
      )
    )

  validate_data_has_expected_cols(
    data = data,
    expected_columns = c(
      "company_id", "company_name", "scenario_geography", "year", "ald_sector",
      "ald_production_unit", "emissions_factor_unit",
      "ald_business_unit", "plan_tech_prod", "plan_emission_factor", "plan_sec_prod"
    )
  )

  return(data)
}
