#' Read in transition scenarios from csv and check that all expected columns
#' are given.
#'
#' @param path A string that points to the location of the file containing the
#'   transition scenarios
#' @param start_of_analysis A numeric vector of length one that indicates the
#'   start year of the analysis
#' @param end_of_analysis A numeric vector of length one that indicates the
#'   end year of the analysis
#'
#' @family import functions
#'
#' @export
read_transition_scenarios <- function(path = NULL,
                                      start_of_analysis = NULL,
                                      end_of_analysis = NULL) {
  path %||% stop("Must provide 'path'")
  start_of_analysis %||% stop("Must provide 'start_of_analysis'")
  end_of_analysis %||% stop("Must provide 'end_of_analysis'")

  data <- validate_file_exists(path) %>%
    readr::read_csv(
    col_types = readr::cols(
      scenario_name = "c",
      year_of_shock = "d",
      overshoot_method = "l",
      duration_of_shock = "d",
      use_prod_forecasts_ls = "l",
      use_prod_forecasts_baseline = "l",
      .default = readr::col_number()
    )
  )

  data <- data %>%
    dplyr::mutate(
      overshoot_method = dplyr::if_else(
        is.na(.data$overshoot_method),
        FALSE,
        .data$overshoot_method
      ),
      duration_of_shock = dplyr::if_else(
        .data$overshoot_method,
        .env$end_of_analysis - .data$year_of_shock + 1,
        .data$duration_of_shock
      )
    )

  min_year_of_shock <- min(data$year_of_shock, na.rm = TRUE)
  max_year_of_shock <- max(data$year_of_shock, na.rm = TRUE)

  if (min_year_of_shock < start_of_analysis) {
    stop("Year of shock out of bounds. Shock cannot happen before the start year
         of the anaylsis.")
  }

  if (max_year_of_shock > end_of_analysis) {
    stop("Year of shock out of bounds. Shock cannot happen after the end year of
         the anaylsis.")
  }

  validate_data_has_expected_cols(
    data = data,
    expected_columns = c(
      "scenario_name", "year_of_shock", "overshoot_method", "duration_of_shock",
      "use_prod_forecasts_ls", "use_prod_forecasts_baseline"
    )
  )

  return(data)
}


#' Generate transition scenario shocks from a range of start years that
#' represent different years of when a large scale climate transition policy is
#' deployed.
#'
#' @param start_of_analysis A numeric vector of length one that indicates the
#'   start year of the analysis
#' @param end_of_analysis A numeric vector of length one that indicates the
#'   end year of the analysis
#' @param shock_years A numeric vector that provides a set of start years for
#'   the shocks to be used in the analysis
#'
#' @family import functions
#'
#' @export
generate_transition_shocks <- function(start_of_analysis,
                                       end_of_analysis,
                                       shock_years) {
  bounds <- list(start_of_analysis, end_of_analysis)

  if (dplyr::n_distinct(purrr::map_int(bounds, length)) > 1) {
    stop("Input arugments for start_of_analysis and end_of_analysis need to have length 1.")
  }

  input_args <- list(start_of_analysis, end_of_analysis, shock_years)

  if (!all(unique(purrr::map_lgl(input_args, is.numeric)))) {
    stop("All input arguments need to be numeric.")
  }

  min_year_of_shock <- min(shock_years, na.rm = TRUE)
  max_year_of_shock <- max(shock_years, na.rm = TRUE)

  if (min_year_of_shock < start_of_analysis) {
    stop("Year of shock out of bounds. Shock cannot happen before the start year
         of the anaylsis.")
  }

  if (max_year_of_shock > end_of_analysis) {
    stop("Year of shock out of bounds. Shock cannot happen after the end year of
         the anaylsis.")
  }

  data <- tibble::tibble(
    year_of_shock = shock_years,
    scenario_name = glue::glue("Carbon balance {year_of_shock}"),
    duration_of_shock = end_of_analysis - year_of_shock + 1
  )

  validate_data_has_expected_cols(
    data = data,
    expected_columns = c(
      "scenario_name", "year_of_shock", "duration_of_shock"
    )
  )

  return(data)
}
