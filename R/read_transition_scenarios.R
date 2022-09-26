#' Generate transition scenario shock from a start year that represents when a
#' large scale climate transition policy is deployed.
#'
#' @param start_of_analysis A numeric vector of length one that indicates the
#'   start year of the analysis.
#' @param end_of_analysis A numeric vector of length one that indicates the
#'   end year of the analysis.
#' @param shock_year A numeric vector of length 1 that provides the start year
#'   of the shock to be used in the analysis.
generate_transition_shocks <- function(start_of_analysis,
                                       end_of_analysis,
                                       shock_year) {
  bounds <- list(start_of_analysis, end_of_analysis)

  if (dplyr::n_distinct(purrr::map_int(bounds, length)) > 1) {
    stop("Input arugments for start_of_analysis and end_of_analysis need to have length 1.")
  }

  input_args <- list(start_of_analysis, end_of_analysis, shock_year)

  if (!all(unique(purrr::map_lgl(input_args, is.numeric)))) {
    stop("All input arguments need to be numeric.")
  }

  if (shock_year < start_of_analysis) {
    stop("Year of shock out of bounds. Shock cannot happen before the start year
         of the anaylsis.")
  }

  if (shock_year > end_of_analysis) {
    stop("Year of shock out of bounds. Shock cannot happen after the end year of
         the anaylsis.")
  }

  data <- tibble::tibble(
    year_of_shock = shock_year,
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

#' Generate litigation scenario shock from a start year that represents when a
#' litigation settlement is reached.
#'
#' @param start_of_analysis A numeric vector of length one that indicates the
#'   start year of the analysis.
#' @param end_of_analysis A numeric vector of length one that indicates the
#'   end year of the analysis.
#' @param shock_year A numeric vector of length 1 that provides the start year
#'   of the shock to be used in the analysis.
#' @param scc A numeric vector of length 1 that provides the social cost of
#'   carbon
#' @param exp_share_damages_paid A numeric vector of length 1 that provides the
#'   share of the liability expected to be paid out in a settlement
generate_litigation_shocks <- function(start_of_analysis,
                                       end_of_analysis,
                                       shock_year,
                                       scc,
                                       exp_share_damages_paid) {
  bounds <- list(start_of_analysis, end_of_analysis)

  if (dplyr::n_distinct(purrr::map_int(bounds, length)) > 1) {
    stop("Input arugments for start_of_analysis and end_of_analysis need to have length 1.")
  }

  input_args <- list(start_of_analysis, end_of_analysis, shock_year)

  if (!all(unique(purrr::map_lgl(input_args, is.numeric)))) {
    stop("All input arguments need to be numeric.")
  }

  if (shock_year < start_of_analysis) {
    stop("Year of shock out of bounds. Shock cannot happen before the start year
         of the anaylsis.")
  }

  if (shock_year > end_of_analysis) {
    stop("Year of shock out of bounds. Shock cannot happen after the end year of
         the anaylsis.")
  }

  # TODO: ideally remove share and scc and read from run_lrisk params
  data <- tibble::tibble(
    year_of_shock = shock_year,
    scenario_name = glue::glue("SCC_{year_of_shock}"),
    duration_of_shock = end_of_analysis - year_of_shock + 1,
    scc = scc,
    exp_share_damages_paid = exp_share_damages_paid
  )

  validate_data_has_expected_cols(
    data = data,
    expected_columns = c(
      "scenario_name", "year_of_shock", "duration_of_shock", "scc",
      "exp_share_damages_paid"
    )
  )

  return(data)
}
