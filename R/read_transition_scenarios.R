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

  valid_input_file_path <- file.exists(file.path(path))
  stopifnot(valid_input_file_path)

  data <- readr::read_csv(
    path,
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
    # TODO: reintroduce write_log, once it is exported
    # write_log("Year of shock out of bounds. Shock cannot happen before the start
    #           year of the anaylsis.")
    stop("Year of shock out of bounds. Shock cannot happen before the start year
         of the anaylsis.")
  }

  if (max_year_of_shock > end_of_analysis) {
    # TODO: reintroduce write_log, once it is exported
    # write_log("Year of shock out of bounds. Shock cannot happen after the end
    #           year of the anaylsis.")
    stop("Year of shock out of bounds. Shock cannot happen after the end year of
         the anaylsis.")
  }

  output_has_expected_columns <- all(
    c(
      "scenario_name", "year_of_shock", "overshoot_method", "duration_of_shock",
      "use_prod_forecasts_ls", "use_prod_forecasts_baseline"
    ) %in% colnames(data)
  )
  stopifnot(output_has_expected_columns)

  return(data)
}
