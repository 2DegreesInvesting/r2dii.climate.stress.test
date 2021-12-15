#' Check and filter data
#'
#' Function filters dataset so that only rows relevant for analysis remain. Also
#' an error is thrown if duplicate entries are detected.
#'
#' Note that dataset `sector_exposures` is not filtered for considered sectors
#' as data is used in [calculate_aum()] where the asset under management of the
#' entire portfolio is calculated.
#'
#' @param st_data_list A list holding imported and prewrangled stress test input
#'   data.
#' @param start_year Numeric holding start year of analysis.
#' @param end_year Numeric holding end year of analysis.
#' @param scenarios_filter String vector holding name of baseline and shock
#'   scenario.
#' @param scenario_geography_filter String holding name of considered
#'   geographical regions.
#'
#' @return List `st_data_list` with tibbles subset to rows required for
#'   analysis.
check_and_filter_data <- function(st_data_list, start_year, end_year,
                                  scenarios_filter, scenario_geography_filter) {


  data_list <- list(
    financial_data = st_data_list$financial_data
  )

  cuc_list <- list(
    c("company_name", "company_id")
  )

  mapply(
    function(data, cuc_cols) {
      if (!is.null(data)) {
        report_all_duplicate_kinds(data = data, composite_unique_cols = cuc_cols)
      }
    },
    data_list, cuc_list
  )

  return(data_list)
}

#' Check that required technologies are provided
#'
#' @param data A tibble holding at least column `technology`
#' @inheritParams read_price_data
#'
#' @return Returns `data` invisibly.
check_technology_availability <- function(data, expected_technologies) {
  if (!all(expected_technologies %in% unique(data$technology))) {
    missing_technologies <- paste0(setdiff(expected_technologies, unique(data$technology)), collapse = ", ")
    rlang::abort(c(
      "Data must hold all expected technologies.",
      x = glue::glue("Missing technologies: {missing_technologies}."),
      i = "Please check input data."
    ))
  }

  return(invisible(data))
}
