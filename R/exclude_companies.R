#' Effectively exclude certain technologies of companies from calculations
#'
#' NOTE: If `exclusion` is NULL `data` are returned.
#'
#' @param data A dataframe of the annual profits by company-tech for one asset
#'   type.
#' @param exclusion A dataframe with two character columns, "company_name" and
#'   "technology", that lists which technologies from which companies should be
#'   set to 0 in the remainder of the analysis.
#' @param scenario_baseline Character. String that defines which scenario is
#'   considered baseline. Usually this should be the same as throughout the
#'   workflow.
#' @param scenario_ls Character. String that defines which scenario is followed
#'   in late and sudden.  Usually this should be the same as throughout the
#'   workflow.
#'
#' @family processing functions
#'
#' @export
exclude_companies <- function(data,
                              exclusion = NULL,
                              scenario_baseline = NULL,
                              scenario_ls = NULL) {
  if (is.null(exclusion)) {
    return(data)
  }

  force(data)
  scenario_baseline %||% stop("Must provide input for 'scenario_baseline'", call. = FALSE)
  scenario_ls %||% stop("Must provide input for 'scenario_ls'", call. = FALSE)

  validate_data_has_expected_cols(
    data = data,
    expected_columns = c(
      "investor_name", "portfolio_name", "id", "company_name", "year",
      "scenario_geography", "ald_sector", "technology",
      "plan_tech_prod", scenario_baseline,
      scenario_ls, "baseline", "scenario_name",
      "scen_to_follow_aligned", "late_sudden", "scenario_change_aligned"
    )
  )

  validate_data_has_expected_cols(
    data = exclusion,
    expected_columns = c(
      "company_name", "technology"
    )
  )

  exclusion <- exclusion %>%
    dplyr::mutate(exclude = TRUE)

  data <- data %>%
    # ADO 1945: we opt for a left join and a flag rather than an anti join so
    # that we know which values were removed in the final outcome. This
    # mechanism should be revisited as part of an overhaul of the compensation.
    dplyr::left_join(exclusion, by = c("company_name", "technology")) %>%
    dplyr::mutate(
      exclude = dplyr::if_else(
        is.na(.data$exclude),
        FALSE,
        .data$exclude
      )
    )

  data <- data %>%
    dplyr::group_by(.data$company_name, .data$technology) %>%
    dplyr::mutate(
      baseline = dplyr::if_else(
        .data$exclude == TRUE,
        0,
        .data$baseline
      )
    ) %>%
    dplyr::mutate(
      late_sudden = dplyr::if_else(
        .data$exclude == TRUE,
        0,
        .data$late_sudden
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data$exclude)

  return(data)
}
