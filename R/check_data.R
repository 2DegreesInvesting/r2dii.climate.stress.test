#' Check that required technologies are provided
#'
#' @param data A tibble holding at least column `technology`
#' @param expected_technologies String vector holding names of expected
#'   technologies.
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

#' Check that required levels of a variable are available in data
#'
#' @param data A tibble.
#' @param data_name Character, holding name of dataset.
#' @param expected_levels_list List holding variable names along with their
#'   expected levels. Variables must be present in data.
#' @param throw_error Boolean indicating if to throw error. If FALSE warning is
#'   signaled instead.
#'
#' @return Returns `data` invisibly.
check_level_availability <- function(data, data_name, expected_levels_list, throw_error = TRUE) {
  purrr::iwalk(expected_levels_list, function(levels, var) {
    if (!all(levels %in% unique(get(var, data)))) {
      expected_levels_collapsed <- paste0(levels, collapse = ", ")
      provided_levels_collapsed <- paste0(unique(get(var, data)), collapse = ", ")
      if (throw_error) {
        rlang::abort(c(
          glue::glue("Dataset {data_name} must hold all expected levels on variable {var}."),
          x = glue::glue("Expected levels are {expected_levels_collapsed}, provided levels are {provided_levels_collapsed}."),
          i = "Please check input data."
        ))
      } else {
        rlang::warn(c(
          glue::glue("Dataset {data_name} does not hold all expected levels on variable {var}."),
          x = glue::glue("Expected levels are {expected_levels_collapsed}, provided levels are {provided_levels_collapsed}."),
          i = "Please check input data if this does not match you expectations."
        ))

      }
    }
  })

  return(invisible(data))
}

#' Check that sector to technology mapping corresponds to template
#'
#' @param data A tibble.
#' @param sector_col Name of column holding sector (needed due to naming
#'   inconsistencies.)
#' @param mapper_template A tibble holding, among others, mapping of sectors to
#'   technologies.
#'
#' @return Returns `data` invisibly.
check_sector_tech_mapping <- function(data, sector_col = "ald_sector",
                                      mapper_template = p4i_p4b_sector_technology_lookup) {
  sector_tech_mapper <- mapper_template %>%
    dplyr::select(.data$sector_p4i, .data$technology_p4i) %>%
    dplyr::rename(ald_sector = .data$sector_p4i, technology = .data$technology_p4i) %>%
    dplyr::filter(.data$ald_sector %in% unique(get(sector_col, data)))

  additional_sector_tech_combinations <- data %>%
    dplyr::rename(ald_sector = rlang::sym(sector_col)) %>%
    dplyr::anti_join(sector_tech_mapper, by = c("ald_sector", "technology"))

  missing_sector_tech_combinations <- sector_tech_mapper %>%
    dplyr::anti_join(data %>%
      dplyr::rename(ald_sector = rlang::sym(sector_col)), by = c("ald_sector", "technology"))

  if (nrow(additional_sector_tech_combinations) > 0 | nrow(missing_sector_tech_combinations) > 0) {
    stop("Incorrect mapping of sectors to technologies indentified in data, please check.")
  }

  return(invisible(data))
}

#' Check mapping of companies to corporate bond tickers
#'
#' There needs to be an n x 1 mapping between companies and
#' corporate bond tickers. Function verifies this is the case by checking that
#' companies do not map to > 1 corporate bond ticker.
#'
#' @param data A tibble holding at least columns `company_name` and
#'   `corporate_bond_ticker`.
#'
#' @return Returns data invisibly
check_company_ticker_mapping <- function(data) {
  companies_with_several_tickers <- data %>%
    dplyr::group_by(company_name) %>%
    dplyr::filter(dplyr::n() > 1)

  if (nrow(companies_with_several_tickers) > 0) {
    stop("Identified companies with > 1 corporate_bond_ticker.")
  }
  return(invisible(data))
}

#' Check expected missing patterns
#'
#' On certain variables missings appear that are not problematic but are
#' expected. Here it is checked whether those missings adhere to expected
#' patterns.
#'
#' @param data Data that expected missings are checked on.
#'
#' @return Returns input invisibly.
check_expected_missings <- function(data) {
  n_missings_production_plan_company_technology <- sum(is.na(data$production_plan_company_technology))
  expected_n_missings_production_plan_company_technology <- as.integer(nrow(data) * (length(unique(data$year)) - (time_horizon_lookup + 1)) / length(unique(data$year)))

  if (n_missings_production_plan_company_technology != expected_n_missings_production_plan_company_technology) {
    cat("-- Company Trajectories: Detected unexpected missings on variable production_plan_company_technology. \n")
  }

  n_missings_production_change_target_scenario <- sum(is.na(data$production_change_target_scenario))
  expected_n_missings_production_change_target_scenario <- as.integer(nrow(data) / length(unique(data$year)))

  if (n_missings_production_change_target_scenario != expected_n_missings_production_change_target_scenario) {
    cat("-- Company Trajectories: Detected unexpected missings on variable production_change_target_scenario. \n")
  }

  return(invisible(data))
}
