#' Write stress test reports to output dir
#'
#' Stress test results are  exported to the output dir.
#'
#' @param results_list A list of st results.
#' @param asset_type String holding asset type.
#' @param iter_var String holding name of iteration variable.
#' @param output_path String holding path to output dir.
#'
#' @return NULL
write_stress_test_results <- function(results_list, asset_type, iter_var,
                                      output_path) {
  results_list$company_value_changes %>%
    readr::write_csv(file.path(
      output_path,
      glue::glue("{asset_type}_company_value_changes_{iter_var}.csv")
    ))

  results_list$portfolio_value_changes %>%
    readr::write_csv(file.path(
      output_path,
      glue::glue("{asset_type}_portfolio_value_changes_{iter_var}.csv")
    ))

  results_list$company_expected_loss %>%
    readr::write_csv(file.path(
      output_path,
      glue::glue("{asset_type}_company_expected_loss_{iter_var}.csv")
    ))

  results_list$company_pd_changes_annual %>%
    readr::write_csv(file.path(
      output_path,
      glue::glue("{asset_type}_company_pd_changes_annual_{iter_var}.csv")
    ))

  results_list$sector_pd_changes_annual %>%
    readr::write_csv(file.path(
      output_path,
      glue::glue("{asset_type}_sector_pd_changes_annual_{iter_var}.csv")
    ))

  results_list$company_pd_changes_overall %>%
    readr::write_csv(file.path(
      output_path,
      glue::glue("{asset_type}_company_pd_changes_overall_{iter_var}.csv")
    ))

  results_list$sector_pd_changes_overall %>%
    readr::write_csv(file.path(
      output_path,
      glue::glue("{asset_type}_sector_pd_changes_overall_{iter_var}.csv")
    ))

  results_list$company_trajectories %>%
    readr::write_csv(file.path(
      output_path,
      glue::glue("{asset_type}_company_trajectories_{iter_var}.csv")
    ))
}


#' Write the results to the project directory
#'
#' @param data A dataframe the results of the stress test for one asset type
#' @param path_to_results Character. A string that contains the path that the
#'   result should be written to.
#' @param investorname Character. A string containing the name of the investor
#'   for which the analysis was run. Is used as part of the file name.
#' @param asset_type Character. A vector of length one indicating which type of
#'   asset the results belong to. Currently supports "equity" and "bonds".
#' @param level Character. A vector of length one indicating which type of output
#'   format to use. This depends on whether portfolio or company level pacta
#'   data were used as input to the analysis. Defaults to the level indicated
#'   in the work flow. Supports "company" and "portfolio".
#' @param file_type Character. A string containing the type of file that should
#'   be written to the result path. Currently supports "csv" and "rda".
#'
#' @family output functions
write_results <- function(data,
                          path_to_results = NULL,
                          investorname = NULL,
                          asset_type = NULL,
                          level = NULL,
                          file_type = NULL) {
  path_to_results %||% stop("Must provide 'path_to_results'")
  level %||% stop("Must provide 'level'")
  investorname %||% stop("Must provide 'investorname'")

  valid_asset_type <- asset_type %in% c("equity", "bonds", "loans")
  stopifnot(valid_asset_type)

  valid_file_type <- file_type %in% c("csv", "rda")
  stopifnot(valid_file_type)

  if (level == "company") {
    expected_columns <- c(
      "investor_name", "portfolio_name", "company_name", "scenario_geography",
      "scenario_name", "year_of_shock", "duration_of_shock", "ald_sector",
      "technology", "production_shock_perc", "asset_portfolio_value",
      "tech_company_exposure", "VaR_tech_company", "tech_company_value_change",
      "company_exposure", "VaR_company", "company_value_change",
      "technology_exposure", "VaR_technology", "technology_value_change",
      "sector_exposure", "VaR_sector", "sector_value_change",
      "analysed_sectors_exposure", "VaR_analysed_sectors",
      "analysed_sectors_value_change", "portfolio_aum",
      "portfolio_value_change_perc", "portfolio_value_change"
    )
  } else {
    expected_columns <- c(
      "investor_name", "portfolio_name", "ald_sector", "technology",
      "scenario_geography", "VaR_technology", "asset_portfolio_value",
      "VaR_sector", "scenario_name", "technology_exposure", "sector_exposure",
      "sector_loss", "climate_relevant_var", "portfolio_aum",
      "portfolio_loss_perc", "year_of_shock", "duration_of_shock",
      "production_shock_perc"
    )
  }
  validate_data_has_expected_cols(
    data = data,
    expected_columns = expected_columns
  )

  if (level == "portfolio") {
    switch(file_type,
      csv = data %>%
        readr::write_csv(file.path(
          path_to_results,
          investorname,
          glue::glue("{asset_type}_results_stress_test.csv")
        )),
      rda = data %>%
        saveRDS(file.path(
          path_to_results,
          investorname,
          glue::glue("{asset_type}_results_stress_test.rda")
        )),
      stop("Invalid file_type provided. Please use csv or rda!")
    )
  } else {
    data <- data %>%
      dplyr::relocate(
        .data$investor_name, .data$portfolio_name, .data$company_name,
        .data$scenario_geography, .data$scenario_name, .data$year_of_shock,
        .data$duration_of_shock, .data$ald_sector, .data$technology,
        .data$production_shock_perc, .data$asset_portfolio_value,
        .data$tech_company_exposure, .data$VaR_tech_company,
        .data$tech_company_value_change, .data$company_exposure,
        .data$VaR_company, .data$company_value_change, .data$technology_exposure,
        .data$VaR_technology, .data$technology_value_change,
        .data$technology_exposure, .data$VaR_technology,
        .data$technology_value_change, .data$sector_exposure, .data$VaR_sector,
        .data$sector_value_change, .data$analysed_sectors_exposure,
        .data$VaR_analysed_sectors, .data$analysed_sectors_value_change,
        .data$portfolio_aum, .data$portfolio_value_change_perc,
        .data$portfolio_value_change
      )

    switch(file_type,
      csv = data %>%
        readr::write_csv(file.path(
          path_to_results,
          investorname,
          glue::glue("stress_test_results_{asset_type}_comp.csv")
        )),
      rda = data %>%
        saveRDS(file.path(
          path_to_results,
          investorname,
          glue::glue("stress_test_results_{asset_type}_comp.rda")
        )),
      stop("Invalid file_type provided. Please use csv or rda!")
    )

    data <- data %>%
      dplyr::select(
        -c(
          .data$company_name,
          .data$VaR_tech_company,
          .data$tech_company_exposure,
          .data$tech_company_value_change,
          .data$VaR_company,
          .data$company_exposure,
          .data$company_value_change
        )
      ) %>%
      # ADO 2393 - after removing the tech_company & company level columns,
      # we run distinct_all to get unique values on the previously duplicated
      # technology and sector levels
      dplyr::distinct_all() %>%
      dplyr::relocate(
        .data$investor_name, .data$portfolio_name, .data$scenario_geography,
        .data$scenario_name, .data$year_of_shock, .data$duration_of_shock,
        .data$ald_sector, .data$technology, .data$production_shock_perc,
        .data$asset_portfolio_value, .data$technology_exposure,
        .data$VaR_technology, .data$technology_value_change,
        .data$sector_exposure, .data$VaR_sector, .data$sector_value_change,
        .data$analysed_sectors_exposure, .data$VaR_analysed_sectors,
        .data$analysed_sectors_value_change, .data$portfolio_aum,
        .data$portfolio_value_change_perc, .data$portfolio_value_change
      ) %>%
      dplyr::arrange(.data$year_of_shock, .data$ald_sector, .data$technology) %>%
      report_all_duplicate_kinds(
        composite_unique_cols = c(
          "investor_name", "portfolio_name", "scenario_geography", "scenario_name",
          "year_of_shock", "duration_of_shock", "ald_sector", "technology"
        )
      )

    switch(file_type,
      csv = data %>%
        readr::write_csv(file.path(
          path_to_results,
          investorname,
          glue::glue("stress_test_results_{asset_type}_port.csv")
        )),
      rda = data %>%
        saveRDS(file.path(
          path_to_results,
          investorname,
          glue::glue("stress_test_results_{asset_type}_port.rda")
        )),
      stop("Invalid file_type provided. Please use csv or rda!")
    )
  }
}
