#' Write stress test reports to output dir
#'
#' Stress test results are wrangled and exported to the output dir.
#'
#' @param results Tibble holding stress test results.
#' @param expected_loss Tibble holding stress test results on expected loss.
#' @param annual_pd_changes_sector Tibble holding stress test results on annual changes
#'   of probability of default.
#' @param overall_pd_changes_sector Tibble holding stress test results on overall
#'   changes of probability of default.
#' @param asset_type String holding asset type.
#' @param calculation_level String holding calculation level.
#' @param sensitivity_analysis_vars String vector holding names of iteration
#'   arguments.
#' @param iter_var String holding name of iteration variable.
#' @param output_path String holding path to output dir.
#'
#' @return NULL
write_stress_test_results <- function(results, expected_loss,
                                      annual_pd_changes_sector, overall_pd_changes_sector,
                                      asset_type, calculation_level,
                                      sensitivity_analysis_vars, iter_var,
                                      output_path) {

  sensitivity_analysis_vars <- paste0(sensitivity_analysis_vars, "_arg")

  results %>%
    write_results_new(
      path_to_results = output_path,
      asset_type = asset_type,
      level = calculation_level,
      file_type = "csv",
      sensitivity_analysis_vars = sensitivity_analysis_vars,
      iter_var = iter_var
    )

  expected_loss %>%
    report_missings(
      name_data = "Expected loss"
    ) %>%
    report_all_duplicate_kinds(
      composite_unique_cols = c(
        "scenario_name", "scenario_geography", "investor_name", "portfolio_name",
        "company_name", "ald_sector",
        sensitivity_analysis_vars
      )
    ) %>%
    readr::write_csv(file.path(
      output_path,
      paste0("stress_test_results_", asset_type, "_comp_el_", iter_var,".csv")
    ))

  annual_pd_changes_sector %>%
    report_missings(
      name_data = "Annual PD changes sector"
    ) %>%
    report_all_duplicate_kinds(
      composite_unique_cols = c(
        "scenario_name", "scenario_geography", "investor_name", "portfolio_name",
        "ald_sector", "year", sensitivity_analysis_vars
      )
    ) %>%
    readr::write_csv(file.path(
      output_path,
      paste0("stress_test_results_", asset_type, "_sector_pd_changes_annual_", iter_var, ".csv")
    ))

  overall_pd_changes_sector %>%
    report_missings(
      name_data = "Overall PD changes sector"
    ) %>%
    report_all_duplicate_kinds(
      composite_unique_cols = c(
        "scenario_name", "scenario_geography", "investor_name", "portfolio_name",
        "ald_sector", "term", sensitivity_analysis_vars
      )
    ) %>%
    readr::write_csv(file.path(
      output_path,
      paste0("stress_test_results_", asset_type, "_sector_pd_changes_overall_", iter_var, ".csv")
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
#'
#' @export
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
    expected_columns <-  c(
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

#' Write the results to the project directory
#'
#' Unlike [write_results()] as used in webscript function only allows level
#' company and does not create a new directory level by `investorname`.
#'
#' @param data A dataframe the results of the stress test for one asset type
#' @param path_to_results Character. A string that contains the path that the
#'   result should be written to.
#' @param asset_type Character. A vector of length one indicating which type of
#'   asset the results belong to. Currently supports "equity" and "bonds".
#' @param level Character. A vector of length one indicating which type of output
#'   format to use. This depends on whether portfolio or company level pacta
#'   data were used as input to the analysis. Defaults to the level indicated
#'   in the work flow. Supports "company" and "portfolio".
#' @param file_type Character. A string containing the type of file that should
#'   be written to the result path. Currently supports "csv" and "rda".
#' @inheritParams write_stress_test_results
#'
#' @family output functions
#' @return NULL
write_results_new <- function(data,
                              path_to_results = NULL,
                              asset_type = NULL,
                              level = NULL,
                              file_type = NULL,
                              sensitivity_analysis_vars,
                              iter_var) {

  path_to_results %||% stop("Must provide 'path_to_results'")
  level %||% stop("Must provide 'level'")

  valid_asset_type <- asset_type %in% c("equity", "bonds", "loans")
  stopifnot(valid_asset_type)

  valid_file_type <- file_type %in% c("csv", "rda")
  stopifnot(valid_file_type)

  if (level != "company") {
    stop("Only calculation level company is supported.")
  }

  validate_data_has_expected_cols(
    data = data,
    expected_columns = c(
      "investor_name", "portfolio_name", "company_name", "scenario_geography",
      "scenario_name", "year_of_shock", "duration_of_shock", "ald_sector",
      "technology", "production_shock_perc", "asset_portfolio_value",
      "tech_company_exposure", "VaR_tech_company", "tech_company_value_change",
      "company_exposure", "VaR_company", "company_value_change",
      "technology_exposure", "VaR_technology", "technology_value_change",
      "sector_exposure", "VaR_sector", "sector_value_change",
      "analysed_sectors_exposure", "VaR_analysed_sectors",
      "analysed_sectors_value_change", "portfolio_aum",
      "portfolio_value_change_perc", "portfolio_value_change", sensitivity_analysis_vars
    )
  )

  data <- data %>%
    # ADO 2549 - select instead of relocate so that no surplus columns can sneak in
    dplyr::select(
      .data$investor_name, .data$portfolio_name, .data$company_name,
      .data$scenario_geography, .data$scenario_name, .data$year_of_shock,
      .data$duration_of_shock, .data$ald_sector, .data$technology,
      .data$production_shock_perc, .data$asset_portfolio_value,
      .data$tech_company_exposure, .data$VaR_tech_company,
      .data$tech_company_value_change, .data$company_exposure,
      .data$VaR_company, .data$company_value_change, .data$technology_exposure,
      .data$VaR_technology, .data$technology_value_change,
      .data$sector_exposure, .data$VaR_sector, .data$sector_value_change,
      .data$analysed_sectors_exposure, .data$VaR_analysed_sectors,
      .data$analysed_sectors_value_change, .data$portfolio_aum,
      .data$portfolio_value_change_perc, .data$portfolio_value_change,
      .data$exclude, !!!rlang::syms(sensitivity_analysis_vars)
    ) %>%
    report_missings(
      name_data = "Stress test results - Company level"
    ) %>%
    report_all_duplicate_kinds(
      composite_unique_cols = c(
        "investor_name", "portfolio_name", "company_name", "scenario_geography",
        "scenario_name", "year_of_shock", "duration_of_shock", "ald_sector", "technology",
        sensitivity_analysis_vars
      )
    )

  switch(file_type,
    csv = data %>%
      readr::write_csv(file.path(
        path_to_results,
        glue::glue("stress_test_results_{asset_type}_comp_{iter_var}.csv")
      )),
    rda = data %>%
      saveRDS(file.path(
        path_to_results,
        glue::glue("stress_test_results_{asset_type}_comp_{iter_var}.rda")
      )),
    stop("Invalid file_type provided. Please use csv or rda!")
  )

  data <- data %>%
    # ADO 2549 - actively select all columns that should remain in the portfolio
    # level results, rather than unselecting some. This avoids extra columns.
    dplyr::select(
      .data$investor_name, .data$portfolio_name, .data$scenario_geography,
      .data$scenario_name, .data$year_of_shock, .data$duration_of_shock,
      .data$ald_sector, .data$technology, .data$production_shock_perc,
      .data$asset_portfolio_value, .data$technology_exposure,
      .data$VaR_technology, .data$technology_value_change,
      .data$sector_exposure, .data$VaR_sector, .data$sector_value_change,
      .data$analysed_sectors_exposure, .data$VaR_analysed_sectors,
      .data$analysed_sectors_value_change, .data$portfolio_aum,
      .data$portfolio_value_change_perc, .data$portfolio_value_change,
      !!!rlang::syms(sensitivity_analysis_vars)
    ) %>%
    # ADO 2549 - all numeric variables should be unique across the CUC variables
    # running distinct all and the check afterwards ensures this is the case
    dplyr::distinct_all() %>%
    dplyr::arrange(.data$year_of_shock, .data$ald_sector, .data$technology) %>%
    report_missings(
      name_data = "Stress test results - Portfolios level"
    ) %>%
    report_all_duplicate_kinds(
      composite_unique_cols = c(
        "investor_name", "portfolio_name", "scenario_geography", "scenario_name",
        "year_of_shock", "duration_of_shock", "ald_sector", "technology",
        sensitivity_analysis_vars
      )
    )

  switch(file_type,
    csv = data %>%
      readr::write_csv(file.path(
        path_to_results,
        glue::glue("stress_test_results_{asset_type}_port_{iter_var}.csv")
      )),
    rda = data %>%
      saveRDS(file.path(
        path_to_results,
        glue::glue("stress_test_results_{asset_type}_port_{iter_var}.rda")
      )),
    stop("Invalid file_type provided. Please use csv or rda!")
  )
}
