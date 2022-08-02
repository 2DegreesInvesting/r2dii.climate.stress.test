#' Run stress testing data preparation for loans
#'
#' @inheritParams run_trisk
#' @param data_prep_output_path Path where results are written. NOTE: This is a
#'   workflow that is needed exclusively in preparation of [run_trisk()] for
#'   loan books. It creates input data for the stress test. A recommended
#'   setting is to set `data_prep_output_path` to to the same path as
#'   `input_path_project_specific`.
#' @param credit_type Type of credit. For accepted values please compare
#'   `credit_type_lookup`.
#'
#' @return NULL
#' @export
run_prep_calculation_loans <- function(input_path_project_specific,
                                       input_path_project_agnostic,
                                       data_prep_output_path,
                                       credit_type = "outstanding") {
  cat("-- Running data preparation. \n")

  #### Validate input-----------------------------------------------------------

  if (!credit_type %in% credit_type_lookup) {
    stop("Argument credit_type does not hold an accepted value.")
  }

  #### Load input data sets-----------------------------------------------------
  # raw loan book
  loanbook <- validate_file_exists(file.path(input_path_project_specific, "raw_loanbook.csv")) %>%
    readr::read_csv(
      col_types = readr::cols(
        id_loan = "c",
        id_direct_loantaker = "c",
        name_direct_loantaker = "c",
        id_intermediate_parent_1 = "c",
        name_intermediate_parent_1 = "c",
        id_ultimate_parent = "c",
        name_ultimate_parent = "c",
        loan_size_outstanding = "d",
        loan_size_outstanding_currency = "c",
        loan_size_credit_limit = "d",
        loan_size_credit_limit_currency = "c",
        sector_classification_system = "c",
        sector_classification_input_type = "c",
        sector_classification_direct_loantaker = "c",
        fi_type = "c",
        flag_project_finance_loan = "c",
        name_project = "c",
        lei_direct_loantaker = "c",
        isin_direct_loantaker = "c"
      )
    )

  # matched loan book
  matched <- validate_file_exists(file.path(input_path_project_specific, "matched_loan_book.csv")) %>%
    readr::read_csv(
      col_types = readr::cols_only(
        id_loan = "c",
        id_direct_loantaker = "c",
        name_direct_loantaker = "c",
        id_intermediate_parent_1 = "c",
        name_intermediate_parent_1 = "c",
        id_ultimate_parent = "c",
        name_ultimate_parent = "c",
        loan_size_outstanding = "d",
        loan_size_outstanding_currency = "c",
        loan_size_credit_limit = "d",
        loan_size_credit_limit_currency = "c",
        sector_classification_system = "c",
        sector_classification_input_type = "c",
        sector_classification_direct_loantaker = "c",
        fi_type = "c",
        flag_project_finance_loan = "c",
        name_project = "c",
        lei_direct_loantaker = "c",
        isin_direct_loantaker = "c",
        id_2dii = "c",
        level = "c",
        sector = "c",
        sector_abcd = "c",
        name = "c",
        name_abcd = "c",
        score = "d",
        source = "c",
        borderline = "l"
      )
    )

  # region iso mapping
  regions <- r2dii.data::region_isos %>%
    # FIXME: remove hardcoded rowbindind and removal of duplicates when countries
    # are included in CRAN version of r2dii.data
    dplyr::bind_rows(countries_for_regions_mapper_lookup) %>%
    dplyr::distinct_all()

  # Production forecast data
  production_forecast_data <- validate_file_exists(file.path(input_path_project_agnostic, "2022-04-07_AR_2021Q4_Free Dataset-PACTA for Banks-Equity Ownership Consolidation.xlsx")) %>%
    readxl::read_xlsx(
      sheet = "Company Indicators - PACTA Comp"
    )

  # AR PAMS data for emission factors
  ar_pams <- validate_file_exists(file.path(input_path_project_agnostic, "2022-02-17_AR_2021Q4_2DII-PAMS-Data.xlsx")) %>%
    readxl::read_xlsx(sheet = "Company Indicators")

  # Scenario data - market share
  scenario_data_market_share <- validate_file_exists(file.path(input_path_project_agnostic, "scenario_2021.csv")) %>%
    readr::read_csv(
      col_types = readr::cols(
        scenario_source = "c",
        scenario = "c",
        sector = "c",
        technology = "c",
        region = "c",
        year = "d",
        tmsr = "d",
        smsp = "d"
      )
    )

  #### Wrangle and prepare data-------------------------------------------------
  # ADO 2690 - remove rows with negative loan values (not allowed in P4B)
  if (credit_type == "outstanding") {
    matched_non_negative <- matched %>%
      dplyr::filter(.data$loan_size_outstanding >= 0)
  } else {
    matched_non_negative <- matched %>%
      dplyr::filter(.data$loan_size_credit_limit >= 0)
  }

  if (nrow(matched_non_negative) < nrow(matched)) {
    warning(
      paste0(
        nrow(matched) - nrow(matched_non_negative),
        " loans removed from the matched loan book because of negative loan
        values. Please check the input loan book to address this issue."
      ),
      call. = FALSE
    )
  }

  portfolio_size <- loanbook %>%
    dplyr::summarise(
      portfolio_loan_size_outstanding = sum(.data$loan_size_outstanding, na.rm = TRUE),
      portfolio_loan_size_credit_limit = sum(.data$loan_size_credit_limit, na.rm = TRUE),
      .groups = "drop"
    )

  matched_portfolio_size <- matched_non_negative %>%
    dplyr::summarise(
      matched_portfolio_loan_size_outstanding = sum(.data$loan_size_outstanding, na.rm = TRUE),
      matched_portfolio_loan_size_credit_limit = sum(.data$loan_size_credit_limit, na.rm = TRUE),
      .groups = "drop"
    )

  #### Calculate loan-tech level loan book size and value share-----------------
  loan_share <- matched_non_negative %>%
    dplyr::group_by(
      # ADO 1933 - we choose `name_abcd` as this is an internal name that can be
      # joined with other 2dii data later on. This is not the case for `name`.
      .data$name_abcd, .data$sector_abcd, .data$loan_size_outstanding_currency,
      .data$loan_size_credit_limit_currency
    ) %>%
    # ADO 2723 - loan shares calculated against matched loan book, not total loan book
    # this is to ensure all scaling happens against the same denominator
    # run_trisk uses the matched portfolio to scale the overall impact
    dplyr::summarise(
      comp_loan_share_outstanding = sum(.data$loan_size_outstanding, na.rm = TRUE) / matched_portfolio_size$matched_portfolio_loan_size_outstanding,
      comp_loan_size_outstanding = sum(.data$loan_size_outstanding, na.rm = TRUE),
      comp_loan_share_credit_limit = sum(.data$loan_size_credit_limit, na.rm = TRUE) / matched_portfolio_size$matched_portfolio_loan_size_credit_limit,
      comp_loan_size_credit_limit = sum(.data$loan_size_credit_limit, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      .data$name_abcd,
      .data$sector_abcd,
      .data$comp_loan_share_outstanding,
      .data$comp_loan_size_outstanding,
      .data$loan_size_outstanding_currency,
      .data$comp_loan_share_credit_limit,
      .data$comp_loan_size_credit_limit,
      .data$loan_size_credit_limit_currency
    )

  #### Calculate sector level loan book size and value share--------------------

  sector_share <- matched_non_negative %>%
    dplyr::group_by(
      .data$sector_abcd,
      .data$loan_size_outstanding_currency,
      .data$loan_size_credit_limit_currency
    ) %>%
    dplyr::summarise(
      sector_loan_share_outstanding = sum(.data$loan_size_outstanding, na.rm = TRUE) / portfolio_size$portfolio_loan_size_outstanding,
      sector_loan_size_outstanding = sum(.data$loan_size_outstanding, na.rm = TRUE),
      sector_loan_share_credit_limit = sum(.data$loan_size_credit_limit, na.rm = TRUE) / portfolio_size$portfolio_loan_size_credit_limit,
      sector_loan_size_credit_limit = sum(.data$loan_size_credit_limit, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::relocate(
      .data$sector_abcd,
      .data$sector_loan_share_outstanding,
      .data$sector_loan_size_outstanding,
      .data$loan_size_outstanding_currency,
      .data$sector_loan_share_credit_limit,
      .data$sector_loan_size_credit_limit,
      .data$loan_size_credit_limit_currency
    )

  #### Wrangle portfolio overview to P4I format---------------------------------
  cat("-- Preparing portfolio overview. \n")

  sector_credit_type <- glue::glue("sector_loan_size_{credit_type}")
  credit_currency <- glue::glue("loan_size_{credit_type}_currency")

  portfolio_overview <- sector_share %>%
    dplyr::inner_join(
      # ADO 2393 - use distinct to only map sectors, not technlogies
      p4i_p4b_sector_technology_lookup %>% dplyr::distinct(.data$sector_p4b, .data$sector_p4i),
      by = c("sector_abcd" = "sector_p4b")
    ) %>%
    dplyr::mutate(sector_abcd = .data$sector_p4i) %>%
    dplyr::select(
      .data$sector_abcd,
      !!rlang::sym(sector_credit_type),
      !!rlang::sym(credit_currency)
    ) %>%
    dplyr::rename(
      financial_sector = .data$sector_abcd,
      valid_value_usd = !!rlang::sym(sector_credit_type),
      currency = !!rlang::sym(credit_currency)
    ) %>%
    dplyr::mutate(
      investor_name = investor_name_placeholder,
      portfolio_name = investor_name_placeholder,
      asset_type = "Loans",
      # ADO 1933 - all remaining value can be assumed valid. Possibly add everything that has been removed as invalid.
      valid_input = TRUE
    ) %>%
    dplyr::group_by(.data$investor_name, .data$portfolio_name, .data$asset_type, .data$valid_input) %>%
    dplyr::mutate(asset_value_usd = sum(.data$valid_value_usd, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    # ADO 2690 - set total loan book value using raw loan book
    dplyr::mutate(
      portfolio_value_usd = dplyr::if_else(
        credit_type == "outstanding",
        portfolio_size$portfolio_loan_size_outstanding,
        portfolio_size$portfolio_loan_size_credit_limit
      )
    ) %>%
    dplyr::select(
      .data$investor_name, .data$portfolio_name, .data$asset_type,
      .data$financial_sector, .data$valid_input, .data$valid_value_usd,
      .data$asset_value_usd, .data$portfolio_value_usd, .data$currency
    )


  if (!dir.exists(data_prep_output_path)) {
    rlang::abort(
      c(
        "Argument output_path must point to an existing directory.",
        x = glue::glue("Invalid file path: {data_prep_output_path}."),
        i = "Did you set data_prep_output_path correctly?."
      )
    )
  }


  portfolio_overview %>%
    saveRDS(
      file.path(data_prep_output_path, "overview_portfolio.rda")
    )

  #### Calculate unweighted company level PACTA results-------------------------
  cat("-- Preparing unweighted company level PACTA results. \n")
  if (credit_type == "credit_limit") {
    use_credit_limit <- TRUE
  } else {
    use_credit_limit <- FALSE
  }

  # running the PACTA analysis by scenario source is necessary because there are
  # scenarios of the same name across scenario sources. E.g. WEO2020 "SDS" and
  # WEO2021 "SDS". If the sources are not passed one by one, PACTA will aggregate
  # results by scenario name across sources, which leads to double counting of
  # production values
  scenario_sources <- unique(scenario_data_market_share$scenario_source)
  p4b_tms_results <- tibble::tibble()

  for (i in scenario_sources) {

    scenario_data_market_share_i <- scenario_data_market_share %>%
      dplyr::filter(.data$scenario_source == i)

    regions_i <- regions %>%
      dplyr::filter(.data$source == i)

    p4b_tms_results_i <- matched_non_negative %>%
      r2dii.analysis::target_market_share(
        abcd = production_forecast_data,
        scenario = scenario_data_market_share_i,
        region_isos = regions_i,
        use_credit_limit = use_credit_limit,
        by_company = TRUE,
        weight_production = FALSE
      )

    p4b_tms_results <- p4b_tms_results %>%
      dplyr::bind_rows(p4b_tms_results_i)

  }

  p4b_tms_results <- p4b_tms_results %>%
    dplyr::rename(
      production_unweighted = .data$production
    ) %>%
    dplyr::mutate(technology_share = round(.data$technology_share, 8)) %>%
    report_all_duplicate_kinds(
      composite_unique_cols = c(
        "sector", "technology", "year", "region", "scenario_source", "name_abcd",
        "metric"
      )
    )

  #### Add loan share information to PACTA results------------------------------

  p4b_tms_results_loan_share <- p4b_tms_results %>%
    # TODO why left_join?
    dplyr::left_join(loan_share, by = c("sector" = "sector_abcd", "name_abcd")) %>%
    dplyr::select(
      -c(
        .data$comp_loan_size_outstanding, .data$comp_loan_size_credit_limit,
        .data$loan_size_outstanding_currency, .data$loan_size_credit_limit_currency
      )
    ) %>%
    dplyr::rename(
      loan_share_outstanding = .data$comp_loan_share_outstanding,
      loan_share_credit_limit = .data$comp_loan_share_credit_limit
    )

  #### Format company level PACTA results---------------------------------------

  # ADO 1933 - for now, this only includes sectors with production pathways
  # in the future, sectors with emissions factors based pathways may follow

  loans_results_company <- p4b_tms_results_loan_share %>%
    format_loanbook_st(
      investor_name = investor_name_placeholder,
      portfolio_name = investor_name_placeholder,
      equity_market = equity_market_filter_lookup,
      credit = credit_type
    )

  # join AR PAMS data to obtain information on emission factors
  loans_results_company <- loans_results_company %>%
    dplyr::inner_join(
      production_forecast_data %>% dplyr::distinct(.data$company_id, .data$name_company),
      by = c("company_name" = "name_company")
    ) %>%
    dplyr::select(-.data$id) %>%
    dplyr::rename(id = .data$company_id)

  # This aggregates AR PAMS to company_technology level to get emission factors
  # Note that this is done including all assets, i.e. we look at global
  # emission factors here
  # TODO: run for each scenario geography. This can be done when decoupling the
  # stress test from running PACTA, as a similar aggregation step will be needed
  # for production data
  ar_pams <- ar_pams %>%
    dplyr::select(-dplyr::starts_with("Direct")) %>%
    tidyr::pivot_longer(
      cols = dplyr::starts_with("Total"),
      names_to = "year",
      names_prefix = "Total ",
      values_to = "ald_production"
    ) %>%
    dplyr::rename(
      id = .data$`Company ID`,
      company_name = .data$`Company Name`,
      ald_sector = .data$`Asset Sector`,
      technology = .data$`Asset Technology`,
      technology_type = .data$`Asset Technology Type`,
      ald_location = .data$`Asset Country`,
      emissions_factor = .data$`Emissions Factor`,
      emissions_factor_unit = .data$`Emissions Factor Unit`,
      ald_production_unit = .data$`Activity Unit`
    ) %>%
    dplyr::mutate(
      technology = dplyr::case_when(
        .data$ald_sector == "Coal" ~ "Coal",
        .data$technology %in% c("Gas", "Natural Gas Liquids") ~ "Gas",
        .data$technology == "Oil and Condensate" ~ "Oil",
        TRUE ~ .data$technology
      ),
      ald_sector = dplyr::if_else(
        .data$ald_sector == "LDV", "Automotive", .data$ald_sector
      )
    )

  avg_ef_pams <- ar_pams %>%
    dplyr::group_by(.data$ald_sector, .data$technology, .data$technology_type, .data$emissions_factor_unit) %>%
    dplyr::summarise(
      emissions_factor = stats::weighted.mean(
        .data$emissions_factor, .data$ald_production, na.rm = TRUE
        )
      ) %>%
    dplyr::ungroup()

  # use avg technology type EFs to fill missing values
  pams_missing_ef <- ar_pams %>%
    dplyr::filter(is.na(.data$emissions_factor))

  pams_missing_ef <- pams_missing_ef %>%
    dplyr::select(-.data$emissions_factor, -.data$emissions_factor_unit) %>%
    dplyr::inner_join(avg_ef_pams, by = c("ald_sector", "technology", "technology_type"))

  ar_pams <- ar_pams %>%
    dplyr::filter(!is.na(.data$emissions_factor)) %>%
    dplyr::bind_rows(pams_missing_ef)

  ar_pams <- ar_pams %>%
    dplyr::transmute(
      id = as.numeric(id),
      company_name = tolower(as.character(.data$company_name)),
      ald_sector = as.character(.data$ald_sector),
      ald_location = as.character(.data$ald_location),
      technology = as.character(.data$technology),
      year = as.numeric(.data$year),
      ald_production = as.numeric(.data$ald_production),
      ald_production = dplyr::if_else(.data$ald_production <= 0, 0, .data$ald_production),
      ald_production_unit = as.character(.data$ald_production_unit),
      ald_emissions_factor = as.numeric(.data$emissions_factor),
      ald_emissions_factor_unit = as.character(.data$emissions_factor_unit)
    ) %>%
    dplyr::group_by(
      .data$id, .data$company_name, .data$ald_sector, .data$technology, .data$year
    ) %>%
    dplyr::summarise(
      # this may introduce NaNs for technologies that were not imputed above.
      # for all currently featured production pathway sectors, this is not an
      # issue though
      plan_emission_factor = stats::weighted.mean(
        x = .data$ald_emissions_factor, w = .data$ald_production, na.rm = TRUE
      ),
      plan_tech_prod = sum(.data$ald_production, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::ungroup() %>%
    # if we get an NaN for the weighted mean of the EF, we set it to 0 in case
    # the production is also 0. Since we only use the EF in the product of
    # EF and production, this will have no impact on later calculations of
    # absolute emissions
    dplyr::mutate(
      plan_emission_factor = dplyr::if_else(
        .data$plan_tech_prod == 0,
        0,
        .data$plan_emission_factor
      )
    )

  # Left Join EFs based on PAMS
  # this ensures that low carbon technologies with zero production plans are not
  # kicked out as they have no information in PAMS. They still need to remain in
  # the loan book, because the SMSP may require a buildout there
  loans_results_company <- loans_results_company %>%
    dplyr::left_join(
      ar_pams %>%
        dplyr::distinct(.data$id, .data$company_name, .data$ald_sector, .data$technology, .data$year, .data$plan_emission_factor),
      by = c("id", "company_name", "ald_sector", "technology", "year")
    ) %>%
    # fill missing EFs for low carbon technologies, if their production forecast
    # is zero
    dplyr::mutate(
      plan_emission_factor = dplyr::if_else(
        .data$plan_tech_prod == 0 & .data$technology %in% c("Electric", "FuelCell", "HydroCap", "NuclearCap", "RenewablesCap"),
        0,
        .data$plan_emission_factor
      )
    )


  loans_results_company %>%
    saveRDS(
      file.path(data_prep_output_path, "Loans_results_company.rda")
    )

  cat("-- Exported prepared data to designated output path. \n")
}
