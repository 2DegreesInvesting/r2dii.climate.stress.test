#' Run stress testing data preparation for loans
#'
#' @inheritParams run_stress_test
#' @param credit_type Type of credit. For accepted values please compare
#'   `credit_type_lookup`.
#'
#' @return NULL
#' @export
run_prep_calculation_loans <- function(data_path_project_specific,
                                       data_path_project_agnostic,
                                       credit_type = "outstanding") {

  #### Validate input-----------------------------------------------------------

  if (!credit_type %in% credit_type_lookup) {
    stop("Argument credit_type does not hold an accepted value.")
  }

  #### Load input data sets-----------------------------------------------------
  # raw loan book
  validate_file_exists(file.path(data_path_project_specific, "inputs", paste0("raw_loanbook.csv")))
  loanbook <- readr::read_csv(
    file.path(data_path_project_specific, "inputs", paste0("raw_loanbook.csv")),
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
  validate_file_exists(file.path(data_path_project_specific, "inputs", "matched_loan_book.csv"))
  matched  <- readr::read_csv(
    file.path(data_path_project_specific, "inputs", "matched_loan_book.csv"),
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
      sector_ald = "c",
      name = "c",
      name_ald = "c",
      score = "d",
      source = "c",
      borderline = "l"
    )
  )

  # region iso mapping
  regions <- r2dii.data::region_isos

  # Production forecast data
  validate_file_exists(file.path(data_path_project_agnostic, "2021-07-15_AR_2020Q4_PACTA-Data (3).xlsx"))
  production_forecast_data <- readxl::read_xlsx(
    file.path(data_path_project_agnostic, "2021-07-15_AR_2020Q4_PACTA-Data (3).xlsx"),
    sheet = "Company Indicators - PACTA"
  )

  # Scenario data - market share
  validate_file_exists(file.path(data_path_project_agnostic, "scenario_2020.csv"))
  scenario_data_market_share <- readr::read_csv(
    file.path(data_path_project_agnostic, glue::glue("scenario_2020.csv")),
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
      )
      , call. = FALSE
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
    dplyr::mutate(
      portfolio_loan_size_outstanding = portfolio_size$portfolio_loan_size_outstanding,
      portfolio_loan_size_credit_limit = portfolio_size$portfolio_loan_size_credit_limit,
      matched_portfolio_loan_size_outstanding = matched_portfolio_size$matched_portfolio_loan_size_outstanding,
      matched_portfolio_loan_size_credit_limit = matched_portfolio_size$matched_portfolio_loan_size_credit_limit
    ) %>%
    dplyr::group_by(
      # ADO 1933 - we choose `name_ald` as this is an internal name that can be
      # joined with other 2dii data later on. This is not the case for `name`.
      .data$name_ald, .data$sector_ald, .data$loan_size_outstanding_currency,
      .data$loan_size_credit_limit_currency
    ) %>%
    # ADO 2723 - loan shares calculated against matched loan book, not total loan book
    # this is to ensure all scaling happens against the same denominator
    # run_stress_test uses the matched portfolio to scale the overall impact
    dplyr::summarise(
      comp_loan_share_outstanding = sum(.data$loan_size_outstanding, na.rm = TRUE) / .data$matched_portfolio_loan_size_outstanding,
      comp_loan_size_outstanding = sum(.data$loan_size_outstanding, na.rm = TRUE),
      comp_loan_share_credit_limit = sum(.data$loan_size_credit_limit, na.rm = TRUE) / .data$matched_portfolio_loan_size_credit_limit,
      comp_loan_size_credit_limit = sum(.data$loan_size_credit_limit, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      .data$name_ald,
      .data$sector_ald,
      .data$comp_loan_share_outstanding,
      .data$comp_loan_size_outstanding,
      .data$loan_size_outstanding_currency,
      .data$comp_loan_share_credit_limit,
      .data$comp_loan_size_credit_limit,
      .data$loan_size_credit_limit_currency
    ) %>%
    # ADO 2393 - somehow the summarise call does not reduce the number of rows.
    # since it creates duplicates, we use distinct_all() to ensure uniqueness
    dplyr::distinct_all()

  #### Calculate tech level loan book size and value share----------------------
  # TODO: figure out a way to get tech share. Is this still relevant?

  #### Calculate sector level loan book size and value share--------------------

  sector_share <- matched_non_negative %>%
    dplyr::group_by(
      .data$sector_ald, .data$loan_size_outstanding_currency,
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
      .data$sector_ald,
      .data$sector_loan_share_outstanding,
      .data$sector_loan_size_outstanding,
      .data$loan_size_outstanding_currency,
      .data$sector_loan_share_credit_limit,
      .data$sector_loan_size_credit_limit,
      .data$loan_size_credit_limit_currency
    )

  #### Wrangle portfolio overview to P4I format---------------------------------

  sector_credit_type <- glue::glue("sector_loan_size_{credit_type}")
  credit_currency <- glue::glue("loan_size_{credit_type}_currency")

  portfolio_overview <- sector_share %>%
    dplyr::inner_join(
      # ADO 2393 - use distinct to only map sectors, not technlogies
      p4i_p4b_sector_technology_lookup %>% dplyr::distinct(.data$sector_p4b, .data$sector_p4i),
      by = c("sector_ald" = "sector_p4b")) %>%
    dplyr::mutate(sector_ald = .data$sector_p4i) %>%
    dplyr::select(
      .data$sector_ald,
      !!rlang::sym(sector_credit_type),
      !!rlang::sym(credit_currency)
    ) %>%
    dplyr::rename(
      financial_sector = .data$sector_ald,
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

  portfolio_overview %>%
    saveRDS(
      file.path(data_path_project_specific, "inputs", "overview_portfolio.rda")
    )

  #### Calculate unweighted company level PACTA results-------------------------

  use_credit_limit <- if (credit_type == "credit_limit") {TRUE} else {FALSE}
  p4b_tms_results <- matched_non_negative %>%
    r2dii.analysis::target_market_share(
      ald = production_forecast_data,
      scenario = scenario_data_market_share,
      region_isos = regions,
      use_credit_limit = use_credit_limit,
      by_company = TRUE,
      weight_production = FALSE
    ) %>%
    # TODO filter must be generalised for diff scenario inputs
    dplyr::filter(
      (.data$sector == "automotive" & .data$scenario_source == "etp_2017") |
        (.data$sector == "coal" & .data$scenario_source == "weo_2019") |
        (.data$sector == "oil and gas" & .data$scenario_source == "weo_2019") |
        (.data$sector == "power" & .data$scenario_source == "weo_2019")
    ) %>%
    dplyr::rename(
      production_unweighted = .data$production
    )  %>%
    dplyr::mutate(technology_share = round(.data$technology_share, 8))# rounding errors can lead to duplicates

  #### Add loan share information to PACTA results------------------------------

  p4b_tms_results_loan_share <- p4b_tms_results %>%
    # TODO why left_join?
    dplyr::left_join(loan_share, by = c("sector" = "sector_ald", "name_ald")) %>%
    dplyr::filter(.data$region == "global") %>%
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

  loans_results_company %>%
    saveRDS(
      file.path(data_path_project_specific, "inputs", "Loans_results_company.rda")
    )

}

