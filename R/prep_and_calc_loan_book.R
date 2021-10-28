#' Run stress testing for loans
#'
#' @param year_production_data Numeric. A vector of length 1 that indicates which
#'   release year the production data should be taken from. For accepted
#'   range compare `year_production_data_range_lookup`.
#' @param year_scenario_data Numeric. A vector of length 1 that indicates which
#'   release year the scenario data should be taken from. For accepted
#'   range compare `year_scenario_data_range_lookup`.
#' @param equity_market Character. A string that indicates the location of the
#'   equity market of the analysis. This is required to match the P4I data
#'   structure and should use the equity_market_filter passed form the user
#'   function as input.
#' @param credit_type Type of credit. For accepted values please compare
#'   `credit_type_loans`.
#' @return NULL
#' @export
run_prep_calculation_loans <- function(year_production_data,
                                       year_scenario_data,
                                       equity_market,
                                       credit_type) {

  if (!dplyr::between(year_production_data, min(p4b_production_data_years_lookup), max(p4b_production_data_years_lookup))) {
    stop("Argument year_production_data is outside accepted range.")
  }

  if (!dplyr::between(year_scenario_data, min(p4b_scenario_data_years_lookup), max(p4b_scenario_data_years_lookup))) {
    stop("Argument year_scenario_data is outside accepted range.")
  }

  if (length(equity_market) != 1) {
    stop("Input argument equity_market must be of length 1")
  }
  ###########################################################################
  # Load input datasets------------------------------------------------------
  ###########################################################################

  # raw loan book
  loanbook <- readr::read_csv(
    file.path(get_st_data_path("ST_PROJECT_FOLDER"), "inputs", paste0("raw_loanbook.csv")),
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
  matched  <- readr::read_csv(
    file.path(get_st_data_path("ST_PROJECT_FOLDER"), "inputs", "matched_loan_book.csv"),
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
  if (year_production_data == 2020) {
    production_forecast_data <- readr::read_csv(
      file.path(get_st_data_path(), "ald_15092020.csv"),
      col_types = readr::cols(
        name_company = "c",
        sector = "c",
        technology = "c",
        year = "d",
        production = "d",
        production_unit = "c",
        emission_factor = "d",
        ald_emission_factor_unit = "c",
        plant_location = "c",
        is_ultimate_owner = "l",
        ald_timestamp = "c"
      )
    )
  } else if (year_production_data == 2021) {
    production_forecast_data <- readxl::read_xlsx(
      file.path(get_st_data_path(), "2021-07-15_AR_2020Q4_PACTA-Data (3).xlsx"),
      sheet = "Company Indicators - PACTA"
    )
  }

  # Scenario data - market share
  scenario_data_market_share <- readr::read_csv(
    file.path(get_st_data_path(), glue::glue("scenario_{year_scenario_data}.csv")),
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

  # Scenario data - emission intensity
  scenario_data_emissions_intensity <- readr::read_csv(
    file.path(get_st_data_path(), glue::glue("co2_intensity_scenario_{year_scenario_data}.csv")),
    col_types = readr::cols(
      scenario_source = "c",
      scenario = "c",
      sector = "c",
      region = "c",
      year = "d",
      emission_factor = "d",
      emission_factor_unit = "c"
    )
  )

  ###########################################################################
  # Wrangle and prepare data-------------------------------------------------
  ###########################################################################

  # TODO: what to do with negative credit limits?
  matched_non_negative <- matched %>%
    dplyr::mutate(
      loan_size_outstanding = dplyr::if_else(
        .data$loan_size_outstanding < 0, 0, .data$loan_size_outstanding
      ),
      loan_size_credit_limit = dplyr::if_else(
        .data$loan_size_credit_limit < 0, 0, .data$loan_size_credit_limit
      )
    )

  portfolio_size <- loanbook %>%
    # TODO: why distinct? Is there any way that id_loan is not unique?
    dplyr::distinct(
      .data$id_loan, .data$loan_size_outstanding, .data$loan_size_credit_limit
    ) %>%
    dplyr::summarise(
      portfolio_loan_size_outstanding = sum(.data$loan_size_outstanding, na.rm = TRUE),
      portfolio_loan_size_credit_limit = sum(.data$loan_size_credit_limit, na.rm = TRUE),
      .groups = "drop"
    )

  matched_portfolio_size <- matched_non_negative %>%
    # TODO: why distinct? Is there any way that id_loan is not unique?
    dplyr::distinct(
      .data$id_loan, .data$loan_size_outstanding, .data$loan_size_credit_limit
    ) %>%
    dplyr::summarise(
      matched_portfolio_loan_size_outstanding = sum(.data$loan_size_outstanding, na.rm = TRUE),
      matched_portfolio_loan_size_credit_limit = sum(.data$loan_size_credit_limit, na.rm = TRUE),
      .groups = "drop"
    )


  ###########################################################################
  # Calculate loan-tech level loan book size and value share-----------------
  ###########################################################################

  loan_share <- matched_non_negative %>%
    dplyr::mutate(
      portfolio_loan_size_outstanding = portfolio_size$portfolio_loan_size_outstanding,
      portfolio_loan_size_credit_limit = portfolio_size$portfolio_loan_size_credit_limit,
      matched_portfolio_loan_size_outstanding = sum(.data$loan_size_outstanding, na.rm = TRUE),
      matched_portfolio_loan_size_credit_limit = sum(.data$loan_size_credit_limit, na.rm = TRUE)
    ) %>%
    dplyr::group_by(
      # ADO 1933 - we choose `name_ald` as this is an internal name that can be
      # joined with other 2dii data later on. This is not the case for `name`.
      .data$name_ald, .data$sector_ald, .data$loan_size_outstanding_currency,
      .data$loan_size_credit_limit_currency
    ) %>%
    dplyr::mutate(
      comp_loan_share_outstanding = sum(.data$loan_size_outstanding, na.rm = TRUE) / .data$portfolio_loan_size_outstanding,
      comp_loan_size_outstanding = sum(.data$loan_size_outstanding, na.rm = TRUE),
      comp_loan_share_credit_limit = sum(.data$loan_size_credit_limit, na.rm = TRUE) / .data$portfolio_loan_size_credit_limit,
      comp_loan_size_credit_limit = sum(.data$loan_size_credit_limit, na.rm = TRUE)
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
    # TODO why distinct_all?
    dplyr::distinct_all()

  loan_share %>%
    readr::write_csv(
      file.path(get_st_data_path("ST_PROJECT_FOLDER"), "inputs", "overview_companies.csv")
    )

  ###########################################################################
  # Calculate tech level loan book size and value share----------------------
  ###########################################################################
  # TODO: figure out a way to get tech share. Is this still relevant?

  ###########################################################################
  # Calculate sector level loan book size and value share--------------------
  ###########################################################################

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

  #----Wrangle portfolio overview to P4I format--------------------
  sector_credit_type <- glue::glue("sector_loan_size_{credit_type}")
  credit_currency <- glue::glue("loan_size_{credit_type}_currency")

  portfolio_overview <- sector_share %>%
    dplyr::inner_join(
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
      # TODO: convert currencies to USD or at least common currency
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
    dplyr::group_by(.data$investor_name, .data$portfolio_name, .data$valid_input) %>%
    dplyr::mutate(portfolio_value_usd = sum(.data$valid_value_usd, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      .data$investor_name, .data$portfolio_name, .data$asset_type,
      .data$financial_sector, .data$valid_input, .data$valid_value_usd,
      .data$asset_value_usd, .data$portfolio_value_usd, .data$currency
    )

  portfolio_overview %>%
    saveRDS(
      file.path(get_st_data_path("ST_PROJECT_FOLDER"), "inputs", "overview_portfolio.rda")
    )

  ###########################################################################
  # Calculate company level PACTA results------------------------------------
  ###########################################################################

  #----Calculate unweighted company level PACTA results--------------------
  p4b_tms_results <- matched_non_negative %>%
    r2dii.analysis::target_market_share(
      ald = production_forecast_data,
      scenario = scenario_data_market_share,
      region_isos = regions,
      use_credit_limit = TRUE,
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
    dplyr::mutate(technology_share = round(.data$technology_share, 8)) %>% # rounding errors can lead to duplicates
    # TODO: why distinct_all?
    dplyr::distinct_all()

  #----Add loan share information to PACTA results--------------------
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

  ###########################################################################
  # Format company level PACTA results---------------------------------------
  ###########################################################################
  # ADO 1933 - for now, this only includes sectors with production pathways
  # in the future, sectors with emissions factors based pathways may follow
  loans_results_company <- p4b_tms_results_loan_share %>%
    format_loanbook_st(
      investor_name = investor_name_placeholder,
      portfolio_name = investor_name_placeholder,
      equity_market = equity_market,
      credit = credit_type
    )

  loans_results_company %>%
    saveRDS(
      file.path(get_st_data_path("ST_PROJECT_FOLDER"), "inputs", "Loans_results_company.rda")
    )

}

