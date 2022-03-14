test_that("without specified arguments, function throws error", {
  testthat::expect_error(
    calculate_pd_change_annual(),
    "argument \"data\" is missing"
  )
})

test_that("with missing argument for shock_year, calculate_pd_change_annual
          throws error", {
  test_data <- read_test_data("loanbook_annual_profits.csv")
  test_exposure <- tibble::tribble(
    ~investor_name, ~portfolio_name, ~company_name, ~year, ~scenario_geography, ~ald_sector, ~technology, ~plan_carsten, ~plan_sec_carsten, ~term, ~pd,
    "Meta Investor", "Meta Investor", "company_1", 2025, "Global", "Power", "NuclearCap", 0.01, 0.01, 1, 0.01,
    "Meta Investor", "Meta Investor", "company_2", 2025, "Global", "Oil&gas", "Oil", 0.01, 0.01, 1, 0.01,
    "Meta Investor", "Meta Investor", "Power Company", 2025, "Global", "Power", "NuclearCap", 0.01, 0.01, 1, 0.01,
  )

  test_end_of_analysis <- 2040
  test_risk_free_rate <- 0.05

  testthat::expect_error(
    calculate_pd_change_annual(
      data = test_data,
      exposure_at_default = test_exposure,
      end_of_analysis = test_end_of_analysis,
      risk_free_interest_rate = test_risk_free_rate
    ),
    "Must provide input for 'shock_year'"
  )
})

test_that("PD_changes point in expected direction", {
  test_data <- read_test_data("loanbook_annual_profits.csv")
  test_exposure <- tibble::tribble(
    ~investor_name, ~portfolio_name, ~company_name, ~year, ~scenario_geography, ~ald_sector, ~technology, ~plan_carsten, ~plan_sec_carsten, ~term, ~pd,
    "Meta Investor", "Meta Investor", "company_1", 2025, "Global", "Power", "NuclearCap", 0.01, 0.01, 1, 0.01,
    "Meta Investor", "Meta Investor", "company_2", 2025, "Global", "Oil&gas", "Oil", 0.01, 0.01, 1, 0.01,
    "Meta Investor", "Meta Investor", "Power Company", 2025, "Global", "Power", "NuclearCap", 0.01, 0.01, 1, 0.01,
  )

  test_shock_year <- 2030
  test_end_of_analysis <- 2040
  test_risk_free_rate <- 0.05

  # Expected direction of PD change depends on direction of expected value change
  # This can be extracted from NPV columns of the annual profit data, which is
  # equal for all years
  filter_year <- min(test_data$year, na.rm = TRUE)
  expected_direction <- test_data %>%
    dplyr::filter(.data$year == filter_year) %>%
    dplyr::select(
      .data$scenario_name,
      .data$scenario_geography,
      .data$company_name,
      .data$ald_sector,
      .data$discounted_net_profit_baseline,
      .data$discounted_net_profit_ls
    ) %>%
    dplyr::mutate(
      pd_expected_rising = dplyr::if_else(
        .data$discounted_net_profit_ls < .data$discounted_net_profit_baseline,
        TRUE,
        FALSE
      )
    )

  test_results <- calculate_pd_change_annual(
    data = test_data,
    exposure_at_default = test_exposure,
    shock_year = test_shock_year,
    end_of_analysis = test_end_of_analysis,
    risk_free_interest_rate = test_risk_free_rate
  )

  results_direction <- test_results %>%
    dplyr::mutate(
      pd_change_rising = dplyr::if_else(
        .data$PD_change > 0,
        TRUE,
        FALSE
      )
    ) %>%
    dplyr::inner_join(
      expected_direction,
      by = c(
        "scenario_name", "scenario_geography", "company_name", "ald_sector"
      )
    )

  testthat::expect_equal(
    results_direction$pd_expected_rising,
    results_direction$pd_change_rising
  )
})
