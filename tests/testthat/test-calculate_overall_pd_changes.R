test_that("without specified arguments, calculate_pd_change_overall throws
          error", {
  testthat::expect_error(
    calculate_pd_change_overall(),
    "argument \"data\" is missing"
  )
})

test_that("with missing argument for shock_year, calculate_pd_change_overall
          throws error", {
  test_data <- read_test_data("loanbook_annual_profits.csv")

  test_end_of_analysis <- 2040
  test_risk_free_rate <- 0.05

  testthat::expect_error(
    calculate_pd_change_overall(
      data = test_data,
      end_of_analysis = test_end_of_analysis,
      risk_free_interest_rate = test_risk_free_rate
    ),
    "Must provide input for 'shock_year'"
  )
})

test_that("PD_changes point in expected direction", {
  test_data <- read_test_data("loanbook_annual_profits.csv")

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
      dplyr::all_of(c(
        "scenario_name",
        "scenario_geography",
        "company_name",
        "ald_sector",
        "discounted_net_profit_baseline",
        "discounted_net_profit_ls"
      ))
    ) %>%
    dplyr::mutate(
      pd_expected_rising = dplyr::if_else(
        .data$discounted_net_profit_ls < .data$discounted_net_profit_baseline,
        TRUE,
        FALSE
      )
    )

  test_results <- calculate_pd_change_overall(
    data = test_data,
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
