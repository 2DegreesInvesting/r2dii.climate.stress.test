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
  test_exclusion <- NULL

  testthat::expect_error(
    calculate_pd_change_overall(
      data = test_data,
      end_of_analysis = test_end_of_analysis,
      exclusion = test_exclusion
    ),
    "Must provide input for 'shock_year'"
  )
})

test_that("with missing argument for exclusion, calculate_pd_change_overall
          returns data.frame", {
  test_data <- read_test_data("loanbook_annual_profits.csv")

  test_shock_year <- 2030
  test_end_of_analysis <- 2040

  test_results <- calculate_pd_change_overall(
    data = test_data,
    shock_year = test_shock_year,
    end_of_analysis = test_end_of_analysis
  )

  testthat::expect_s3_class(test_results, "data.frame")
})

test_that("with excluded company, calculate_pd_change_overall returns data.frame
          with equal number of rows as without", {
  test_data <- read_test_data("loanbook_annual_profits.csv")
  test_exclusion <- read_test_data("exclude_companies.csv")

  test_shock_year <- 2030
  test_end_of_analysis <- 2040

  test_results_no_exclusion <- calculate_pd_change_overall(
    data = test_data,
    shock_year = test_shock_year,
    end_of_analysis = test_end_of_analysis,
    exclusion = NULL
  )

  test_results_with_exclusion <- calculate_pd_change_overall(
    data = test_data,
    shock_year = test_shock_year,
    end_of_analysis = test_end_of_analysis,
    exclusion = test_exclusion
  )

  testthat::expect_equal(
    nrow(test_results_no_exclusion),
    nrow(test_results_with_exclusion)
  )
})


test_that("PD_changes point in expected direction", {
  test_data <- read_test_data("loanbook_annual_profits.csv")

  test_shock_year <- 2030
  test_end_of_analysis <- 2040

  expected_direction <- test_data %>%
    dplyr::distinct(
      .data$scenario_name,
      .data$scenario_geography,
      .data$company_name,
      .data$ald_sector,
      .data$technology,
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

  test_results <- calculate_pd_change_overall(
    data = test_data,
    shock_year = test_shock_year,
    end_of_analysis = test_end_of_analysis,
    exclusion = NULL
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
        "scenario_name", "scenario_geography", "company_name", "ald_sector",
        "technology"
      )
    )

  testthat::expect_equal(
    results_direction$pd_expected_rising,
    results_direction$pd_change_rising
  )
})
