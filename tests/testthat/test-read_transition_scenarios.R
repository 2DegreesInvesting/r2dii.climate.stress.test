test_that("without specified arguments, read_transition_scenarios throws
          error", {
  testthat::expect_error(
    read_transition_scenarios(),
    "Must provide 'path'"
  )
})

test_that("with missing argument for end_of_analysis, read_transition_scenarios
          throws error", {
  test_path <- testthat::test_path("test_data", "transition_scenario_input.csv")
  test_start <- 2019

  testthat::expect_error(
    read_transition_scenarios(
      path = test_path,
      start_of_analysis = test_start
    ),
    "Must provide 'end_of_analysis'"
  )
})

test_that("with invalid shock years, read_transition_scenarios throws error", {
  test_path <- testthat::test_path("test_data", "transition_scenario_invalid_year.csv")
  test_start <- 2019
  test_end <- 2040

  testthat::expect_error(
    read_transition_scenarios(
      path = test_path,
      start_of_analysis = test_start,
      end_of_analysis = test_end
    ),
    "Year of shock out of bounds"
  )
})

test_that("with all arguments set, and valid shock years
          read_transition_scenarios returns data.frame", {
  test_path <- testthat::test_path("test_data", "transition_scenario_input.csv")
  test_start <- 2019
  test_end <- 2040

  test_data_new <- read_transition_scenarios(
    path = test_path,
    start_of_analysis = test_start,
    end_of_analysis = test_end
  )

  testthat::expect_s3_class(test_data_new, "data.frame")
})

test_that("without specified arguments, generate_transition_shocks throws
          error", {
  testthat::expect_error(
    generate_transition_shocks(),
    "no default"
  )
})

test_that("with invalid shock years, generate_transition_shocks throws error", {
  test_start <- 2019
  test_end <- 2040
  test_shock_year <- 2010

  testthat::expect_error(
    generate_transition_shocks(
      start_of_analysis = test_start,
      end_of_analysis = test_end,
      shock_year = test_shock_year
    ),
    "Year of shock out of bounds"
  )
})

test_that("with wrong input type, generate_transition_shocks throws error", {
  test_start <- "2019"
  test_end <- 2040
  test_shock_year <- 2025

  testthat::expect_error(
    generate_transition_shocks(
      start_of_analysis = test_start,
      end_of_analysis = test_end,
      shock_year = test_shock_year
    ),
    "numeric"
  )
})

test_that("with wrong input length for start_of_analysis or end_of_analysis,
          generate_transition_shocks throws error", {
  test_start <- c(2019, 2025)
  test_end <- 2040
  test_shock_year <- 2025

  testthat::expect_error(
    generate_transition_shocks(
      start_of_analysis = test_start,
      end_of_analysis = test_end,
      shock_year = test_shock_year
    ),
    "length 1"
  )

  test_start <- 2019
  test_end <- c(2035, 2040)
  test_shock_years <- 2025

  testthat::expect_error(
    generate_transition_shocks(
      start_of_analysis = test_start,
      end_of_analysis = test_end,
      shock_year = test_shock_year
    ),
    "length 1"
  )
})

test_that("with valid inputs, generate_transition_shocks generates a data frame
          with number of rows equal to number of shock years", {
  test_start <- 2019
  test_end <- 2040
  test_shock_year <- 2025

  n_shock_years <- length(test_shock_year)

  shock_scenarios <- generate_transition_shocks(
    start_of_analysis = test_start,
    end_of_analysis = test_end,
    shock_year = test_shock_year
  )

  testthat::expect_equal(
    nrow(shock_scenarios),
    n_shock_years
  )
})
