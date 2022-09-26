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
