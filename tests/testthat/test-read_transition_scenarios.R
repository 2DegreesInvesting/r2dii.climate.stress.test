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
