# apply_filters -----------------------------------------------------------
test_that("without specified arguments, apply_filters() throws error", {
  testthat::expect_error(
    apply_filters(),
    "argument \"data\" is missing"
  )
})

test_that("with missing argument for technologies, apply_filters
          throws error", {
  test_data <- read_test_data("input_apply_filters.csv")
  test_investor <- "Meta Investor"
  test_sectors <- c("Power", "Oil&Gas", "Coal", "Automotive")
  test_scenario_geography_filter <- "Global"
  test_scenarios <- c("NPS", "SDS")
  test_allocation_method <- "portfolio_weight"
  test_start_analysis <- 2020

  testthat::expect_error(
    apply_filters(
      data = test_data,
      investor = test_investor,
      sectors = test_sectors,
      scenario_geography_filter = test_scenario_geography_filter,
      scenarios = test_scenarios,
      allocation_method = test_allocation_method,
      start_analysis = test_start_analysis
    ),
    "Must provide input for 'technologies'"
  )
})

test_that("apply_filters returns expected number of rows based on input filter
          values", {
  test_data <- read_test_data("input_apply_filters.csv")
  test_investor <- "Meta Investor"
  test_sectors <- c("Power", "Oil&Gas", "Coal", "Automotive")
  test_technologies <- c("Electric", "Coal", "NuclearCap")
  test_scenario_geography_filter <- "Global"
  test_scenarios <- c("NPS", "SDS")
  test_allocation_method <- "portfolio_weight"
  test_start_analysis <- 2020

  expected_nrows <- length(test_investor) *
    length(test_technologies) *
    length(test_scenario_geography_filter) *
    length(test_scenarios) *
    length(test_allocation_method) *
    length(unique(test_data$year))

  test_results <- apply_filters(
    data = test_data,
    investor = test_investor,
    sectors = test_sectors,
    technologies = test_technologies,
    scenario_geography_filter = test_scenario_geography_filter,
    scenarios = test_scenarios,
    allocation_method = test_allocation_method,
    start_analysis = test_start_analysis
  )

  testthat::expect_equal(nrow(test_results), expected_nrows)
})

# filter_negative_late_and_sudden -----------------------------------------


