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
test_that("input remains unchanged if no negative late_and_sudden levels are
          present", {
  input_data <- tibble::tibble(
    company_name = c("firm", "firm", "biz", "biz"),
    technology = c("some", "other", "some", "other"),
    late_and_sudden = 1:4,
    some_col = rep("sth", 4)
  )

  filtered_data <- filter_negative_late_and_sudden(input_data)

  expect_equal(input_data, filtered_data)
})

test_that("technology x company_name combinations that hold at least 1 negative
          value on late_and_sudden are removed", {
  input_data <- tibble::tibble(
    company_name = c("firm", "firm", "firm", "biz", "biz"),
    technology = c("some", "some", "other", "some", "other"),
    late_and_sudden = c(-1, 1, 1, 0, 1),
    some_col = rep("sth", 5)
  )

  filtered_data <- filter_negative_late_and_sudden(input_data)

  expect_equal(input_data %>% dplyr::filter(!(company_name == "firm" & technology == "some")), filtered_data)
})

test_that("removal works if several company_name x technology combinations are affected", {
  input_data <- tibble::tibble(
    company_name = c("firm", "firm", "firm", "biz", "biz"),
    technology = c("some", "some", "other", "some", "other"),
    late_and_sudden = c(-1, 1, -1, -1, 1),
    some_col = rep("sth", 5)
  )

  filtered_data <- filter_negative_late_and_sudden(input_data)

  expect_equal(input_data %>% dplyr::filter(company_name == "biz" & technology == "other"), filtered_data)
})

test_that("error is thrown if no rows remain", {
  input_data <- tibble::tibble(
    company_name = c("firm", "firm", "firm", "biz", "biz"),
    technology = c("some", "some", "other", "some", "other"),
    late_and_sudden = rep(-1, 5),
    some_col = rep("sth", 5)
  )

  expect_error(filtered_data <- filter_negative_late_and_sudden(input_data), "No rows remain")
})
