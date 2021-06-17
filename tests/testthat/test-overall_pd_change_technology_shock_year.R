test_that("without specified arguments, function throws error", {
  testthat::expect_error(
    overall_pd_change_technology_shock_year(),
    "argument \"data\" is missing"
  )
})

test_that("production trajectories function returns a ggplot object", {
  test_input_graph <- read_test_data("graph_overall_pd_change_technology_shock_year.csv")

  test_scenario_filter <- c("Carbon balance 2025", "Carbon balance 2030")
  test_geography_filter <- "Global"

  test_plot <- overall_pd_change_technology_shock_year(
    data = test_input_graph,
    scenario_filter = test_scenario_filter,
    geography_filter = test_geography_filter
  )

  testthat::expect_s3_class(test_plot, "ggplot")
})
