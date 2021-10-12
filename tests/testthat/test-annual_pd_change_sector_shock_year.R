test_that("without specified arguments, function throws error", {
  testthat::expect_error(
    annual_pd_change_sector_shock_year(),
    "argument \"data\" is missing"
  )
})

test_that("production trajectories function returns a ggplot object", {
  test_input_graph <- read_test_data("graph_annual_pd_change_sector_shock_year.csv")

  test_shock_year <- 2030
  test_geography_filter <- "Global"

  test_plot <- annual_pd_change_sector_shock_year(
    data = test_input_graph,
    shock_year = test_shock_year,
    geography_filter = test_geography_filter
  )

  testthat::expect_s3_class(test_plot, "ggplot")
})
