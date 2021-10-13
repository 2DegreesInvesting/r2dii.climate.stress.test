test_that("without specified arguments, function throws error", {
  testthat::expect_error(
    annual_pd_change_company_sector(),
    "argument \"data\" is missing"
  )
})

test_that("production trajectories function returns a ggplot object", {
  test_input_graph <- read_test_data("graph_annual_pd_change_company_sector.csv")

  test_shock_year <- 2030
  test_company_filter <- c("Oil company", "Car Manufacturer")
  test_geography_filter <- "Global"

  test_plot <- annual_pd_change_company_sector(
    data = test_input_graph,
    shock_year = test_shock_year,
    company_filter = test_company_filter,
    geography_filter = test_geography_filter
  )

  testthat::expect_s3_class(test_plot, "ggplot")
})
