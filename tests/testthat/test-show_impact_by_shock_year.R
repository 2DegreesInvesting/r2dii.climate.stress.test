test_that("impact over time function returns a ggplot object", {
  # NOTE: the test results are entirely made up and inconsistent on purpose
  test_equity_results <- readRDS(
    testthat::test_path("test_data", "test_equity_results.rds")
  )

  ggplot_impact_over_time <- show_impact_by_shock_year(
    data = test_equity_results,
    level = "technology"
  )

  testthat::expect_s3_class(ggplot_impact_over_time, "ggplot")
})
