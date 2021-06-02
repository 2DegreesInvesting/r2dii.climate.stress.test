test_that("change over time function returns a ggplot object", {
  # NOTE: the test results are entirely made up and inconsistent on purpose
  test_equity_results <- readRDS(
    testthat::test_path("test_data", "test_equity_results.rds")
  )

  ggplot_change_over_time <- show_var_change_by_shock_year(
    data = test_equity_results,
    level = "technology"
  )

  testthat::expect_s3_class(ggplot_change_over_time, "ggplot")
})
