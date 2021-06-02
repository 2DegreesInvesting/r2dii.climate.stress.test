test_that("price trajectories function returns a ggplot object", {
  test_price_data <- readRDS(testthat::test_path("test_data", "price_test.rds"))

  ggplot_price_trajectories <- show_price_trajectories(
    data = test_price_data,
    price_scenarios = c("B2DS", "NPS", "SDS", "Baseline")
  )

  testthat::expect_s3_class(ggplot_price_trajectories, "ggplot")
})

test_that("bogus price scenario results in error", {
  test_price_data <- readRDS(testthat::test_path("test_data", "price_test.rds"))

  testthat::expect_error(
    show_price_trajectories(
      data = test_price_data,
      price_scenarios = c("scoobah doobah"),
      regexp = "data_has_expected_columns is not TRUE",
      fixed = TRUE
    )
  )
})
