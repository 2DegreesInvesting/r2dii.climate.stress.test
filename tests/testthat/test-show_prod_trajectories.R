test_that("production trajectories function returns a ggplot object", {
  test_scenario_data <- readRDS(
    testthat::test_path("test_data", "test_interpolate_auto.rds")
  )

  ggplot_production_trajectories <- show_prod_trajectories(
    data = test_scenario_data,
    end_year = 2040,
    source = c("ETP2017", "WEO2019"),
    ald_sector = c("Automotive", "Coal"),
    technology = c("Electric", "Coal"),
    geography_filter = "Global"
  )

  testthat::expect_s3_class(ggplot_production_trajectories, "ggplot")
})
