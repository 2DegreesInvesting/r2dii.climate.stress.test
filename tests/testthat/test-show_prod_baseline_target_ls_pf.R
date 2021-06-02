test_that("comparison of baseline, target and l&s function returns a ggplot object", {
  # NOTE: the test annual profits are entirely made up and inconsistent on purpose
  test_annual_profits <- read_test_data("annual_profits.csv")

  ggplot_baseline_target_late_sudden <- show_prod_baseline_target_ls_pf(
    data = test_annual_profits,
    geography_filter = "Global",
    shock_year = 2028
  )

  testthat::expect_s3_class(ggplot_baseline_target_late_sudden, "ggplot")
})
