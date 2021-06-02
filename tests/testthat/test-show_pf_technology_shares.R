test_that("portfolio technology share function returns a ggplot object", {
  # NOTE: the test annual profits are entirely made up and inconsistent on purpose
  test_pf_tech_shares <- readRDS(
    testthat::test_path("test_data", "test_pf_tech_share.rds")
  )

  ggplot_pf_technology_shares <- show_pf_technology_shares(
    data = test_pf_tech_shares
  )

  testthat::expect_s3_class(ggplot_pf_technology_shares, "ggplot")
})
