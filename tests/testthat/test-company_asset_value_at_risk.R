test_that("without specified arguments, company_technology_asset_value_at_risk
          throws error", {
  testthat::expect_error(
    company_technology_asset_value_at_risk(),
    "argument \"data\" is missing"
  )
})
