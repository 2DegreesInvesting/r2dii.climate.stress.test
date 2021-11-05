# check_valid_financial_data_values ---------------------------------------
test_that("function detects values out of range", {
  fin_data <- tibble::tibble(
    company_name = c("firm", "firm_b", "company"),
    company_id = c(1, 2, 3),
    pd = c(-1, 0, 2)
  )

  expect_error(check_valid_financial_data_values(
    financial_data = fin_data,
    asset_type = "equity"
  ), "pd detected")
})
