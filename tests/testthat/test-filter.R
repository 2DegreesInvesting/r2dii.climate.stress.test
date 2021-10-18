# keep_merton_compatible_rows ---------------------------------------------
test_that("error is thrown if argument stage has invalid value", {
  input_data <- tibble::tibble(
    debt = c(1, 2),
    equity_0_baseline = c(1, 2),
    equity_0_late_sudden = c(1, -1.99),
    volatility = c(1, 2),
    risk_free_rate = c(1, 2),
    term = c(1, 2)
  )
  expect_error(keep_merton_compatible_rows(input_data, stage = "some"), "Invalid")
})


test_that("input remains unaltered if rows are merton compatible", {
  input_data <- tibble::tibble(
    debt = c(1, 2),
    equity_0_baseline = c(1, 2),
    equity_0_late_sudden = c(1, -1.99),
    volatility = c(1, 2),
    risk_free_rate = c(1, 2),
    term = c(1, 2)
  )
  filtered_data <- keep_merton_compatible_rows(input_data, stage = "overall")
  expect_equal(input_data, filtered_data)
})

test_that("merton incompatible rows are removed", {
  input_data <- tibble::tibble(
    debt = c(1, 2, -1, 2, 1),
    equity_0_baseline = c(1, 2, 2, 2, 1),
    equity_0_late_sudden = c(1, -2.1, 2, 2, 1),
    volatility = c(1, 2, 2, 2, 1),
    risk_free_rate = c(1, 2, 2, 2, -1),
    term = c(1, 2, 2, 0, 1)
  )
  expect_message(filtered_data <- keep_merton_compatible_rows(input_data, stage = "overall"), "Removed 4 rows.")
  expect_equal(input_data %>% dplyr::slice(1), filtered_data)
})
