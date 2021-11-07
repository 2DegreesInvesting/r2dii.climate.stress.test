test_that("works with bonds", {
  skip_if_not(is_registered_dev())

  none <- NA
  expect_error({
    # Quietly
    x <- capture.output(suppressWarnings(
      run_stress_test("bonds")
    ))},
    none
  )
})
