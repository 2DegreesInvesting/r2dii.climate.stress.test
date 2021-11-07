test_that("works with bonds", {
  skip_if_not(is_registered_dev())

  expect_no_error({
    x <- capture.output(suppressWarnings(run_stress_test("bonds")))
  })
})
