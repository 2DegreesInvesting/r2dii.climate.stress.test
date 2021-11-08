test_that("works with integer passed to `term`", {
  expect_no_error(
    # Quietly
    x <- suppressWarnings(capture.output(
      run_stress_test("bonds", term = 1L)
    ))
  )
})
