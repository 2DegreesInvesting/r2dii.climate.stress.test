test_that("works with `term` of type 'integer'", {
  is_me <- identical(path.expand("~"), "/home/mauro")
  skip_if_not(is_me)

  expect_no_error(
    # Quietly
    x <- suppressWarnings(capture.output(
      run_stress_test("bonds", term = 1L)
    ))
  )
})
