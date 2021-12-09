test_that("with bonds, with iteration, using default settings output is unchanged
          and names have the suffix '_arg'", {
  skip_if_not(opt_in_snapshots())
  skip_on_ci()
  skip_on_cran()
  skip_slow_tests()

  in_agnostic <- Sys.getenv("ST_AGNOSTIC")
  in_specific <- Sys.getenv("ST_SPECIFIC_BONDS")
  out <- tempfile()
  fs::dir_create(out)

  suppressed_console_output <- suppressWarnings(capture.output(
    results <- run_stress_test("bonds",
      input_path_project_agnostic = in_agnostic,
      input_path_project_specific = in_specific,
      output_path = out,
      term = 1:2,
      return_results = TRUE
    )
  ))

  expect_snapshot(lapply(results, as.data.frame))
})

test_that("with loans, without iteration, using minimal values of input
          arguments output is unchanged", {
  skip_if_not(opt_in_snapshots())
  skip_on_ci()
  skip_on_cran()
  skip_slow_tests()

  in_agnostic <- Sys.getenv("ST_AGNOSTIC")
  in_specific <- Sys.getenv("ST_SPECIFIC_LOANS")
  out <- tempfile()
  fs::dir_create(out)

  suppressed_console_output <- suppressWarnings(capture.output(
    results <- run_stress_test("loans",
      input_path_project_agnostic = in_agnostic,
      input_path_project_specific = in_specific,
      output_path = out,
      term = 1,
      return_results = TRUE
    )
  ))

  expect_snapshot(lapply(results, as.data.frame))
})

test_that("with equity, without iteration, using minimal values of input
          arguments output is unchanged", {
  skip_if_not(opt_in_snapshots())
  skip_on_ci()
  skip_on_cran()
  skip_slow_tests()

  in_agnostic <- Sys.getenv("ST_AGNOSTIC")
  in_specific <- Sys.getenv("ST_SPECIFIC_EQUITY")
  out <- tempfile()
  fs::dir_create(out)

  suppressed_console_output <- suppressWarnings(capture.output(
    results <- run_stress_test("equity",
      input_path_project_agnostic = in_agnostic,
      input_path_project_specific = in_specific,
      output_path = out,
      term = 1:2,
      return_results = TRUE
    )
  ))

  expect_snapshot(lapply(results, as.data.frame))
})
