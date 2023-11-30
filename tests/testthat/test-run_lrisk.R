test_that("with iteration, using default settings output is unchanged", {
  skip_if_not(opt_in_snapshots())
  skip_on_ci()
  skip_on_cran()
  skip_slow_tests()

  in_agnostic <- here::here(fs::path("test_data","ST_INPUTS_DEV"))
  out <- tempfile()
  fs::dir_create(out)

  suppressed_console_output <- suppressWarnings(capture.output(
    results <- run_lrisk(
      input_path = in_agnostic,
      output_path = out,
      shock_year = c(2025, 2030),
      return_results = TRUE
    )
  ))

  expect_snapshot(lapply(results, as.data.frame))
})
