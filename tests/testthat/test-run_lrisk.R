test_that("with iteration, using default settings output is unchanged", {
  skip_if_not(opt_in_snapshots())
  skip_on_ci()
  skip_on_cran()
  skip_slow_tests()

  in_agnostic <- fs::path("test_data", "ST_INPUTS_DEV")
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

  results$crispy_output <- results$crispy_output |> dplyr::select(-c(.data$run_id))
  results$company_trajectories <- results$company_trajectories |> dplyr::select(-c(.data$run_id))

  expect_snapshot(lapply(results, as.data.frame))
})
