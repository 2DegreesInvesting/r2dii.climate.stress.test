test_that("without iteration, using minimum values of input arguments output is unchanged", {
  skip_if_not(opt_in_snapshots())
  skip_on_ci()
  skip_on_cran()
  skip_slow_tests()

  in_agnostic <- fs::path("test_data", "ST_INPUTS_DEV")
  out <- tempfile()
  fs::dir_create(out)

  suppressed_console_output <- suppressWarnings(suppressMessages(capture.output(
    results <- run_trisk(
      input_path = in_agnostic,
      output_path = out,
      lgd = get_st_argument("lgd", "min"),
      risk_free_rate = get_st_argument("risk_free_rate", "min"),
      discount_rate = get_st_argument("discount_rate", "min"),
      growth_rate = get_st_argument("growth_rate", "min"),
      div_netprofit_prop_coef = get_st_argument("div_netprofit_prop_coef", "min"),
      shock_year = get_st_argument("shock_year", "min"),
      return_results = TRUE
    )
  )))

  results$crispy_output <- results$crispy_output |> dplyr::select(-c(.data$run_id))
  results$company_trajectories <- results$company_trajectories |> dplyr::select(-c(.data$run_id))

  expect_snapshot(lapply(results, as.data.frame))
})

test_that("without iteration, using maximum values of input arguments output is unchanged", {
  skip_if_not(opt_in_snapshots())
  skip_on_ci()
  skip_on_cran()
  skip_slow_tests()

  in_agnostic <- fs::path("test_data", "ST_INPUTS_DEV")
  out <- tempfile()
  fs::dir_create(out)

  suppressed_console_output <- suppressWarnings(suppressMessages(capture.output(
    results <- run_trisk(
      input_path = in_agnostic,
      output_path = out,
      scenario_geography = "OECD",
      lgd = get_st_argument("lgd", "max"),
      risk_free_rate = get_st_argument("risk_free_rate", "max"),
      discount_rate = get_st_argument("discount_rate", "max"),
      growth_rate = get_st_argument("growth_rate", "max"),
      div_netprofit_prop_coef = get_st_argument("div_netprofit_prop_coef", "max"),
      shock_year = get_st_argument("shock_year", "max"),
      return_results = TRUE
    )
  )))

  results$crispy_output <- results$crispy_output |> dplyr::select(-c(.data$run_id))
  results$company_trajectories <- results$company_trajectories |> dplyr::select(-c(.data$run_id))

  expect_snapshot(lapply(results, as.data.frame))
})

test_that("All scenario combinations are valid", {
  skip_if_not(opt_in_snapshots())
  skip_on_ci()
  skip_on_cran()
  skip_slow_tests()

  in_agnostic <- fs::path("test_data", "ST_INPUTS_DEV")
  scenario_geography_x_ald_sector <- get_scenario_geography_x_ald_sector(in_agnostic) |>
    dplyr::distinct(.data$baseline_scenario, .data$shock_scenario, .data$scenario_geography)

  error_happened <- FALSE
  for (i in 1:nrow(scenario_geography_x_ald_sector)) {
    row_params <- scenario_geography_x_ald_sector[i, ]
    tryCatch({
      suppressWarnings(suppressMessages(capture.output(
        run_trisk(
          input_path = in_agnostic,
          output_path = tempdir(),
          baseline_scenario = row_params$baseline_scenario,
          shock_scenario = row_params$shock_scenario,
          scenario_geography = row_params$scenario_geography
        )
      )))

      cat(paste("Pass", row_params, '\n'))
    },
    error = function(e) {
      cat(paste("Failed", row_params, '\n'))
      error_happened <- TRUE
    })
  }

  expect_false(error_happened)
})
