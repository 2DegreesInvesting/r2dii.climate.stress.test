test_that("without iteration, using minimum values of input arguments output is unchanged", {
  skip_if_not(opt_in_snapshots())
  skip_on_ci()
  skip_on_cran()
  skip_slow_tests()

  in_agnostic <- here::here(fs::path("test_data","ST_INPUTS_DEV"))
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

  expect_snapshot(lapply(results, as.data.frame))
})

test_that("without iteration, using maximum values of input arguments output is unchanged", {
  skip_if_not(opt_in_snapshots())
  skip_on_ci()
  skip_on_cran()
  skip_slow_tests()

  in_agnostic <- here::here(fs::path("test_data","ST_INPUTS_DEV"))
  out <- tempfile()
  fs::dir_create(out)


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


  expect_snapshot(lapply(results, as.data.frame))
})
