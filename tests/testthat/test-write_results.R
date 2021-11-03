test_that("without specified arguments, write_results throws error", {
  testthat::expect_error(write_results())
})

test_that("with invalid asset type, write_results throws error", {
  x <- readr::read_rds(testthat::test_path("test_data", "test_data-write-results.rds"))
  result_path <- tempdir()
  investorname_test <- "test_investor"

  target_dir <- test_create_target_directory(
    path = result_path,
    add_level = investorname_test
  )

  testthat::expect_error(
    write_results(
      data = x,
      path_to_results = result_path,
      investorname = investorname_test,
      asset_type = "blah",
      level = "company",
      file_type = "csv"
    ),
    "valid_asset_type"
  )
})

test_that("with invalid asset type, write_results throws error", {
  x <- readr::read_rds(testthat::test_path("test_data", "test_data-write-results.rds"))
  result_path <- tempdir()
  investorname_test <- "test_investor"

  target_dir <- test_create_target_directory(
    path = result_path,
    add_level = investorname_test
  )

  testthat::expect_error(
    write_results(
      data = x,
      path_to_results = result_path,
      investorname = investorname_test,
      asset_type = "equity",
      level = "company",
      file_type = "tsv"
    ),
    "valid_file_type"
  )
})

test_that("with valid inputs, write_results throws no error", {
  x <- readr::read_rds(testthat::test_path("test_data", "test_data-write-results.rds"))
  result_path <- tempdir()
  investorname_test <- "test_investor"

  target_dir <- test_create_target_directory(
    path = result_path,
    add_level = investorname_test
  )

  run_with_valid_inputs <- write_results(
    data = x,
    path_to_results = result_path,
    investorname = investorname_test,
    asset_type = "equity",
    level = "company",
    file_type = "csv"
  )

  testthat::expect_error(run_with_valid_inputs, NA)
})

test_that("company/portfolio level results produce csv with correct number
          of columns for company level pacta input", {
  x <- readr::read_rds(testthat::test_path("test_data", "test_data-write-results.rds"))
  result_path <- tempdir()
  investorname_test <- "test_investor"

  expect_ncol_comp_input_comp_output <- 29
  expect_ncol_comp_input_pf_output <- 22

  target_dir <- test_create_target_directory(
    path = result_path,
    add_level = investorname_test
  )

  run_with_valid_inputs <- write_results(
    data = x,
    path_to_results = result_path,
    investorname = investorname_test,
    asset_type = "equity",
    level = "company",
    file_type = "csv"
  )

  test_comp <- readr::read_csv(
    file.path(
      result_path,
      investorname_test,
      "stress_test_results_equity_comp.csv"
    ),
    col_types = readr::cols()
  )
  testthat::expect_equal(ncol(test_comp), expect_ncol_comp_input_comp_output)

  test_port <- readr::read_csv(
    file.path(
      result_path,
      investorname_test,
      "stress_test_results_equity_port.csv"
    ),
    col_types = readr::cols()
  )
  testthat::expect_equal(ncol(test_port), expect_ncol_comp_input_pf_output)
})

test_that("portfolio level results produce csv with correct number of columns
          for portfolio level pacta input", {
  x <- readr::read_rds(testthat::test_path("test_data", "test_data-write-results-pf.rds"))
  result_path <- tempdir()
  investorname_test <- "test_investor"

  expect_ncol_pf_input_pf_output <- 18

  target_dir <- test_create_target_directory(
    path = result_path,
    add_level = investorname_test
  )

  run_with_valid_inputs <- write_results(
    data = x,
    path_to_results = result_path,
    investorname = investorname_test,
    asset_type = "equity",
    level = "portfolio",
    file_type = "csv"
  )

  test_port <- readr::read_csv(
    file.path(
      result_path,
      investorname_test,
      "equity_results_stress_test.csv"
    ),
    col_types = readr::cols()
  )
  testthat::expect_equal(ncol(test_port), expect_ncol_pf_input_pf_output)
})
