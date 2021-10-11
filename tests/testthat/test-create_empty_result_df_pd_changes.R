test_that("without specified arguments, create_empty_result_df_pd_changes()
          throws error", {
  testthat::expect_error(
    create_empty_result_df_pd_changes(),
    "argument \"data\" is missing"
  )
})

test_that("without argument for horizon, create_empty_result_df_pd_changes()
          throws error", {
  test_input_merton_overall <- read_test_data("input_merton_overall.csv")

  testthat::expect_error(
    create_empty_result_df_pd_changes(data = test_input_merton_overall),
    "Must provide input for 'horizon'"
  )
})

test_that("with expected data input, create_empty_result_df_pd_changes() returns
          data.frame", {
  test_input_merton_overall <- read_test_data("input_merton_overall.csv")

  test_horizon <- "overall"

  create_empty_result <- create_empty_result_df_pd_changes(
    data = test_input_merton_overall,
    horizon = test_horizon
  )

  testthat::expect_s3_class(create_empty_result, "data.frame")
})

test_that("with expected data input, create_empty_result_df_pd_changes() returns
          same number of rows as data input and initial columns plus the model
          output columns", {
  test_input_merton_overall <- read_test_data("input_merton_overall.csv")

  test_horizon <- "overall"

  create_empty_result <- create_empty_result_df_pd_changes(
    data = test_input_merton_overall,
    horizon = test_horizon
  )

  model_output_cols <- c("Survival")

  testthat::expect_equal(
    nrow(create_empty_result),
    nrow(test_input_merton_overall)
  )
  testthat::expect_equal(
    ncol(create_empty_result),
    ncol(test_input_merton_overall) + length(model_output_cols)
  )
})
