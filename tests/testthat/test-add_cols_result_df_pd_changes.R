test_that("without specified arguments, add_cols_result_df_pd_changes()
          throws error", {
  testthat::expect_error(
    add_cols_result_df_pd_changes(),
    "argument \"data\" is missing"
  )
})

test_that("without argument for horizon, add_cols_result_df_pd_changes()
          throws error", {
  test_input_merton_overall_ls <- read_test_data("input_merton_overall_late_sudden.csv")

  testthat::expect_error(
    add_cols_result_df_pd_changes(data = test_input_merton_overall_ls),
    "Must provide input for 'horizon'"
  )
})

test_that("with expected data input, add_cols_result_df_pd_changes() returns
          data.frame", {
  test_input_merton_overall_ls <- read_test_data("input_merton_overall_late_sudden.csv")

  test_horizon <- "overall"

  create_empty_result <- add_cols_result_df_pd_changes(
    data = test_input_merton_overall_ls,
    horizon = test_horizon
  )

  testthat::expect_s3_class(create_empty_result, "data.frame")
})

test_that("with expected data input, add_cols_result_df_pd_changes() returns
          same number of rows as data input and initial columns plus an
          additional set of model output columns", {
  test_input_merton_overall_ls <- read_test_data("input_merton_overall_late_sudden.csv")

  test_horizon <- "overall"

  rename_and_add_cols <- add_cols_result_df_pd_changes(
    data = test_input_merton_overall_ls,
    horizon = test_horizon
  )

  add_model_output_cols <- c("Survival")

  testthat::expect_equal(
    nrow(rename_and_add_cols),
    nrow(test_input_merton_overall_ls)
  )
  testthat::expect_equal(
    ncol(rename_and_add_cols),
    ncol(test_input_merton_overall_ls) + length(add_model_output_cols)
  )
})

test_that("with expected data input, add_cols_result_df_pd_changes() contains
          a set of model output columns with the suffix \"_baseline\"", {
  test_input_merton_overall_ls <- read_test_data("input_merton_overall_late_sudden.csv")

  test_horizon <- "overall"

  rename_and_add_cols <- add_cols_result_df_pd_changes(
    data = test_input_merton_overall_ls,
    horizon = test_horizon
  )

  changed_col_names <- c(
    "Survival_baseline"
  )

  columns_renamed <- all(
    changed_col_names %in% colnames(rename_and_add_cols)
  )

  testthat::expect_equal(
    columns_renamed,
    TRUE
  )
})
