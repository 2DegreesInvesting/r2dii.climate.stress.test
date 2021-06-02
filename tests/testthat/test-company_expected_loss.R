test_that("without specified arguments, company_expected_loss throws error", {
  testthat::expect_error(
    company_expected_loss(),
    "argument \"data\" is missing"
  )
})

test_that("with missing argument for loss_given_default, company_expected_loss
          throws error", {
  test_data <- read_test_data("loanbook_pd_changes.csv")
  test_ead <- read_test_data("loanbook_exposure_at_default.csv")
  test_port_aum <- read_test_data("loanbook_aum.csv")

  testthat::expect_error(
    company_expected_loss(
      data = test_data,
      exposure_at_default = test_ead,
      port_aum = test_port_aum
    ),
    "Must provide input for 'loss_given_default'"
  )
})

test_that("with missing argument for shock_year, calculate_pd_change throws
          error", {
  test_data <- read_test_data("loanbook_pd_changes.csv")
  test_lgd <- read_test_data("loss_given_default_by_sector.csv")
  test_ead <- read_test_data("loanbook_exposure_at_default.csv")
  test_port_aum <- read_test_data("loanbook_aum.csv")

  test_results <- company_expected_loss(
    data = test_data,
    loss_given_default = test_lgd,
    exposure_at_default = test_ead,
    port_aum = test_port_aum
  )

  testthat::expect_s3_class(test_results, "data.frame")
})

test_that("expected losses point in correct direction", {
  test_data <- read_test_data("loanbook_pd_changes.csv")
  test_lgd <- read_test_data("loss_given_default_by_sector.csv")
  test_ead <- read_test_data("loanbook_exposure_at_default.csv")
  test_port_aum <- read_test_data("loanbook_aum.csv")

  test_results <- company_expected_loss(
    data = test_data,
    loss_given_default = test_lgd,
    exposure_at_default = test_ead,
    port_aum = test_port_aum
  )

  results_direction <- test_results %>%
    dplyr::mutate(
      expect_increased_loss = dplyr::if_else(
        .data$PD_change > 0 & .data$exposure_at_default > 0,
        TRUE,
        FALSE
      )
    ) %>%
    dplyr::mutate(
      check_increased_loss = dplyr::if_else(
        .data$expected_loss_late_sudden > .data$expected_loss_baseline,
        TRUE,
        FALSE
      )
    )

  testthat::expect_equal(
    results_direction$expect_increased_loss,
    results_direction$check_increased_loss
  )
})
