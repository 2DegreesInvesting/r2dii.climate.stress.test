test_that("calculate_net_profits penalizes companies for late build out of low
          carbon technologies", {
  input_data <- tibble::tribble(
    ~company_name, ~baseline, ~late_sudden, ~Baseline_price, ~late_sudden_price, ~net_profit_margin, ~direction, ~proximity_to_target,
    "leader", 100, 150, 10, 10, 0.1, "increasing", 1,
    "laggard", 100, 150, 10, 10, 0.1, "increasing", 0.5
  )

  net_profits <- calculate_net_profits(input_data)

  net_profits_baseline_climate_leader <- net_profits %>%
    dplyr::filter(.data$company_name == "leader") %>%
    dplyr::pull(.data$net_profits_baseline)

  net_profits_baseline_climate_laggard <- net_profits %>%
    dplyr::filter(.data$company_name == "laggard") %>%
    dplyr::pull(.data$net_profits_baseline)

  net_profits_late_sudden_climate_leader <- net_profits %>%
    dplyr::filter(.data$company_name == "leader") %>%
    dplyr::pull(.data$net_profits_ls)

  net_profits_late_sudden_climate_laggard <- net_profits %>%
    dplyr::filter(.data$company_name == "laggard") %>%
    dplyr::pull(.data$net_profits_ls)

  testthat::expect_equal(
    net_profits_baseline_climate_leader,
    net_profits_baseline_climate_laggard
  )
  testthat::expect_gt(
    net_profits_late_sudden_climate_leader,
    net_profits_late_sudden_climate_laggard
  )
})

test_that("calculate_net_profits does not apply penalty on lost profits for high
          carbon technologies", {
  input_data <- tibble::tribble(
    ~company_name, ~baseline, ~late_sudden, ~Baseline_price, ~late_sudden_price, ~net_profit_margin, ~direction, ~proximity_to_target,
    "leader", 100, 50, 10, 10, 0.1, "declining", 1,
    "laggard", 100, 50, 10, 10, 0.1, "declining", 0.5
  )

  net_profits <- calculate_net_profits(input_data)

  net_profits_baseline_climate_leader <- net_profits %>%
    dplyr::filter(.data$company_name == "leader") %>%
    dplyr::pull(.data$net_profits_baseline)

  net_profits_baseline_climate_laggard <- net_profits %>%
    dplyr::filter(.data$company_name == "laggard") %>%
    dplyr::pull(.data$net_profits_baseline)

  net_profits_late_sudden_climate_leader <- net_profits %>%
    dplyr::filter(.data$company_name == "leader") %>%
    dplyr::pull(.data$net_profits_ls)

  net_profits_late_sudden_climate_laggard <- net_profits %>%
    dplyr::filter(.data$company_name == "laggard") %>%
    dplyr::pull(.data$net_profits_ls)

  testthat::expect_equal(
    net_profits_baseline_climate_leader,
    net_profits_baseline_climate_laggard
  )
  testthat::expect_equal(
    net_profits_late_sudden_climate_leader,
    net_profits_late_sudden_climate_laggard
  )
})
