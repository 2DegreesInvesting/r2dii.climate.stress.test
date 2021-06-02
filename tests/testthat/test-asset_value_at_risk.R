test_that("without specified arguments, asset_value_at_risk throws error", {
  testthat::expect_error(
    asset_value_at_risk(),
    "argument \"data\" is missing"
  )
})

test_that("with missing argument for plan_carsten, asset_value_at_risk
          throws error", {
  test_annual_profits <- read_test_data("annual_profits.csv")
  test_shock_scenario <- read_test_data("shock_scenario.csv")
  test_port_aum <- read_test_data("port_aum.csv")

  test_terminal_value <- 0
  test_proportionality_coefficient <- 1
  test_flat_multiplier <- 1

  testthat::expect_error(
    asset_value_at_risk(
      data = test_annual_profits,
      terminal_value = test_terminal_value,
      shock_scenario = test_shock_scenario,
      div_netprofit_prop_coef = test_proportionality_coefficient,
      port_aum = test_port_aum,
      flat_multiplier = test_flat_multiplier
    ),
    "Must provide input for 'plan_carsten'"
  )
})

test_that("with all required arguments provided, asset_value_at_risk returns
          data.frame", {
  test_annual_profits <- read_test_data("annual_profits.csv")
  test_shock_scenario <- read_test_data("shock_scenario.csv")
  test_plan_carsten <- read_test_data("portfolio_plan_carsten.csv")
  test_port_aum <- read_test_data("port_aum.csv")

  test_terminal_value <- 0
  test_proportionality_coefficient <- 1
  test_flat_multiplier <- 1

  test_results <- asset_value_at_risk(
    data = test_annual_profits,
    terminal_value = test_terminal_value,
    shock_scenario = test_shock_scenario,
    div_netprofit_prop_coef = test_proportionality_coefficient,
    plan_carsten = test_plan_carsten,
    port_aum = test_port_aum,
    flat_multiplier = test_flat_multiplier
  )

  testthat::expect_s3_class(test_results, "data.frame")
})
