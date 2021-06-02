test_that("production trajectories function returns a ggplot object", {
  test_data_annual_profits <- readRDS(
    testthat::test_path("test_data", "test_show_carbon_budgets.rds")
  )

  test_scenario_data <- readRDS(
    testthat::test_path("test_data", "test_show_carbon_budgets_scenarios.rds")
  )

  target_scenario_test <- "SDS"
  scenario_name_test <- "Carbon balance 2030"

  ggplot_carbon_budgets <- show_carbon_budget(
    data = test_data_annual_profits,
    scenarios = test_scenario_data,
    target_scenario = target_scenario_test,
    scenario_name_qa = scenario_name_test,
    cumulative = FALSE
  )

  testthat::expect_s3_class(ggplot_carbon_budgets, "ggplot")
})
