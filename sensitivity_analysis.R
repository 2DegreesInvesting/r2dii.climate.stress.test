library(r2dii.climate.stress.test)

# mlflow server --backend-store-uri dummy_project/mlruns --default-artifact-root dummy_project/mlartifacts --serve-artifacts --host 127.0.0.1 --port 5000

mlflow_python_bin <-
  "/Users/bertrandgallice/opt/miniconda3/envs/mlflow_env/bin/python"
mlflow_bin <-
  "/Users/bertrandgallice/opt/miniconda3/envs/mlflow_env/bin/mlflow"

Sys.setenv(MLFLOW_PYTHON_BIN = mlflow_python_bin,
           MLFLOW_BIN = mlflow_bin)


main2 <- function(){
get_mlflow_runs_artifacts(
  tracking_uri = "http://127.0.0.1:5000",
  experiment_name = "TEST64",
  parameter_focus="risk_free_rate")
}

main <- function() {
  scenario_pairs = tibble::tribble(
    ~ baseline_scenario,
    ~ shock_scenario,
    "WEO2021_STEPS",
    "WEO2021_SDS",
    "WEO2021_APS",
    "WEO2021_NZE_2050"
  )
  params_grid = list(
    discount_rate = c(0.015, 0.04, 0.07, 0.1),
    lgd = c(0.3, 0.45, 0.6, 0.75, 0.9),
    risk_free_rate = c(0, 0.02, 0.05),
    growth_rate = c(0.01, 0.03, 0.099),
    div_netprofit_prop_coef = c(0.8, 0.85, 0.9, 0.95, 1),
    shock_year = c(2025, 2027, 2029, 2030, 2032, 2034, 2035),
    scenario_geography = c("Global"),
    settlement_factor = c(0, 0.3, 0.6, 1),
    exp_share_damages_paid = c(0, 0.027, 0.1, 0.5, 1),
    scc = c(0, 40, 400, 4000, 10000),
    carbon_price_model = c("no_carbon_tax", "NZ2050", "NDC", "DN0", "B2DS"),
    market_passthrough = c(0, 0.3, 0.6, 1)
  )


  multirun_trisk_mlflow(
    tracking_uri = "http://127.0.0.1:5000",
    experiment_name = "sa_st_master",
    trisk_input_path = "sensitivity_analysis/project_input",
    trisk_output_path = "sensitivity_analysis/output",
    scenario_pairs = scenario_pairs,
    params_grid = params_grid
  )
}



#================================================================
# UTILS FUNCTIONS
#================================================================
