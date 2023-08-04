devtools::load_all()

# mlflow server --backend-store-uri dummy_project/mlruns --default-artifact-root dummy_project/mlartifacts --serve-artifacts --host 127.0.0.1 --port 5000

mlflow_python_bin <-
  "/Users/bertrandgallice/opt/miniconda3/envs/mlflow_env/bin/python"
mlflow_bin <-
  "/Users/bertrandgallice/opt/miniconda3/envs/mlflow_env/bin/mlflow"

Sys.setenv(
  MLFLOW_PYTHON_BIN = mlflow_python_bin,
  MLFLOW_BIN = mlflow_bin
)


# main <- function() {
baseline_scenarios <- c(
  "WEO2021_STEPS", "GECO2021_CurPol", "WEO2021_APS", "NGFS2021_GCAM_CP",
  "NGFS2021_GCAM_NDC", "NGFS2021_MESSAGE_CP", "NGFS2021_MESSAGE_NDC",
  "NGFS2021_REMIND_CP", "NGFS2021_REMIND_NDC", "IPR2021_baseline",
  "Oxford2021_base"
)
shock_scenarios <- c(
  "WEO2021_SDS", "WEO2021_NZE_2050", "GECO2021_1.5C-Unif", "GECO2021_NDC-LTS",
  "NGFS2021_GCAM_B2DS", "NGFS2021_GCAM_DN0", "NGFS2021_GCAM_DT",
  "NGFS2021_GCAM_NZ2050", "NGFS2021_MESSAGE_B2DS", "NGFS2021_MESSAGE_DN0",
  "NGFS2021_MESSAGE_DT", "NGFS2021_MESSAGE_NZ2050", "NGFS2021_REMIND_B2DS",
  "NGFS2021_REMIND_DN0", "NGFS2021_REMIND_DT", "NGFS2021_REMIND_NZ2050",
  "IPR2021_FPS", "IPR2021_RPS", "Oxford2021_fast"
)
# baseline_scenarios <- c("WEO2021_STEPS")
# shock_scenarios <- c("WEO2021_SDS")
# baseline_scenarios <- c("NGFS2021_MESSAGE_CP", "NGFS2021_MESSAGE_NDC")
# shock_scenarios <- c("NGFS2021_MESSAGE_B2DS", "NGFS2021_MESSAGE_DN0",
#                      "NGFS2021_MESSAGE_DT", "NGFS2021_MESSAGE_NZ2050")
# baseline_scenarios <- c("Oxford2021_base")
# shock_scenarios <- c("Oxford2021_fast")
scenario_pairs <- expand.grid(
  baseline_scenario = baseline_scenarios,
  shock_scenario = shock_scenarios
)

params_grid <- list(
  discount_rate = c(0.015, 0.04, 0.07, 0.1),
  # lgd = c(0.3, 0.45, 0.6, 0.75, 0.9),
  risk_free_rate = c(0, 0.02, 0.05),
  growth_rate = c(0.01, 0.03, 0.099),
  # div_netprofit_prop_coef = c(0.8, 0.85, 0.9, 0.95, 1),
  shock_year = c(2025, 2027, 2029, 2030, 2032, 2034, 2035),
  # scenario_geography = c("Global"),
  # settlement_factor = c(0, 0.3, 0.6, 1),
  # exp_share_damages_paid = c(0, 0.027, 0.1, 0.5, 1),
  # scc = c(0, 40, 400, 4000, 10000),
  carbon_price_model = c(
    "no_carbon_tax", "NZ2050", "NDC", "DN0", "B2DS")
  # carbon_price_model= c("no_carbon_tax", "NZ2050", "NDC", "DN0", "B2DS")
  # market_passthrough = c(0, 0.3, 0.6, 1)
)

# In a single experiment, do not change the names of parameters being tweaked in the param grid

multirun_trisk_mlflow(
  tracking_uri = "http://127.0.0.1:5000",
  experiment_name = "good_newdata_long",
  trisk_input_path = "good_new_st_inputs/input",
  trisk_output_path = "good_new_st_inputs/output",
  scenario_pairs = scenario_pairs,
  params_grid = params_grid,
  artifact_names = c("crispy_output"),
  max_param_combinations = 1,
)
# }
