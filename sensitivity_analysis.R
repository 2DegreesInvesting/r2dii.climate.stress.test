library(mlflow)
library(r2dii.climate.stress.test)

run_trisk_mlflow <- function(tracking_uri, experiment_name, ...) {

  # starts mlflow client to connect to mlflow server
  mlflow::mlflow_set_tracking_uri(uri = tracking_uri)
  mlflow::mlflow_client()

  # creates mlflow experiment if it doesn't exist, else gets it
  tryCatch({
    mlflow::mlflow_get_experiment(name = experiment_name)
  },
  error = function(cond) {
    mlflow::mlflow_create_experiment(experiment_name)
  })
  mlflow::mlflow_set_experiment(experiment_name)

  with(mlflow::mlflow_start_run(), {
    # log all parameters in current run. Also logs default parameters of
    # run_trisk, so all running parameters are logged in mlflow.
    parameters_list <- list(...)
    default_args <- formals(run_trisk)
    default_args <- default_args[!(names(default_args) %in% names(parameters_list))]
    all_params <- c(parameters_list, default_args)

    for (param_name in names(all_params)) {
      if (!(param_name %in% c("input_path", "output_path", "return_results"))) {
        mlflow::mlflow_log_param(param_name, all_params[[param_name]])
      }
    }
    # creates a temporary output directory to save TRISK outputs
    # TODO return the name of the output folder in the run_trisk output
    #  and remove this mechanic
    mlflow_run_output_dir <- tempfile()
    dir.create(mlflow_run_output_dir, recursive = TRUE)

    time_spent <- system.time({
      st_results_wrangled_and_checked <-
        r2dii.climate.stress.test::run_trisk(return_results = TRUE, ...)
    })

    time_spent <- tibble::as_tibble(as.list(time_spent))
    readr::write_delim(time_spent, file.path(mlflow_run_output_dir, "time_spent.csv"), delim=",")


    r2dii.climate.stress.test:::write_stress_test_results(
      results_list = st_results_wrangled_and_checked,
      iter_var = "",
      output_path = mlflow_run_output_dir
    )
    mlflow::mlflow_log_artifact(path = mlflow_run_output_dir)

    # deletes temp directory
    unlink(mlflow_run_output_dir, recursive = TRUE)

  })
}

multirun_trisk_mlflow <-
  function(tracking_uri="http://localhost:5000",
           experiment_name=NULL,
           trisk_input_path,
           trisk_output_path,
           scenario_pairs,
           params_grid) {

    auto_experiment_name <- ifelse(is.null(experiment_name), TRUE, FALSE)

    # iterates over scenario pairs. If no experiment name is defined,
    # the experiment name is the join of the baseline and shock scenario names.
    for (i in 1:length(scenario_pairs)) {
      baseline_scenario = scenario_pairs[[i, "baseline_scenario"]]
      shock_scenario = scenario_pairs[[i, "shock_scenario"]]

      experiment_name <- ifelse(auto_experiment_name,
                                paste(baseline_scenario, shock_scenario, sep="-"),
                                experiment_name)

      # iterates over the params defined in the params grid,
      # then over the values defined for this parameter.
      for (param_name in names(params_grid)) {
        for (param_value in params_grid[[param_name]]) {
          param_value = ifelse(is.character(param_value),
                               paste('"', param_value, '"'),
                               param_value)
          eval(parse(
            text = paste(
              "run_trisk_mlflow(",
              'tracking_uri = "', tracking_uri, '",',
              'experiment_name = "', experiment_name, '",',
              'input_path = "', trisk_input_path, '",',
              'output_path = "', trisk_output_path, '",',
              'baseline_scenario = "', baseline_scenario, '",',
              'shock_scenario = "', shock_scenario, '",',
              param_name," = ", param_value,
              ")"
              , sep="")
          ))

        }
      }


    }
  }

fetch_mlflow_runs <- function() {

}

#mlflow server --backend-store-uri dummy_project/mlruns --default-artifact-root dummy_project/mlartifacts --host 127.0.0.1 --port 5000

mlflow_python_bin <-
  "/Users/bertrandgallice/opt/miniconda3/envs/mlflow_env/bin/python"
mlflow_bin <-
  "/Users/bertrandgallice/opt/miniconda3/envs/mlflow_env/bin/mlflow"

Sys.setenv(MLFLOW_PYTHON_BIN = mlflow_python_bin,
           MLFLOW_BIN = mlflow_bin)

scenario_pairs = tibble::tribble(
  ~ baseline_scenario, ~ shock_scenario,
  "WEO2021_STEPS", "WEO2021_SDS",
  "WEO2021_APS", "WEO2021_NZE_2050"
)
params_grid = list(
  lgd = c(0.3, 0.45, 0.9),
  risk_free_rate = c(0, 0.02, 0.05),
  discount_rate = c(0.015, 0.04, 0.07, 0.1),
  growth_rate = c(0.01, 0.03, 0.099),
  div_netprofit_prop_coef = c(0.8, 0.9, 1),
  shock_year = c(2025, 2030, 2035),
  scenario_geography = c("Global"),
  settlement_factor = c(1),
  #exp_share_damages_paid = c(0.027),
  scc = c(40),
  #carbon_price_model = c("no_carbon_tax", "NZ2050", "NDC", "DN0", "B2DS"),
  carbon_price_model = c("no_carbon_tax"),
  market_passthrough = c(0)
)


multirun_trisk_mlflow(
  tracking_uri = "http://127.0.0.1:5000",
  experiment_name = "TEST2",
  trisk_input_path = "dummy_project/project_input",
  trisk_output_path = "dummy_project/output",
  scenario_pairs = scenario_pairs,
  params_grid = params_grid
)
