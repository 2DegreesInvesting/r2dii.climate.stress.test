# TODO bivariate analysis for discount rate & growth_rate

library(ggplot2)
library(dplyr)

mlflow_python_bin <-
  "/Users/bertrandgallice/opt/miniconda3/envs/mlflow_env/bin/python"
mlflow_bin <-
  "/Users/bertrandgallice/opt/miniconda3/envs/mlflow_env/bin/mlflow"

Sys.setenv(MLFLOW_PYTHON_BIN = mlflow_python_bin,
           MLFLOW_BIN = mlflow_bin)

get_run_ids_matching_tag <- function(tracking_uri, experiment_name, parameter_focus){
  mlflow::mlflow_set_tracking_uri(uri = tracking_uri)
  mlflow::mlflow_client()
  experiment <- mlflow::mlflow_get_experiment(name = experiment_name)
  experiment_id <- experiment[[1, "experiment_id"]]

  # fetch the run uuids with tag matching the current parameter_focus
  parameter_focus_runs <-
    mlflow::mlflow_search_runs(
      filter = paste("tags.", parameter_focus, " = 'TRUE'", sep = ""),
      experiment_ids = as.character(experiment_id)
    )
  parameter_focus_run_ids <- parameter_focus_runs[["run_uuid"]]

  return(parameter_focus_run_ids)
}

get_run_artifact <- function(tracking_uri, experiment_name, run_id, artifact_name){
  mlflow::mlflow_set_tracking_uri(uri = tracking_uri)
  mlflow::mlflow_client()

  artifacts_path <- mlflow::mlflow_download_artifacts(path="", run_id = run_id)
  artifact <- readr::read_csv(file.path(artifacts_path, artifact_name))
  artifact
}


get_npv_roc_plot_data <- function(crispy_output, parameter_focus){
  crispy_output %>%
    dplyr::filter(.data$term == 5) %>%
    dplyr::mutate(
      scenario_duo = paste(baseline_scenario, shock_scenario, sep="_"),
      npv_rateofchange = net_present_value_baseline/net_present_value_shock,
      param_tweeked = parameter_focus,
      param_value = .data[[parameter_focus]]
    ) %>%
    dplyr::select(
      scenario_duo,
      param_tweeked,
      sector,
      business_unit,
      company_name,
      param_value,
      npv_rateofchange,
    )
}

get_pd_diff_plot_data <- function(crispy_output, parameter_focus){
  crispy_output %>%
    dplyr::filter(.data$term == 5) %>%
    dplyr::mutate(
      scenario_duo = paste(baseline_scenario, shock_scenario, sep="_"),
      param_tweeked = parameter_focus,
      param_value = .data[[parameter_focus]]
    ) %>%
    dplyr::select(
      scenario_duo,
      param_tweeked,
      sector,
      business_unit,
      company_name,
      param_value,
      pd_difference,
    )
}

filter_nans_inf <- function(df, filter_col){
  df[!is.na(df[[filter_col]]) & !is.infinite(df[[filter_col]]), ]
}

draw_scatter_npv_rateofchange <- function(npv_roc_plot_data){

}

draw_scatter_pd_difference <- function(pd_diff_plot_data){

}


all_parameters_focus <- c("lgd", "risk_free_rate", "div_netprofit_prop_coef",
                  "shock_year", "market_passthrough")


for (parameter_focus in all_parameters_focus) {
  parameter_focus_run_uuid <- get_run_ids_matching_tag(
    tracking_uri = "http://localhost:5000",
    experiment_name = "sa_st_master",
    parameter_focus = parameter_focus
  )

  npv_roc_plot_data <- NULL
  pd_diff_plot_data <- NULL
  for (run_uuid in parameter_focus_run_uuid) {
    crispy_output <- get_run_artifact(
      tracking_uri = "http://localhost:5000",
      experiment_name = "sa_st_master",
      run_id = run_uuid,
      artifact_name = "crispy_output_.csv"
    )

    npv_roc_plot_data_run <-
      get_npv_roc_plot_data(crispy_output, parameter_focus)
    npv_roc_plot_data <- dplyr::bind_rows(npv_roc_plot_data,
                                          npv_roc_plot_data_run)

    pd_diff_plot_data_run <- get_pd_diff_plot_data(crispy_output, parameter_focus)
    pd_diff_plot_data <- dplyr::bind_rows(pd_diff_plot_data,
                                          pd_diff_plot_data_run)

  }
  npv_roc_plot_data <- filter_nans_inf(npv_roc_plot_data, "npv_rateofchange")
  pd_diff_plot_data <- filter_nans_inf(pd_diff_plot_data, "pd_difference")
  browser()
  npv_rateofchange_plot <- draw_scatter_npv_rateofchange(npv_roc_plot_data)
}


#' #' download artifacts of all runs given a specific parameter marginally tuned
#' get_mlflow_runs_artifacts <- function(tracking_uri, experiment_name, parameter_focus) {
#'
#'   mlflow::mlflow_set_tracking_uri(uri = tracking_uri)
#'   mlflow::mlflow_client()
#'   experiment <- mlflow::mlflow_get_experiment(name = experiment_name)
#'
#'   experiment_id <- experiment[[1, "experiment_id"]]
#'
#'   # fetch the run uuids with tag matching the current parameter_focus
#'   parameter_focus_runs <-
#'     mlflow::mlflow_search_runs(
#'       filter = paste("tags.", parameter_focus, " = 'TRUE'", sep = ""),
#'       experiment_ids = as.character(experiment_id)
#'     )
#'   parameter_focus_run_ids <- parameter_focus_runs[["run_uuid"]]
#'
#'   all_runs_artifacts <- list()
#'   for (run_id in parameter_focus_run_ids){
#'     # path refers to the place of desired artifacts INSIDE the run_id folder
#'     # here it is empty since we want all artifacts, from the root
#'     artifacts_path <- mlflow::mlflow_download_artifacts(path="", run_id = run_id)
#'
#'     run_artifacts <-
#'       list(
#'         company_trajectories = readr::read_csv(file.path(artifacts_path, "company_trajectories_.csv")),
#'         crispy_output_ = readr::read_csv(file.path(artifacts_path, "crispy_output_.csv")),
#'         time_spent = readr::read_csv(file.path(artifacts_path, "time_spent.csv"))
#'       )
#'
#'     all_runs_artifacts[[run_id]] <-  run_artifacts
#'   }
#'   return(all_runs_artifacts)
#' }
