#' For a given parameter, aggregates all the outputs where this parameter has been
#' tweaked by querying the MLFLow server where the experiment runs are stored.
#'
#' @param tracking_uri address of the mlflow server.
#'   Defaults to locally run server on port 5000.
#' @param experiment_name name of the experiment where the search for the runs
#'    and associated artifacts
#' @param tweaked_parameter name of the parameter being tweaked. The possible values
#'    are the names of the parameters defined in the `params_grid` variable when
#'    running multirun_trisk_mlflow()
#' @param csv_filename name of the file returned as an output. Possible values are
#'    "crispy_output.csv" or "company_trajectories.csv"
#' @export
gather_trisk_outputs_for_tweaked_parameter <-
  function(tracking_uri = "http://127.0.0.1:5000",
           experiment_name,
           tweaked_parameter,
           csv_filename = "crispy_output.csv") {
    tweaked_parameter_runs <- get_tweaked_parameter_runs(
      tracking_uri, experiment_name, tweaked_parameter
    )

    tweaked_parameter_runs_data <- NULL
    for (run_id in tweaked_parameter_runs[["run_uuid"]]) {
      tryCatch(
        {
          csv_artifact <- read_csv_from_zipped_artifacts(
            tracking_uri,
            experiment_name,
            run_id,
            csv_filename = csv_filename
          )
          run_id_row <- tweaked_parameter_runs[tweaked_parameter_runs$run_uuid == run_id, ]

          run_params <- run_id_row$params[[1]]
          param_value <- run_params[run_params$key == tweaked_parameter, ]$value

          run_data <- csv_artifact %>%
            dplyr::mutate(
              scenario_duo = paste(baseline_scenario, shock_scenario, sep = "&"),
              tweaked_param_name = tweaked_parameter,
              tweaked_param_value = param_value
            )

          tweaked_parameter_runs_data <- dplyr::bind_rows(tweaked_parameter_runs_data, run_data)
        },
        error = function(cond) {
          print(cond)
        }
      )
    }
    return(tweaked_parameter_runs_data)
  }


get_tweaked_parameter_runs <- function(tracking_uri, experiment_name, tweaked_parameter) {
  mlflow::mlflow_set_tracking_uri(uri = tracking_uri)
  mlflow::mlflow_client()
  experiment <- mlflow::mlflow_get_experiment(name = experiment_name)
  experiment_id <- experiment[[1, "experiment_id"]]

  if (!is.null(tweaked_parameter)) {
    str_filter <- paste(
      paste("tags.", tweaked_parameter, " = 'TRUE'", sep = "", collapse = " and "),
      " and ",
      "tags.LOG_STATUS = 'SUCCESS'",
      sep = ""
    )
  }

  # fetch the run uuids with tag matching the current tweaked_parameter
  tweaked_parameter_runs <-
    mlflow::mlflow_search_runs(
      filter = str_filter,
      experiment_ids = as.character(experiment_id)
    )

  return(tweaked_parameter_runs)
}

read_csv_from_zipped_artifacts <- function(tracking_uri, experiment_name, run_id, csv_filename) {
  mlflow::mlflow_set_tracking_uri(uri = tracking_uri)
  mlflow::mlflow_client()

  artifacts_path <- mlflow::mlflow_download_artifacts(path = "", run_id = run_id)
  f_conn <- unz(file.path(artifacts_path, "artifacts.zip"), csv_filename)
  artifact <- readr::read_csv(f_conn, show_col_types = FALSE)
  return(artifact)
}
