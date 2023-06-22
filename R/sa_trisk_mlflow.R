library(mlflow)

#' This function launches multiple instances of TRISK, each with a different set
#' of parameters. Each run of TRISK and its outputs are logged in MLFlow, under
#' the experiment_name defined in input.
#'
#' The set of parameters is created from the values contained in the params_grid,
#' which are then either combined to test all pairs/triplets/... of parameters (depending
#' on the value set for max_param_combination), or varied individually if this
#' parameter is set to 1.
#' Those parameters combinations are then repeated for each scenario pairs provideo
#' in input.
#'
#' The results of TRISK are logged as artifacts in MLFlow, and can be queried later on
#' for aggregation and analysis.
#'
#' @param tracking_uri address of the mlflow server. Defaults to locally run server on port 5000.
#' @param experiment_name name of the experiment. It is recommended to change this value
#'    when changing the parameters names in the `params_grid` variable.
#' @param trisk_input_path input path for the data to be used by run_trisk
#' @param trisk_output_path output path to save outputs of run_trisk. Not actually
#'    used when logging trisk outputs of
#' @param scenario_pairs table with 2 columns: baseline_scenario, and shock_scenario.
#'    For each row of this table, all parameters values (and eventutally their combinations)
#'    defined in the `param_grid` will be tested in a run.
#' @param params_grid list of parameters, their names mapping to a range of values to
#'    be tested in independant runs.
#' @param max_param_combinations how many parameters defined in the `param_grid` will be combined
#'    when generating the run_parameters table. If equal to 1, each value of a parameter will
#'    generate 1 run. When bigger than 1,
#'    e.g. when equal to 2,
#' @param artifact_names name of the output in the st_results_wrangled_and_checked results
#'    to be logged as artifacts in MLFlow
#' @param additional_tags list of key/values pairs. User-defined tags to be logged with every run
#' @export
multirun_trisk_mlflow <-
  function(tracking_uri = "http://127.0.0.1:5000",
           experiment_name,
           trisk_input_path,
           trisk_output_path,
           scenario_pairs,
           params_grid,
           max_param_combinations = 1,
           artifact_names = c("crispy_output", "company_trajectories"),
           additional_tags = NULL) {
    # starts mlflow client to connect to mlflow server
    mlflow::mlflow_set_tracking_uri(uri = tracking_uri)
    mlflow::mlflow_client()

    set_mlflow_experiment(experiment_name)

    # Creates the dataframe of all runs parameters
    # Gather runs and filter already completed ones
    # cannot check each run 1 by 1 with the server as it is too time consuming.
    filtered_run_parameters <- generate_and_filter_run_parameters(
      tracking_uri,
      experiment_name,
      scenario_pairs,
      params_grid,
      max_param_combinations
    )
    print(paste("Starting the execution of", nrow(filtered_run_parameters), "total runs"))


    n_completed_runs <- 0
    for (i in 1:nrow(filtered_run_parameters)) {
      row_params <- filtered_run_parameters[i, ]
      use_params <- row_params[, which(!is.na(row_params))]

      tags <- create_tags_list(names(use_params), params_grid, additional_tags)
      # aggregate input parameters in a string to then evaluate
      trisk_run_params <- as.list(use_params)
      for (param_name in names(use_params)) {
        param_value <- use_params[[param_name]]
        param_value <- ifelse(is.factor(param_value),
          as.character(param_value),
          param_value
        )
        trisk_run_params[param_name] <- param_value
      }

      do.call(
        run_trisk_mlflow,
        c(
          list(
            tracking_uri = tracking_uri,
            experiment_name = experiment_name,
            tags = tags,
            artifact_names = artifact_names,
            input_path = trisk_input_path,
            output_path = trisk_output_path
          ),
          trisk_run_params
        )
      )

      n_completed_runs <- n_completed_runs + 1
      print(paste("Done", n_completed_runs, "/", nrow(filtered_run_parameters), "total runs"))
    }
  }

#' This function runs 1 instance of TRISK, and logs the result in MLFLow under
#' the experiment name defined in input.
#'
#' There are 4 kinds of outputs that are logged :
#' - Parameters: The input parameters of the TRISK function
#' - Tags : key/values pairs used to search experiments using text filters.
#' - Metrics: key/value pairs where the value is an aggregated quantity created from
#'    the TRISK output, and computed in the compute_trisk_metrics() function.
#'    Metrics can be used in combination with the parameters to create charts
#'    in the MLFLow UI.
#' - Artifacts: Files returned as the output of TRISK. The artifacts can later be
#'    downloaded to aggregate and analyse the results over several runs.
#'
#' @param tracking_uri address of the mlflow server.
#' @param experiment_name name of the experiment
#' @param tags list of key/value pairs to be logged in mlflow with the current run
#' @param artifact_names name of the variables in the TRISK output to be saved as
#'   a CSV, and logged into mlflow
#' @param ... input parameters of run_trisk
run_trisk_mlflow <-
  function(tracking_uri,
           experiment_name,
           tags,
           artifact_names,
           ...) {
    with(mlflow::mlflow_start_run(), {
      for (tag_name in names(tags)) {
        mlflow::mlflow_set_tag(tag_name, tags[[tag_name]])
      }
      # creates a temporary output directory to save TRISK outputs
      # TODO return the name of the output folder in the run_trisk output,
      #  log the artifacts from this folder, and remove this mechanic
      mlflow_run_output_dir <- tempfile()
      dir.create(mlflow_run_output_dir, recursive = TRUE)

      # log all parameters in current run. Also logs default parameters of
      # run_trisk, so all running parameters are logged in mlflow.
      input_params <- list(...)
      default_params <- formals(run_trisk)
      default_params <- default_params[!(names(default_params) %in% names(input_params))]
      all_params <- c(input_params, default_params)
      # log parameters name and their value
      for (param_name in names(all_params)) {
        if (!(param_name %in% c("input_path", "output_path", "return_results"))) {
          mlflow::mlflow_log_param(param_name, all_params[[param_name]])
        }
      }

      tryCatch(
        {
          st_results_wrangled_and_checked <- run_trisk(return_results = TRUE, ...)
          print("TRISK run completed")

          metrics_df <- compute_trisk_metrics(st_results_wrangled_and_checked)
          log_metrics_df(metrics_df)


          if (!is.null(artifact_names)) {
            write_and_zip_csv_artifacts(
              st_results_wrangled_and_checked,
              mlflow_run_output_dir,
              artifact_names
            )
          }

          mlflow::mlflow_set_tag("LOG_STATUS", "SUCCESS")
        },
        error = function(cond) {
          fileConn <- file(file.path(mlflow_run_output_dir, "error_message.txt"))
          writeLines(as.character(cond), fileConn)
          close(fileConn)

          mlflow::mlflow_set_tag("LOG_STATUS", "FAILED")
        },
        finally = {
          browser()
          if (!is.null(artifact_names)) {
            mlflow::mlflow_log_artifact(path = mlflow_run_output_dir)
          }
          # deletes temp directory
          unlink(mlflow_run_output_dir, recursive = TRUE)
        }
      )
    })
  }
