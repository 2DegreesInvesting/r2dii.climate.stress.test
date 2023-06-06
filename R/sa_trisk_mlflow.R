library(mlflow)

check_if_run_already_done <-
  function(tracking_uri,
           experiment_name,
           baseline_scenario,
           shock_scenario,
           param_name,
           param_value) {
  mlflow::mlflow_set_tracking_uri(tracking_uri)
  experiment_id <- mlflow::mlflow_get_experiment(name = experiment_name)$experiment_id[1]
  found_runs <- mlflow::mlflow_search_runs(
    filter = paste(
      'tags.LOG_STATUS = "SUCCESS"',
      " and ",
      "params.baseline_scenario = '", baseline_scenario, "'",
      " and ",
      "params.shock_scenario = '", shock_scenario,"'",
      " and ",
      "tags.", param_name, " = 'TRUE'",
      " and ",
      "metrics.", param_name, " = ", param_value,
      sep = ""),
    experiment_ids = as.character(experiment_id)
  )
  if (nrow(found_runs) > 0){
    print(paste("Skipping run with ",
                "params.baseline_scenario = '", baseline_scenario, "'",
                " and ",
                "params.shock_scenario = '", shock_scenario,"'",
                " and ",
                "tags.", param_name, " = 'TRUE'",
                " and ",
                "metrics.", param_name, " = ", param_value,
                " as it's been completed already",
                sep = ""))
    return(T)
  }
  else{
    return(F)
  }
}

#' @export
multirun_trisk_mlflow <-
  function(tracking_uri = "http://localhost:5000",
           experiment_name,
           trisk_input_path,
           trisk_output_path,
           scenario_pairs,
           params_grid,
           save_artifacts=TRUE,
           additional_tags=NULL) {

    # starts mlflow client to connect to mlflow server
    mlflow::mlflow_set_tracking_uri(uri = tracking_uri)
    mlflow::mlflow_client()

    # gets mlflow experiment, or if it doesn't exist creates it
    tryCatch({
      mlflow::mlflow_get_experiment(name = experiment_name)
    },
    error = function(cond) {
      mlflow::mlflow_create_experiment(experiment_name)
    },
    finally = {
      mlflow::mlflow_set_experiment(experiment_name)
    })

    # iterates over scenario pairs. If no experiment name is defined,
    # the experiment name is the join of the baseline and shock scenario names.
    for (i in 1:nrow(scenario_pairs)) {
      baseline_scenario = as.character(scenario_pairs[[i, "baseline_scenario"]])
      shock_scenario = as.character(scenario_pairs[[i, "shock_scenario"]])

      # iterates over the params defined in the params grid,
      # then over the values defined for this parameter.
      for (param_name in names(params_grid)) {
        for (param_value in params_grid[[param_name]]) {
          # surround parameter by double quotes if it is a string
          param_value <- ifelse(is.character(param_value),
                                paste('"', param_value, '"', sep=''),
                                param_value)

          nondefault_params <- tibble::tibble(param_name = names(params_grid),
                                              is_nondefault = rep(FALSE, length(params_grid)))
          nondefault_params[nondefault_params$param_name == param_name, "is_nondefault"] <- TRUE

          if(!check_if_run_already_done(tracking_uri, experiment_name,
                                        baseline_scenario, shock_scenario,
                                        param_name, param_value)){
            eval(parse(
              text = paste(
                "run_trisk_mlflow(",
                'tracking_uri = tracking_uri, ',
                'experiment_name = experiment_name, ',
                'nondefault_params = nondefault_params, ',
                'save_artifacts = save_artifacts, ',
                'input_path = trisk_input_path, ',
                'output_path = trisk_output_path, ',
                'additional_tags = additional_tags, ',
                'baseline_scenario = baseline_scenario, ',
                'shock_scenario = shock_scenario, ',
                param_name," = ", param_value,
                ")"
                , sep="")
            ))
            }

        }
      }
    }
  }


run_trisk_mlflow <-
  function(tracking_uri,
           experiment_name,
           nondefault_params,
           save_artifacts,
           additional_tags,
           ...) {


  with(mlflow::mlflow_start_run(), {
    for (i in 1:nrow(nondefault_params)){
      tag_param_name <- nondefault_params[[i, "param_name"]]
      tag_is_nondefault <- nondefault_params[[i, "is_nondefault"]]
      mlflow::mlflow_set_tag(tag_param_name, tag_is_nondefault)
    }
    if (!is.null(additional_tags) ){
    for (tag_name in names(additional_tags)){
      mlflow::mlflow_set_tag(tag_name, additional_tags[[tag_name]])
    }}
    # creates a temporary output directory to save TRISK outputs
    # TODO return the name of the output folder in the run_trisk output,
    #  log the artifacts from this folder, and remove this mechanic
    mlflow_run_output_dir <- tempfile()
    dir.create(mlflow_run_output_dir, recursive = TRUE)


    # log all parameters in current run. Also logs default parameters of
    # run_trisk, so all running parameters are logged in mlflow.
    input_params <- list(...)
    default_params <- formals(run_trisk)
    default_params <-
      default_params[!(names(default_params) %in% names(input_params))]
    all_params <- c(input_params, default_params)

    # log parameters name and their value
    for (param_name in names(all_params)) {
      if (!(param_name %in% c("input_path", "output_path", "return_results"))) {
        mlflow::mlflow_log_param(param_name, all_params[[param_name]])
      }
    }
    tryCatch({
      time_spent <- system.time({
        st_results_wrangled_and_checked <-
          r2dii.climate.stress.test::run_trisk(return_results = TRUE, ...)
      })

      metrics_df <- compute_trisk_metrics(st_results_wrangled_and_checked)

      for (i in 1:nrow(metrics_df)){
        metric_name <- metrics_df[[i, "metric_name"]]
        metric_name <- stringr::str_replace_all(metric_name, "[^A-Za-z0-9]", "_")
        metric_value <- metrics_df[[i, "metric_value"]]
        mlflow::mlflow_log_metric(metric_name, metric_value)
      }

      time_spent <- tibble::as_tibble(as.list(time_spent))
      readr::write_delim(time_spent,
                         file.path(mlflow_run_output_dir, "time_spent.csv"),
                         delim = ",")

      if (save_artifacts==TRUE){
        filepath <- file.path(
          mlflow_run_output_dir,
          "crispy_output.csv"
        )
        zip_path <- file.path(
          mlflow_run_output_dir,
          "artifacts.zip"
        )
        readr::write_csv(st_results_wrangled_and_checked$crispy_output, filepath)
        zip::zip(zip_path, "crispy_output.csv", root=mlflow_run_output_dir)
        unlink(filepath)
        }
      mlflow::mlflow_set_tag("LOG_STATUS", "SUCCESS")
      },

    error=function(cond){
      fileConn<-file(file.path(mlflow_run_output_dir, "error_message.txt"))
      writeLines(as.character(cond), fileConn)
      close(fileConn)
      mlflow::mlflow_set_tag("LOG_STATUS", "FAILED")
      },

    finally={
      if (save_artifacts==TRUE){
        mlflow::mlflow_log_artifact(path = mlflow_run_output_dir)
        }
      # deletes temp directory
      unlink(mlflow_run_output_dir, recursive = TRUE)
      }
    )

  })
}

