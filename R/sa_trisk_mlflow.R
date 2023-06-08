library(mlflow)


set_mlflow_experiment <- function(experiment_name){
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
}

create_tags_list <- function(param_names, params_grid, additional_tags){
  param_names <- param_names[param_names %in% names(params_grid)] # remove baseline and shock param
  param_tweaked <- rep(FALSE, length(params_grid))
  names(param_tweaked) <- names(params_grid)
  param_tweaked[param_names] <- TRUE
  tags <- c(param_tweaked, additional_tags)

  tags
}

generate_runs_parameters <- function(scenario_pairs, params_grid, max_param_combinations){

  param_names_combinations <- utils::combn(names(params_grid), m=max_param_combinations)

  param_values_combinations <- NULL
  for (i in 1:ncol(param_names_combinations)){
    param_names_to_combine <- param_names_combinations[,i]
    param_values_combinations <- dplyr::bind_rows(
      param_values_combinations,
      expand.grid(params_grid[param_names_to_combine])
    )
  }

  runs_parameters <- scenario_pairs %>% dplyr::cross_join(param_values_combinations)
  runs_parameters
}

fetch_completed_runs_parameters <- function(runs_parameters, tracking_uri, experiment_name){
  mlflow::mlflow_set_tracking_uri(tracking_uri)
  experiment_id <- mlflow::mlflow_get_experiment(name = experiment_name)$experiment_id[1]
  # iterate the runs search by filtering over each baseline_scenario
  # because mlflow returns at most 1000 runs from a search. It will work
  # as long as there are less than 1000 runs per baseline scenario.
  completed_runs_params <- NULL
  for (baseline_scenario in unique(runs_parameters$baseline_scenario)){
    finished_baseline_runs <- mlflow::mlflow_search_runs(
      filter = paste("params.baseline_scenario = '",baseline_scenario,"'",
                     " and ",
                     "attributes.status = 'FINISHED'", sep=''),
      experiment_ids = as.character(experiment_id))
    if (nrow(finished_baseline_runs) == 1000){
      stop("mlflow_search_runs has reached the limit of runs that can be returned with this filter.
           Must refine the filter conditions in generate_runs_parameters().")
    }
    if (nrow(finished_baseline_runs) > 0){
    # lengthy block of code to convert the output from MLFlow
    # to the same dataframe format as the `runs_parameters` dataframe
    params_df_list <- lapply(1:nrow(finished_baseline_runs),
                             function(x) (dplyr::bind_rows(
                               dplyr::inner_join(finished_baseline_runs$params[[x]],
                                                            finished_baseline_runs$tags[[x]],
                                                            by="key", suffix=c(".params", ".tags")),
                               finished_baseline_runs$params[[x]] %>%
                                 dplyr::filter(key %in% c("baseline_scenario", "shock_scenario")) %>%
                                 dplyr::mutate(value.params=value, value.tags="TRUE") %>%
                                 dplyr::select(key, value.params, value.tags)
                               )))
    params_df_list <- lapply(1:length(params_df_list),
                             function(x) (params_df_list[[x]] %>%
                                            dplyr::mutate(value.params=ifelse(value.tags, value.params, NA)) %>%
                                            dplyr::select(key, value.params)))
    params_df_list <- lapply(1:length(params_df_list),
                             function(x) (tidyr::pivot_wider(params_df_list[[x]],
                                                             names_from = "key",
                                                             values_from = "value.params")))
    completed_runs_params_baseline_scenario <- dplyr::bind_rows(params_df_list)

    # aggregate the baseline_scenario runs together
    completed_runs_params <- dplyr::bind_rows(completed_runs_params,
                                              completed_runs_params_baseline_scenario)
    }
  }
  completed_runs_params <- as.data.frame(completed_runs_params)
  completed_runs_params
}

filter_out_completed_runs <- function(runs_parameters, completed_runs_params){
  uncompleted_runs <- dplyr::anti_join(
    runs_parameters %>% dplyr::mutate(dplyr::across(dplyr::everything(), purrr::partial(type.convert, as.is=TRUE))),
    completed_runs_params %>% dplyr::mutate(dplyr::across(dplyr::everything(), purrr::partial(type.convert, as.is=TRUE)))
    , by=names(runs_parameters))

  uncompleted_runs
}

generate_and_filter_run_parameters <-
  function(tracking_uri,
           experiment_name,
           scenario_pairs,
           params_grid,
           max_param_combinations) {

  runs_parameters <- generate_runs_parameters(scenario_pairs, params_grid, max_param_combinations)
  print("Gathering previous runs parameters...")
  completed_runs_params <- fetch_completed_runs_parameters(runs_parameters, tracking_uri, experiment_name)
  if (nrow(completed_runs_params) > 0){
    filtered_run_parameters <- filter_out_completed_runs(runs_parameters, completed_runs_params)
    print(paste("Removed",nrow(runs_parameters) - nrow(filtered_run_parameters),
                "runs that have already been executed"))
  } else{
    filtered_run_parameters <- runs_parameters
  }
  filtered_run_parameters
}

#' @export
multirun_trisk_mlflow <-
  function(tracking_uri = "http://localhost:5000",
           experiment_name,
           trisk_input_path,
           trisk_output_path,
           baseline_scenarios,
           shock_scenarios,
           params_grid,
           save_artifacts=c("crispy_output"),
           max_param_combinations=1,
           additional_tags=NULL) {

    # starts mlflow client to connect to mlflow server
    mlflow::mlflow_set_tracking_uri(uri = tracking_uri)
    mlflow::mlflow_client()

    set_mlflow_experiment(experiment_name)

    # Creates the dataframe of all runs parameters
    # Gather runs and filter already completed ones
    # cannot check each run 1 by 1 with the server as it is too time consuming.
    scenario_pairs <- expand.grid(baseline_scenario=baseline_scenarios,
                                  shock_scenario=shock_scenarios)
    filtered_run_parameters <- generate_and_filter_run_parameters(tracking_uri,
                                                                  experiment_name,
                                                                  scenario_pairs,
                                                                  params_grid,
                                                                  max_param_combinations)
    print(paste("Starting the execution of",nrow(filtered_run_parameters),"total runs"))


    n_completed_runs <- 0
    for (i in 1:nrow(filtered_run_parameters)) {
      row_params <- filtered_run_parameters[i,]
      use_params <- row_params[,which(!is.na(row_params))]

      tags <- create_tags_list(names(use_params), params_grid, additional_tags)

      # aggregate input parameters in a string to then evaluate
      eval_run_params <- NULL
      for (param_name in names(use_params)){
        param_value <- use_params[[param_name]]
        param_value <- ifelse(is.factor(param_value),
                              as.character(param_value),
                              param_value)
        param_value <- ifelse(is.character(param_value),
                              paste('"', param_value, '"', sep=''),
                              param_value)
        eval_run_params <- paste(
          eval_run_params,
          paste(param_name," = ", param_value, sep="")
        , sep=",")
      }


      eval(parse(
        text = paste(
          "run_trisk_mlflow(",
          'tracking_uri = tracking_uri, ',
          'experiment_name = experiment_name, ',
          'tags = tags, ',
          'save_artifacts = save_artifacts, ',
          'input_path = trisk_input_path, ',
          'output_path = trisk_output_path',
          eval_run_params,
          ")"
          , sep="")
        ))

      n_completed_runs <- n_completed_runs + 1
      print(paste("Done",n_completed_runs,"/",nrow(filtered_run_parameters),"total runs"))
    }
  }


write_and_zip_csv_artifacts <-
  function(st_results_wrangled_and_checked,
           mlflow_run_output_dir,
           save_artifacts) {
  written_csv_paths <- NULL
  for (artifact_name in save_artifacts){
    filepath <- file.path(
      mlflow_run_output_dir,
      paste(artifact_name, ".csv", sep="")
    )
    readr::write_csv(st_results_wrangled_and_checked[[artifact_name]], filepath)
    written_csv_paths <- c(written_csv_paths, filepath)
  }
  zip_path <- file.path(
    mlflow_run_output_dir,
    "artifacts.zip"
  )
  zip::zip(zip_path, paste(save_artifacts, ".csv", sep=""), root=mlflow_run_output_dir)
  unlink(written_csv_paths)
  }

log_metrics_df <- function(metrics_df){
  for (i in 1:nrow(metrics_df)){
    metric_name <- metrics_df[[i, "metric_name"]]
    metric_name <- stringr::str_replace_all(metric_name, "[^A-Za-z0-9]", "_")
    metric_value <- metrics_df[[i, "metric_value"]]
    mlflow::mlflow_log_metric(metric_name, metric_value)
  }
}

run_trisk_mlflow <-
  function(tracking_uri,
           experiment_name,
           tags,
           save_artifacts,
           ...) {

  with(mlflow::mlflow_start_run(), {

    for (tag_name in names(tags)){
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

    tryCatch({
      time_spent <- system.time({
        st_results_wrangled_and_checked <-
          r2dii.climate.stress.test::run_trisk(return_results = TRUE, ...)
      })

      metrics_df <- compute_trisk_metrics(st_results_wrangled_and_checked)
      log_metrics_df(metrics_df)

      time_spent <- tibble::as_tibble(as.list(time_spent))
      readr::write_delim(time_spent,
                         file.path(mlflow_run_output_dir, "time_spent.csv"),
                         delim = ",")

      if (!is.null(save_artifacts)){
        write_and_zip_csv_artifacts(st_results_wrangled_and_checked,
                                    mlflow_run_output_dir,
                                    save_artifacts)
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
      if (!is.null(save_artifacts)){
        mlflow::mlflow_log_artifact(path = mlflow_run_output_dir)
        }
      # deletes temp directory
      unlink(mlflow_run_output_dir, recursive = TRUE)
      }
    )
  })
}

