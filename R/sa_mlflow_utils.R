library(mlflow)

#' Get or create MLFlow experiment from its name.
#' Gets mlflow experiment, or if it doesn't exist creates it, then set it.
set_mlflow_experiment <- function(experiment_name) {
  tryCatch(
    {
      mlflow::mlflow_get_experiment(name = experiment_name)
    },
    error = function(cond) {
      mlflow::mlflow_create_experiment(experiment_name)
    },
    finally = {
      mlflow::mlflow_set_experiment(experiment_name)
    }
  )
}

#' Creates a TRUE/FALSE flag for each parameter indicating which parameter
#' is being tweaked in the current run of trisk.
create_tags_list <- function(param_names, params_grid, additional_tags) {
  param_names <- param_names[param_names %in% names(params_grid)] # remove baseline and shock param
  param_tweaked <- rep(FALSE, length(params_grid))
  names(param_tweaked) <- names(params_grid)
  param_tweaked[param_names] <- TRUE
  tags <- c(param_tweaked, additional_tags)

  tags
}

#' creates a dataframe where each row is a set of parameters to be used as input.
#' Baseline and shock scenario are always included in the columns, the other columns are
#' the parameters names defined in the param_grid.
#' The values inside the param_grid are repeated for each pair of baseline/shock scenario.
#' The values inside the param_grid are combined by duets, triplets, etc..
#'  depending on the max_param_combinations value. The values of the parameters are
#'  tweaked one at a time if max_param_combinations=1 .
#'
#' @return a dataframe where each row is a set of parameters for 1 TRISK run
generate_runs_parameters <- function(scenario_pairs, params_grid, max_param_combinations) {
  if (length(params_grid) > 0) {
    param_names_combinations <- utils::combn(names(params_grid), m = max_param_combinations)

    param_values_combinations <- NULL
    for (i in 1:ncol(param_names_combinations)) {
      param_names_to_combine <- param_names_combinations[, i]
      param_values_combinations <- dplyr::bind_rows(
        param_values_combinations,
        expand.grid(params_grid[param_names_to_combine])
      )
    }

    runs_parameters <- scenario_pairs %>% dplyr::cross_join(param_values_combinations)
  } else {
    runs_parameters <- scenario_pairs
  }
  runs_parameters
}

#' Get all runs already completed in the current experiment defined by its name.
#' @return a dataframe where each row is a set of parameters that have been completed for 1 TRISK run
fetch_completed_runs_parameters <- function(runs_parameters, tracking_uri, experiment_name) {
  mlflow::mlflow_set_tracking_uri(tracking_uri)
  experiment_id <- mlflow::mlflow_get_experiment(name = experiment_name)$experiment_id[1]
  # iterate the runs search by filtering over each baseline_scenario
  # because mlflow returns at most 1000 runs from a search. It will work
  # as long as there are less than 1000 runs per baseline scenario.
  completed_runs_params <- NULL
  for (baseline_scenario in unique(runs_parameters$baseline_scenario)) {
    finished_baseline_runs <- mlflow::mlflow_search_runs(
      filter = paste("params.baseline_scenario = '", baseline_scenario, "'",
        " and ",
        "attributes.status = 'FINISHED'",
        sep = ""
      ),
      experiment_ids = as.character(experiment_id)
    )
    if (nrow(finished_baseline_runs) == 1000) {
      stop("mlflow_search_runs has reached the limit of runs that can be returned with this filter.
           Must refine the filter conditions in generate_runs_parameters().")
    }
    if (nrow(finished_baseline_runs) > 0) {
      # lengthy block of code to convert the output from MLFlow
      # to the same dataframe format as the `runs_parameters` dataframe
      params_df_list <- lapply(
        1:nrow(finished_baseline_runs),
        function(x) {
          (dplyr::bind_rows(
            dplyr::inner_join(finished_baseline_runs$params[[x]],
              finished_baseline_runs$tags[[x]],
              by = "key", suffix = c(".params", ".tags")
            ),
            finished_baseline_runs$params[[x]] %>%
              dplyr::filter(key %in% c("baseline_scenario", "shock_scenario")) %>%
              dplyr::mutate(value.params = value, value.tags = "TRUE") %>%
              dplyr::select(key, value.params, value.tags)
          ))
        }
      )
      params_df_list <- lapply(
        1:length(params_df_list),
        function(x) {
          (params_df_list[[x]] %>%
            dplyr::mutate(value.params = ifelse(value.tags, value.params, NA)) %>%
            dplyr::select(key, value.params))
        }
      )
      params_df_list <- lapply(
        1:length(params_df_list),
        function(x) {
          (tidyr::pivot_wider(params_df_list[[x]],
            names_from = "key",
            values_from = "value.params"
          ))
        }
      )
      completed_runs_params_baseline_scenario <- dplyr::bind_rows(params_df_list)

      # aggregate the baseline_scenario runs together
      completed_runs_params <- dplyr::bind_rows(
        completed_runs_params,
        completed_runs_params_baseline_scenario
      )
    }
  }
  completed_runs_params <- as.data.frame(completed_runs_params)
  completed_runs_params
}

#' removes rows from runs_parameters where an equivalent run has been completed
#' from completed_runs_params.
filter_out_completed_runs <- function(runs_parameters, completed_runs_params) {
  uncompleted_runs <- dplyr::anti_join(
    runs_parameters %>% dplyr::mutate(dplyr::across(dplyr::everything(), purrr::partial(type.convert, as.is = TRUE))),
    completed_runs_params %>% dplyr::mutate(dplyr::across(dplyr::everything(), purrr::partial(type.convert, as.is = TRUE))),
    by = names(runs_parameters)
  )

  uncompleted_runs
}

#' Generate a table of parameters for all runs, where each row defines the parameters
#' of 1 run. Filter this table of parameters if there are already completed runs
#' with a matching set in the mlflow experiment.
generate_and_filter_run_parameters <-
  function(tracking_uri,
           experiment_name,
           scenario_pairs,
           params_grid,
           max_param_combinations) {
    runs_parameters <- generate_runs_parameters(scenario_pairs, params_grid, max_param_combinations)
    print("Gathering previous runs parameters...")
    completed_runs_params <- fetch_completed_runs_parameters(runs_parameters, tracking_uri, experiment_name)
    if (nrow(completed_runs_params) > 0) {
      filtered_run_parameters <- filter_out_completed_runs(runs_parameters, completed_runs_params)
      print(paste(
        "Removed", nrow(runs_parameters) - nrow(filtered_run_parameters),
        "runs that have already been executed"
      ))
    } else {
      filtered_run_parameters <- runs_parameters
    }
    filtered_run_parameters
  }

write_and_zip_csv_artifacts <-
  function(st_results_wrangled_and_checked,
           mlflow_run_output_dir,
           save_artifacts) {
    written_csv_paths <- NULL
    for (artifact_name in save_artifacts) {
      filepath <- file.path(
        mlflow_run_output_dir,
        paste(artifact_name, ".csv", sep = "")
      )
      readr::write_csv(st_results_wrangled_and_checked[[artifact_name]], filepath)
      written_csv_paths <- c(written_csv_paths, filepath)
    }
    zip_path <- file.path(
      mlflow_run_output_dir,
      "artifacts.zip"
    )
    zip::zip(zip_path, paste(save_artifacts, ".csv", sep = ""), root = mlflow_run_output_dir)
    unlink(written_csv_paths)
  }

log_metrics_df <- function(metrics_df) {
  for (i in 1:nrow(metrics_df)) {
    metric_name <- metrics_df[[i, "metric_name"]]
    metric_name <- stringr::str_replace_all(metric_name, "[^A-Za-z0-9]", "_")
    metric_value <- metrics_df[[i, "metric_value"]]
    mlflow::mlflow_log_metric(metric_name, metric_value)
  }
}
