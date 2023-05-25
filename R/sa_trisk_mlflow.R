library(mlflow)
library(ggplot2)

#' @export
multirun_trisk_mlflow <-
  function(tracking_uri = "http://localhost:5000",
           experiment_name = NULL,
           trisk_input_path,
           trisk_output_path,
           scenario_pairs,
           params_grid) {

    auto_experiment_name <-
      ifelse(is.null(experiment_name), TRUE, FALSE)

    # iterates over scenario pairs. If no experiment name is defined,
    # the experiment name is the join of the baseline and shock scenario names.
    for (i in 1:nrow(scenario_pairs)) {
      baseline_scenario = scenario_pairs[[i, "baseline_scenario"]]
      shock_scenario = scenario_pairs[[i, "shock_scenario"]]

      experiment_name <- ifelse(
        auto_experiment_name,
        paste(baseline_scenario, shock_scenario, sep =
                "-"),
        experiment_name
      )

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

          eval(parse(
            text = paste(
              "run_trisk_mlflow(",
              'tracking_uri = "', tracking_uri, '",',
              'experiment_name = "', experiment_name, '",',
              'nondefault_params = nondefault_params,',
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


run_trisk_mlflow <- function(tracking_uri, experiment_name, nondefault_params, ...) {
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

  with(mlflow::mlflow_start_run(), {
    for (i in 1:nrow(nondefault_params)){
      tag_param_name <- nondefault_params[[i, "param_name"]]
      tag_is_nondefault <- nondefault_params[[i, "is_nondefault"]]
      mlflow::mlflow_set_tag(tag_param_name, tag_is_nondefault)
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

      #plots <- draw_trisk_plots(st_results_wrangled_and_checked)
      #for (plot_name in names(plots)){
      #  plot_path <- file.path(mlflow_run_output_dir, paste(plot_name, 'png', sep='.'))
      #  ggplot2::ggsave(plot_path,plot=plots[[plot_name]])
      #}

      time_spent <- tibble::as_tibble(as.list(time_spent))
      readr::write_delim(time_spent,
                         file.path(mlflow_run_output_dir, "time_spent.csv"),
                         delim = ",")


      r2dii.climate.stress.test:::write_stress_test_results(
        results_list = st_results_wrangled_and_checked,
        iter_var = "",
        output_path = mlflow_run_output_dir
      )
      mlflow::mlflow_set_tag("LOG_STATUS", "SUCCESS")

      },

    error=function(cond){
      fileConn<-file(file.path(mlflow_run_output_dir, "error_message.txt"))
      writeLines(as.character(cond), fileConn)
      close(fileConn)
      mlflow::mlflow_set_tag("LOG_STATUS", "FAILED")
      },

    finally={
      mlflow::mlflow_log_artifact(path = mlflow_run_output_dir)
      # deletes temp directory
      unlink(mlflow_run_output_dir, recursive = TRUE)
      }
    )

  })
}

