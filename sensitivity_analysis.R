library(mlflow)
library(r2dii.climate.stress.test)
library(ggplot2)

compute_net_profits_trajectories_slope_and_intercept_metrics <- function(company_trajectories){
  avg_diff_baseline_shock_discounted_net_profits_trajectories <- company_trajectories %>%
    dplyr::group_by(scenario_name, ald_sector, technology, year) %>%
    dplyr::mutate(diff_baseline_shock_discounted_net_profit =
                    discounted_net_profits_baseline_scenario
                  - discounted_net_profits_shock_scenario) %>%
    dplyr::summarise_at(dplyr::vars(diff_baseline_shock_discounted_net_profit),
                        list(avg_diff_baseline_shock_discounted_net_profit = mean)) %>%
    dplyr::ungroup()

  net_profits_trajectories_slope_and_intercept <- avg_diff_baseline_shock_discounted_net_profits_trajectories %>%
    dplyr::group_by(scenario_name, ald_sector, technology) %>%
    dplyr::arrange(year, .by_group = TRUE) %>% # sort inside group per ascending year
    dplyr::do(model = lm(avg_diff_baseline_shock_discounted_net_profit ~ year, data = .)) %>%
    dplyr::mutate(intercept = coef(.data$model)[1], slope=coef(.data$model)[2]) %>%
    dplyr::ungroup()

  metrics_df <- net_profits_trajectories_slope_and_intercept %>%
    dplyr::mutate(metric_name=paste("slope", ald_sector, technology, sep='_')) %>%
    dplyr::mutate(metric_value = slope) %>%
    dplyr::select(metric_name, metric_value)

  metrics_df
}

compute_pd_metrics <- function(crispy_output){
  avg_pd_difference <- crispy_output %>%
    dplyr::filter(.data$term == 5) %>%
    dplyr::group_by(sector, business_unit) %>%
    dplyr::summarise_at(dplyr::vars(pd_difference), list(avg_pd_difference=mean)) %>%
    dplyr::ungroup()

  metrics_df <- avg_pd_difference %>%
    dplyr::mutate(
      metric_name=paste("avg_pd_diff", sector, business_unit, sep="_"),
      metric_value=avg_pd_difference) %>%
    dplyr::select(metric_name, metric_value)

  metrics_df
}

compute_npv_metrics <- function(crispy_ouput){
  npv_metrics <- crispy_ouput %>%
    dplyr::filter(.data$term == 5) %>%
    dplyr::group_by(sector, business_unit) %>%
    dplyr::summarise_at(dplyr::vars(net_present_value_difference),
                        list(avg_net_present_value_difference = mean)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(metric_name=paste("avg_npv_diff", sector, business_unit, sep='_'),
                  metric_value=avg_net_present_value_difference) %>%
    dplyr::select(metric_name, metric_value)

  npv_metrics
}

compute_trisk_metrics <- function(st_results_wrangled_and_checked){
  net_profits_metrics_df <-
    compute_net_profits_trajectories_slope_and_intercept_metrics(
      st_results_wrangled_and_checked$company_trajectories)

  pd_metrics_df <- compute_pd_metrics(st_results_wrangled_and_checked$crispy_output)

  npv_metrics_df <- compute_npv_metrics(st_results_wrangled_and_checked$crispy_output)

  metrics_df <- dplyr::bind_rows(net_profits_metrics_df,
                                 pd_metrics_df,
                                 npv_metrics_df)

}

draw_trisk_plots <- function(st_results_wrangled_and_checked){

  diff_baseline_shock_discounted_net_profit_plot <-
    st_results_wrangled_and_checked$company_trajectories %>%
    dplyr::mutate(
      diff_baseline_shock_discounted_net_profit =
        discounted_net_profits_baseline_scenario
      - discounted_net_profits_shock_scenario
    ) %>%
    ggplot() +
    geom_line(
      mapping = aes(
        x = year,
        y = diff_baseline_shock_discounted_net_profit,
        group = company_name,
        color = technology
      )
    )

  list(diff_baseline_shock_discounted_net_profit_plot=diff_baseline_shock_discounted_net_profit_plot)

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
    time_spent <- system.time({
      st_results_wrangled_and_checked <-
        r2dii.climate.stress.test::run_trisk(return_results = TRUE, ...)
    })

    metrics_df <- compute_trisk_metrics(st_results_wrangled_and_checked)

    for (i in 1:nrow(metrics_df)){
      metric_name <- metrics_df[[i, "metric_name"]]
      metric_value <- metrics_df[[i, "metric_value"]]
      mlflow::mlflow_log_metric(metric_name, metric_value)
    }

    plots <- draw_trisk_plots(st_results_wrangled_and_checked)

    # creates a temporary output directory to save TRISK outputs
    # TODO return the name of the output folder in the run_trisk output,
    #  log the artifacts from this folder, and remove this mechanic
    mlflow_run_output_dir <- tempfile()
    dir.create(mlflow_run_output_dir, recursive = TRUE)

    for (plot_name in names(plots)){
      plot_path <- file.path(mlflow_run_output_dir, paste(plot_name, 'png', sep='.'))
      ggsave(plot_path,plot=plots[[plot_name]])
    }

    time_spent <- tibble::as_tibble(as.list(time_spent))
    readr::write_delim(time_spent,
                       file.path(mlflow_run_output_dir, "time_spent.csv"),
                       delim = ",")


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
          param_value = ifelse(is.character(param_value),
                               paste('"', param_value, '"'),
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

#' download artifacts of all runs given a specific parameter marginally tuned
get_mlflow_runs_artifacts <- function(tracking_uri, experiment_name, parameter_focus) {

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

  all_runs_artifacts <- list()
  for (run_id in parameter_focus_run_ids){
    # path refers to the place of desired artifacts INSIDE the run_id folder
    # here it is empty since we want all artifacts, from the root
    artifacts_path <- mlflow::mlflow_download_artifacts(path="", run_id = run_id)

    run_artifacts <-
      list(
        company_trajectories = readr::read_csv(file.path(artifacts_path, "company_trajectories_.csv")),
        crispy_output_ = readr::read_csv(file.path(artifacts_path, "crispy_output_.csv")),
        time_spent = readr::read_csv(file.path(artifacts_path, "time_spent.csv"))
      )

    all_runs_artifacts[[run_id]] <-  run_artifacts
  }
  return(all_runs_artifacts)
}


#================================================================
# MAIN FUNCTIONS
#================================================================

#mlflow server --backend-store-uri dummy_project/mlruns --default-artifact-root dummy_project/mlartifacts --serve-artifacts --host 127.0.0.1 --port 5000

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
    experiment_name = "TEST666",
    trisk_input_path = "dummy_project/project_input",
    trisk_output_path = "dummy_project/output",
    scenario_pairs = scenario_pairs,
    params_grid = params_grid
  )
}



#================================================================
# UTILS FUNCTIONS
#================================================================
