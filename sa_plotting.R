# TODO bivariate analysis for discount rate & growth_rate
# mlflow server --backend-store-uri sensitivity_analysis/mlruns --default-artifact-root sensitivity_analysis/mlartifacts --serve-artifacts --host 127.0.0.1 --port 5000

library(ggplot2)
library(dplyr)

mlflow_python_bin <-
  "/Users/bertrandgallice/opt/miniconda3/envs/mlflow_env/bin/python"
mlflow_bin <-
  "/Users/bertrandgallice/opt/miniconda3/envs/mlflow_env/bin/mlflow"

Sys.setenv(MLFLOW_PYTHON_BIN = mlflow_python_bin,
           MLFLOW_BIN = mlflow_bin)

get_run_matching_tag <- function(tracking_uri, experiment_name, parameter_focus){
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

  return(parameter_focus_runs)
}

get_run_artifact <- function(tracking_uri, experiment_name, run_id, artifact_name){
  mlflow::mlflow_set_tracking_uri(uri = tracking_uri)
  mlflow::mlflow_client()

  artifacts_path <- mlflow::mlflow_download_artifacts(path="", run_id = run_id)
  artifact <- readr::read_csv(file.path(artifacts_path, artifact_name), show_col_types = FALSE)
  artifact
}

filter_nans_inf <- function(df, filter_col){
  df[!is.na(df[[filter_col]]) & !is.infinite(df[[filter_col]]), ]
}


draw_lineplot_npv_share <- function(all_runs_data){
  m <- all_runs_data %>%
    dplyr::filter(term==5) %>%
    dplyr::group_by(scenario_duo, tweaked_param_name, tweaked_param_value, sector, business_unit) %>%
    dplyr::mutate(npv_baseline_share=net_present_value_baseline/sum(net_present_value_baseline),
                  npv_shock_share=net_present_value_shock/sum(net_present_value_shock)) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = company_name, values_from=npv_baseline_share)


}


draw_npv_diff_distribution_over_tweaked_values <- function(all_runs_data){
  symlog <- function(x){sign(x)*log(abs(x))}

  plot_data <- all_runs_data %>%
    dplyr::filter(term == 5)

  out_plot <- ggplot(data = plot_data,
         aes(
           x = symlog(1 + net_present_value_baseline - net_present_value_shock),
           group = scenario_duo,
           fill = scenario_duo
         )) +
    geom_density(adjust = .005, alpha = .4) +
    facet_wrap( ~ business_unit)

}

make_npv_summary_table_per_company <- function(all_runs_data){

  npv_runs_data <- all_runs_data %>%
    dplyr::filter(term == 5) %>% # filtering on term as NPV stays constant accross terms
    dplyr::select(scenario_duo, company_name, sector, business_unit,
                  tweaked_param_name, tweaked_param_value,
                  net_present_value_baseline, net_present_value_shock)

  npv_roc_summary <- npv_runs_data %>%
    dplyr::mutate(npv_rate_of_change=net_present_value_baseline/net_present_value_shock,
                  # npv_loss_at_shock bigger or equal to 1, considering 1 means there is no loss
                  # and after noticing that when an roc is equal to 1, others values for
                  # the same business_unit / company can be slightly bigger than 1.
                  npv_loss_at_shock=npv_rate_of_change >= 1) %>%
    dplyr::group_by(company_name, sector, business_unit) %>%
    dplyr::mutate(
      # indicator variable if there is an inversion in the npv rate of change during
      #  the tweaking of a parameter BETWEEN/ACCROSS DIFFERENT scenarios.
      # i.e. if the baseline gets bigger than the shock or vice-versa
      npv_roc_inversion_accross_scenarios=!(all(npv_loss_at_shock) | all(!npv_loss_at_shock))
      ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(scenario_duo, company_name, sector, business_unit) %>%
    dplyr::mutate(
      # indicator variable if there is an inversion in the npv rate of change during
      #  the tweaking of a parameter INSIDE THE SAME scenario.
      # i.e. if the baseline gets bigger than the shock or vice-versa
      npv_roc_inversion_inside_scenarios=!(all(npv_loss_at_shock) | all(!npv_loss_at_shock))
    ) %>%
    dplyr::ungroup()

  npv_diff_summary <- npv_runs_data %>%
    dplyr::mutate(npv_diff=net_present_value_baseline - net_present_value_shock,
                  npv_diff_negative_sign=sign(npv_diff) == -1) %>%
    dplyr::group_by(company_name, sector, business_unit) %>%
    dplyr::mutate(
      # indicator variable if there is an inversion in the npv rate of change during
      #  the tweaking of a parameter BETWEEN/ACCROSS DIFFERENT scenarios.
      # i.e. if the baseline gets bigger than the shock or vice-versa
      npv_diff_sign_inversion_accross_scenarios=!(all(npv_diff_negative_sign) | all(!npv_diff_negative_sign))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(scenario_duo, company_name, sector, business_unit) %>%
    dplyr::mutate(
      # indicator variable if there is an inversion in the npv rate of change during
      #  the tweaking of a parameter INSIDE THE SAME scenario.
      # i.e. if the baseline gets bigger than the shock or vice-versa
      npv_diff_sign_inversion_inside_scenarios=!(all(npv_diff_negative_sign) | all(!npv_diff_negative_sign))
    ) %>%
    dplyr::ungroup()

  npv_summary_table <- dplyr::inner_join(npv_roc_summary, npv_diff_summary,
                                   by=c("scenario_duo", "company_name", "sector", "business_unit",
                                        "tweaked_param_name", "tweaked_param_value"))
  npv_summary_table
}

make_pd_summary_table_per_company <- function(all_runs_data){

  pd_runs_data <- all_runs_data %>%
    dplyr::select(scenario_duo, sector, business_unit, company_name,
                  pd_baseline, pd_shock)

}

outputs_folder <- file.path("sensitivity_analysis", "outputs")
plots_folder <- file.path(outputs_folder, "plots")
summary_tables_folder <- file.path(outputs_folder, "summary_tables")
dir.create(outputs_folder)
dir.create(plots_folder)
dir.create(summary_tables_folder)

all_parameters_focus <- c("shock_year", "discount_rate", "growth_rate",
                          "div_netprofit_prop_coef", "lgd",
                          "risk_free_rate", "market_passthrough")


for (parameter_focus in all_parameters_focus) {
  parameter_focus_runs <- get_run_matching_tag(
    tracking_uri = "http://localhost:5000",
    experiment_name = "sa_st_master",
    parameter_focus = parameter_focus
  )

  all_runs_data <- NULL
  npv_roc_plot_data <- NULL
  pd_diff_plot_data <- NULL
  for (run_uuid in parameter_focus_runs[["run_uuid"]]) {
    tryCatch({
    crispy_output <- get_run_artifact(
      tracking_uri = "http://localhost:5000",
      experiment_name = "sa_st_master",
      run_id = run_uuid,
      artifact_name = "crispy_output_.csv"
    )
    run_params <-
      parameter_focus_runs[parameter_focus_runs$run_uuid == run_uuid, ]$params[[1]]
    param_value <-
      run_params[run_params$key == parameter_focus,]$value

    run_data <- crispy_output %>%
      dplyr::mutate(scenario_duo = paste(baseline_scenario, shock_scenario, sep='&'),
                    tweaked_param_name=parameter_focus,
                    tweaked_param_value=param_value) %>%
      dplyr::select(
        scenario_duo,
        tweaked_param_name,
        tweaked_param_value,
        sector,
        business_unit,
        company_name,
        term,
        pd_baseline,
        pd_shock,
        net_present_value_baseline,
        net_present_value_shock
      )
    all_runs_data <- dplyr::bind_rows(all_runs_data, run_data)

  },
  error=function(cond){
    print(cond)
  })
  }

  npv_summary_table <- make_npv_summary_table_per_company(all_runs_data)
  filename <- paste(parameter_focus, "npv_summary_table.csv", sep='__')
  readr::write_csv(npv_summary_table, file=file.path(summary_tables_folder, filename))

  # pd_summary_table <- make_pd_summary_table_per_company(all_runs_data)
  # filename <- paste(parameter_focus, "pd_summary_table.csv", sep='__')
  # readr::write_csv(npv_summary_table, file=file.path(pd_summary_table, filename))

  npv_diff_distribution_over_tweaked_values_plot <- draw_npv_diff_distribution_over_tweaked_values(all_runs_data)
  filename <- paste(parameter_focus, "npv_diff_distribution_over_tweaked_values.jpg", sep='__')
  ggsave(plot=npv_diff_distribution_over_tweaked_values_plot, path=plots_folder, filename=filename,
         width=40, height=30, units="cm")


  print(paste("done analysing", parameter_focus))

  # npv_roc_plot_data <- filter_nans_inf(npv_roc_plot_data, "npv_rateofchange")
  # pd_diff_plot_data <- filter_nans_inf(pd_diff_plot_data, "pd_difference")


}

