# TODO bivariate analysis for discount rate & growth_rate
# mlflow server --backend-store-uri sensitivity_analysis/mlruns --default-artifact-root sensitivity_analysis/mlartifacts --serve-artifacts --host 127.0.0.1 --port 5000

devtools::load_all()
library(dplyr)
library(ggplot2)


mlflow_python_bin <-
  "/Users/bertrandgallice/opt/miniconda3/envs/mlflow_env/bin/python"
mlflow_bin <-
  "/Users/bertrandgallice/opt/miniconda3/envs/mlflow_env/bin/mlflow"

Sys.setenv(MLFLOW_PYTHON_BIN = mlflow_python_bin,
           MLFLOW_BIN = mlflow_bin)

outputs_folder <-
  file.path("sensitivity_analysis", "outputs_bugged_companies")

params_to_analyze <- c("discount_rate",
                       "shock_year",
                       "growth_rate",
                       # "div_netprofit_prop_coef", "lgd", "market_passthrough",
                       "risk_free_rate")
experiment_name <- "st_master_data_fullruns"

dir.create(outputs_folder, showWarnings = FALSE)


symlog <- function(x) {
  sign(x) * log(abs(x))
}

npv_pd_diff_boxplots <- function(data) {
  npv_diff_boxplot <- data %>%
    ggplot(aes(
      x = as.character(tweaked_param_value),
      y = symlog(net_present_value_baseline - net_present_value_shock),
      fill = scenario_duo
    )) +
    geom_boxplot() +
    facet_wrap( ~ business_unit) +
    xlab(tweaked_parameter) +
    ylab("NPV Difference")

  pd_diff_boxplot <- data %>%
    ggplot(aes(
      x = as.character(tweaked_param_value),
      y = symlog(pd_baseline - pd_shock),
      fill = scenario_duo
    )) +
    geom_boxplot() +
    facet_wrap( ~ business_unit) +
    xlab(tweaked_parameter) +
    ylab("PD Difference")

  return(list(
    "npv_diff_boxplot" = npv_diff_boxplot,
    "pd_diff_boxplot" = pd_diff_boxplot
  ))
}

for (tweaked_parameter in params_to_analyze) {
  tweaked_parameter_runs_data <-
    gather_trisk_outputs_for_tweaked_parameter(
      tracking_uri = "http://localhost:5000",
      experiment_name = experiment_name,
      tweaked_parameter = tweaked_parameter,
      csv_filename = "crispy_output.csv"
    )
  if (!is.null(tweaked_parameter_runs_data)) {
    # do analysis
    plots_list <- npv_pd_diff_boxplots(tweaked_parameter_runs_data)
    for (plot_name in names(plots_list)) {
      ggsave(
        plot = plots_list[[plot_name]],
        path = outputs_folder,
        filename = paste(tweaked_parameter, "_", plot_name, ".jpg", sep =""),
        width = 40,
        height = 30,
        units = "cm"
      )
    }
    print(paste("Done analysing", tweaked_parameter))
  } else {
    print(
      paste(
        "No data found in experiment",
        experiment_name,
        "with tweaked parameter",
        tweaked_parameter
      )
    )
  }
}
