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
  artifact <- readr::read_csv(file.path(artifacts_path, artifact_name))
  artifact
}


get_npv_roc_plot_data <- function(crispy_output, parameter_focus, param_value){
  crispy_output %>%
    dplyr::filter(.data$term == 5) %>%
    dplyr::mutate(
      scenario_duo = paste(baseline_scenario, shock_scenario, sep="&"),
      npv_rateofchange = net_present_value_baseline/net_present_value_shock,
      param_tweeked = parameter_focus,
      # TODO why div_netprofit_prop_coef not in crispy?
      # param_value = .data[[parameter_focus]]
      param_value = param_value
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

get_pd_diff_plot_data <- function(crispy_output, parameter_focus, param_value){
  crispy_output %>%
    dplyr::filter(.data$term == 5) %>%
    dplyr::mutate(
      scenario_duo = paste(baseline_scenario, shock_scenario, sep='&'),
      param_tweeked = parameter_focus,
      # TODO why div_netprofit_prop_coef not in crispy?
      # param_value = .data[[parameter_focus]]
      param_value = param_value
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

get_clean_crispy_plot_data <- function(crispy_output, parameter_focus, param_value){
  crispy_output %>%
    dplyr::filter(.data$term == 5) %>%
    dplyr::mutate(
      scenario_duo = paste(baseline_scenario, shock_scenario, sep='&'),
      param_tweeked = parameter_focus,
      # TODO why div_netprofit_prop_coef not in crispy?
      # param_value = .data[[parameter_focus]]
      param_value = param_value
    ) %>%
    dplyr::select(
      scenario_duo,
      param_tweeked,
      sector,
      business_unit,
      company_name,
      param_value,
      pd_difference,
      net_present_value_baseline,
      net_present_value_shock
    )
}

filter_nans_inf <- function(df, filter_col){
  df[!is.na(df[[filter_col]]) & !is.infinite(df[[filter_col]]), ]
}

draw_scatter_npv_rateofchange <- function(npv_roc_plot_data){
  # scatter_npv_rateofchange_plot <- npv_roc_plot_data %>%
  #   dplyr::mutate(param_value=runif(param_value,
  #                         min=param_value-var(param_value),
  #                         max=param_value+var(param_value))) %>%
  #   ggplot(aes(x = param_value, y = sqrt(npv_rateofchange), color = scenario_duo)) +
  #   geom_point(size=0.5, alpha=0.1)

  param_tweeked <- unique(npv_roc_plot_data[["param_tweeked"]])

  scatter_npv_rateofchange_plot <- npv_roc_plot_data %>%
    ggplot(aes(x = as.character(param_value), y = sqrt(npv_rateofchange), fill = scenario_duo)) +
    geom_boxplot() +
    facet_wrap(~scenario_duo) +
    xlab(param_tweeked)

  # scatter_npv_rateofchange_plot <- npv_roc_plot_data %>%
  #   dplyr::mutate(zscore=(npv_rateofchange-(mean(npv_rateofchange)/sd(npv_rateofchange)))) %>%
  #   dplyr::filter(abs(.data$zscore) < 3) %>%
  #   ggplot(aes(x = as.character(param_value), y = npv_rateofchange, fill = scenario_duo)) +
  #   geom_boxplot()

  scatter_npv_rateofchange_plot
}

draw_scatter_pd_difference <- function(pd_diff_plot_data){
  param_tweeked <- unique(pd_diff_plot_data[["param_tweeked"]])

  scatter_pd_difference_plot <- pd_diff_plot_data %>%
    ggplot(aes(x = as.character(param_value), y = pd_difference, fill = scenario_duo)) +
    geom_boxplot() +
    facet_wrap(~scenario_duo) +
    xlab(param_tweeked)

  scatter_pd_difference_plot

}

draw_violin_npv <- function(crispy_clean, parameter_focus){
  # plot_data <- crispy_output %>%
  #   dplyr::filter(.data$term == 5) %>%
  #   dplyr::mutate(
  #     scenario_duo = paste(baseline_scenario, shock_scenario, sep='&'),
  #     param_tweaked = parameter_focus,
  #     # TODO why div_netprofit_prop_coef not in crispy?
  #     # param_value = .data[[parameter_focus]]
  #     param_value = param_value
  #   ) %>%
  #   dplyr::select(scenario_duo, param_value, param_tweaked,
  #                 net_present_value_baseline, net_present_value_shock)

  # plot_data <- dplyr::bind_rows(
  #   crispy_clean %>%
  #     mutate(net_present_value = net_present_value_baseline, scenario_type ="baseline")%>%
  #     select(scenario_duo, business_unit, tweaked_param_name, tweaked_param_value, scenario_type, net_present_value),
  #   crispy_clean %>%
  #     mutate(net_present_value = net_present_value_shock, scenario_type ="shock")%>%
  #     select(scenario_duo, business_unit, tweaked_param_name, tweaked_param_value, scenario_type, net_present_value)
  # )
  plot_data_baseline <- crispy_clean %>%
    mutate(net_present_value = net_present_value_baseline, scenario_type ="baseline")%>%
    select(scenario_duo, business_unit, tweaked_param_name, tweaked_param_value, scenario_type, net_present_value)

  plot_data_shock <-  crispy_clean %>%
    mutate(net_present_value = net_present_value_shock, scenario_type ="shock")%>%
    select(scenario_duo, business_unit, tweaked_param_name, tweaked_param_value, scenario_type, net_present_value)

  #plot <- ggplot2::ggplot()
  add_var=F
  for (biz in unique(crispy_clean[["business_unit"]])){
    #plot <- plot +
    vioplot::vioplot(net_present_value~tweaked_param_value, data=plot_data_baseline[plot_data_baseline$business_unit == biz,], col = "palevioletred", plotCentre = "line", side = "left", add=add_var)
    add_var=T
    vioplot::vioplot(net_present_value~tweaked_param_value, data=plot_data_shock[plot_data_shock$business_unit == biz,], col = "lightblue", plotCentre = "line", side = "right", add=add_var)
  }
  browser()

  ggplot(plot_data, aes(x = as.character(param_value), y = net_present_value, fill = scenario_type)) +
    introdataviz::geom_split_violin(alpha = .4, trim = FALSE) +
    geom_boxplot(width = .2, alpha = .6, fatten = NULL, show.legend = FALSE) +
    stat_summary(fun.data = "mean_se", geom = "pointrange", show.legend = F,
                 position = position_dodge(.175)) +
    scale_x_discrete(name = "Condition", labels = c("Non-word", "Word")) +
    scale_y_continuous(name = "Reaction time (ms)",
                       breaks = seq(200, 800, 100),
                       limits = c(200, 800)) +
    scale_fill_brewer(palette = "Dark2", name = "Language group") +
    theme_minimal()
}

draw_lineplot_pd_diff <- function(all_runs_data){

}

draw_lineplot_npv <- function(all_runs_data){

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


draw_boxplot_npv <- function(all_runs_data){

}

draw_mean_vs_std_npv <- function(npv_summary){
  small_comp <- sample(unique(npv_summary$company_name), 100)
  subset <- npv_summary[npv_summary$company_name %in% small_comp,]

  ggplot(data=subset, aes(x=npv_baseline_mean, y=npv_baseline_std)) +
    geom_point(aes(colour=business_unit)) +
    facet_wrap(~scenario_duo)


}

draw_npv_distribution_of_tweaked_values <- function(all_runs_data){
  small_comp <- sample(unique(all_runs_data$company_name), 100)
  subset <- all_runs_data[all_runs_data$company_name %in% small_comp,]

  ggplot(data = subset,
         aes(
           x = log(net_present_value_shock),
           group = factor(scenario_duo),
           fill = factor(scenario_duo)
         )) +
    geom_density(adjust = 1.5, alpha = .4) +
    facet_wrap( ~ business_unit)

  out_plot <- ggplot(data=subset,
                     aes(x=log(net_present_value_baseline), group=factor(business_unit), fill=factor(business_unit))) +
    geom_density(adjust=1.5, alpha=.1) +
    facet_wrap(~scenario_duo)

  out_plot
  # out_plot <- ggplot() + theme_void()
  # for (company_name in unique(all_runs_data$company_name)){
  #   subset <- all_runs_data[all_runs_data$company_name == company_name,]
  #   p <- ggplot(data=subset, aes(x=net_present_value_baseline, group=business_unit, fill=business_unit)) +
  #     geom_density(adjust=1.5, alpha=.4) +
  #     facet_wrap(~scenario_duo)
  #   out_plot <- out_plot + p
  # }
}

draw_npv_roc_density <- function(all_runs_data){
  plot_data <- all_runs_data %>%
    dplyr::filter(term == 5) %>%
    dplyr::mutate(npv_roc=net_present_value_baseline/net_present_value_shock)

  #small_comp <- sample(unique(plot_data$company_name), 1000)
  #subset <- plot_data[plot_data$company_name %in% small_comp,]

  out_plot <- ggplot(data=plot_data,
                     aes(x=log(net_present_value_baseline), group=factor(scenario_duo), fill=factor(scenario_duo))) +
    geom_density(adjust=1.5, alpha=.5) +
    facet_wrap(~business_unit)
}

make_summary_table_per_company <- function(all_runs_data){
  # npv_summary <- all_runs_data %>%
  #   dplyr::filter(term==5) %>% # filtering on term as NPV stays constant accross terms
  #   dplyr::group_by(scenario_duo, sector, business_unit, company_name) %>%
  #   dplyr::summarise(npv_baseline_mean = mean(net_present_value_baseline),
  #                    npv_baseline_std = sd(net_present_value_baseline),
  #                    npv_shock_mean = mean(net_present_value_shock),
  #                    npv_shock_std = sd(net_present_value_shock)) %>%
  #   dplyr::ungroup()

  npv_summary <- all_runs_data %>%
    dplyr::filter(term == 5) %>% # filtering on term as NPV stays constant accross terms
    dplyr::mutate(npv_rate_of_change=net_present_value_baseline/net_present_value_shock,
                  # npv_loss_at_shock bigger or equal to 1, considering 1 means there is no loss
                  # and after noticing that when an roc is equal to 1, others values for
                  # the same business_unit / company can be slightly bigger than 1.
                  npv_loss_at_shock=npv_rate_of_change >= 1) %>%
    dplyr::group_by(company_name, sector, business_unit) %>%
    dplyr::mutate(
      # indicator variable if there is an inversion in the npv rate of change during
      #  the tweaking of a parameter.
      # i.e. if the baseline gets bigger than the shock or vice-versa
      npv_roc_inversion_accross_scenarios=!(all(npv_loss_at_shock) | all(!npv_loss_at_shock))
      ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(scenario_duo, company_name, sector, business_unit) %>%
    dplyr::mutate(
      # indicator variable if there is an inversion in the npv rate of change during
      #  the tweaking of a parameter.
      # i.e. if the baseline gets bigger than the shock or vice-versa
      npv_roc_inversion_inside_scenarios=!(all(npv_loss_at_shock) | all(!npv_loss_at_shock))
    ) %>%
    dplyr::ungroup()

  pd_summary <- all_runs_data %>%
    dplyr::group_by(scenario_duo, sector, business_unit, company_name)

}

plots_folder <- file.path("sensitivity_analysis", "plots")
dir.create(plots_folder)

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
      #dplyr::filter(.data$term == 5) %>%
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
#
#     npv_roc_plot_data_run <-
#       get_npv_roc_plot_data(crispy_output, parameter_focus, param_value)
#     npv_roc_plot_data <- dplyr::bind_rows(npv_roc_plot_data,
#                                           npv_roc_plot_data_run)
#
#     pd_diff_plot_data_run <- get_pd_diff_plot_data(crispy_output, parameter_focus, param_value)
#     pd_diff_plot_data <- dplyr::bind_rows(pd_diff_plot_data,
#                                           pd_diff_plot_data_run)
  },
  error=function(cond){
    print(cond)
  })
  }

  browser()
  a_plot <- draw_npv_distribution_of_tweaked_values(all_runs_data)
  summary_table_per_company <- make_summary_table_per_company(all_runs_data)

  violin_npv_plot <- draw_violin_npv(all_runs_data, parameter_focus)
  filename <- paste(parameter_focus, "violin_npv.jpg", sep='__')
  ggsave(plot=violin_npv_plot, path=plots_folder, filename=filename)
  print(paste("saved plot", filename))

  # npv_roc_plot_data <- filter_nans_inf(npv_roc_plot_data, "npv_rateofchange")
  # pd_diff_plot_data <- filter_nans_inf(pd_diff_plot_data, "pd_difference")
  #
  # npv_rateofchange_plot <- draw_scatter_npv_rateofchange(npv_roc_plot_data)
  # filename <- paste(parameter_focus, "npv_rateofchange.jpg", sep='__')
  # ggsave(plot=npv_rateofchange_plot, path=plots_folder, filename=filename)
  # print(paste("saved plot", filename))
  #
  # scatter_pd_difference_plot <- draw_scatter_pd_difference(pd_diff_plot_data)
  # filename <- paste(parameter_focus, "pd_difference.jpg", sep='__')
  # ggsave(plot=scatter_pd_difference_plot, path=plots_folder, filename=filename)
  # print(paste("saved plot", filename))


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
