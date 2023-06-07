# TODO bivariate analysis for discount rate & growth_rate
# mlflow server --backend-store-uri sensitivity_analysis/mlruns --default-artifact-root sensitivity_analysis/mlartifacts --serve-artifacts --host 127.0.0.1 --port 5000

symlog <- function(x){sign(x)*log(abs(x))}

library(ggplot2)
library(dplyr)

mlflow_python_bin <-
  "/Users/bertrandgallice/opt/miniconda3/envs/mlflow_env/bin/python"
mlflow_bin <-
  "/Users/bertrandgallice/opt/miniconda3/envs/mlflow_env/bin/mlflow"

Sys.setenv(MLFLOW_PYTHON_BIN = mlflow_python_bin,
           MLFLOW_BIN = mlflow_bin)

# ==================
# MLFLOW
# ==================

get_tweaked_parameter_runs <- function(tracking_uri, experiment_name, tweaked_parameter){
  mlflow::mlflow_set_tracking_uri(uri = tracking_uri)
  mlflow::mlflow_client()
  experiment <- mlflow::mlflow_get_experiment(name = experiment_name)
  experiment_id <- experiment[[1, "experiment_id"]]

  # fetch the run uuids with tag matching the current tweaked_parameter
  tweaked_parameter_runs <-
    mlflow::mlflow_search_runs(
      filter = paste("tags.", tweaked_parameter, " = 'TRUE'",
                     " and ",
                     "tags.LOG_STATUS = 'SUCCESS'",
                     sep = ""),
      experiment_ids = as.character(experiment_id)
    )

  return(tweaked_parameter_runs)
}

read_csv_from_zipped_artifacts <- function(tracking_uri, experiment_name, run_id, csv_filename){
  mlflow::mlflow_set_tracking_uri(uri = tracking_uri)
  mlflow::mlflow_client()

  artifacts_path <- mlflow::mlflow_download_artifacts(path="", run_id = run_id)
  f_conn <- unz(file.path(artifacts_path, "artifacts.zip"), csv_filename)
  artifact <- readr::read_csv(f_conn, show_col_types = FALSE)
  return(artifact)
}

gather_cripsy_outputs_for_tweaked_parameter <-
  function(tracking_uri,
           experiment_name,
           tweaked_parameter) {

  tweaked_parameter_runs <- get_tweaked_parameter_runs(
    tracking_uri, experiment_name, tweaked_parameter
  )

  tweaked_parameter_runs_data <- NULL
  for (run_id in tweaked_parameter_runs[["run_uuid"]]) {
    tryCatch({
      crispy_output <- read_csv_from_zipped_artifacts(
        tracking_uri,
        experiment_name,
        run_id,
        csv_filename = "crispy_output.csv"
      )

      run_params <-
        tweaked_parameter_runs[tweaked_parameter_runs$run_uuid == run_id, ]$params[[1]]
      param_value <-
        run_params[run_params$key == tweaked_parameter,]$value

      run_data <- crispy_output %>%
        dplyr::mutate(scenario_duo = paste(baseline_scenario, shock_scenario, sep='&'),
                      tweaked_param_name=tweaked_parameter,
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
      tweaked_parameter_runs_data <- dplyr::bind_rows(tweaked_parameter_runs_data, run_data)

    },
    error=function(cond){
      print(cond)
    })
  }
  return(tweaked_parameter_runs_data)
  }

# ==================
# ANALYSIS
# ==================

compare_npv_roc_between_scenarios <- function(tweaked_parameter_runs_data){

  npv_scenarios_change_in_roc <- tweaked_parameter_runs_data %>%
    # filtering on term as NPV stays constant accross terms
    dplyr::filter(term == 5) %>%
    dplyr::select(scenario_duo, company_name, sector, business_unit,
                  tweaked_param_name, tweaked_param_value,
                  net_present_value_baseline, net_present_value_shock) %>%
    # compute the npv rate of change and round the result to the closest first integer
    # As it has been observed that for some business units (eg Renewables), the
    # NPV at shock and at baseline are very close, and gravitate around 1. While
    # for other business units (eg Oil) the npv is far greater than 1
    dplyr::mutate(npv_rate_of_change=
                    round(net_present_value_baseline/net_present_value_shock,
                          digits=0)) %>%
    # npv_loss_at_shock tells if the npv_rate_of_change is bigger or equal to 1
    # If it is bigger, npv at the baseline is higher than at the shock
    # meaning the shock induces a loss.
    dplyr::mutate(npv_loss_at_shock = npv_rate_of_change >= 1) %>%
    tidyr::pivot_wider(id_cols=c("company_name" ,"sector" ,"business_unit", "tweaked_param_name", "tweaked_param_value"),
                           names_from=scenario_duo,
                           values_from=npv_loss_at_shock)

  unique_scenario_duos <- unique(tweaked_parameter_runs_data$scenario_duo)
  n_scenario_duos <- length(unique_scenario_duos)

  scenarios_npv_loss_at_shock_agreeing_rate <- data.frame(matrix(nrow=n_scenario_duos, ncol = n_scenario_duos))
  colnames(scenarios_npv_loss_at_shock_agreeing_rate) <- unique_scenario_duos
  rownames(scenarios_npv_loss_at_shock_agreeing_rate) <- unique_scenario_duos

  for (scenario_duo1 in unique_scenario_duos){
    for (scenario_duo2 in unique_scenario_duos){
      not_nan_scenario_duo_1 <- sum(!is.na(npv_scenarios_change_in_roc[,scenario_duo1]))
      not_nan_scenario_duo_2 <- sum(!is.na(npv_scenarios_change_in_roc[,scenario_duo2]))

      identical_loss_at_shock <- sum(
        npv_scenarios_change_in_roc[,scenario_duo1] == npv_scenarios_change_in_roc[,scenario_duo2]
        , na.rm=TRUE)

      scenarios_npv_loss_at_shock_agreeing_rate[scenario_duo1, scenario_duo2] <-
        identical_loss_at_shock/not_nan_scenario_duo_1
      scenarios_npv_loss_at_shock_agreeing_rate[scenario_duo2, scenario_duo1] <-
        identical_loss_at_shock/not_nan_scenario_duo_2
    }
  }
  scenarios_npv_loss_at_shock_agreeing_rate
}

compare_pd_diff_between_scenarios <- function(tweaked_parameter_runs_data){
  pd_scenarios_change_in_diff_sign <- tweaked_parameter_runs_data %>%
    # filtering on term 5 to focus on a single pd value
    dplyr::filter(term == 5) %>%
    dplyr::select(scenario_duo, company_name, sector, business_unit,
                  tweaked_param_name, tweaked_param_value,
                  pd_baseline, pd_shock) %>%
    dplyr::mutate(pd_diff=pd_baseline - pd_shock) %>%
    dplyr::mutate(pd_diff_sign = sign(pd_diff)) %>%
    tidyr::pivot_wider(id_cols=c("company_name" ,"sector" ,"business_unit", "tweaked_param_name", "tweaked_param_value"),
                       names_from=scenario_duo,
                       values_from=pd_diff_sign)

  unique_scenario_duos <- unique(tweaked_parameter_runs_data$scenario_duo)
  n_scenario_duos <- length(unique_scenario_duos)

  scenarios_pd_diff_sign_agreeing_rate <- data.frame(matrix(nrow=n_scenario_duos, ncol = n_scenario_duos))
  colnames(scenarios_pd_diff_sign_agreeing_rate) <- unique_scenario_duos
  rownames(scenarios_pd_diff_sign_agreeing_rate) <- unique_scenario_duos

  for (scenario_duo1 in unique_scenario_duos){
    for (scenario_duo2 in unique_scenario_duos){
      not_nan_scenario_duo_1 <- sum(!is.na(pd_scenarios_change_in_diff_sign[,scenario_duo1]))
      not_nan_scenario_duo_2 <- sum(!is.na(pd_scenarios_change_in_diff_sign[,scenario_duo2]))

      identical_pd_diff_sign <- sum(
        pd_scenarios_change_in_diff_sign[,scenario_duo1] == pd_scenarios_change_in_diff_sign[,scenario_duo2]
        , na.rm=TRUE)

      scenarios_pd_diff_sign_agreeing_rate[scenario_duo1, scenario_duo2] <-
        identical_pd_diff_sign/not_nan_scenario_duo_1
      scenarios_pd_diff_sign_agreeing_rate[scenario_duo2, scenario_duo1] <-
        identical_pd_diff_sign/not_nan_scenario_duo_2
    }
  }
  scenarios_pd_diff_sign_agreeing_rate
}

share_of_zeros <- function(tweaked_parameter_runs_data){
  tot_number_of_companies <- length(unique(tweaked_parameter_runs_data$company_name))

  share_of_zeros_df <- tweaked_parameter_runs_data %>%
    dplyr::filter(term == 5) %>%
    dplyr::group_by(scenario_duo, tweaked_param_value) %>%
    dplyr::summarize(
      zero_share_npv_baseline = sum(net_present_value_baseline == 0, na.rm=T) / sum(!is.na(net_present_value_baseline)),
      zero_share_npv_shock = sum(net_present_value_shock == 0, na.rm=T) / sum(!is.na(net_present_value_shock)),
      zero_share_pd_baseline = sum(pd_baseline == 0, na.rm=T) / sum(!is.na(pd_baseline)),
      zero_share_pd_shock = sum(pd_shock == 0, na.rm=T) / sum(!is.na(pd_shock))
                  )
  share_of_zeros_df
}


compute_npv_and_pd_slopes_over_one_parameter_values <- function(tweaked_parameter_runs_data){
  slopes_npv_over_param_tweaked <- tweaked_parameter_runs_data %>%
    dplyr::filter(term == 5) %>%
    dplyr::group_by(
      scenario_duo,
      tweaked_param_name,
      sector,
      business_unit,
      company_name) %>%
    dplyr::do(model_npv_baseline = lm(net_present_value_baseline ~ as.double(tweaked_param_value), data=.),
              model_npv_shock = lm(net_present_value_shock ~ as.double(tweaked_param_value), data=.),
              model_pd_baseline_term_5 = lm(pd_baseline ~ as.double(tweaked_param_value), data=.),
              model_pd_shock_term_5 = lm(pd_shock ~ as.double(tweaked_param_value), data=.)) %>%
    dplyr::mutate(slope_npv_baseline=coef(model_npv_baseline)[2],
                  slope_npv_shock=coef(model_npv_shock)[2],
                  slope_pd_baseline_term_5=coef(model_pd_baseline_term_5)[2],
                  slope_pd_shock_term_5=coef(model_pd_shock_term_5)[2]) %>%
    dplyr::select(-c("model_npv_baseline", "model_npv_shock",
                     "model_pd_baseline_term_5", "model_pd_shock_term_5"))

  slopes_avg_pd_over_param_tweaked <- tweaked_parameter_runs_data %>%
    dplyr::group_by(
      scenario_duo,
      tweaked_param_name,
      tweaked_param_value,
      sector,
      business_unit,
      company_name) %>%
    dplyr::summarise(mean_terms_pd_baseline=mean(pd_baseline),
                     mean_terms_pd_shock=mean(pd_shock)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(
      scenario_duo,
      tweaked_param_name,
      sector,
      business_unit,
      company_name
    ) %>%
    dplyr::do(model_mean_terms_pd_baseline = lm(mean_terms_pd_baseline ~ as.double(tweaked_param_value), data=.),
              model_mean_terms_pd_shock = lm(mean_terms_pd_shock ~ as.double(tweaked_param_value), data=.)) %>%
    dplyr::mutate(slope_mean_terms_pd_baseline=coef(model_mean_terms_pd_baseline)[2],
                  slope_mean_terms_pd_shock=coef(model_mean_terms_pd_shock)[2]) %>%
    dplyr::select(-c("model_mean_terms_pd_baseline", "model_mean_terms_pd_shock" ))

  slopes_over_param_tweaked <- dplyr::inner_join(
    slopes_npv_over_param_tweaked,
    slopes_avg_pd_over_param_tweaked,
    by=c("scenario_duo", "tweaked_param_name", "sector", "business_unit", "company_name"))

}

draw_boxplots_foreach_param_value <- function(tweaked_parameter_runs_data, tweaked_parameter){
  boxplot_npv_baseline <- tweaked_parameter_runs_data %>%
    ggplot(aes(x = as.character(tweaked_param_value), y = log(1+net_present_value_baseline), fill = scenario_duo)) +
    geom_boxplot() +
    facet_wrap(~business_unit) +
    xlab(tweaked_parameter) +
    ylab("net_present_value_baseline")

  boxplot_npv_shock <- tweaked_parameter_runs_data %>%
    ggplot(aes(x = as.character(tweaked_param_value), y = log(1+net_present_value_shock), fill = scenario_duo)) +
    geom_boxplot() +
    facet_wrap(~business_unit) +
    xlab(tweaked_parameter)+
    ylab("net_present_value_shock")

  boxplot_pd_baseline <- tweaked_parameter_runs_data %>%
    ggplot(aes(x = as.character(tweaked_param_value), y = pd_baseline, fill = scenario_duo)) +
    geom_boxplot() +
    facet_wrap(~business_unit) +
    xlab(tweaked_parameter)+
    ylab("probability_of_default_baseline")

  boxplot_pd_shock <- tweaked_parameter_runs_data %>%
    ggplot(aes(x = as.character(tweaked_param_value), y = pd_shock, fill = scenario_duo)) +
    geom_boxplot() +
    facet_wrap(~business_unit) +
    xlab(tweaked_parameter)+
    ylab("probability_of_default_shock")

  list("boxplot_npv_baseline"=boxplot_npv_baseline,
       "boxplot_npv_shock"=boxplot_npv_shock,
       "boxplot_pd_baseline"=boxplot_pd_baseline,
       "boxplot_pd_shock"=boxplot_pd_shock)
}

draw_slopes_boxplots <- function(slopes_of_all_parameters){

  boxplot_slope_npv_baseline <- slopes_of_all_parameters %>%
    ggplot(aes(x = tweaked_param_name, y = symlog(1+slope_npv_baseline), fill = scenario_duo)) +
    geom_boxplot() +
    facet_wrap(~business_unit) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    ggtitle("Slope of NPV baseline evolution when increasing tweaked parameters values")

  boxplot_slope_npv_shock <- slopes_of_all_parameters %>%
    ggplot(aes(x = tweaked_param_name, y = symlog(1+slope_npv_shock), fill = scenario_duo)) +
    geom_boxplot() +
    facet_wrap(~business_unit) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    ggtitle("Slope of NPV shock evolution when increasing tweaked parameters values")

  boxplot_slope_mean_terms_pd_baseline <- slopes_of_all_parameters %>%
    ggplot(aes(x = tweaked_param_name, y = slope_mean_terms_pd_baseline, fill = scenario_duo)) +
    geom_boxplot() +
    facet_wrap(~business_unit) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    ggtitle("Slope of the `mean PD baseline over terms` evolution when increasing tweaked parameters values")

  boxplot_slope_mean_terms_pd_shock <- slopes_of_all_parameters %>%
    ggplot(aes(x = tweaked_param_name, y = slope_mean_terms_pd_shock, fill = scenario_duo)) +
    geom_boxplot() +
    facet_wrap(~business_unit) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    ggtitle("Slope of the `mean PD shock over terms` evolution when increasing tweaked parameters values")


  list("boxplot_slope_npv_baseline"=boxplot_slope_npv_baseline,
       "boxplot_slope_npv_shock"=boxplot_slope_npv_shock,
       "boxplot_slope_mean_terms_pd_baseline"=boxplot_slope_mean_terms_pd_baseline,
       "boxplot_slope_mean_terms_pd_shock"=boxplot_slope_mean_terms_pd_shock)
}

# ==============================
# MAIN
# ==============================

outputs_folder <- file.path("sensitivity_analysis", "sensitivity_analysis_outputs2")
plots_folder <- file.path(outputs_folder, "plots")
summary_tables_folder <- file.path(outputs_folder, "summary_tables")

params_to_analyze <- c("shock_year", "discount_rate", "growth_rate",
                       #"div_netprofit_prop_coef", "lgd", "market_passthrough",
                       "risk_free_rate")


dir.create(outputs_folder, showWarnings = FALSE)
dir.create(plots_folder, showWarnings = FALSE)
dir.create(summary_tables_folder, showWarnings = FALSE)

slopes_of_all_parameters <- NULL
for (tweaked_parameter in params_to_analyze) {

  tweaked_parameter_runs_data <- gather_cripsy_outputs_for_tweaked_parameter(
    tracking_uri="http://localhost:5000",
    experiment_name="st_master_data_fullruns",
    tweaked_parameter=tweaked_parameter)

  scenarios_npv_loss_at_shock_agreeing_rate <- compare_npv_roc_between_scenarios(tweaked_parameter_runs_data)
  scenarios_pd_diff_sign_agreeing_rate <- compare_pd_diff_between_scenarios(tweaked_parameter_runs_data)
  share_of_zeros_df <- share_of_zeros(tweaked_parameter_runs_data)

  readr::write_csv(scenarios_npv_loss_at_shock_agreeing_rate,
                   file=file.path(summary_tables_folder,
                                  paste(tweaked_parameter, "scenarios_npv_loss_at_shock_agreeing_rate.csv", sep="_")))
  readr::write_csv(scenarios_pd_diff_sign_agreeing_rate,
                   file=file.path(summary_tables_folder,
                                  paste(tweaked_parameter, "scenarios_pd_diff_sign_agreeing_rate.csv", sep="_")))
  readr::write_csv(share_of_zeros_df,
                   file=file.path(summary_tables_folder,
                                  paste(tweaked_parameter, "share_of_zeros.csv", sep="_")))

  tweaked_param_boxplots <- draw_boxplots_foreach_param_value(tweaked_parameter_runs_data, tweaked_parameter)
  for (plot_name in names(tweaked_param_boxplots)){
    ggsave(plot=tweaked_param_boxplots[[plot_name]],
           path=plots_folder,
           filename=paste(tweaked_parameter,"_",plot_name, '.jpg', sep=''),
           width=40, height=30, units="cm")
  }

  # ! very slow !
  npv_slopes_over_param_tweaked <- compute_npv_and_pd_slopes_over_one_parameter_values(tweaked_parameter_runs_data)
  slopes_of_all_parameters <- dplyr::bind_rows(slopes_of_all_parameters, npv_slopes_over_param_tweaked)

  print(paste("done analysing", tweaked_parameter))

}

readr::write_csv(slopes_of_all_parameters,
                 file=file.path(summary_tables_folder, "slopes_of_all_parameters.csv"))

plots_list <- draw_slopes_boxplots(slopes_of_all_parameters)

ggsave(plot=plots_list$boxplot_slope_npv_baseline,
       path=plots_folder,
       filename="boxplot_slope_npv_baseline.jpg",
       width=40, height=30, units="cm")

ggsave(plot=plots_list$boxplot_slope_npv_shock,
       path=plots_folder,
       filename="boxplot_slope_npv_shock.jpg",
       width=40, height=30, units="cm")

ggsave(plot=plots_list$boxplot_slope_mean_terms_pd_baseline,
       path=plots_folder,
       filename="boxplot_slope_mean_terms_pd_baseline.jpg",
       width=40, height=30, units="cm")

ggsave(plot=plots_list$boxplot_slope_mean_terms_pd_shock,
       path=plots_folder,
       filename="boxplot_slope_mean_terms_pd_shock.jpg",
       width=40, height=30, units="cm")



amplitudes <- slopes_of_all_parameters %>%
  dplyr::group_by(scenario_duo, sector, business_unit, tweaked_param_name) %>%
  dplyr::summarise(
    mean_slope_npv_baseline=mean(symlog(1+slope_npv_baseline)),
    mean_slope_npv_shock = mean(symlog(1+slope_npv_shock)),
    mean_slope_mean_terms_pd_baseline = mean(slope_mean_terms_pd_baseline),
    mean_slope_mean_terms_pd_shock = mean(slope_mean_terms_pd_shock)
    ) %>%
  dplyr::ungroup()

for (colname in c("mean_slope_npv_baseline", "mean_slope_npv_shock",
                  "mean_slope_mean_terms_pd_baseline", "mean_slope_mean_terms_pd_shock")){

amplitudes_pivoted <- amplitudes %>%
  tidyr::pivot_wider(
    id_cols=c("scenario_duo", "sector", "business_unit"),
    names_from=tweaked_param_name,
    values_from=colname)

filename <- paste("amplitudes_",colname, ".xls", sep='')
readr::write_excel_csv(amplitudes_pivoted, file=file.path(summary_tables_folder, filename))
}
