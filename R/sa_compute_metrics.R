compute_trisk_metrics <- function(st_results_wrangled_and_checked){
  net_profits_metrics_df <-
    compute_net_profits_trajectories_slope_and_intercept_metrics(
      st_results_wrangled_and_checked$company_trajectories)

  pd_diff_df <- compute_pd_diff(st_results_wrangled_and_checked$crispy_output)

  npv_diff_df <- compute_npv_diff(st_results_wrangled_and_checked$crispy_output)

  npv_rate_of_change_df <- compute_npv_rate_of_change(
    st_results_wrangled_and_checked$crispy_output)

  pd_rate_of_change <- compute_pd_rate_of_change(
    st_results_wrangled_and_checked$crispy_output)

  metrics_df <- dplyr::bind_rows(net_profits_metrics_df,
                                 pd_diff_df,
                                 npv_diff_df,
                                 npv_rate_of_change_df,
                                 pd_rate_of_change)

}

compute_npv_rate_of_change <- function(crispy_output){
  npv_rate_of_change_df <- crispy_output %>%
    dplyr::filter(.data$term == 5) %>%
    dplyr::mutate(npv_roc=net_present_value_baseline/net_present_value_shock) %>%
    dplyr::group_by(sector, business_unit) %>%
    dplyr::summarise_at(dplyr::vars(npv_roc), list(avg_npv_roc=mean)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(metric_name=paste("avg_npv_roc", sector, business_unit, sep="_"),
                  metric_value = avg_npv_roc) %>%
    dplyr::select(metric_name, metric_value)

  npv_rate_of_change_df
}

compute_pd_rate_of_change <- function(crispy_output){
  pd_rate_of_change_df <- crispy_output %>%
    dplyr::filter(.data$term == 5) %>%
    dplyr::mutate(pd_roc=pd_baseline/pd_shock) %>%
    dplyr::group_by(sector, business_unit) %>%
    dplyr::summarise_at(dplyr::vars(pd_roc), list(avg_pd_roc=mean)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(metric_name=paste("avg_pd_roc", sector, business_unit, sep="_"),
                  metric_value = avg_pd_roc) %>%
    dplyr::select(metric_name, metric_value)

  pd_rate_of_change_df
}

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


compute_pd_diff <- function(crispy_output){
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

compute_npv_diff <- function(crispy_ouput){
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

