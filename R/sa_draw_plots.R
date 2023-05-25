library(ggplot2)

draw_trisk_plots <- function(st_results_wrangled_and_checked){

  diff_baseline_shock_discounted_net_profit_plot <-
    draw_diff_baseline_shock_discounted_net_profit_plot(
      st_results_wrangled_and_checked$company_trajectories)

  discounted_net_profits_shock_plot <-
    draw_discounted_net_profits_shock_plot(
      st_results_wrangled_and_checked$company_trajectories)

  list(diff_baseline_shock_discounted_net_profit_plot=diff_baseline_shock_discounted_net_profit_plot,
       discounted_net_profits_shock_plot=discounted_net_profits_shock_plot)

}

draw_diff_baseline_shock_discounted_net_profit_plot <- function(company_trajectories){
  diff_baseline_shock_discounted_net_profit_plot <-
    company_trajectories %>%
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
  return(diff_baseline_shock_discounted_net_profit_plot)
}

draw_discounted_net_profits_shock_plot <- function(company_trajectories){
  discounted_net_profits_plot <- company_trajectories %>%
    ggplot() +
    geom_line(
      mapping = aes(
        x = year,
        y = discounted_net_profits_shock_scenario,
        group = company_name,
        color = technology
      )
    )
  return(discounted_net_profits_plot)
}
