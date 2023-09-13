devtools::load_all()
library(dplyr)
library(xlsx)
library(ggplot2)
library(RColorBrewer)
library(ggbeeswarm)



# all_crispy_filtered <- all_crispy_filtered %>%
#   dplyr::filter(sector=="Power") %>%
#   group_by(company_name) %>%
#   summarise(net_present_value_baseline=sum(net_present_value_baseline),
#             net_present_value_shock=sum(net_present_value_shock),
#             net_present_value_difference=net_present_value_shock-net_present_value_baseline,
#             pd_baseline = sum(pd_baseline),
#             pd_shock=sum(pd_shock),
#             pd_difference=pd_shock-pd_baseline,
#             .groups="drop")%>%mutate(npv_roc=(net_present_value_shock-net_present_value_baseline)/net_present_value_baseline)

# # selection of a random set of colors
# col_vector <-
#   unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
# unique_scenario_provider <-
#   unique(all_crispy_filtered$scenario_provider)
# unique_colors <-
#   sample(col_vector, length(unique_scenario_provider))
# mapper_scenario_provider_color <-
#   setNames(unique_colors, unique_scenario_provider)


mapper_scenario_provider_color <-  c(
  REMIND = '#8A8A8A',
  MESSAGE = '#800000',
  OXFORD = '#F2A83B',
  GCAM = '#EA3AF7',
  IEA = '#001AF5',
  IPR = '#377D22'
)


symlog <- function(x) {
  sign(x) * log(abs(x))
}

### NPV DISTRIB PLOTS

dir.create(fs::path(output_dir, "npv_plots"),
           recursive = T,
           showWarnings = F)

npv_pivoted <-
  all_crispy_filtered %>%
  select(
    company_name,
    # business_unit,
    baseline_scenario,
    shock_scenario,
    scenario_duo,
    scenario_provider,
    target_duo,
    net_present_value_baseline,
    net_present_value_shock,
    net_present_value_difference
  ) %>%
  tidyr::pivot_longer(
    cols = dplyr::starts_with("net_present_value"),
    names_to = "metric",
    names_repair = "minimal"
  )
npv_pivoted$metric <-
  factor(
    npv_pivoted$metric,
    levels = c(
      "net_present_value_baseline",
      "net_present_value_shock",
      "net_present_value_difference"
    )
  )

for (scenario in
     all_crispy_filtered %>% distinct(target_duo) %>% pull())
{
  filtred_data <- npv_pivoted %>%
    filter(target_duo == scenario)

  le_plot <- filtred_data  %>%
    ggplot(aes(
      x = metric,
      y = symlog(value),
      fill = scenario_provider
    )) +
    geom_boxplot() +
    # facet_wrap( ~ business_unit) +
    xlab("metric") +
    ylab("NPV") +
    theme(axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = 8
    )) +
    scale_fill_manual(values = mapper_scenario_provider_color) +
    ggtitle(paste0(
      "Companies distribution of NPV values for ",
      scenario ,
      "scenarios"
    ))

  ggplot2::ggsave(
    filename = fs::path(output_dir, "npv_plots", scenario, ext = "png"),
    plot = le_plot,
    width = 25,
    height = 20,
    units = "cm"
  )
}










### PD DISTRIB PLOTS

dir.create(fs::path(output_dir, "pd_plots"),
           recursive = T,
           showWarnings = F)

pd_pivoted <-
  all_crispy_filtered %>%
  select(
    company_name,
    # business_unit,
    baseline_scenario,
    shock_scenario,
    scenario_duo,
    scenario_provider,
    target_duo,
    pd_baseline,
    pd_shock,
    pd_difference
  ) %>%
  tidyr::pivot_longer(
    cols = dplyr::starts_with("pd"),
    names_to = "metric",
    names_repair = "minimal"
  )
pd_pivoted$metric <-
  factor(
    pd_pivoted$metric,
    levels = c(
      "pd_baseline",
      "pd_shock",
      "pd_difference"
    )
  )
for (scenario in
     all_crispy_filtered %>% distinct(target_duo) %>% pull())
{
  filtred_data <- pd_pivoted %>%
    filter(target_duo == scenario)

  le_plot <- filtred_data %>%
    ggplot(aes(x = metric,
               y = value,
               fill = scenario_provider)) +
    geom_boxplot() +
    # facet_wrap( ~ business_unit) +
    xlab("metric") +
    ylab("PD") +
    theme(axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = 8
    )) +
    scale_fill_manual(values = mapper_scenario_provider_color) +
    ggtitle(paste0(
      "Companies distribution of PD values for ",
      scenario ,
      "scenarios"
    ))

  ggplot2::ggsave(
    filename = fs::path(output_dir,
                        "pd_plots",
                        scenario,
                        ext = "png"),
    plot = le_plot,
    width = 25,
    height = 20,
    units = "cm"
  )
}













# CORRELATIONS SWARMPLOT
# all_crispy_filtered%>% readr::write_csv("CGFI paper/CORRECT_CRISPY_DATA.csv")

swarm_plot_dir <- "correl_swarm_plots_npv_diff_pearson"

dir.create(
  fs::path(output_dir,
           swarm_plot_dir),
  recursive = T,
  showWarnings = F
)

for (scenario in
     c("all", all_crispy_filtered %>% distinct(target_duo) %>% pull()))
{
  # compute volumes and correlations
  if (scenario == "all") {
    scenarios_correlations <-
      correl_npv_diff_between_scenarios(all_crispy_filtered)
    match_volumes <- count_non_zero_matches(all_crispy_filtered)
    size <- 1
  } else{
    scenarios_correlations <-
      correl_npv_diff_between_scenarios(all_crispy_filtered %>%
                                         filter(target_duo == scenario))
    match_volumes <- count_non_zero_matches(all_crispy_filtered %>%
                                              filter(target_duo == scenario))
    size <- 10
  }

  # pivot correlations to source->target scenario_duo, correlation as values
  data_plot <-
    scenarios_correlations %>%
    mutate(source_scenario = row.names(scenarios_correlations)) %>%
    tidyr::pivot_longer(!source_scenario,
                        names_to = "dest_scenario",
                        values_to = "correlation") %>%
    left_join(
      all_crispy_filtered %>% distinct(scenario_duo, scenario_provider),
      by = c("source_scenario" = "scenario_duo")
    ) %>%
    rename(source_scenario_provider = scenario_provider) %>%
    left_join(
      all_crispy_filtered %>% distinct(scenario_duo, scenario_provider),
      by = c("dest_scenario" = "scenario_duo")
    ) %>%
    rename(dest_scenario_provider = scenario_provider) %>%
    filter(source_scenario != dest_scenario)

  # adds volume of match to scenario_pair source/target
  match_volumes_plot <- match_volumes %>%
    mutate(source_scenario = row.names(scenarios_correlations)) %>%
    tidyr::pivot_longer(!source_scenario,
                        names_to = "dest_scenario",
                        values_to = "volume")

  data_plot <- data_plot %>% left_join(match_volumes_plot)

  # assign colors to points
  data_plot <- data_plot %>%
    mutate(
      source_scenario_provider_color = mapper_scenario_provider_color[source_scenario_provider],
      dest_scenario_provider_color = mapper_scenario_provider_color[dest_scenario_provider]
    )

  # assign colors to axis text, and arrange x labels order from lowest average corr to highest
  scenario_axis_color_order <- data_plot %>%
    group_by(source_scenario,
             source_scenario_provider,
             source_scenario_provider_color) %>%
    summarise(avg_correlation = mean(correlation),
              .groups = "drop") %>%
    arrange(avg_correlation) %>%
    select(source_scenario, source_scenario_provider_color)
  data_plot$source_scenario <-
    factor(data_plot$source_scenario, levels = scenario_axis_color_order$source_scenario)


  le_plot <- ggplot2::ggplot(clip = "off") +
    geom_beeswarm(
      data_plot,
      mapping = aes(
        x = source_scenario,
        y = correlation,
        colour = "#000000"
        # alpha = 1
      ),
      priority = 'density',
      method = "center",
      cex = 2.5,
      size = size + 1
    ) +
    geom_beeswarm(
      data_plot,
      mapping = aes(
        x = source_scenario,
        y = correlation,
        colour = dest_scenario_provider,
        size = volume / max(volume)
      )
      ,
      priority = 'density',
      # alpha = 0.85,
      method = "center",
      cex = 2.5,
      size = size
    ) +
    theme(
      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        size = 8,
        color = scenario_axis_color_order$source_scenario_provider_color,
        # face = "bold"
      )
    ) +
    scale_color_manual(values = mapper_scenario_provider_color) +
    ggtitle(
      paste0(
        "Correlations between NPV difference of companies between IAM, for ",
        scenario ,
        " scenarios"
      )
    )

  ggplot2::ggsave(
    filename = fs::path(output_dir,
                        swarm_plot_dir,
                        scenario,
                        ext = "png"),
    plot = le_plot,
    width = 30,
    height = 20,
    units = "cm"
  )

}













# CORRELATIONS SWARMPLOT
# all_crispy_filtered%>% readr::write_csv("CGFI paper/CORRECT_CRISPY_DATA.csv")

swarm_plot_dir <- "correl_swarm_plots_npv_roc_pearson"

dir.create(
  fs::path(output_dir,
           swarm_plot_dir),
  recursive = T,
  showWarnings = F
)

for (scenario in
     c("all", all_crispy_filtered %>% distinct(target_duo) %>% pull()))
{
  # compute volumes and correlations
  if (scenario == "all") {
    scenarios_correlations <-
      correl_npv_roc_between_scenarios(all_crispy_filtered)
    match_volumes <- count_non_zero_matches(all_crispy_filtered)
    size <- 1
  } else{
    scenarios_correlations <-
      correl_npv_roc_between_scenarios(all_crispy_filtered %>%
                                          filter(target_duo == scenario))
    match_volumes <- count_non_zero_matches(all_crispy_filtered %>%
                                              filter(target_duo == scenario))
    size <- 10
  }

  # pivot correlations to source->target scenario_duo, correlation as values
  data_plot <-
    scenarios_correlations %>%
    mutate(source_scenario = row.names(scenarios_correlations)) %>%
    tidyr::pivot_longer(!source_scenario,
                        names_to = "dest_scenario",
                        values_to = "correlation") %>%
    left_join(
      all_crispy_filtered %>% distinct(scenario_duo, scenario_provider),
      by = c("source_scenario" = "scenario_duo")
    ) %>%
    rename(source_scenario_provider = scenario_provider) %>%
    left_join(
      all_crispy_filtered %>% distinct(scenario_duo, scenario_provider),
      by = c("dest_scenario" = "scenario_duo")
    ) %>%
    rename(dest_scenario_provider = scenario_provider) %>%
    filter(source_scenario != dest_scenario)
  data_plot <- data_plot %>% tidyr::drop_na()

  # adds volume of match to scenario_pair source/target
  match_volumes_plot <- match_volumes %>%
    mutate(source_scenario = row.names(scenarios_correlations)) %>%
    tidyr::pivot_longer(!source_scenario,
                        names_to = "dest_scenario",
                        values_to = "volume")

  data_plot <- data_plot %>% left_join(match_volumes_plot)

  # assign colors to points
  data_plot <- data_plot %>%
    mutate(
      source_scenario_provider_color = mapper_scenario_provider_color[source_scenario_provider],
      dest_scenario_provider_color = mapper_scenario_provider_color[dest_scenario_provider]
    )

  # assign colors to axis text, and arrange x labels order from lowest average corr to highest
  scenario_axis_color_order <- data_plot %>%
    group_by(source_scenario,
             source_scenario_provider,
             source_scenario_provider_color) %>%
    summarise(avg_correlation = mean(correlation),
              .groups = "drop") %>%
    arrange(avg_correlation) %>%
    select(source_scenario, source_scenario_provider_color)
  data_plot$source_scenario <-
    factor(data_plot$source_scenario, levels = scenario_axis_color_order$source_scenario)


  le_plot <- ggplot2::ggplot(clip = "off") +
    geom_beeswarm(
      data_plot,
      mapping = aes(
        x = source_scenario,
        y = correlation,
        colour = "#000000"
        # alpha = 1
      ),
      priority = 'density',
      method = "center",
      cex = 2.5,
      size = size + 1
    ) +
    geom_beeswarm(
      data_plot,
      mapping = aes(
        x = source_scenario,
        y = correlation,
        colour = dest_scenario_provider,
        size = volume / max(volume)
      )
      ,
      priority = 'density',
      # alpha = 0.85,
      method = "center",
      cex = 2.5,
      size = size
    ) +
    theme(
      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        size = 8,
        color = scenario_axis_color_order$source_scenario_provider_color,
        # face = "bold"
      )
    ) +
    scale_color_manual(values = mapper_scenario_provider_color) +
    ggtitle(
      paste0(
        "Correlations between NPV rate of change of companies between IAM, for ",
        scenario ,
        " scenarios"
      )
    )

  ggplot2::ggsave(
    filename = fs::path(output_dir,
                        swarm_plot_dir,
                        scenario,
                        ext = "png"),
    plot = le_plot,
    width = 30,
    height = 20,
    units = "cm"
  )

}
