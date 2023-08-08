devtools::load_all()
library(dplyr)
library(xlsx)
library(ggplot2)

mlflow_python_bin <-
  "/Users/bertrandgallice/opt/miniconda3/envs/mlflow_env/bin/python"
mlflow_bin <-
  "/Users/bertrandgallice/opt/miniconda3/envs/mlflow_env/bin/mlflow"

Sys.setenv(MLFLOW_PYTHON_BIN = mlflow_python_bin,
           MLFLOW_BIN = mlflow_bin)

mlflow_uri <- "http://localhost:5000"
exp_name <- "all_scenarios_default_params_old_data"
output_dir <-
  file.path("sa_st_inputs_master_raw", "scenar_comparison_corr")
output_excel <- "scenario_pairs_comparisons.xlsx"
dir.create(output_dir, showWarnings = FALSE)

### CODE

read_csv_from_zipped_artifacts <-
  function(tracking_uri,
           experiment_name,
           run_id,
           csv_filename) {
    mlflow::mlflow_set_tracking_uri(uri = tracking_uri)
    mlflow::mlflow_client()

    artifacts_path <-
      mlflow::mlflow_download_artifacts(path = "", run_id = run_id)
    f_conn <-
      unz(file.path(artifacts_path, "artifacts.zip"), csv_filename)
    artifact <- readr::read_csv(f_conn, show_col_types = FALSE)
    return(artifact)
  }


mlflow::mlflow_set_tracking_uri(uri = mlflow_uri)
mlflow::mlflow_client()
experiment <- mlflow::mlflow_get_experiment(name = exp_name)
experiment_id <- experiment[[1, "experiment_id"]]

all_runs <-
  mlflow::mlflow_search_runs(filter = "tags.LOG_STATUS = 'SUCCESS'",
                             experiment_ids = as.character(experiment_id))


all_crispy <- NULL
for (run_id in all_runs[["run_uuid"]]) {
  crispy <- read_csv_from_zipped_artifacts(
    tracking_uri = mlflow_uri,
    experiment_name = exp_name,
    run_id = run_id,
    csv_filename = "crispy_output.csv"
  )

  all_crispy <- dplyr::bind_rows(all_crispy, crispy)
}

all_crispy <- all_crispy %>%
  dplyr::mutate(scenario_duo = paste(baseline_scenario, "&", shock_scenario, sep = ""))


nz_duos <-
  c(
    "NGFS2021_REMIND_CP&NGFS2021_REMIND_NZ2050",
    # "NGFS2021_REMIND_NDC&NGFS2021_REMIND_NZ2050",
    # "NGFS2021_MESSAGE_NDC&NGFS2021_MESSAGE_NZ2050",
    "NGFS2021_MESSAGE_CP&NGFS2021_MESSAGE_NZ2050",
    # "NGFS2021_GCAM_NDC&NGFS2021_GCAM_NZ2050",
    "NGFS2021_GCAM_CP&NGFS2021_GCAM_NZ2050",
    # "WEO2021_APS&WEO2021_NZE_2050",
    "WEO2021_STEPS&WEO2021_NZE_2050" # stated policy scenario == current policies ?
  )

dt_duos <-
  c(
    # "NGFS2021_REMIND_NDC&NGFS2021_REMIND_DT",
    "NGFS2021_REMIND_CP&NGFS2021_REMIND_DT",
    # "NGFS2021_MESSAGE_NDC&NGFS2021_MESSAGE_DT",
    "NGFS2021_MESSAGE_CP&NGFS2021_MESSAGE_DT",
    # "NGFS2021_GCAM_NDC&NGFS2021_GCAM_DT",
    "NGFS2021_GCAM_CP&NGFS2021_GCAM_DT"
  )

dn0_duos <-
  c(
    # "NGFS2021_REMIND_NDC&NGFS2021_REMIND_DN0",
    "NGFS2021_REMIND_CP&NGFS2021_REMIND_DN0",
    # "NGFS2021_MESSAGE_NDC&NGFS2021_MESSAGE_DN0",
    "NGFS2021_MESSAGE_CP&NGFS2021_MESSAGE_DN0",
    # "NGFS2021_GCAM_NDC&NGFS2021_GCAM_DN0",
    "NGFS2021_GCAM_CP&NGFS2021_GCAM_DN0"
  )

b2ds_duos <-
  c(
    # "NGFS2021_REMIND_NDC&NGFS2021_REMIND_B2DS",
    "NGFS2021_REMIND_CP&NGFS2021_REMIND_B2DS",
    # "NGFS2021_MESSAGE_NDC&NGFS2021_MESSAGE_B2DS",
    "NGFS2021_MESSAGE_CP&NGFS2021_MESSAGE_B2DS",
    # "NGFS2021_GCAM_NDC&NGFS2021_GCAM_B2DS",
    "NGFS2021_GCAM_CP&NGFS2021_GCAM_B2DS"
  )

other_duos <-
  c(
    "Oxford2021_base&Oxford2021_fast",
    "IPR2021_baseline&IPR2021_RPS",
    "IPR2021_baseline&IPR2021_FPS",
    # "GECO2021_CurPol&GECO2021_NDC-LTS",
    # "GECO2021_CurPol&GECO2021_1.5C-Unif",
    "WEO2021_APS&WEO2021_SDS",
    "WEO2021_STEPS&WEO2021_SDS"
  )


all_crispy_target_named <- all_crispy %>%
  dplyr::mutate(
    target_duo = dplyr::case_when(
      scenario_duo %in% nz_duos ~ "NZ2050",
      scenario_duo %in% dt_duos ~ "DT",
      scenario_duo %in% dn0_duos ~ "DN0",
      scenario_duo %in% b2ds_duos ~ "B2DS",
      .default = "other"
    )
  )

remind_duos <-
  c(
    # "NGFS2021_REMIND_CP&NGFS2021_REMIND_NZ2050",
    "NGFS2021_REMIND_CP&NGFS2021_REMIND_DT",
    "NGFS2021_REMIND_CP&NGFS2021_REMIND_DN0",
    "NGFS2021_REMIND_CP&NGFS2021_REMIND_B2DS"
  )

message_duos <-
  c(
    # "NGFS2021_MESSAGE_CP&NGFS2021_MESSAGE_NZ2050",
    "NGFS2021_MESSAGE_CP&NGFS2021_MESSAGE_DT",
    "NGFS2021_MESSAGE_CP&NGFS2021_MESSAGE_DN0",
    "NGFS2021_MESSAGE_CP&NGFS2021_MESSAGE_B2DS"
  )

gcam_duos <-
  c(
    # "NGFS2021_GCAM_CP&NGFS2021_GCAM_NZ2050",
    "NGFS2021_GCAM_CP&NGFS2021_GCAM_DN0",
    "NGFS2021_GCAM_CP&NGFS2021_GCAM_DT",
    "NGFS2021_GCAM_CP&NGFS2021_GCAM_B2DS"
  )

iea_duos <- c(# stated policy scenario == current policies ?
  # "WEO2021_STEPS&WEO2021_NZE_2050",
  "WEO2021_STEPS&WEO2021_SDS")

ipr_duos <- c("IPR2021_baseline&IPR2021_RPS",
              "IPR2021_baseline&IPR2021_FPS")


oxford_duos <-
  c("Oxford2021_base&Oxford2021_fast")


all_crispy_scenario_named <- all_crispy_target_named %>%
  dplyr::mutate(
    scenario_provider = dplyr::case_when(
      scenario_duo %in% remind_duos ~ "REMIND",
      scenario_duo %in% message_duos ~ "MESSAGE",
      scenario_duo %in% gcam_duos ~ "GCAM",
      scenario_duo %in% iea_duos ~ "IEA",
      scenario_duo %in% ipr_duos ~ "IPR",
      scenario_duo %in% oxford_duos ~ "OXFORD",
      .default = NA
    )
  )


use_duos <-
  c(remind_duos,
    message_duos,
    gcam_duos,
    iea_duos,
    ipr_duos,
    oxford_duos)

all_crispy_filtered <- all_crispy_scenario_named %>%
  dplyr::filter(scenario_duo %in% use_duos,
                term == 5)

symlog <- function(x) {
  sign(x) * log(abs(x))
}

dir.create(
  fs::path("transition_risk_paper", "npv_plots"),
  recursive = T,
  showWarnings = F
)

npv_pivoted <-
  all_crispy_filtered %>%
  select(
    company_name,
    business_unit,
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
    facet_wrap( ~ business_unit) +
    xlab("metric") +
    ylab("NPV") +
    theme(axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = 8
    )) +
    scale_fill_manual(
      values = c(
        REMIND = 'aquamarine1',
        MESSAGE = 'yellow',
        OXFORD = 'chartreuse3',
        GCAM = 'brown2',
        IEA = 'deepskyblue2',
        IPR = 'darkviolet'
      )
    )

  ggplot2::ggsave(
    filename = fs::path("transition_risk_paper", "npv_plots", scenario, ext = "png"),
    plot = le_plot,
    width = 25,
    height = 20,
    units = "cm"
  )
}

dir.create(
  fs::path("transition_risk_paper", "pd_plots"),
  recursive = T,
  showWarnings = F
)

pd_pivoted <-
  all_crispy_filtered %>%
  select(
    company_name,
    business_unit,
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
    facet_wrap( ~ business_unit) +
    xlab("metric") +
    ylab("PD") +
    theme(axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = 8
    )) +
    scale_fill_manual(
      values = c(
        REMIND = 'aquamarine1',
        MESSAGE = 'yellow',
        OXFORD = 'chartreuse3',
        GCAM = 'brown2',
        IEA = 'deepskyblue2',
        IPR = 'darkviolet'
      )
    )

  ggplot2::ggsave(
    filename = fs::path("transition_risk_paper",
                        "pd_plots",
                        scenario,
                        ext = "png"),
    plot = le_plot,
    width = 25,
    height = 20,
    units = "cm"
  )
}

scenarios_correlations <-
  correl_npv_diff_between_scenarios(all_crispy_filtered)
match_volumes <- count_non_zero_matches(all_crispy_filtered)


data_plot <-
  scenarios_correlations %>%
  mutate(source_scenario = row.names(scenarios_correlations)) %>%
  tidyr::pivot_longer(!source_scenario, names_to = "dest_scenario", values_to = "correlation") %>%
  left_join(
    all_crispy_filtered %>% distinct(scenario_duo, target_duo),
    by = c("source_scenario" = "scenario_duo")
  ) %>%
  rename(source_target_duo = target_duo) %>%
  left_join(
    all_crispy_filtered %>% distinct(scenario_duo, target_duo),
    by = c("dest_scenario" = "scenario_duo")
  ) %>%
  rename(dest_target_duo = target_duo) %>%
  filter(source_scenario != dest_scenario)

match_volumes_plot <- match_volumes%>%
  mutate(source_scenario = row.names(scenarios_correlations)) %>%
  tidyr::pivot_longer(!source_scenario, names_to = "dest_scenario", values_to = "volume")

data_plot <- data_plot %>% left_join(match_volumes_plot)

col_vector <-
  unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
unique_target_duo <- unique(data_plot$source_target_duo)
unique_colors <- sample(col_vector, length(unique_target_duo))
mapper_target_duo_color <-
  setNames(unique_colors, unique_target_duo)


data_plot <- data_plot %>%
  mutate(source_target_duo_color = mapper_target_duo_color[source_target_duo],
         dest_target_duo_color = mapper_target_duo_color[dest_target_duo])

scenario_axis_color_order <- data_plot %>%
  group_by(source_scenario, source_target_duo, source_target_duo_color) %>%
  summarise(avg_correlation=median(correlation), .groups = "drop") %>%
  arrange(avg_correlation) %>%
  select(source_scenario, source_target_duo_color)
data_plot$source_scenario <- factor(data_plot$source_scenario, levels=scenario_axis_color_order$source_scenario)


ggplot2::ggplot(data_plot,aes(source_scenario, correlation)) +
  geom_beeswarm(
    priority = 'density',
    size =  data_plot$volume/max(data_plot$volume)*2.5,
    color = data_plot$dest_target_duo_color,
    alpha=0.85,
    method="center",cex = 2.5
  ) + theme(axis.text.x = element_text(
    angle = 45,
    hjust = 1,
    size = 8,
    color=scenario_axis_color_order$source_target_duo_color
  ))

ggplot2::ggplot(data_plot,aes(source_scenario, correlation)) +
  geom_beeswarm(
    priority = 'density',
    size =  data_plot$volume/max(data_plot$volume)*2.5,
    color = data_plot$dest_target_duo_color,
    alpha=0.7
  ) + theme(axis.text.x = element_text(
    angle = 45,
    hjust = 1,
    size = 8,
    color=scenario_axis_color_order$source_target_duo_color
  ))


ggplot2::ggplot(data_plot,aes(source_scenario, correlation)) +
  ggbeeswarm::geom_beeswarm(priority='density',size=2.5, color=data_plot$dest_target_duo_color)

ggplot2::ggplot(data_plot,aes(source_scenario, correlation)) +
  geom_beeswarm(
    priority = 'density',
    size = 2.5,
    color = data_plot$dest_target_duo_color,
    alpha=0.7,
    cex=1.5
  ) + theme(axis.text.x = element_text(
    angle = 45,
    hjust = 1,
    size = 8,
    color=scenario_axis_color_order$source_target_duo_color
  ))

ggplot2::ggplot(data_plot,aes(correlation, source_scenario)) +
  geom_beeswarm(
    priority = 'density',
    size = 2.5,
    color = data_plot$dest_target_duo_color,
    alpha=0.7,
    cex=1.5
  ) + theme(axis.text.x = element_text(
    angle = 45,
    hjust = 1,
    size = 8
  ), axis.text.y = element_text(color=scenario_axis_color_order$source_target_duo_color))

