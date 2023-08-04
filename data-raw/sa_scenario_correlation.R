devtools::load_all()
library(dplyr)
library(xlsx)

mlflow_python_bin <-
  "/Users/bertrandgallice/opt/miniconda3/envs/mlflow_env/bin/python"
mlflow_bin <-
  "/Users/bertrandgallice/opt/miniconda3/envs/mlflow_env/bin/mlflow"

Sys.setenv(
  MLFLOW_PYTHON_BIN = mlflow_python_bin,
  MLFLOW_BIN = mlflow_bin
)

mlflow_uri <- "http://localhost:5000"
exp_name <- "all_scenarios_default_params_old_data"
output_dir <- file.path("sa_st_inputs_master_raw","scenar_comparison_corr_oxford")
dir.create(output_dir, showWarnings = FALSE)

### GATHER DATA

read_csv_from_zipped_artifacts <- function(tracking_uri, experiment_name, run_id, csv_filename) {
  mlflow::mlflow_set_tracking_uri(uri = tracking_uri)
  mlflow::mlflow_client()

  artifacts_path <- mlflow::mlflow_download_artifacts(path = "", run_id = run_id)
  f_conn <- unz(file.path(artifacts_path, "artifacts.zip"), csv_filename)
  artifact <- readr::read_csv(f_conn, show_col_types = FALSE)
  return(artifact)
}


mlflow::mlflow_set_tracking_uri(uri = mlflow_uri)
mlflow::mlflow_client()
experiment <- mlflow::mlflow_get_experiment(name = exp_name)
experiment_id <- experiment[[1, "experiment_id"]]

all_runs <-
  mlflow::mlflow_search_runs(
    filter = "tags.LOG_STATUS = 'SUCCESS'",
    experiment_ids = as.character(experiment_id)
  )


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

### ANALYSE


count_non_zero_matches <- function(all_crispy) {
  npv_scenarios_diff <- all_crispy %>%
    # filtering on term 5 to focus on a single npv value
    dplyr::filter(term == 5) %>%
    dplyr::mutate(npv_diff = round(net_present_value_difference)) %>%
    dplyr::mutate(npv_diff=dplyr::if_else(npv_diff==0, NA, npv_diff))

  unique_scenario_duos <- sort(unique(all_crispy$scenario_duo))
  n_scenario_duos <- length(unique_scenario_duos)

  scenarios_nonzero_match_matrix <- data.frame(matrix(nrow = n_scenario_duos, ncol = n_scenario_duos))
  colnames(scenarios_nonzero_match_matrix) <- unique_scenario_duos
  rownames(scenarios_nonzero_match_matrix) <- unique_scenario_duos

  for (scenario_duo1 in unique_scenario_duos) {
    for (scenario_duo2 in unique_scenario_duos) {
      scenar1 <- npv_scenarios_diff %>%
        dplyr::filter(scenario_duo == scenario_duo1, !is.na(npv_diff)) %>%
        dplyr::select(company_name,sector,business_unit, npv_diff)
      scenar2 <- npv_scenarios_diff %>%
        dplyr::filter(scenario_duo == scenario_duo2, !is.na(npv_diff)) %>%
        dplyr::select(company_name,sector,business_unit, npv_diff)

      merge_comps <- dplyr::inner_join(scenar1, scenar2,
                                       by= dplyr::join_by(company_name,sector,business_unit))

      nonzero_rows <- nrow(merge_comps)

      scenarios_nonzero_match_matrix[scenario_duo1, scenario_duo2] <- nonzero_rows
      scenarios_nonzero_match_matrix[scenario_duo2, scenario_duo1] <- nonzero_rows
    }
  }

  return(scenarios_nonzero_match_matrix)
}

correl_npv_diff_between_scenarios <- function(all_crispy) {
  npv_scenarios_diff <- all_crispy %>%
    # filtering on term 5 to focus on a single npv value
    dplyr::filter(term == 5) %>%
    dplyr::mutate(npv_diff = round(net_present_value_difference)) %>%
    dplyr::mutate(npv_diff=dplyr::if_else(npv_diff==0, NA, npv_diff))

  unique_scenario_duos <- sort(unique(all_crispy$scenario_duo))
  n_scenario_duos <- length(unique_scenario_duos)

  scenarios_npv_diff_corr_matrix <- data.frame(matrix(nrow = n_scenario_duos, ncol = n_scenario_duos))
  colnames(scenarios_npv_diff_corr_matrix) <- unique_scenario_duos
  rownames(scenarios_npv_diff_corr_matrix) <- unique_scenario_duos

  for (scenario_duo1 in unique_scenario_duos) {
    for (scenario_duo2 in unique_scenario_duos) {
      scenar1 <- npv_scenarios_diff %>%
        dplyr::filter(scenario_duo == scenario_duo1, !is.na(npv_diff)) %>%
        dplyr::select(company_name,sector,business_unit, npv_diff)
      scenar2 <- npv_scenarios_diff %>%
        dplyr::filter(scenario_duo == scenario_duo2, !is.na(npv_diff)) %>%
        dplyr::select(company_name,sector,business_unit, npv_diff)

      merge_comps <- dplyr::inner_join(scenar1, scenar2,
                                       by= dplyr::join_by(company_name,sector,business_unit))

      correlation <- cor(merge_comps$npv_diff.x,
                         merge_comps$npv_diff.y,
                         method = 'pearson')

      scenarios_npv_diff_corr_matrix[scenario_duo1, scenario_duo2] <- correlation
      scenarios_npv_diff_corr_matrix[scenario_duo2, scenario_duo1] <- correlation
    }
  }

  return(scenarios_npv_diff_corr_matrix)
}

std_diff_of_npv_diff_between_scenarios <- function(all_crispy) {
  npv_scenarios_diff <- all_crispy %>%
    # filtering on term 5 to focus on a single npv value
    dplyr::filter(term == 5) %>%
    dplyr::mutate(npv_diff = round(net_present_value_difference)) %>%
    dplyr::mutate(npv_diff=dplyr::if_else(npv_diff==0, NA, npv_diff))

  unique_scenario_duos <- sort(unique(all_crispy$scenario_duo))
  n_scenario_duos <- length(unique_scenario_duos)

  scenarios_sd_diff_npv_diff_matrix <- data.frame(matrix(nrow = n_scenario_duos, ncol = n_scenario_duos))
  colnames(scenarios_sd_diff_npv_diff_matrix) <- unique_scenario_duos
  rownames(scenarios_sd_diff_npv_diff_matrix) <- unique_scenario_duos

  for (scenario_duo1 in unique_scenario_duos) {
    for (scenario_duo2 in unique_scenario_duos) {
      scenar1 <- npv_scenarios_diff %>%
        dplyr::filter(scenario_duo == scenario_duo1, !is.na(npv_diff)) %>%
        dplyr::select(company_name,sector,business_unit, npv_diff)
      scenar2 <- npv_scenarios_diff %>%
        dplyr::filter(scenario_duo == scenario_duo2, !is.na(npv_diff)) %>%
        dplyr::select(company_name,sector,business_unit, npv_diff)

      merge_comps <- dplyr::inner_join(scenar1, scenar2,
                                       by= dplyr::join_by(company_name,sector,business_unit))

      sd_diff_npv_diff <- abs(mean(merge_comps$npv_diff.y - merge_comps$npv_diff.x))

      scenarios_sd_diff_npv_diff_matrix[scenario_duo1, scenario_duo2] <- sd_diff_npv_diff
      scenarios_sd_diff_npv_diff_matrix[scenario_duo2, scenario_duo1] <- sd_diff_npv_diff
    }
  }

  return(scenarios_sd_diff_npv_diff_matrix)
}



use_duos <-
  c(
    "Oxford2021_base&Oxford2021_fast",
    "IPR2021_baseline&IPR2021_RPS",
    "IPR2021_baseline&IPR2021_FPS",
    "NGFS2021_REMIND_CP&NGFS2021_REMIND_NZ2050",
    "NGFS2021_REMIND_NDC&NGFS2021_REMIND_DT",
    "NGFS2021_REMIND_CP&NGFS2021_REMIND_DT",
    "NGFS2021_REMIND_NDC&NGFS2021_REMIND_DN0",
    "NGFS2021_REMIND_CP&NGFS2021_REMIND_DN0",
    "NGFS2021_REMIND_NDC&NGFS2021_REMIND_B2DS",
    "NGFS2021_REMIND_CP&NGFS2021_REMIND_B2DS",
    "NGFS2021_REMIND_NDC&NGFS2021_REMIND_NZ2050",
    "NGFS2021_MESSAGE_NDC&NGFS2021_MESSAGE_NZ2050",
    "NGFS2021_MESSAGE_CP&NGFS2021_MESSAGE_NZ2050",
    "NGFS2021_MESSAGE_NDC&NGFS2021_MESSAGE_DT",
    "NGFS2021_MESSAGE_CP&NGFS2021_MESSAGE_DT",
    "NGFS2021_MESSAGE_NDC&NGFS2021_MESSAGE_DN0",
    "NGFS2021_MESSAGE_CP&NGFS2021_MESSAGE_DN0",
    "NGFS2021_MESSAGE_NDC&NGFS2021_MESSAGE_B2DS",
    "NGFS2021_MESSAGE_CP&NGFS2021_MESSAGE_B2DS",
    "NGFS2021_GCAM_NDC&NGFS2021_GCAM_NZ2050",
    "NGFS2021_GCAM_CP&NGFS2021_GCAM_NZ2050",
    "NGFS2021_GCAM_NDC&NGFS2021_GCAM_DT",
    "NGFS2021_GCAM_CP&NGFS2021_GCAM_DT",
    "NGFS2021_GCAM_NDC&NGFS2021_GCAM_DN0",
    "NGFS2021_GCAM_CP&NGFS2021_GCAM_DN0",
    "NGFS2021_GCAM_NDC&NGFS2021_GCAM_B2DS",
    "NGFS2021_GCAM_CP&NGFS2021_GCAM_B2DS",
    "GECO2021_CurPol&GECO2021_NDC-LTS",
    "GECO2021_CurPol&GECO2021_1.5C-Unif",
    "WEO2021_APS&WEO2021_NZE_2050",
    "WEO2021_STEPS&WEO2021_NZE_2050",
    "WEO2021_APS&WEO2021_SDS",
    "WEO2021_STEPS&WEO2021_SDS"
  )
output_excel <- "scenario_pairs_comparisons.xlsx"

all_crispy_small <-
  all_crispy %>% dplyr::filter(scenario_duo %in% use_duos)
nonzero_matches <- count_non_zero_matches(all_crispy_small)
write.xlsx(
  nonzero_matches,
  file = file.path(output_dir, output_excel),
  sheetName = "nonzero_rows",
  row.names = TRUE
)
comparison_npv_diff <-
  correl_npv_diff_between_scenarios(all_crispy_small)
write.xlsx(
  comparison_npv_diff,
  file = file.path(output_dir, output_excel),
  sheetName = "comparison_npv_diff",
  row.names = TRUE,
  append=TRUE
)
scenarios_sd_diff_npv_diff_matrix <- std_diff_of_npv_diff_between_scenarios(all_crispy_small)
write.xlsx(
  scenarios_sd_diff_npv_diff_matrix,
  file = file.path(output_dir, output_excel),
  sheetName = "mean_diff_npv_diff",
  row.names = TRUE,
  append=TRUE
)

for (bu in c(
  # "Electric", "ICE", "Hybrid", "FuelCell",
  "Coal", "CoalCap",
  "Gas", "GasCap", "Oil", "OilCap", "HydroCap", "NuclearCap", "RenewablesCap"
)) {
  # use_duos <- c("Oxford2021_base&Oxford2021_fast", "IPR2021_baseline&IPR2021_RPS",
  #               "NGFS2021_REMIND_CP&NGFS2021_REMIND_NZ2050",
  #               "NGFS2021_MESSAGE_CP&NGFS2021_MESSAGE_NZ2050",
  #               "NGFS2021_GCAM_CP&NGFS2021_GCAM_NZ2050",
  #               "WEO2021_STEPS&WEO2021_NZE_2050")

  all_crispy_small <-
    all_crispy %>% dplyr::filter(scenario_duo %in% use_duos
                                 , business_unit == bu)

  comparison_npv_diff <-
    correl_npv_diff_between_scenarios(all_crispy_small)

  output_excel <- glue::glue("scenario_pairs_comparisons_{bu}.xlsx")
  nonzero_matches <- count_non_zero_matches(all_crispy_small)
  write.xlsx(
    nonzero_matches,
    file = file.path(output_dir, output_excel),
    sheetName = "nonzero_rows",
    row.names = TRUE
  )
  write.xlsx(
    comparison_npv_diff,
    file = file.path(output_dir, output_excel),
    sheetName = bu,
    row.names = TRUE,
    append=TRUE
  )
  # scenarios_sd_diff_npv_diff_matrix <- std_diff_of_npv_diff_between_scenarios(all_crispy_small)
  # write.xlsx(
  #   scenarios_sd_diff_npv_diff_matrix,
  #   file = file.path(output_dir, output_excel),
  #   sheetName = "sd_diff_npv_diff",
  #   row.names = TRUE,
  #   append=TRUE
  # )
}

