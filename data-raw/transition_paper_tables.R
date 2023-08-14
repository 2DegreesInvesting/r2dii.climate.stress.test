devtools::load_all()
library(dplyr)
library(xlsx)
library(openxlsx)

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
    "IPR2021_baseline&IPR2021_RPS",
    "Oxford2021_base&Oxford2021_fast",
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
    "IPR2021_baseline&IPR2021_FPS",
    # "NGFS2021_REMIND_NDC&NGFS2021_REMIND_B2DS",
    "NGFS2021_REMIND_CP&NGFS2021_REMIND_B2DS",
    # "NGFS2021_MESSAGE_NDC&NGFS2021_MESSAGE_B2DS",
    "NGFS2021_MESSAGE_CP&NGFS2021_MESSAGE_B2DS",
    # "NGFS2021_GCAM_NDC&NGFS2021_GCAM_B2DS",
    "NGFS2021_GCAM_CP&NGFS2021_GCAM_B2DS",
    # "WEO2021_APS&WEO2021_SDS",
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

use_duos <- c(
  nz_duos,
  dt_duos,
  dn0_duos,
  b2ds_duos)

all_crispy_filtered <- all_crispy_target_named %>%
  dplyr::filter(scenario_duo %in% use_duos, term == 5)

compare_sign_npv_diff_between_scenarios <- function(data) {
  npv_scenarios_change_in_diff_sign <- data %>%
    dplyr::select(
      scenario_duo,
      company_name,
      sector,
      business_unit,
      net_present_value_difference
    ) %>%
    dplyr::mutate(npv_diff = round(net_present_value_difference)) %>%
    dplyr::mutate(npv_diff = dplyr::if_else(npv_diff == 0, NA, npv_diff)) %>%
    dplyr::filter(npv_diff != 0) %>%
    dplyr::mutate(npv_diff_sign = sign(npv_diff)) %>%
    tidyr::pivot_wider(
      id_cols = c("company_name", "sector", "business_unit"),
      names_from = scenario_duo,
      values_from = npv_diff_sign
    )
  unique_scenario_duos <- sort(unique(data$scenario_duo))
  n_scenario_duos <- length(unique_scenario_duos)

  scenarios_npv_diff_sign_agreeing_rate <-
    data.frame(matrix(nrow = n_scenario_duos, ncol = n_scenario_duos))
  colnames(scenarios_npv_diff_sign_agreeing_rate) <-
    unique_scenario_duos
  rownames(scenarios_npv_diff_sign_agreeing_rate) <-
    unique_scenario_duos

  for (scenario_duo1 in unique_scenario_duos) {
    for (scenario_duo2 in unique_scenario_duos) {
      not_nan_scenario_duo_1 <-
        sum(!is.na(npv_scenarios_change_in_diff_sign[, scenario_duo1]))
      not_nan_scenario_duo_2 <-
        sum(!is.na(npv_scenarios_change_in_diff_sign[, scenario_duo2]))

      identical_npv_diff_sign <- sum(
        npv_scenarios_change_in_diff_sign[, scenario_duo1] == npv_scenarios_change_in_diff_sign[, scenario_duo2],
        na.rm = TRUE
      )

      scenarios_npv_diff_sign_agreeing_rate[scenario_duo1, scenario_duo2] <-
        identical_npv_diff_sign / not_nan_scenario_duo_1
      scenarios_npv_diff_sign_agreeing_rate[scenario_duo2, scenario_duo1] <-
        identical_npv_diff_sign / not_nan_scenario_duo_2
    }
  }

  return(scenarios_npv_diff_sign_agreeing_rate)
}

count_non_zero_matches <- function(data) {
  npv_scenarios_diff <- data %>%
    dplyr::mutate(npv_diff = round(net_present_value_difference)) %>%
    dplyr::mutate(npv_diff = dplyr::if_else(npv_diff == 0, NA, npv_diff))

  unique_scenario_duos <- sort(unique(data$scenario_duo))
  n_scenario_duos <- length(unique_scenario_duos)

  scenarios_nonzero_match_matrix <-
    data.frame(matrix(nrow = n_scenario_duos, ncol = n_scenario_duos))
  colnames(scenarios_nonzero_match_matrix) <- unique_scenario_duos
  rownames(scenarios_nonzero_match_matrix) <- unique_scenario_duos

  for (scenario_duo1 in unique_scenario_duos) {
    for (scenario_duo2 in unique_scenario_duos) {
      scenar1 <- npv_scenarios_diff %>%
        dplyr::filter(scenario_duo == scenario_duo1, !is.na(npv_diff)) %>%
        dplyr::select(company_name, sector, business_unit, npv_diff)
      scenar2 <- npv_scenarios_diff %>%
        dplyr::filter(scenario_duo == scenario_duo2, !is.na(npv_diff)) %>%
        dplyr::select(company_name, sector, business_unit, npv_diff)

      merge_comps <- dplyr::inner_join(scenar1,
                                       scenar2,
                                       by = dplyr::join_by(company_name, sector, business_unit))

      nonzero_rows <- nrow(merge_comps)

      scenarios_nonzero_match_matrix[scenario_duo1, scenario_duo2] <-
        nonzero_rows
      scenarios_nonzero_match_matrix[scenario_duo2, scenario_duo1] <-
        nonzero_rows
    }
  }

  return(scenarios_nonzero_match_matrix)
}

correl_npv_diff_between_scenarios <- function(data) {
  npv_scenarios_diff <- data %>%
    dplyr::mutate(npv_diff = round(net_present_value_difference)) %>%
    dplyr::mutate(npv_diff = dplyr::if_else(npv_diff == 0, NA, npv_diff))

  unique_scenario_duos <- sort(unique(data$scenario_duo))
  n_scenario_duos <- length(unique_scenario_duos)

  scenarios_npv_diff_corr_matrix <-
    data.frame(matrix(nrow = n_scenario_duos, ncol = n_scenario_duos))
  colnames(scenarios_npv_diff_corr_matrix) <- unique_scenario_duos
  rownames(scenarios_npv_diff_corr_matrix) <- unique_scenario_duos

  for (scenario_duo1 in unique_scenario_duos) {
    for (scenario_duo2 in unique_scenario_duos) {
      scenar1 <- npv_scenarios_diff %>%
        dplyr::filter(scenario_duo == scenario_duo1, !is.na(npv_diff)) %>%
        dplyr::select(company_name, sector, business_unit, npv_diff)
      scenar2 <- npv_scenarios_diff %>%
        dplyr::filter(scenario_duo == scenario_duo2, !is.na(npv_diff)) %>%
        dplyr::select(company_name, sector, business_unit, npv_diff)

      merge_comps <- dplyr::inner_join(scenar1,
                                       scenar2,
                                       by = dplyr::join_by(company_name, sector, business_unit))

      correlation <- cor(merge_comps$npv_diff.x,
                         merge_comps$npv_diff.y,
                         method = 'pearson')

      scenarios_npv_diff_corr_matrix[scenario_duo1, scenario_duo2] <-
        correlation
      scenarios_npv_diff_corr_matrix[scenario_duo2, scenario_duo1] <-
        correlation
    }
  }

  return(scenarios_npv_diff_corr_matrix)
}


dir.create(
  fs::path("transition_risk_paper", "npv_diff_correlations"),
  recursive = T,
  showWarnings = F
)

for (scenario in
     all_crispy_filtered %>% distinct(target_duo) %>% pull())
{
  data_filtered <-
    all_crispy_filtered %>% filter(target_duo == scenario)

  wb <- openxlsx::createWorkbook()
  addWorksheet(wb, sheetName = "companies_volumes")

  increment = 2
  for (bu in data_filtered %>% distinct(business_unit) %>% pull()) {
    volumes_table <-
      count_non_zero_matches(data_filtered %>%
                               filter(business_unit == bu))
    openxlsx::writeData(
      wb,
      sheet = "companies_volumes",
      x = data.frame(value = bu),
      startCol = 0,
      startRow = increment,
      colNames = F
    )
    openxlsx::writeData(
      wb,
      sheet = "companies_volumes",
      x = volumes_table,
      startCol = 2,
      startRow = increment,
      colNames = T,
      rowNames = T
    )
    increment <- increment + nrow(volumes_table) + 2
  }

  addWorksheet(wb, sheetName = "global")
  scenarios_npv_diff_corr_matrix <-
    correl_npv_diff_between_scenarios(data_filtered)
  openxlsx::writeData(
    wb,
    sheet = "global",
    x = scenarios_npv_diff_corr_matrix,
    startCol = 1,
    startRow = 1,
    colNames = T,
    rowNames = T
  )


  for (bu in data_filtered %>% distinct(business_unit) %>% pull()) {
    addWorksheet(wb, sheetName = bu)
    scenarios_npv_diff_corr_matrix <-
      correl_npv_diff_between_scenarios(data_filtered %>%
                                          filter(business_unit == bu))
    openxlsx::writeData(
      wb,
      sheet = bu,
      x = scenarios_npv_diff_corr_matrix,
      startCol = 1,
      startRow = 1,
      colNames = T,
      rowNames = T
    )
  }

  saveWorkbook(
    wb,
    file = fs::path(
      "transition_risk_paper",
      "npv_diff_correlations",
      paste("correlations",scenario),
      ext = "xlsx"
    ),
    overwrite = T
  )

}

dir.create(
  fs::path("transition_risk_paper", "npv_diff_equal_signs"),
  recursive = T,
  showWarnings = F
)

for (scenario in
     all_crispy_filtered %>% distinct(target_duo) %>% pull())
{
  data_filtered <-
    all_crispy_filtered %>% filter(target_duo == scenario)

  wb <- openxlsx::createWorkbook()
  addWorksheet(wb, sheetName = "companies_volumes")

  increment = 2
  for (bu in data_filtered %>% distinct(business_unit) %>% pull()) {
    volumes_table <-
      count_non_zero_matches(data_filtered %>%
                               filter(business_unit == bu))
    openxlsx::writeData(
      wb,
      sheet = "companies_volumes",
      x = data.frame(value = bu),
      startCol = 0,
      startRow = increment,
      colNames = F
    )
    openxlsx::writeData(
      wb,
      sheet = "companies_volumes",
      x = volumes_table,
      startCol = 2,
      startRow = increment,
      colNames = T,
      rowNames = T
    )
    increment <- increment + nrow(volumes_table) + 2
  }

  addWorksheet(wb, sheetName = "global")
  scenarios_npv_diff_sign_agreeing_rate <-
    compare_sign_npv_diff_between_scenarios(data_filtered)
  openxlsx::writeData(
    wb,
    sheet = "global",
    x = scenarios_npv_diff_sign_agreeing_rate,
    startCol = 1,
    startRow = 1,
    colNames = T,
    rowNames = T
  )


  for (bu in data_filtered %>% distinct(business_unit) %>% pull()) {
    addWorksheet(wb, sheetName = bu)
    scenarios_npv_diff_sign_agreeing_rate <-
      compare_sign_npv_diff_between_scenarios(data_filtered %>%
                                          filter(business_unit == bu))
    openxlsx::writeData(
      wb,
      sheet = bu,
      x = scenarios_npv_diff_sign_agreeing_rate,
      startCol = 1,
      startRow = 1,
      colNames = T,
      rowNames = T
    )
  }

  saveWorkbook(
    wb,
    file = fs::path(
      "transition_risk_paper",
      "npv_diff_equal_signs",
      paste("equal_signs",scenario),
      ext = "xlsx"
    ),
    overwrite = T
  )

}
