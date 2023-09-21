devtools::load_all()
library(dplyr)
library(xlsx)
library(openxlsx)
source(file = "data-raw/sa_functions.R")


stopifnot(length(unique(data$scenario_duo)) == length(unique(data$scenario_duo_bckp)))

# mlflow_python_bin <-
#   "/Users/bertrandgallice/opt/miniconda3/envs/mlflow_env/bin/python"
# mlflow_bin <-
#   "/Users/bertrandgallice/opt/miniconda3/envs/mlflow_env/bin/mlflow"
#
# Sys.setenv(MLFLOW_PYTHON_BIN = mlflow_python_bin,
#            MLFLOW_BIN = mlflow_bin)
#
# mlflow_uri <- "http://localhost:5000"
# exp_name <- "all_scenarios_default_params_old_data"


#
# ### CODE
#
# read_csv_from_zipped_artifacts <-
#   function(tracking_uri,
#            experiment_name,
#            run_id,
#            csv_filename) {
#     mlflow::mlflow_set_tracking_uri(uri = tracking_uri)
#     mlflow::mlflow_client()
#
#     artifacts_path <-
#       mlflow::mlflow_download_artifacts(path = "", run_id = run_id)
#     f_conn <-
#       unz(file.path(artifacts_path, "artifacts.zip"), csv_filename)
#     artifact <- readr::read_csv(f_conn, show_col_types = FALSE)
#     return(artifact)
#   }
#
#
# mlflow::mlflow_set_tracking_uri(uri = mlflow_uri)
# mlflow::mlflow_client()
# experiment <- mlflow::mlflow_get_experiment(name = exp_name)
# experiment_id <- experiment[[1, "experiment_id"]]
#
# all_runs <-
#   mlflow::mlflow_search_runs(filter = "tags.LOG_STATUS = 'SUCCESS'",
#                              experiment_ids = as.character(experiment_id))
#
#
# all_crispy <- NULL
# for (run_id in all_runs[["run_uuid"]]) {
#   crispy <- read_csv_from_zipped_artifacts(
#     tracking_uri = mlflow_uri,
#     experiment_name = exp_name,
#     run_id = run_id,
#     csv_filename = "crispy_output.csv"
#   )
#   crispy <- crispy %>%
#     dplyr::mutate(scenario_duo = paste(baseline_scenario, "&", shock_scenario, sep = ""))
#   crispy <- crispy %>%
#     dplyr::mutate(run_id = run_id)
#
#   all_crispy <- dplyr::bind_rows(all_crispy, crispy)
# }
#
#
#
# nz_duos <-
#   c(
#     "IPR2021_baseline&IPR2021_RPS",
#     "Oxford2021_base&Oxford2021_fast",
#     "NGFS2021_REMIND_CP&NGFS2021_REMIND_NZ2050",
#     # "NGFS2021_REMIND_NDC&NGFS2021_REMIND_NZ2050",
#     # "NGFS2021_MESSAGE_NDC&NGFS2021_MESSAGE_NZ2050",
#     "NGFS2021_MESSAGE_CP&NGFS2021_MESSAGE_NZ2050",
#     # "NGFS2021_GCAM_NDC&NGFS2021_GCAM_NZ2050",
#     "NGFS2021_GCAM_CP&NGFS2021_GCAM_NZ2050",
#     # "WEO2021_APS&WEO2021_NZE_2050",
#     "WEO2021_STEPS&WEO2021_NZE_2050" # stated policy scenario == current policies ?
#   )
#
# dt_duos <-
#   c(
#     # "NGFS2021_REMIND_NDC&NGFS2021_REMIND_DT",
#     "NGFS2021_REMIND_CP&NGFS2021_REMIND_DT",
#     # "NGFS2021_MESSAGE_NDC&NGFS2021_MESSAGE_DT",
#     "NGFS2021_MESSAGE_CP&NGFS2021_MESSAGE_DT",
#     # "NGFS2021_GCAM_NDC&NGFS2021_GCAM_DT",
#     "NGFS2021_GCAM_CP&NGFS2021_GCAM_DT"
#   )
#
# dn0_duos <-
#   c(
#     # "NGFS2021_REMIND_NDC&NGFS2021_REMIND_DN0",
#     "NGFS2021_REMIND_CP&NGFS2021_REMIND_DN0",
#     # "NGFS2021_MESSAGE_NDC&NGFS2021_MESSAGE_DN0",
#     "NGFS2021_MESSAGE_CP&NGFS2021_MESSAGE_DN0",
#     # "NGFS2021_GCAM_NDC&NGFS2021_GCAM_DN0",
#     "NGFS2021_GCAM_CP&NGFS2021_GCAM_DN0"
#   )
#
# b2ds_duos <-
#   c(
#     "IPR2021_baseline&IPR2021_FPS",
#     # "NGFS2021_REMIND_NDC&NGFS2021_REMIND_B2DS",
#     "NGFS2021_REMIND_CP&NGFS2021_REMIND_B2DS",
#     # "NGFS2021_MESSAGE_NDC&NGFS2021_MESSAGE_B2DS",
#     "NGFS2021_MESSAGE_CP&NGFS2021_MESSAGE_B2DS",
#     # "NGFS2021_GCAM_NDC&NGFS2021_GCAM_B2DS",
#     "NGFS2021_GCAM_CP&NGFS2021_GCAM_B2DS",
#     # "WEO2021_APS&WEO2021_SDS",
#     "WEO2021_STEPS&WEO2021_SDS"
#   )
#
#
#
# all_crispy_target_named <- all_crispy %>%
#   dplyr::mutate(
#     target_duo = dplyr::case_when(
#       scenario_duo %in% nz_duos ~ "NZ2050",
#       scenario_duo %in% dt_duos ~ "DT",
#       scenario_duo %in% dn0_duos ~ "DN0",
#       scenario_duo %in% b2ds_duos ~ "B2DS",
#       .default = "other"
#     )
#   )
#
# use_duos <- c(
#   nz_duos,
#   dt_duos,
#   dn0_duos,
#   b2ds_duos)
#
# all_crispy_filtered <- all_crispy_target_named %>%
#   dplyr::filter(scenario_duo %in% use_duos, term == 5)





# NPV DIFF CORRELATION ==========================
dir.create(
  fs::path(output_dir, "npv_diff_correlations"),
  recursive = T,
  showWarnings = F
)

for (scenario in c("all_scenario_types", all_crispy_filtered %>% distinct(target_duo) %>% pull()))
{
  if (scenario == "all_scenario_types"){
    data_filtered <- all_crispy_filtered
  }
  else{
    data_filtered <-
      all_crispy_filtered %>% filter(target_duo == scenario)
  }
  wb <- openxlsx::createWorkbook()
  addWorksheet(wb, sheetName = "companies_volumes")

  increment = 2
  for (sector_name in data_filtered %>% distinct(sector) %>% pull()) {
    volumes_table <-
      count_matching_volumes(data_filtered %>%
                               filter(sector==sector_name))
    openxlsx::writeData(
      wb,
      sheet = "companies_volumes",
      x = data.frame(value = sector_name),
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


  for (sector_name in data_filtered %>% distinct(sector) %>% pull()) {
    addWorksheet(wb, sheetName = sector_name)
    scenarios_npv_diff_corr_matrix <-
      correl_npv_diff_between_scenarios(data_filtered %>%
                                         filter(sector == sector_name))
    openxlsx::writeData(
      wb,
      sheet = sector_name,
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
      output_dir,
      "npv_diff_correlations",
      paste("correlations",scenario),
      ext = "xlsx"
    ),
    overwrite = T
  )

}


# NPV ROC CORRELATION ==========================

dir.create(
  fs::path(output_dir, "npv_roc_correlations"),
  recursive = T,
  showWarnings = F
)


for (scenario in c("all_scenario_types", all_crispy_filtered %>% distinct(target_duo) %>% pull()))
{
  if (scenario == "all_scenario_types"){
    data_filtered <- all_crispy_filtered
  }
  else{
  data_filtered <-
    all_crispy_filtered %>% filter(target_duo == scenario)
}
  wb <- openxlsx::createWorkbook()
  addWorksheet(wb, sheetName = "companies_volumes")

  increment = 2
  for (sector_name in data_filtered %>% distinct(sector) %>% pull()) {
    volumes_table <-
      count_matching_volumes(data_filtered %>%
                               filter(sector==sector_name))
    openxlsx::writeData(
      wb,
      sheet = "companies_volumes",
      x = data.frame(value = sector_name),
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
  scenarios_npv_roc_corr_matrix <-
    correl_npv_roc_between_scenarios(data_filtered)
  openxlsx::writeData(
    wb,
    sheet = "global",
    x = scenarios_npv_roc_corr_matrix,
    startCol = 1,
    startRow = 1,
    colNames = T,
    rowNames = T
  )


  for (sector_name in data_filtered %>% distinct(sector) %>% pull()) {
    addWorksheet(wb, sheetName = sector_name)
    scenarios_npv_roc_corr_matrix <-
      correl_npv_roc_between_scenarios(data_filtered %>%
                                          filter(sector == sector_name))
    openxlsx::writeData(
      wb,
      sheet = sector_name,
      x = scenarios_npv_roc_corr_matrix,
      startCol = 1,
      startRow = 1,
      colNames = T,
      rowNames = T
    )
  }

  saveWorkbook(
    wb,
    file = fs::path(
      output_dir,
      "npv_roc_correlations",
      paste("correlations",scenario),
      ext = "xlsx"
    ),
    overwrite = T
  )

}

#================================== NPV DIFF SIGN AGREEING RATE

dir.create(
  fs::path(output_dir, "npv_diff_equal_signs"),
  recursive = T,
  showWarnings = F
)

for (scenario in
     c("all_scenario_types",
     all_crispy_filtered %>% distinct(target_duo) %>% pull())
     )
{
  if (scenario=="all_scenario_types"){
    data_filtered <- all_crispy_filtered
  }else{
  data_filtered <-
    all_crispy_filtered %>% filter(target_duo == scenario)
}
  wb <- openxlsx::createWorkbook()
  addWorksheet(wb, sheetName = "companies_volumes")

  increment = 2
  for (sector_name in data_filtered %>% distinct(sector) %>% pull()) {
    volumes_table <-
      count_matching_volumes(data_filtered %>%
                               filter(sector == sector_name))
    openxlsx::writeData(
      wb,
      sheet = "companies_volumes",
      x = data.frame(value = sector_name),
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


  for (sector_name in data_filtered %>% distinct(sector) %>% pull()) {
    addWorksheet(wb, sheetName = sector_name)
    scenarios_npv_diff_sign_agreeing_rate <-
      compare_sign_npv_diff_between_scenarios(data_filtered %>%
                                          filter(sector == sector_name))
    openxlsx::writeData(
      wb,
      sheet = sector_name,
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
      output_dir,
      "npv_diff_equal_signs",
      paste("equal_signs",scenario),
      ext = "xlsx"
    ),
    overwrite = T
  )

}



#================================== PD DIFF CORRELATION

dir.create(
  fs::path(output_dir, "pd_diff_corr"),
  recursive = T,
  showWarnings = F
)

for (scenario in
     c("all_scenario_types",
     all_crispy_filtered %>% distinct(target_duo) %>% pull()))
{
  if (scenario=="all_scenario_types"){
    data_filtered <- all_crispy_filtered
  }else{
    data_filtered <-
      all_crispy_filtered %>% filter(target_duo == scenario)
  }
  wb <- openxlsx::createWorkbook()
  addWorksheet(wb, sheetName = "companies_volumes")

  increment = 2
  for (sector_name in data_filtered %>% distinct(sector) %>% pull()) {
    volumes_table <-
      count_matching_volumes(data_filtered %>%
                               filter(sector == sector_name))
    openxlsx::writeData(
      wb,
      sheet = "companies_volumes",
      x = data.frame(value = sector_name),
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
  scenarios_pd_diff_correlation <-
    correl_pd_diff_between_scenarios(data_filtered)
  openxlsx::writeData(
    wb,
    sheet = "global",
    x = scenarios_pd_diff_correlation,
    startCol = 1,
    startRow = 1,
    colNames = T,
    rowNames = T
  )


  for (sector_name in data_filtered %>% distinct(sector) %>% pull()) {
    addWorksheet(wb, sheetName = sector_name)
    scenarios_pd_diff_correlation <-
      correl_pd_diff_between_scenarios(data_filtered %>%
                                                filter(sector == sector_name))
    openxlsx::writeData(
      wb,
      sheet = sector_name,
      x = scenarios_pd_diff_correlation,
      startCol = 1,
      startRow = 1,
      colNames = T,
      rowNames = T
    )
  }

  saveWorkbook(
    wb,
    file = fs::path(
      output_dir,
      "pd_diff_corr",
      paste("pd_diff_corr_",scenario),
      ext = "xlsx"
    ),
    overwrite = T
  )

}

#
# #======= QUADRANT ANALYSIS

dir.create(
  fs::path(output_dir, "quadrant_analysis"),
  recursive = T,
  showWarnings = F
)

for (scenario in
     c("all_scenario_types",
     all_crispy_filtered %>% distinct(target_duo) %>% pull())
)
{
  if (scenario=="all_scenario_types"){
    data_filtered <- all_crispy_filtered
  } else {
    data_filtered <-
      all_crispy_filtered %>% filter(target_duo == scenario)
  }
  wb <- openxlsx::createWorkbook()
  addWorksheet(wb, sheetName = "quadrant_analysis")

  quadrants_comparisons <- quadrants_counter(data_filtered)
  quadrants_tables <- make_quadrants_tables(quadrants_comparisons)

  quadrants_comparisons <- quadrants_comparisons %>% mutate(total=Q1+Q2+Q3+Q4)
  quadrants_comparisons %>%readr::write_csv("quadrant_analysis_long.csv")

  openxlsx::writeData(
    wb,
    sheet = "quadrant_analysis",
    x = quadrants_effectifs,
    startCol = 1,
    startRow = 1,
    colNames = T,
    rowNames = T
  )

  saveWorkbook(
    wb,
    file = fs::path(
      output_dir,
      "quadrant_analysis",
      paste("quadrant_analysis_",scenario),
      ext = "xlsx"
    ),
    overwrite = T
  )


}
