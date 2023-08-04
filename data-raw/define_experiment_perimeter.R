devtools::load_all()

filter_matching_shocks <-
  function(baseline_choice, available_shocks) {
    if (grepl("NGFS2021", baseline_choice)){
      matching_shocks <- available_shocks[grepl("NGFS2021", available_shocks)]
    }

    if (grepl("IPR2021", baseline_choice) ) {
      matching_shocks <- available_shocks[grepl("IPR2021", available_shocks)]
    }

    if (grepl("GECO2021", baseline_choice)) {
      matching_shocks <- available_shocks[grepl("GECO2021", available_shocks)]
    }

    if (grepl("Oxford2021", baseline_choice)){
      matching_shocks <- available_shocks[grepl("Oxford2021", available_shocks)]
    }

    if (grepl("WEO2021", baseline_choice)) {
      matching_shocks <- available_shocks[grepl("WEO2021", available_shocks)]
    }
    return(matching_shocks)
  }


available_baselines <- as.character(
  stress_test_arguments %>%
    dplyr::filter(name == "baseline_scenario") %>%
    dplyr::select(allowed)
)

baseline_choice <-
  readline(prompt = paste("select a baseline scenario :", available_baselines))

available_shocks <- as.character(
  stress_test_arguments %>%
    dplyr::filter(name == "shock_scenario") %>%
    dplyr::select(allowed)
)
available_shocks <- stringr::str_split(available_shocks, ', ')[[1]]

matching_shocks <- filter_matching_shocks(baseline_choice, available_shocks)
shock_choice <- readline(prompt = paste("select a shock scenario :", paste(matching_shocks, collapse = ', ')))

available_geographies <- scenario_geography_x_ald_sector %>%
  dplyr::filter(scenario %in% c(baseline_choice, shock_choice)) %>%
  dplyr::distinct(scenario_geography) %>%
  dplyr::pull()

cat(paste("The following geographies are available for the selected baseline/shock scenarios:\n", paste(available_geographies, collapse=', ')))
