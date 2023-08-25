
compare_sign_npv_diff_between_scenarios <- function(data) {
  npv_scenarios_change_in_diff_sign <- data %>%
    dplyr::select(
      scenario_duo,
      company_name,
      sector,
      # business_unit,
      net_present_value_difference
    ) %>%
    dplyr::mutate(npv_diff = round(net_present_value_difference)) %>%
    dplyr::mutate(npv_diff = dplyr::if_else(npv_diff == 0, NA, npv_diff)) %>%
    dplyr::filter(npv_diff != 0) %>%
    dplyr::mutate(npv_diff_sign = sign(npv_diff)) %>%
    tidyr::pivot_wider(
      id_cols = c("company_name", "sector"),
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
        dplyr::select(company_name, sector,
                      # business_unit,
                      npv_diff)
      scenar2 <- npv_scenarios_diff %>%
        dplyr::filter(scenario_duo == scenario_duo2, !is.na(npv_diff)) %>%
        dplyr::select(company_name, sector,
                      # business_unit,
                      npv_diff)

      merge_comps <- dplyr::inner_join(scenar1,
                                       scenar2,
                                       by = dplyr::join_by(company_name, sector,
                                                           # business_unit
                                                           ))

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
        dplyr::select(company_name, sector
                      # ,business_unit
                      , npv_diff
                      # , shock_year
                      )
      scenar2 <- npv_scenarios_diff %>%
        dplyr::filter(scenario_duo == scenario_duo2, !is.na(npv_diff)) %>%
        dplyr::select(company_name, sector
                      # , business_unit
                      , npv_diff
                      # , shock_year
                      )

      merge_comps <- dplyr::inner_join(scenar1,
                                       scenar2,
                                       by = dplyr::join_by(company_name, sector
                                                           # , business_unit, shock_year
                                                           ))

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


correl_npv_roc_between_scenarios <- function(data) {
  npv_scenarios_roc <- data %>%
    dplyr::mutate(npv_roc = round(net_present_value_difference)/net_present_value_baseline) %>%
    dplyr::mutate(npv_roc = dplyr::if_else(npv_roc == 0, NA, npv_roc))

  unique_scenario_duos <- sort(unique(data$scenario_duo))
  n_scenario_duos <- length(unique_scenario_duos)

  scenarios_npv_roc_corr_matrix <-
    data.frame(matrix(nrow = n_scenario_duos, ncol = n_scenario_duos))
  colnames(scenarios_npv_roc_corr_matrix) <- unique_scenario_duos
  rownames(scenarios_npv_roc_corr_matrix) <- unique_scenario_duos

  for (scenario_duo1 in unique_scenario_duos) {
    for (scenario_duo2 in unique_scenario_duos) {
      scenar1 <- npv_scenarios_roc %>%
        dplyr::filter(scenario_duo == scenario_duo1, !is.na(npv_roc)) %>%
        dplyr::select(company_name, sector,
                      # business_unit,
                      npv_roc,
                      # shock_year
                      )
      scenar2 <- npv_scenarios_roc %>%
        dplyr::filter(scenario_duo == scenario_duo2, !is.na(npv_roc)) %>%
        dplyr::select(company_name, sector,
                      # business_unit,
                      npv_roc,
                      # shock_year
                      )

      merge_comps <- dplyr::inner_join(scenar1,
                                       scenar2,
                                       by = dplyr::join_by(company_name, sector,
                                                           # business_unit,
                                                           # shock_year
                                                           ))

      correlation <- cor(merge_comps$npv_roc.x,
                         merge_comps$npv_roc.y,
                         method = 'pearson')

      scenarios_npv_roc_corr_matrix[scenario_duo1, scenario_duo2] <-
        correlation
      scenarios_npv_roc_corr_matrix[scenario_duo2, scenario_duo1] <-
        correlation
    }
  }

  return(scenarios_npv_roc_corr_matrix)
}


correl_pd_diff_between_scenarios <- function(data) {
  pd_diffs_scenarios <- data %>%
    dplyr::mutate(pd_difference = dplyr::if_else(pd_difference == 0, NA, pd_difference))

  unique_scenario_duos <- sort(unique(data$scenario_duo))
  n_scenario_duos <- length(unique_scenario_duos)

  scenarios_pd_diff_corr_matrix <-
    data.frame(matrix(nrow = n_scenario_duos, ncol = n_scenario_duos))
  colnames(scenarios_pd_diff_corr_matrix) <- unique_scenario_duos
  rownames(scenarios_pd_diff_corr_matrix) <- unique_scenario_duos

  for (scenario_duo1 in unique_scenario_duos) {
    for (scenario_duo2 in unique_scenario_duos) {
      scenar1 <- pd_diffs_scenarios %>%
        dplyr::filter(scenario_duo == scenario_duo1, !is.na(pd_difference)) %>%
        dplyr::select(company_name, sector,
                      # business_unit,
                      pd_difference,
                      # shock_year
        )
      scenar2 <- pd_diffs_scenarios %>%
        dplyr::filter(scenario_duo == scenario_duo2, !is.na(pd_difference)) %>%
        dplyr::select(company_name, sector,
                      # business_unit,
                      pd_difference,
                      # shock_year
        )

      merge_comps <- dplyr::inner_join(scenar1,
                                       scenar2,
                                       by = dplyr::join_by(company_name, sector,
                                                           # business_unit,
                                                           # shock_year
                                       ))

      correlation <- cor(merge_comps$pd_difference.x,
                         merge_comps$pd_difference.y,
                         method = 'pearson')

      scenarios_pd_diff_corr_matrix[scenario_duo1, scenario_duo2] <-
        correlation
      scenarios_pd_diff_corr_matrix[scenario_duo2, scenario_duo1] <-
        correlation
    }
  }

  return(scenarios_pd_diff_corr_matrix)
}




#
#
# rsquared_npv_roc_between_scenarios <- function(data) {
#   npv_scenarios_roc <- data %>%
#     dplyr::mutate(npv_roc = round(net_present_value_difference)/net_present_value_baseline) %>%
#     dplyr::mutate(npv_roc = dplyr::if_else(npv_roc == 0, NA, npv_roc))
#
#   unique_scenario_duos <- sort(unique(data$scenario_duo))
#   n_scenario_duos <- length(unique_scenario_duos)
#
#   scenarios_npv_roc_corr_matrix <-
#     data.frame(matrix(nrow = n_scenario_duos, ncol = n_scenario_duos))
#   colnames(scenarios_npv_roc_corr_matrix) <- unique_scenario_duos
#   rownames(scenarios_npv_roc_corr_matrix) <- unique_scenario_duos
#
#   for (scenario_duo1 in unique_scenario_duos) {
#     for (scenario_duo2 in unique_scenario_duos) {
#       scenar1 <- npv_scenarios_roc %>%
#         dplyr::filter(scenario_duo == scenario_duo1, !is.na(npv_roc)) %>%
#         dplyr::select(company_name, sector,
#                       # business_unit,
#                       npv_roc,
#                       # shock_year
#         )
#       scenar2 <- npv_scenarios_roc %>%
#         dplyr::filter(scenario_duo == scenario_duo2, !is.na(npv_roc)) %>%
#         dplyr::select(company_name, sector,
#                       # business_unit,
#                       npv_roc,
#                       # shock_year
#         )
#
#       merge_comps <- dplyr::inner_join(scenar1,
#                                        scenar2,
#                                        by = dplyr::join_by(company_name, sector,
#                                                            # business_unit,
#                                                            # shock_year
#                                        ))
#
#
#       scenarios_npv_roc_corr_matrix[scenario_duo1, scenario_duo2] <-
#         summary(lm(npv_roc.x ~ npv_roc.y, data = merge_comps))$r.squared
#       scenarios_npv_roc_corr_matrix[scenario_duo2, scenario_duo1] <-
#         summary(lm(npv_roc.y ~ npv_roc.x, data = merge_comps))$r.squared
#     }
#   }
#
#   return(scenarios_npv_roc_corr_matrix)
# }
#
#
#
#
# rsquared_npv_diff_between_scenarios <- function(data) {
#   npv_scenarios_diff <- data %>%
#     dplyr::mutate(npv_diff = round(net_present_value_difference)) %>%
#     dplyr::mutate(npv_diff = dplyr::if_else(npv_diff == 0, NA, npv_diff))
#
#   unique_scenario_duos <- sort(unique(data$scenario_duo))
#   n_scenario_duos <- length(unique_scenario_duos)
#
#   scenarios_npv_diff_corr_matrix <-
#     data.frame(matrix(nrow = n_scenario_duos, ncol = n_scenario_duos))
#   colnames(scenarios_npv_diff_corr_matrix) <- unique_scenario_duos
#   rownames(scenarios_npv_diff_corr_matrix) <- unique_scenario_duos
#
#   for (scenario_duo1 in unique_scenario_duos) {
#     for (scenario_duo2 in unique_scenario_duos) {
#       scenar1 <- npv_scenarios_diff %>%
#         dplyr::filter(scenario_duo == scenario_duo1, !is.na(npv_diff)) %>%
#         dplyr::select(company_name, sector,
#                       # business_unit,
#                       npv_diff,
#                       # shock_year
#                       )
#       scenar2 <- npv_scenarios_diff %>%
#         dplyr::filter(scenario_duo == scenario_duo2, !is.na(npv_diff)) %>%
#         dplyr::select(company_name, sector,
#                       # business_unit,
#                       npv_diff,
#                       # shock_year
#                       )
#
#       merge_comps <- dplyr::inner_join(scenar1,
#                                        scenar2,
#                                        by = dplyr::join_by(company_name, sector
#                                                            # , business_unit
#                                                            # , shock_year
#                                                            ))
#
#
#       scenarios_npv_diff_corr_matrix[scenario_duo1, scenario_duo2] <-
#         summary(lm(npv_diff.x ~ npv_diff.y, data = merge_comps))$r.squared
#       scenarios_npv_diff_corr_matrix[scenario_duo2, scenario_duo1] <-
#         summary(lm(npv_diff.y ~ npv_diff.x, data = merge_comps))$r.squared
#
#     }
#   }
#
#   return(scenarios_npv_diff_corr_matrix)
# }
