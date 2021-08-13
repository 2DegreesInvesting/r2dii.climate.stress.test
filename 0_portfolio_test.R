# Portfolio test functions
aggregate_holdings <- function(portfolio){

  portfolio <- portfolio %>%
    ungroup() %>%
    # group_by(vars(all_of(grouping_variables))) %>%
    # group_by(holding_id, id, financial_sector, add = T) %>%
    group_by(!!!rlang::syms(grouping_variables), company_name, id, financial_sector, current_shares_outstanding_all_classes, has_ald_in_fin_sector) %>%
    summarise(number_holdings = n_distinct(holding_id),
              value_usd = sum(value_usd, na.rm = T),
              number_of_shares = sum(number_of_shares, na.rm = T),
              port_weight = sum(port_weight),
              .groups = "drop_last")

  return(portfolio)

}

calculate_ownership_weight <- function(portfolio){

  portfolio <- portfolio %>%
    mutate(ownership_weight = number_of_shares/current_shares_outstanding_all_classes)

  return(portfolio)
}

calculate_port_weight <- function(portfolio, grouping_variables){

  portfolio <- portfolio %>%
    ungroup() %>%
    group_by(!!!rlang::syms(grouping_variables)) %>%
    mutate(port_total_aum = sum(value_usd, na.rm =  T),
           port_weight = value_usd/port_total_aum)

  # temp <- portfolio %>%
  #   group_by(!!!rlang::syms(grouping_variables)) %>%
  #   mutate(total_port_weight = sum(port_weight))
  #
  # total_port_weight_per_portfolio <- signif(unique(temp$total_port_weight),2)
  # # check that all portfolio port_weight's sum to 1
  # if (!all(total_port_weight_per_portfolio == 1.0)) {stop("Port weight calculation error")}


  portfolio

}

calculate_with_weights <- function(df, weight_col_name, weight_method_name) {

  df[,"plan_br_dist_alloc_wt"] <- df[,weight_col_name] * df$plan_br_wt_factor
  df[,"plan_carsten"] <- df[,weight_col_name] * df$plan_br_wt_techshare
  df[,"scen_br_dist_alloc_wt"] <- df[,weight_col_name] * df$scen_br_wt_factor
  df[,"scen_carsten"] <- df[,weight_col_name] * df$scen_br_wt_techshare

  df[,"plan_alloc_wt_tech_prod"] <- df[,weight_col_name] * df$plan_tech_prod
  df[,"scen_alloc_wt_tech_prod"] <- df[,weight_col_name] * df$scen_tech_prod

  df[,"allocation"] <- weight_method_name
  df[,"allocation_weight"] <- df[,weight_col_name]
  df
}
