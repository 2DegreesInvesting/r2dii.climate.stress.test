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

calculate_weights <- function(portfolio, portfolio_type, grouping_variables){

  port_sub <- portfolio %>%
    select(all_of(grouping_variables), holding_id,id, id_name, company_name, value_usd, number_of_shares,
           current_shares_outstanding_all_classes, financial_sector, has_ald_in_fin_sector)

  port_sub <- calculate_port_weight(port_sub, grouping_variables)

  port_sub <- aggregate_holdings(port_sub)


  if (portfolio_type == "Equity"){

    port_sub <- calculate_ownership_weight(port_sub)

  }


  return(port_sub)
}

merge_in_ald <- function(portfolio, ald_scen){


  portfolio <- portfolio %>% left_join(ald_scen, by = "id")

  return(portfolio)

}

port_weight_allocation <- function(port_ald){

  port_ald_pw <- port_ald %>% filter(has_ald_in_fin_sector == TRUE, financial_sector == ald_sector)

  if(data_check(port_ald_pw)){

    port_ald_pw <- calculate_with_weights(port_ald_pw, "port_weight", "portfolio_weight")

  }

  return(port_ald_pw)

}

ownership_allocation <- function(portfolio){

  # Only for equity portfolios

  port_ald_own <- calculate_with_weights(portfolio, "ownership_weight", "ownership_weight")

  port_ald_own <- port_ald_own %>% mutate(plan_carsten = NA,
                                          scen_carsten = NA)

  return(port_ald_own)

}

merge_in_geography <- function(portfolio, ald_raw){

  # ald_raw <- ald_raw_eq
  company_all <- portfolio %>%
    distinct(!!!rlang::syms(grouping_variables), allocation, allocation_weight, id, financial_sector)


  company_all <- company_all %>% filter(financial_sector %in% sector_list)

  ### join with MASTER to get country production
  company_all_data <- left_join(company_all, ald_raw %>% distinct(id, country_of_domicile, ald_location, year,
                                                                  ald_sector, technology, ald_production, ald_production_unit),
                                by=c("id"="id", "financial_sector"="ald_sector")) %>%
    mutate(ald_sector = financial_sector)

  ### complete rows of technology within a sector - we need to have a row for each tech to get a real tech share
  # dont' calculate tech share
  # specific_tech_list <- unique(company_all_data$technology)
  # specific_sector_list <- unique(company_all_data$ald_sector)
  #
  # company_all_data <- company_all_data %>% ungroup() %>%
  #   group_by(investor_name, portfolio_name, allocation, id, financial_sector, allocation, allocation_weight, ald_location,
  #            year, ald_sector) %>%
  #   complete(ald_sector = specific_sector_list,
  #           technology = specific_tech_list,
  #            fill = list(ald_production = 0))
  #
  # company_all_data <- removeInvalidSectorTechCombos(company_all_data)

  company_all_data$plan_alloc_wt_tech_prod <- company_all_data$ald_production * company_all_data$allocation_weight


  return(company_all_data)


}

calculate_scenario_alignment <- function(df){

  browntechs <- c("Oil","Gas","Coal","CoalCap","GasCap", "OilCap","ICE")

  df$trajectory_deviation <- (df$plan_alloc_wt_tech_prod - df$scen_alloc_wt_tech_prod) / df$scen_alloc_wt_tech_prod
  df$trajectory_deviation <- ifelse(df$scen_alloc_wt_tech_prod == 0, ifelse(df$plan_alloc_wt_tech_prod == 0, 0, -1), df$trajectory_deviation)

  df$trajectory_alignment <-  ifelse(!df$technology %in% browntechs, 1 * df$trajectory_deviation, -1 * df$trajectory_deviation)

  df
}

calculate_technology_share <- function(df){
  df <- df %>%
    ungroup() %>%
    mutate(plan_tech_share = plan_alloc_wt_tech_prod/plan_alloc_wt_sec_prod,
           scen_tech_share = scen_alloc_wt_tech_prod/scen_alloc_wt_sec_prod)
  df
}

gather_and_save_project_results <- function(
  results_folder_path = results_path,
  aggregation_level = "portfolio",
  portfolios_per_file = 500,
  year_filter = NA,
  allocation_filter = NA){

  all_investors <- list.dirs(results_folder_path)
  all_investors <- basename(all_investors)[-1]

  k <- 1
  j <- 1

  for (i in 1:length(all_investors)){

    investor_name_select <- all_investors[i]
    print(investor_name_select)

    results_path_investor <- paste0(results_path,"/",investor_name_select,"/")

    if(file.exists(paste0(results_path_investor,"/Equity_results_",aggregation_level,".rda"))){

      results_eq <- as.data.frame(read_rds(paste0(results_path_investor,"/Equity_results_",aggregation_level,".rda")))

      if(typeof(year_filter) %in% c("integer","double")){
        results_eq <- results_eq %>% filter(year %in% year_filter)
      }

      if(typeof(allocation_filter) %in% c("character")){
        results_eq <- results_eq %>% filter(allocation %in% allocation_filter)
      }

      if(nrow(results_eq)!=0){
        if (exists("all_results_eq")){
          all_results_eq <- rbind(all_results_eq, results_eq)
        }else{
          all_results_eq <- results_eq
        }}

    }

    if(file.exists(paste0(results_path_investor,"Bonds_results_",aggregation_level,".rda"))){

      results_cb <- read_rds(paste0(results_path_investor,"Bonds_results_",aggregation_level,".rda"))

      if(typeof(year_filter) %in% c("integer","double")){
        results_cb <- results_cb %>% filter(year %in% year_filter)
      }

      if(typeof(allocation_filter) %in% c("character")){
        results_cb <- results_cb %>% filter(allocation %in% allocation_filter)
      }

      if (exists("all_results_cb")){
        all_results_cb <- rbind(all_results_cb, results_cb)
      }else{
        all_results_cb <- results_cb
      }
    }



    if (j==portfolios_per_file){
      if(exists("all_results_cb")){
        saveRDS(all_results_cb,paste0(results_path,"/Bonds_results_",aggregation_level,"_",k,".rda"))
        rm(all_results_cb)}
      if(exists("all_results_eq")){
        saveRDS(all_results_eq,paste0(results_path,"/Equity_results_",aggregation_level,"_",k,".rda"))
        rm(all_results_eq)}
      j = 1
      k = k + 1
    }else{
      j = j + 1
    }
  }

  if(exists("all_results_cb")){
    saveRDS(all_results_cb,paste0(results_path,"/Bonds_results_",aggregation_level,".rda"))
    write_csv(all_results_cb,paste0(results_path,"/Bonds_results_",aggregation_level,".csv"))}
  if(exists("all_results_eq")){
    saveRDS(all_results_eq,paste0(results_path,"/Equity_results_",aggregation_level,".rda"))
    write_csv(all_results_eq,paste0(results_path,"/Equity_results_",aggregation_level,".csv"))

  }

}


