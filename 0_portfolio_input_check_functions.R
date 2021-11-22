### Fin data cleaning functions
map_security_sectors <- function(fin_data, sector_bridge){

  initial_no_rows = nrow(fin_data)

  fin_data <- fin_data %>% dplyr::left_join(sector_bridge %>% dplyr::filter(source == "BICS") %>% dplyr::select(-source),
                                     by = c("security_bics_subgroup" = "industry_classification")) %>%
    dplyr::mutate(security_icb_subsector = as.character(security_icb_subsector))

  fin_data_na <- fin_data %>% dplyr::filter(is.na(sector)) %>% dplyr::select(-c(sector,sector_boe,sector_ipr,subsector_ipr,sector_dnb,subsector_boe))

  fin_data <- fin_data %>% dplyr::filter(!is.na(sector))

  fin_data_na <- fin_data_na %>% dplyr::left_join(sector_bridge %>% dplyr::filter(source == "ICB") %>% dplyr::select(-source),
                                           by = c("security_icb_subsector" = "industry_classification"))

  fin_data <- fin_data %>% dplyr::bind_rows(fin_data_na)

  fin_data <- fin_data %>% dplyr::select(-security_mapped_sector,sector_boe,sector_ipr,subsector_ipr,sector_dnb,subsector_boe) %>% dplyr::rename(security_mapped_sector = sector)

  fin_data %>% dplyr::group_by(security_mapped_sector) %>% dplyr::filter(is.na(security_mapped_sector)) %>% dplyr::summarise(count = dplyr::n(), .groups = "drop_last")
  fin_data_na <- fin_data %>% dplyr::filter(is.na(security_mapped_sector))

  if(nrow(fin_data) != initial_no_rows){stop("Rows being dropped in mapping sectors")}

  return(fin_data)

}

override_sector_classification <- function(fin_data, overrides){

  start_rows <- nrow(fin_data)

  overrides <- overrides %>%
    dplyr::mutate(across(c(company_name, corporate_bond_ticker, fin_sector_override), as.character))

  overrides$sector_override <- TRUE


  # Merge in by company corp ticker
  overrides_cbt <- overrides %>%
    dplyr::filter(corporate_bond_ticker != "" , !is.na(corporate_bond_ticker)) %>%
    dplyr::select(corporate_bond_ticker, fin_sector_override, sector_override) %>%
    dplyr::distinct()

  fin_data <- dplyr::left_join(fin_data, overrides_cbt, by = "corporate_bond_ticker")

  # Merge in by bloomberg_id
  overrides_bbg <- overrides %>%
    dplyr::filter(is.na(corporate_bond_ticker)|corporate_bond_ticker == "")%>%
    dplyr::select(bloomberg_id, fin_sector_override, sector_override) %>%
    dplyr::distinct()

  fin_data <- dplyr::left_join(fin_data, overrides_bbg, by = "bloomberg_id")

  # Clean resulting financial data
  fin_data <- fin_data %>%
    dplyr::mutate(sector_override = sector_override.x,
           sector_override = dplyr::if_else(sector_override.y != ""&!is.na(sector_override.y), sector_override.y, sector_override),
           fin_sector_override = fin_sector_override.x,
           fin_sector_override = dplyr::if_else(!is.na(fin_sector_override.y)&fin_sector_override.y != "", fin_sector_override.y, fin_sector_override),
           sector_override = dplyr::if_else(is.na(sector_override),FALSE,TRUE)) %>%
    dplyr::select(-sector_override.x, -sector_override.y, -fin_sector_override.x, -fin_sector_override.y)

  fin_data <- fin_data %>%
    dplyr::mutate(security_mapped_sector = dplyr::if_else(sector_override, fin_sector_override, security_mapped_sector)) %>%
    dplyr::select(-fin_sector_override)

  if (nrow(fin_data) != start_rows){stop("Additional rows being added by fin sector override")}

  fin_data

}

check_asset_types <- function(fin_data){

  fin_data <- fin_data %>%
    dplyr::mutate(asset_type = dplyr::if_else(asset_type == "Other", "Others", asset_type),
           asset_type = dplyr::if_else(is.na(asset_type), "Others", asset_type),
    )

  fin_data$asset_type <- stringr::str_to_sentence(fin_data$asset_type)

  ### TEST
  if (!any(unique(fin_data$asset_type) %in% allowable_asset_list)){
    stop("Check Financial Data Asset Types")
  }

  fin_data

}

check_mapped_assets_flag <- function(fin_data){


  # convert old naming of "mapped to assets" column to be mapped_to_assets

  if("EQ.mapped_to_assets" %in% colnames(fin_data) | "CB.mapped_to_assets" %in% colnames(fin_data) | "has_prod_after_2018" %in% colnames(fin_data)) {

    if ("EQ.mapped_to_assets" %in% colnames(fin_data)| "CB.mapped_to_assets" %in% colnames(fin_data)){
      fin_data <- fin_data %>%
        dplyr::mutate(
          mapped_to_assets = dplyr::case_when(Asset.Type == "Equity" ~ EQ.mapped_to_assets,
                                       Asset.Type == "Bonds" ~ CB.mapped_to_assets,
                                       TRUE ~ 0)) %>%
        dplyr::select(-CB.mapped_to_assets,-EQ.mapped_to_assets)
    }else if("has_prod_after_2018" %in% colnames(fin_data)){
      fin_data <- fin_data %>%
        dplyr::mutate(
          mapped_to_assets = has_prod_after_2018
        ) %>% dplyr::select(-has_prod_after_2018)
    }

  }

  unique(fin_data$mapped_to_assets)

  # Ensure that flag is a logical

  fin_data <- fin_data %>%
    dplyr::mutate(mapped_to_assets = dplyr::case_when(mapped_to_assets %in% c("t",1) ~ TRUE,
                                        mapped_to_assets %in% c("f",0) ~ FALSE
    ))

  ### TEST
  any(!fin_data$mapped_to_assets %in% c(TRUE, FALSE))
  ###


  fin_data


}

check_fin_mapped_sectors <- function(fin_data){

  fin_data <- fin_data %>%
    dplyr::mutate(security_mapped_sector = dplyr::case_when(security_mapped_sector == "Others" ~ "Other",
                                              security_mapped_sector == "OIl&Gas" ~ "Oil&Gas",
                                              is.na(security_mapped_sector) ~ "Other",
                                              TRUE ~ security_mapped_sector))

  actual_sectors <- unique(fin_data$security_mapped_sector)

  if(any(!actual_sectors %in% c(sector_list, other_sector_list, "Other"))){
    stop("Additional Sectors in fin_data")
  }

  fin_data

}

convert_corporate_bonds <- function(fin_data){

  cb_groups <- c("Convertible Bonds", "Corporate Bonds", "Corporate inflation linked Bonds","Convertible Preferreds" )

  fin_data <- fin_data %>%
    dplyr::mutate(asset_type = dplyr::if_else(security_type %in% cb_groups,"Bonds",asset_type),
           asset_type = dplyr::if_else(!security_type %in% cb_groups & asset_type == "Bonds","Others",asset_type),
    )

  fin_data
}

identify_sb <- function(fin_data){

  sb_groups <- c("Sovereign Debt","Sovereign Agency Debt", "Government inflation linked Bonds", "Sovereign","Sovereign Agency", "Sovereigns")

  fin_data <- fin_data %>%
    dplyr::mutate(is_sb = dplyr::case_when(security_type %in% sb_groups ~ TRUE,
                             security_bics_subgroup %in% sb_groups ~ TRUE,
                             TRUE ~ FALSE))

  fin_data

}

classify_all_funds <- function(fin_data){

  nrow(fin_data[fin_data$asset_type == "Funds",])

  fin_data <- fin_data %>%
    dplyr::mutate(asset_type = dplyr::case_when(grepl("Fund", security_type) ~ "Funds" ,
                                  grepl("ETF", security_type) ~ "Funds",
                                  grepl("Fund", security_bclass4) ~ "Funds" ,
                                  grepl("ETF", security_bclass4) ~ "Funds",
                                  grepl("Fund", security_icb_subsector) ~ "Funds" ,
                                  grepl("ETF", security_icb_subsector) ~ "Funds",
                                  TRUE ~ asset_type))


  ### TEST?

  fin_data
}

### Portfolio Check Functions

# Checks whether a variable is a dataframe. Considers also logicals and null values.
data_check <- function(df) {
  if (is.data.frame(df)) {
    if (nrow(df) > 0) {
      check <- TRUE
    } else {
      check <- FALSE
    }
  } else {
    check <- FALSE
  }

  return(check)
}
