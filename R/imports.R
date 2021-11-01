#' @importFrom rlang %||% abort warn .data .env
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes vars geom_col geom_line geom_point geom_hline
#'   geom_vline facet_wrap facet_grid theme expand_limits labs
#'   scale_fill_gradient2 element_text margin
NULL

globalVariables(
  c(
    ".",
    ".data",
    # FIXME
    "B2DS",
    "Baseline",
    "Baseline_price",
    "NPS",
    "SDS",
    "SDS_price",
    "ald_sector",
    "asset_volatility_s_avg",
    "calculate_net_profits",
    "check_portfolio_consistency",
    "check_price_consistency",
    "check_scenario_availability",
    "check_scenario_settings",
    "check_scenario_timeframe",
    "company_id",
    "company_name",
    "corporate_bond_ticker",
    "dcf_model_techlevel",
    "debt_equity_ratio",
    "df_price",
    "has_credit",
    "has_map",
    "has_revenue",
    "has_sb",
    "id",
    "inc_emission_factors",
    "inc_meta_portfolio",
    "inc_stresstest",
    "investor_name",
    "join_price_data",
    "late_sudden_prices",
    "leverage_s_avg",
    "loan_share_credit_type",
    "net_profit_margin",
    "pacta_sectors_not_analysed",
    "pd",
    "plan_sec_carsten",
    "portfolio_name",
    "profit_margin_preferred",
    "profit_margin_unpreferred",
    "scenario",
    "scenario_data",
    "scenario_geography",
    "scenario_name",
    "sector",
    "sector_ald",
    "tech_roadmap_sectors",
    "technology",
    "term",
    "valid_input",
    "valid_value_usd",
    "volatility",
    "weighted.mean",
    "year",
    "year_of_shock"
  )
)
