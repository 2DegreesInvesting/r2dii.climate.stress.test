# styler: off
stress_test_arguments <- tibble::tribble(
                      ~name,       ~type, ~default,               ~allowed,       ~min,       ~max,
               "asset_type", "character",       NA, "equity, bonds, loans",         NA,         NA,
        "lgd_senior_claims",    "double",   "0.45",                     NA,      "0.3",      "0.6",
  "lgd_subordinated_claims",    "double",   "0.75",                     NA,      "0.6",      "0.9",
#           "terminal_value",    "double",      "0",                     NA,        "0",      "0.1",
           "risk_free_rate",    "double",   "0.02",                     NA,        "0",     "0.05",
            "discount_rate",    "double",   "0.07",                     NA,    "-0.01",      "0.1",
  "div_netprofit_prop_coef",    "double",      "1",                     NA,      "0.8",        "1",
               "shock_year",    "double",   "2030",                     NA,     "2025",     "2035",
                     "term",    "double",      "2",                     NA,        "1",       "10",
        "company_exclusion",   "logical",   "TRUE",          "TRUE, FALSE",         NA,         NA
)
# styler: on

usethis::use_data(stress_test_arguments, overwrite = TRUE)
