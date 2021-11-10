# From commit 9ad2e4082fd7dc9ff29bf00d4fcd9f947b325371

library(r2dii.climate.stress.test)
library(tidyverse)
library(glue)

formals <- formals(run_stress_test)
formals$asset_type <- NA

defaults <- formals %>%
  as.list() %>%
  enframe() %>%
  unnest(value) %>%
  rename(default = value)

x <- glue("{names(formals)}_range_lookup")
x <- setdiff(x, first(x))
x <- setdiff(x, last(x))
ranges <- x %>%
  map(get, "package:r2dii.climate.stress.test") %>%
  map(toString) %>%
  set_names(x) %>%
  enframe() %>%
  unnest(value) %>%
  rename(range = value) %>%
  mutate(name = sub("_range_lookup", "", name))

l <- left_join(defaults, ranges, by = "name")
l[] <- map(l, as.character)
l[l$name == "asset_type", "range"] <- toString(asset_types_lookup)
l[l$name == "company_exclusion", "default"] <- as.character(formals$company_exclusion)
l[l$name == "company_exclusion", "range"] <- toString(c(TRUE, FALSE))
lookup <- l

# datapasta::tribble_paste(lookup)
# styler: off
st_arguments <- tibble::tribble(
                      ~name, ~default,                 ~range,
               "asset_type",       NA, "equity, bonds, loans",
        "lgd_senior_claims",   "0.45",             "0.3, 0.6",
  "lgd_subordinated_claims",   "0.75",             "0.6, 0.9",
           "terminal_value",      "0",               "0, 0.1",
           "risk_free_rate",   "0.02",              "0, 0.05",
            "discount_rate",   "0.02",          "-0.01, 0.05",
  "div_netprofit_prop_coef",      "1",               "0.8, 1",
               "shock_year",   "2030",           "2025, 2040",
                     "term",      "2",                "1, 10",
        "company_exclusion",   "TRUE",          "TRUE, FALSE"
)
# styler: on

use_data(st_arguments, overwrite = TRUE)
