
<!-- README.md is generated from README.Rmd. Please edit that file -->

# r2dii.climate.stress.test <a href='https://github.com/2DegreesInvesting/r2dii.climate.stress.test'><img src='https://imgur.com/A5ASZPE.png' align='right' height='43' /></a>

<!-- badges: start -->

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R-CMD-check](https://github.com/2DegreesInvesting/r2dii.climate.stress.test/workflows/R-CMD-check/badge.svg)](https://github.com/2DegreesInvesting/r2dii.climate.stress.test/actions)
<!-- badges: end -->

The goal of r2dii.climate.stress.test is to provide a tool that can be
used to conduct what-if climate stress test analyses for financial
institutions, supervisors, regulators and other stakeholders. The tool
aims at highlighting potential financial risk in especially climate
relevant sectors, split by production technology where required. The
sectors covered by the 2DII climate stress test and therefore by this
package, follow mostly the logic of the Paris Agreement Capital
Transition Assessment (PACTA) tool, but can in principle be adapted to
other settings. Application of the code requires availability of custom
data. For more information about the methodology and inquiries on
running a pilot of the stress test in cooperation with 2DII, please
contact <jacob@2degrees-investing.org>.

## Installation

Install the development version of r2dii.climate.stress.test from GitHub
with:

``` r
# install.packages("devtools")
devtools::install_github("2DegreesInvesting/r2dii.climate.stress.test")
```

## Example

-   Use `library()` to attach the package

``` r
library(r2dii.climate.stress.test)
```

-   Stress tests for corporate loans require a preparatory step for
    input data preparation for initial application of stress testing on
    a loan book

``` r
run_prep_calculation_loans(
  input_path_project_specific = "/path/to/specific/data",
  input_path_project_agnostic = "/path/to/agnostic/data",
  data_prep_output_path = "/path/to/specific/data"
)
```

-   Run climate stress tests

``` r
## run stress testing for assets of type corporate loans using default parameters
run_stress_test(
  asset_type = "loans",
  input_path_project_specific = "/path/to/specific/data",
  input_path_project_agnostic = "/path/to/agnostic/data",
  output_path = "/path/to/output/directory"
)

## run stress testing for asset of type corporate loans using various risk_free_rates to analyse sensitivities
run_stress_test(
  asset_type = "loans",
  input_path_project_specific = "/path/to/specific/data",
  input_path_project_agnostic = "/path/to/agnostic/data",
  output_path = "/path/to/output/directory",
  risk_free_rate = c(0.01, 0.03)
)
```

## Details

### Look up valid ranges of input arguments

You can look up allowed values of input arguments as such:

``` r
lgd_senior_claims_range_lookup
#> [1] 0.3 0.6

lgd_subordinated_claims_range_lookup
#> [1] 0.6 0.9

terminal_value_range_lookup
#> [1] 0.0 0.1

risk_free_rate_range_lookup
#> [1] 0.00 0.05

discount_rate_range_lookup
#> [1] -0.01  0.05

div_netprofit_prop_coef_range_lookup
#> [1] 0.8 1.0

shock_year_range_lookup
#> [1] 2025 2035

term_range_lookup
#> [1]  1 10

credit_type_lookup
#> [1] "outstanding"  "credit_limit"
```

[Further
Information](https://2degreesinvesting.github.io/r2dii.climate.stress.test/articles/).
