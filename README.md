
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

  - Use `library()` to attach the package

<!-- end list -->

``` r
library(r2dii.climate.stress.test)
```

  - Run climate stress tests

<!-- end list -->

``` r
## run stress testing for assets of type corporate loans using default parameters
run_stress_test(asset_type = "loans")

## run stress testing for asset of type corporate loans using various risk_free_rates to analyse sensitivities
run_stress_test(asset_type = "loans", risk_free_rate = c(0.01, 0.03))
```

## Details

### Prepare input data

Stress tests for corporate loans require an additional step for input
data preparation for initial application of stress testing on a
loanbook.

``` r
run_prep_calculation_loans()
```

### Look up valid ranges of model parameters

You can look up allowed values of model parameters as such:

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
#> [1] 2025 2040

term_range_lookup
#> [1]  1 10

credit_type_lookup
#> [1] "outstanding"  "credit_limit"
```

\[Get started\] \#\# tbc: add link to vignette
