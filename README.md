
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
sectors covered by the 2Dii climate stress test and therefore by this
package, follow mostly the logic of the Paris Agreement Capital
Transition Assessment (PACTA) tool, but can in principle be adapted to
other settings. Application of the code requires availability of custom
data. In order to participate in a climate stress testing project with 2
degrees investing initiative please contact
<jacob@2degrees-investing.org>.

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

-   Run climate stress tests

``` r
## run stress testing for assets of type corporate loans using default parameters
run_stress_test(asset_type = "loans")

## run stress testing for asset of type corporate bond using various risk_free_rates to analyse sensitivities
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
# looking up allowed range for risk_free_rate
risk_free_rate_range_lookup
#> [1] 0.00 0.05

# looking up allowed values for credit_type
credit_type_lookup
#> [1] "outstanding"  "credit_limit"
```

\[Get started\] \#\# tbc: add link to vignette

## Funding

This project has received funding from the [European Union LIFE
program](https://wayback.archive-it.org/12090/20210412123959/https://ec.europa.eu/easme/en/)
and the [International Climate Initiative
(IKI)](https://www.international-climate-initiative.com/en/details/project/measuring-paris-agreement-alignment-and-financial-risk-in-financial-markets-18_I_351-2982).
The Federal Ministry for the Environment, Nature Conservation and
Nuclear Safety (BMU) supports this initiative on the basis of a decision
adopted by the German Bundestag. The views expressed are the sole
responsibility of the authors and do not necessarily reflect the views
of the funders. The funders are not responsible for any use that may be
made of the information it contains.
