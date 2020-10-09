
<!-- README.md is generated from README.Rmd. Please edit that file -->

# finance

<!-- badges: start -->

<!-- badges: end -->

The goal of finance is to provide personal finance tools such as asset
allocation.

## Installation

You can install the released version of finance from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("finance")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("eanway/finance")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(finance)
allocate_assets(1, 1, 1)
#> $total_amount
#> [1] 1
#> 
#> $annual_amount
#> [1] 1
#> 
#> $years_until_available
#> [1] 1
#> 
#> $amount_needed_until_available
#> [1] 0.9754115
#> 
#> $ratio_total_annual
#> [1] 1
#> 
#> $ratio_total_annual_asymptote
#> [1] 19.02459
#> 
#> $years_until_available_asymptote
#> [1] 59.91465
#> 
#> $is_infinite
#> [1] FALSE
#> 
#> $ratio_total_annual_infinite
#> [1] 20
#> 
#> $years_until_depleted
#> [1] 2.079908
#> 
#> $total_amount_infinite_min
#> [1] 19.02459
#> 
#> $annual_amount_infinite_max
#> [1] 0.05256355
#> 
#> $percent_bonds
#> [1] 0.8397397
#> 
#> $amount_bonds
#> [1] 0.8397397
#> 
#> $amount_bonds_infinite_max
#> [1] 0.3315446
#> 
#> attr(,"class")
#> [1] "model"
```
