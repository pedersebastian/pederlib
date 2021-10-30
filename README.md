
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pederlib

<!-- badges: start -->
<!-- badges: end -->

## Installation

You can install the development version of pederlib from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("pedersebastian/pederlib")
```

## Example

``` r
library(pederlib)
#> 
#> Attaching package: 'pederlib'
#> The following object is masked from 'package:base':
#> 
#>     mode
```

| Add on to  | Function name       | Description                                                                                                                                        |
|------------|---------------------|----------------------------------------------------------------------------------------------------------------------------------------------------|
| ggplot2    | theme\_center()     | Based on theme\_minimal(), except light gray for background for the title of facets, and all titles are centered                                   |
|            | theme\_pedr()       | Font: BentonSans, used in eg. Sykepleien forskning. Otherwise like theme\_center()                                                                 |
| scales     | komma()             | like scales::comma\_format(), only that the decimal mark is “,” - not “.”                                                                          |
|            | prosent             | ike scales::percent\_format(), only that the decimal mark is “,” - not “.”                                                                         |
| yardstick  | rmsle()             | Root mean squared log error for yardstick                                                                                                          |
|            | g\_mean()           | Geometric mean of sensitivity and specificity                                                                                                      |
|            | mset()              | return a metric\_set object with different types of metric for regression and classification                                                       |
|            |                     |                                                                                                                                                    |
| “original” | read\_csv\_europe() | import csv using European encoding (“CP1252”), uses the vroom-package                                                                              |
|            | mode()              | find the mode from a vector                                                                                                                        |
|            | sum\_fun()          | Summarize function, returns a tibble                                                                                                               |
|            | use\_split()        | Like the package usemodels, this function returns standard for spiting data into testing and training. Also create re samples from the testing-set |
