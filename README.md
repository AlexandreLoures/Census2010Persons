
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Census2010Persons

<!-- badges: start -->

[![build](https://github.com/AlexandreLoures/Census2010Persons/actions/workflows/main.yml/badge.svg)](https://github.com/AlexandreLoures/Census2010Persons/actions)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/Census2010Persons)](https://cran.r-project.org/package=Census2010Persons)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![License:
Mit](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![Github
commit](https://img.shields.io/github/last-commit/AlexandreLoures/Census2010Persons)](https://github.com/AlexandreLoures/Census2010Persons/commit/main)
[![](https://cranlogs.r-pkg.org/badges/grand-total/Census2010Persons?color=blue)](https://cran.r-project.org/package=Census2010Persons)
<!-- badges: end -->

# Installation

To install the `package`, one of the two standard methods for installing
`packages`in R can be adopted. Directly through the
[cran](https://cran.r-project.org/package=Census2010Persons) (choosing
the closest repository):

``` r
install.packages ("Census2010Persons")
```

Or even using the command below. In the latter case, the latest version
of the `package` will be installed.

``` r
# install.packages ("devtools")
devtools::install_github ("AlexandreLoures/Census2010Persons")
```

# Download, read and analyze the microdata

The purpose of this section is to analyze data on cases, deaths,
proportions and geographic distribution on the COVID-19 pandemic in
Brazil, made available by the [Ministerio da Saude - Sistema Unico de
Saude (SUS)](https://www.gov.br/saude/pt-br). Data are available as of
2020-01-30, when the first suspected case appeared in Brazil.
