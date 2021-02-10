
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bsrto

<!-- badges: start -->

[![R build
status](https://github.com/paleolimbot/bsrto/workflows/R-CMD-check/badge.svg)](https://github.com/paleolimbot/bsrto/actions)
<!-- badges: end -->

The goal of bsrto is to generate data products for the [Barrow Strait
Real Time Observatory](https://noise.phys.ocean.dal.ca/barrow/).

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("paleolimbot/bsrto")
```

``` r
library(bsrto)
```

The main event of the bsrto package are the `bs_build_()` functions,
which generate data products from data available on the ftp server. A
number of helper functions support them which can be useful when
debugging problems with the intermediary outputs. You will need to set
up the location of the ftp server and you will probably want to set a
persistent cache directory in your .Rprofile (e.g.,
`usethis::edit_r_profile()`).

``` r
# address of the ftp server
options(bsrto.ftp = "...")

# local path to copy of the ftp server
options(bsrto.cache = "...")

# local build cache (will need invalidating if the
# read functions are updated)
options(bsrto.build_cache = "...")
```

Then you can build the real-time data\! It will take \~60 minutes the
first time (because it needs to download a few thousand files) and \~1
minute each time thereafter.

``` r
bs_build_realtime()
```

You can build other products as well, including the
[Navigator](https://navigator.oceansdata.ca/) data:

``` r
bs_build_navigator()
```
