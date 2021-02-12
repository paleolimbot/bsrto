
FROM rocker/r-ver:4.0.3

RUN apt-get update -qq && apt-get install -y --no-install-recommends \
  git-core \
  libssl-dev \
  libcurl4-gnutls-dev \
  libnetcdf-dev \
  curl \
  libsodium-dev \
  libxml2-dev

# Freezes packages to CRAN at a specific date
# Update here: https://packagemanager.rstudio.com/client/#/repos/1/overview
ENV CRAN=https://packagemanager.rstudio.com/all/__linux__/focal/1265616

# Set up cache directories (runners should mount these caches)
RUN mkdir -p /bsrto/built
RUN mkdir /bsrto/cache
RUN mkdir /bsrto/build-cache

ENV R_BSRTO_CACHE=/bsrto/cache
ENV R_BSRTO_BUILD_CACHE=/bsrto/build-cache

WORKDIR /bsrto

## Start install
RUN R -e 'install.packages(c("remotes", "rlang", "glue", "readr", "tibble", "vctrs", "stringr", "curl", "hms", "progress", "cli", "ncdf4", "dplyr", "abind", "sessioninfo", "gsw", "testthat", "withr", "tidyr"), repos = Sys.getenv("CRAN"))'
RUN R -e 'remotes::install_github("paleolimbot/bsrto@fa5013c", repos = Sys.getenv("CRAN"))'
## End install

