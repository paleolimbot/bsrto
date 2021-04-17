
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
ENV CRAN=https://packagemanager.rstudio.com/all/__linux__/focal/2380008

# Set up cache directories (runners should mount these caches)
RUN mkdir -p /bsrto/built
RUN mkdir /bsrto/cache
RUN mkdir /bsrto/build-cache

ENV R_BSRTO_CACHE=/bsrto/cache
ENV R_BSRTO_BUILD_CACHE=/bsrto/build-cache

WORKDIR /bsrto

## Start install
RUN R -e 'install.packages(c("remotes", "rlang", "glue", "readr", "tibble", "vctrs", "stringr", "curl", "hms", "progress", "cli", "ncdf4", "dplyr", "abind", "sessioninfo", "gsw", "testthat", "withr", "tidyr"), repos = Sys.getenv("CRAN"))'
RUN R -e 'remotes::install_github("paleolimbot/readrdi@f85f8bf", repos = Sys.getenv("CRAN"))'
RUN R -e 'remotes::install_github("paleolimbot/headings@e7489d8", repos = Sys.getenv("CRAN"))'
RUN R -e 'remotes::install_github("paleolimbot/nmea@ff70d0a", repos = Sys.getenv("CRAN"))'
RUN R -e 'remotes::install_github("paleolimbot/bsrto@f44949e", repos = Sys.getenv("CRAN"))'
## End install

