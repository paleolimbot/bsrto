
library(tidyverse)

deps <- union("remotes", setdiff(desc::desc_get_deps(".")$package, c("R", "oce", "headings", "nmea", "readrdi")))
deps_str <- paste0('"', deps, '"', collapse = ", ")
deps_line <- glue::glue("RUN R -e 'install.packages(c({ deps_str }), repos = Sys.getenv(\"CRAN\"))'")

readrdi_latest_ref <- gh::gh("/repos/paleolimbot/readrdi/commits/master", per_page = 1)$sha %>%
  stringr::str_sub(1, 7)
headings_latest_ref <- gh::gh("/repos/paleolimbot/headings/commits/master", per_page = 1)$sha %>%
  stringr::str_sub(1, 7)
nmea_latest_ref <- gh::gh("/repos/paleolimbot/nmea/commits/master", per_page = 1)$sha %>%
  stringr::str_sub(1, 7)
bsrto_latest_ref <- gh::gh("/repos/paleolimbot/bsrto/commits/master", per_page = 1)$sha %>%
  stringr::str_sub(1, 7)

readrdi_line <- glue::glue("RUN R -e 'remotes::install_github(\"paleolimbot/readrdi@{ readrdi_latest_ref }\", repos = Sys.getenv(\"CRAN\"))'")
headings_line <- glue::glue("RUN R -e 'remotes::install_github(\"paleolimbot/headings@{ headings_latest_ref }\", repos = Sys.getenv(\"CRAN\"))'")
nmea_line <- glue::glue("RUN R -e 'remotes::install_github(\"paleolimbot/nmea@{ nmea_latest_ref }\", repos = Sys.getenv(\"CRAN\"))'")
bsrto_line <- glue::glue("RUN R -e 'remotes::install_github(\"paleolimbot/bsrto@{ bsrto_latest_ref }\", repos = Sys.getenv(\"CRAN\"))'")

# Update Dockerfile
readr::read_file("inst/docker/Dockerfile") %>%
  stringr::str_replace(
    stringr::regex("## Start install(.*?)## End install", dotall = TRUE),
    glue::glue("## Start install\n{ deps_line }\n{ readrdi_line }\n{ headings_line }\n{ nmea_line }\n{ bsrto_line }\n## End install")
  ) %>%
  readr::write_file("inst/docker/Dockerfile.tmp")

unlink("inst/docker/Dockerfile")
stopifnot(file.rename("inst/docker/Dockerfile.tmp", "inst/docker/Dockerfile"))
unlink("inst/docker/Dockerfile.tmp")
