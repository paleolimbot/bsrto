
library(tidyverse)

deps <- union("remotes", setdiff(desc::desc_get_deps(".")$package, c("R", "oce")))
deps_str <- paste0('"', deps, '"', collapse = ", ")
deps_line <- glue::glue("RUN R -e 'install.packages(c({ deps_str }), repos = Sys.getenv(\"CRAN\"))'")

bsrto_latest_ref <- gh::gh("/repos/paleolimbot/bsrto/commits/master", per_page = 1)$sha
bsrto_latest_ref <- stringr::str_sub(bsrto_latest_ref, 1, 7)
bsrto_line <- glue::glue("RUN R -e 'remotes::install_github(\"paleolimbot/bsrto@{ bsrto_latest_ref }\", repos = Sys.getenv(\"CRAN\"))'")

# Update Dockerfile
readr::read_file("inst/docker/Dockerfile") %>%
  stringr::str_replace(
    stringr::regex("## Start install(.*?)## End install", dotall = TRUE),
    glue::glue("## Start install\n{ deps_line }\n{ bsrto_line }\n## End install")
  ) %>%
  readr::write_file("inst/docker/Dockerfile.tmp")

unlink("inst/docker/Dockerfile")
stopifnot(file.rename("inst/docker/Dockerfile.tmp", "inst/docker/Dockerfile"))
unlink("inst/docker/Dockerfile.tmp")
