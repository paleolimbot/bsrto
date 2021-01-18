
library(future)
library(furrr)
plan(multisession, workers = parallel::detectCores() - 1)

local_cache <- file.path(getwd(), "bsrto_cache")

# consider deleting 'bsrto_cache' or certain files before a true refresh
# of the local cache

# takes a long time...use multiple workers for this
data_dirs <- c(
  paste0("barrow/", setdiff(1998:2019, 2013)),
  "barrow/data reports",
  "barrow/cruise reports",
  paste0("BSRTO/", c("2018-2019", "2019-2020"))
)

# takes ~ 8 minutes
data_files <- future_map(data_dirs, ~bsrto::bs_ftp_list(.x, recursive = TRUE))
barrow_files <- bsrto::bs_ftp_list("barrow")

files <- tibble::tibble(
  file = sort(c(unlist(data_files), barrow_files)),
  url = paste0("ftp://dfoftp.ocean.dal.ca/pub/dfo/", file),
  cached = file.path(local_cache, file)
)

# make sure destinations exist
all_dirs <- sort(unique(dirname(files$cached)))
result <- lapply(all_dirs[!dir.exists(all_dirs)], dir.create, recursive = TRUE)

# skip files already downloaded
files_dl <- files[!file.exists(files$cached), ]

progressr::handlers("progress")
progressr::with_progress({
  prog <- progressr::progressor(along = files_dl$cached)
  download_file <- function(url, dest) {
    prog(message = url)
    !inherits(try(curl::curl_download(url, dest)), "try-error")
  }

  files_dl$downloaded <- future_map2_lgl(files_dl$url, files_dl$cached, download_file)
})


