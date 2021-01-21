
library(future)
library(furrr)
library(dplyr)
plan(multisession, workers = parallel::detectCores() - 1)

local_cache <- file.path(getwd(), "bsrto_cache")

# consider deleting 'bsrto_cache' or certain files before a true refresh
# of the local cache

# takes a bit...use multiple workers for this
if (file.exists("data-raw/files-cached.csv")) {
  files <- readr::read_csv("data-raw/files-cached.csv", col_types = readr::cols())
} else {
  data_dirs <- c(
    paste0("barrow/", setdiff(1998:2019, 2013)),
    "barrow/data reports",
    "barrow/cruise reports",
    paste0("BSRTO/", c("2018-2019", "2019-2020"))
  )

  # takes ~ 8 minutes
  data_files_df <- future_map(data_dirs, ~bsrto::bs_ftp_list(.x, recursive = TRUE))
  barrow_files_df <- bsrto::bs_ftp_list("barrow")
  files <- rbind(do.call(rbind, data_files_df), barrow_files_df)
  readr::write_csv(files, "data-raw/files-cached.csv")
}

# assign url and local name
files$url <- paste0("ftp://dfoftp.ocean.dal.ca/pub/dfo/", files$file)
files$cached = file.path(local_cache, files$file)

# make sure destination folders exist
all_dirs <- sort(unique(dirname(files$cached)))
result <- lapply(all_dirs[!dir.exists(all_dirs)], dir.create, recursive = TRUE)

# skip files already downloaded
files_dl <- files[!file.exists(files$cached), ]
# URLescape filenames with percentage signs in them
files_dl$url <- gsub("\\%", "%25", files_dl$url)

download_file <- function(url, dest) {
  !inherits(try(curl::curl_download(url, dest)), "try-error")
}

files_dl$downloaded <- future_map2_lgl(files_dl$url, files_dl$cached, download_file, .progress = TRUE)

# only one undownloadable file I know about
stopifnot(
  identical(
    files$file[!file.exists(files$cached)],
    "BSRTO/2019-2020/ips/200224AA.bn4"
  )
)

files <- files %>%
  filter(file.exists(cached))

# make sure file size on disk aligns with file size on ftp
stopifnot(
  all(file.size(files$cached) == files$size)
)

# calculate checksum for all files
bs_ftp_snapshot_latest <- files %>%
  mutate(
    size = file.size(cached),
    sha1 = future_map_chr(cached, digest::digest, "sha1", file = TRUE, .progress = TRUE)
  ) %>%
  select(file, size, sha1)

# only keep the last 8 digits to keep the file size down
bs_ftp_snapshot_latest$sha1 <- substr(bs_ftp_snapshot_latest$sha1, 33, 40)

# add the time when the snapshot was calculated
attr(bs_ftp_snapshot_latest, "snapshot_time") <- lubridate::with_tz(Sys.time(), "UTC")

# add the snapshot to the package data
usethis::use_data(bs_ftp_snapshot_latest, overwrite = TRUE)

# don't keep the csv cache
unlink("data-raw/files-cached.csv")
