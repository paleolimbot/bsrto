
bs_build_2019 <- function(out_dir = tempfile()) {


}

build_2019_met <- function() {

}

build_2019_hpb <- function(out_dir = ".") {
  cli::cat_rule(glue("build_2019_hpb('{ out_dir }')"))
  dir <- "BSRTO/2019-2020/hpb"
  cached <- build_2019_list_and_cache(dir)

  build_2019_log_about_to_read(cached)
  all <- read_hpb_vector(cached)
  all$file <- build_2019_file_relative(all$file)

  out_file <- file.path(out_dir, "2019-hpb.csv")
  cli::cat_line(glue("Writing '{ out_file }'"))
  readr::write_csv(all, out_file)
}

build_2019_icl <- function() {

}

build_2019_imm <- function() {

}

build_2019_ips <- function() {

}

build_2019_lgh <- function() {

}

build_2019_mca <- function() {

}

build_2019_mch <- function() {

}

build_2019_pcm <- function() {

}

build_2019_rdi <- function() {

}

build_2019_log_about_to_read <- function(cached) {
  cli::cat_line(glue("Reading { length(cached) } file(s)"))
  if (length(cached) >= 2) {
    cli::cat_line(glue("'{ basename(cached[1]) }'...'{ basename(cached[length(cached)]) }'"))
  } else if (length(cached) >= 1) {
    cli::cat_line(glue("'{ basename(cached) }'"))
  }
}

build_2019_list_and_cache <- function(dir, retries = 4) {
  cli::cat_line(glue("Updating cache for '{ dir }'"))
  for (i in seq_len(retries)) {
    tryCatch({
      files <- bs_ftp_list(dir)
      break
    }, error = function(e) {
      cli::cat_line(paste0(e, collapse = " "), col = "red")
    })
  }

  if (!exists("files", inherits = FALSE)) {
    abort(glue("Failed to list '{ dir }' after { retries } retries."))
  }

  summary_size <- build_2019_friendly_file_size(sum(files$size))
  cli::cat_line(glue("Summary: { nrow(files) } file(s) ({ summary_size })"))

  needs_download <- !file.exists(bs_cache_dir(files$file))
  if (any(needs_download)) {
    download_size <- build_2019_friendly_file_size(sum(files$size[needs_download]))
    cli::cat_line(
      glue(
        "About to download { sum(needs_download) } file(s) ({ download_size })"
      )
    )
  }

  cached <- bs_cached(files, async = TRUE)

  zero_size <- file.size(cached) == 0
  if (sum(zero_size) > 0) {
    zero_size_files <- paste0("'", basename(cached[zero_size]), "'", collapse = "\n")
    cli::cat_line(glue("Skipping { sum(zero_size) } files(s) with zero size:\n{ zero_size_files }"))
  }

  cached[!zero_size]
}

build_2019_file_relative <- function(file) {
  stringr::str_extract(file, "BSRTO/2019-2020/.*$")
}

build_2019_friendly_file_size <- function(size) {
  if (size > 2^20) {
    sprintf("%0.1f MiB", size / 2^20)
  } else if (size > 2^10) {
    sprintf("%0.1f KiB", size / 2^10)
  } else if (size != 1) {
    sprintf("%d bytes", size)
  } else {
    sprintf("%d byte", size)
  }
}
