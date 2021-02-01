
#' Read icl files
#'
#' @inheritParams read_odf
#' @param tz Timezone for which date times are representative.
#' @param file_vector A vector of files or URLs
#' @param pb A [progress bar][progress::progress_bar] or `NULL`.
#'
#' @return A [tibble::tibble()]
#' @export
#'
#' @examples
#' icl_file <- bs_example("icl/SAF2564_20191010_19.txt")
#' read_icl(icl_file)
#' read_icl_vector(icl_file)
#' read_icl_header(icl_file)
#' read_icl_header_lines(icl_file)
#'
read_icl <- function(file, pb = NULL, tz = "UTC") {
  bs_tick(pb, file)

  header_lines <- try(read_icl_header_lines(file))
  if (inherits(header_lines, "try-error")) {
    return(tibble::tibble())
  }

  skip <- length(header_lines) + 1

  # need the date from the header
  file_date <- stringr::str_extract(header_lines[4], "[0-9-]{10}\\s*$")
  file_date_parsed <- readr::parse_date(file_date, "%Y-%m-%d")

  # these files are frequently malformed, which causes readr to prematurely
  # abandon reading some files
  col_names <- readr::read_lines(file, skip = skip, n_max = 1)
  col_names <- stringr::str_split_fixed(col_names, stringr::fixed("\t"), n = 416)
  lines <- readr::read_lines(file, skip = skip + 1)
  lines_split <- stringr::str_split_fixed(lines, stringr::fixed("\t"), n = 416)
  colnames(lines_split) <- as.character(col_names)

  # need column names to match exactly or it is difficult to process
  # vectors of these files
  if (!identical(as.character(col_names), icl_col_names)) {
    col_names_label <- paste0("'", col_names, "'", collapse = ",")
    message(
      glue(
        "Unexpected column names in file '{ file }':\n{ col_names_label }"
      )
    )
    return(tibble::tibble())
  }

  result <- tibble::as_tibble(lines_split)

  # much easier later on to make this a date + time
  file_datetime <- as.POSIXct(file_date_parsed)
  attr(file_datetime, "tzone") <- tz
  result$Time <- file_datetime + readr::parse_time(result$Time)

  numeric_cols <- setdiff(names(result), c("Time", "Comment"))
  result[numeric_cols] <- lapply(result[numeric_cols], readr::parse_double)
  result
}

#' @rdname read_icl
#' @export
read_icl_vector <- function(file_vector) {
  pb <- bs_progress(file_vector)
  on.exit(bs_progress_finish(pb))

  results <- lapply(file_vector, read_icl, pb = pb)
  lengths <- vapply(results, nrow, integer(1))
  results_all <- vctrs::vec_rbind(!!! results)
  vctrs::vec_cbind(
    file = vctrs::vec_rep_each(file_vector, lengths),
    results_all
  )
}

#' @rdname read_icl
#' @export
read_icl_header <- function(file, header_lines = read_icl_header_lines(file)) {
  section_start <- grep(":$", header_lines)
  section_end <- grep("^\\s*$", header_lines)
  if (length(section_start) != length(section_end)) {
    abort(glue("Differing lengths for section start and end in read_icl_header('{ file }')"))
  }

  section_title <- gsub(":$", "", header_lines[section_start])
  section_content <- Map("[", list(header_lines), Map(":", section_start + 1, section_end))
  section_content_parsed <- lapply(section_content, function(x) {
    x <- x[-length(x)]
    split <- stringr::str_split_fixed(x, "\t", 2)
    tibble::tibble(key = split[, 1], value = split[, 2])
  })

  names(section_content_parsed) <- section_title
  section_content_parsed
}

#' @rdname read_icl
#' @export
read_icl_header_lines <- function(file, n_header = 29) {
  header_lines(
    file,
    function(x) grepl("^Data:\\s*$", x),
    n_header = n_header
  )
}

# the way this is set up makes it difficult to use any column names that don't
# match exactly
icl_col_names <- c(
  "Time", "Comment", "Temperature [C]", "Humidity [%]", "Sequence #",
  "Data Points", "0", "15.63", "31.25", "46.88", "62.5", "78.13",
  "93.75", "109.38", "125", "140.63", "156.25", "171.88", "187.5",
  "203.13", "218.75", "234.38", "250", "265.63", "281.25", "296.88",
  "312.5", "328.13", "343.75", "359.38", "375", "390.63", "406.25",
  "421.88", "437.5", "453.13", "468.75", "484.38", "500", "515.63",
  "531.25", "546.88", "562.5", "578.13", "593.75", "609.38", "625",
  "640.63", "656.25", "671.88", "687.5", "703.13", "718.75", "734.38",
  "750", "765.63", "781.25", "796.88", "812.5", "828.13", "843.75",
  "859.38", "875", "890.63", "906.25", "921.88", "937.5", "953.13",
  "968.75", "984.38", "1000", "1015.63", "1031.25", "1046.88",
  "1062.5", "1078.13", "1093.75", "1109.38", "1125", "1140.63",
  "1156.25", "1171.88", "1187.5", "1203.13", "1218.75", "1234.38",
  "1250", "1265.63", "1281.25", "1296.88", "1312.5", "1328.13",
  "1343.75", "1359.38", "1375", "1390.63", "1406.25", "1421.88",
  "1437.5", "1453.13", "1468.75", "1484.38", "1500", "1515.63",
  "1531.25", "1546.88", "1562.5", "1578.13", "1593.75", "1609.38",
  "1625", "1640.63", "1656.25", "1671.88", "1687.5", "1703.13",
  "1718.75", "1734.38", "1750", "1765.63", "1781.25", "1796.88",
  "1812.5", "1828.13", "1843.75", "1859.38", "1875", "1890.63",
  "1906.25", "1921.88", "1937.5", "1953.13", "1968.75", "1984.38",
  "2000", "2015.63", "2031.25", "2046.88", "2062.5", "2078.13",
  "2093.75", "2109.38", "2125", "2140.63", "2156.25", "2171.88",
  "2187.5", "2203.13", "2218.75", "2234.38", "2250", "2265.63",
  "2281.25", "2296.88", "2312.5", "2328.13", "2343.75", "2359.38",
  "2375", "2390.63", "2406.25", "2421.88", "2437.5", "2453.13",
  "2468.75", "2484.38", "2500", "2515.63", "2531.25", "2546.88",
  "2562.5", "2578.13", "2593.75", "2609.38", "2625", "2640.63",
  "2656.25", "2671.88", "2687.5", "2703.13", "2718.75", "2734.38",
  "2750", "2765.63", "2781.25", "2796.88", "2812.5", "2828.13",
  "2843.75", "2859.38", "2875", "2890.63", "2906.25", "2921.88",
  "2937.5", "2953.13", "2968.75", "2984.38", "3000", "3015.63",
  "3031.25", "3046.88", "3062.5", "3078.13", "3093.75", "3109.38",
  "3125", "3140.63", "3156.25", "3171.88", "3187.5", "3203.13",
  "3218.75", "3234.38", "3250", "3265.63", "3281.25", "3296.88",
  "3312.5", "3328.13", "3343.75", "3359.38", "3375", "3390.63",
  "3406.25", "3421.88", "3437.5", "3453.13", "3468.75", "3484.38",
  "3500", "3515.63", "3531.25", "3546.88", "3562.5", "3578.13",
  "3593.75", "3609.38", "3625", "3640.63", "3656.25", "3671.88",
  "3687.5", "3703.13", "3718.75", "3734.38", "3750", "3765.63",
  "3781.25", "3796.88", "3812.5", "3828.13", "3843.75", "3859.38",
  "3875", "3890.63", "3906.25", "3921.88", "3937.5", "3953.13",
  "3968.75", "3984.38", "4000", "4015.63", "4031.25", "4046.88",
  "4062.5", "4078.13", "4093.75", "4109.38", "4125", "4140.63",
  "4156.25", "4171.88", "4187.5", "4203.13", "4218.75", "4234.38",
  "4250", "4265.63", "4281.25", "4296.88", "4312.5", "4328.13",
  "4343.75", "4359.38", "4375", "4390.63", "4406.25", "4421.88",
  "4437.5", "4453.13", "4468.75", "4484.38", "4500", "4515.63",
  "4531.25", "4546.88", "4562.5", "4578.13", "4593.75", "4609.38",
  "4625", "4640.63", "4656.25", "4671.88", "4687.5", "4703.13",
  "4718.75", "4734.38", "4750", "4765.63", "4781.25", "4796.88",
  "4812.5", "4828.13", "4843.75", "4859.38", "4875", "4890.63",
  "4906.25", "4921.88", "4937.5", "4953.13", "4968.75", "4984.38",
  "5000", "5015.63", "5031.25", "5046.88", "5062.5", "5078.13",
  "5093.75", "5109.38", "5125", "5140.63", "5156.25", "5171.88",
  "5187.5", "5203.13", "5218.75", "5234.38", "5250", "5265.63",
  "5281.25", "5296.88", "5312.5", "5328.13", "5343.75", "5359.38",
  "5375", "5390.63", "5406.25", "5421.88", "5437.5", "5453.13",
  "5468.75", "5484.38", "5500", "5515.63", "5531.25", "5546.88",
  "5562.5", "5578.13", "5593.75", "5609.38", "5625", "5640.63",
  "5656.25", "5671.88", "5687.5", "5703.13", "5718.75", "5734.38",
  "5750", "5765.63", "5781.25", "5796.88", "5812.5", "5828.13",
  "5843.75", "5859.38", "5875", "5890.63", "5906.25", "5921.88",
  "5937.5", "5953.13", "5968.75", "5984.38", "6000", "6015.63",
  "6031.25", "6046.88", "6062.5", "6078.13", "6093.75", "6109.38",
  "6125", "6140.63", "6156.25", "6171.88", "6187.5", "6203.13",
  "6218.75", "6234.38", "6250", "6265.63", "6281.25", "6296.88",
  "6312.5", "6328.13", "6343.75", "6359.38", "6375", "6390.63"
)
