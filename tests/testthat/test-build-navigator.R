
test_that("bs_build_navigator() works for the example built data", {
  # update the example build data periodically:
  # bs_build_realtime()
  # unlink("inst/ex-built/ctd.csv")
  # file.copy("ctd.csv", "inst/ex-built")

  built_dir <- system.file("ex-built", package = "bsrto")
  out_dir <- tempfile()
  out_file <- expect_output(
    bs_build_navigator(built_dir, out_dir = out_dir),
    "Writing"
  )

  df <- bs_check_navigator(out_file)

  if (interactive()) {
    oce_ctd <- df %>%
      dplyr::group_by(DEPTH) %>%
      dplyr::summarise(
        ctd = list(oce::as.ctd(PSAL, TEMP, PRES, time = date_time))
      )

    oce::plotTS(oce_ctd$ctd)

    withr::with_par(list(mfrow = c(3, 1)), {
      for (i in 1:3)
        for (j in 1:3)
          oce::plotScan(oce_ctd$ctd[[i]], which = j)
    })
  }

  unlink(out_dir)
})


test_that("build_navigator_meta() returns a named list() of character()", {
  meta <- build_navigator_meta(Sys.time(), Sys.time())
  expect_true(all(vapply(meta, is.character, logical(1))))
  expect_true(all(vapply(meta, length, integer(1)) == 1))
  expect_true(!is.null(names(meta)))
  expect_true(all(names(meta) != ""))
})
