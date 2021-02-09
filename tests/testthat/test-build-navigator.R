
test_that("bs_build_navigator() works for the example built data", {
  # update the example build data periodically:
  # bs_build_realtime()
  # file.copy("ctd.csv", "inst/ex-built")

  built_dir <- system.file("ex-built", package = "bsrto")
  out_dir <- tempfile()
  out_file <- expect_output(
    bs_build_navigator(built_dir, out_dir = out_dir),
    "Writing"
  )
  expect_true(file.exists(out_file))

  out_nc <- ncdf4::nc_open(out_file)
  expect_setequal(names(out_nc$dim), c("DEPTH", "TIME"))
  expect_setequal(out_nc$dim$DEPTH$vals, c(40, 60, 160))
  expect_true(all(round(diff(out_nc$dim$TIME$vals), 5) == round(2 / 24, 5)))

  # check that the CTDs were assigned the right depth
  ctd_vars <- c("PRES", "TEMP", "DOXY", "PSAL")
  qc_vars <- paste0(ctd_vars, "_QC")

  expect_setequal(names(out_nc$var), c(ctd_vars, qc_vars))

  df <- expand.grid(
    DEPTH = out_nc$dim$DEPTH$vals,
    TIME = out_nc$dim$TIME$vals
  )
  df[ctd_vars] <- lapply(
    ctd_vars,
    function(var) as.numeric(ncdf4::ncvar_get(out_nc, var))
  )
  df[qc_vars] <- lapply(
    qc_vars,
    function(var) as.numeric(ncdf4::ncvar_get(out_nc, var))
  )

  pres_depth_diff <- df$PRES - df$DEPTH
  expect_true(all(abs(pres_depth_diff < 20)))

  ncdf4::nc_close(out_nc)
  unlink(out_dir)
})


test_that("build_navigator_meta() returns a named list() of character()", {
  meta <- build_navigator_meta(Sys.time(), Sys.time())
  expect_true(all(vapply(meta, is.character, logical(1))))
  expect_true(all(vapply(meta, length, integer(1)) == 1))
  expect_true(!is.null(names(meta)))
  expect_true(all(names(meta) != ""))
})
