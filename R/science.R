
#' Barometric/Sea-level pressure conversion
#'
#' Formula from <https://en.wikipedia.org/wiki/Atmospheric_pressure>.
#'
#' @param p Measured barometric pressure (can be any pressure unit)
#' @param p0 Pressure at sea level in (can be any pressure unit)
#' @param h Elevation of barometric pressure measurement in meters
#' @param g Accelleration due to gravity in m/s2
#' @param M Molar mass of dry air (kg / mol)
#' @param R0 Universal gas constant (mol-Kelvins)
#' @param T0 Sea level standard temperature (Kelvins)
#'
#' @return The corrected pressure in the same unit as `p` or `p0`.
#' @export
#'
#' @examples
#' sea_level_pressure_from_barometric(102113, 68)
#' barometric_pressure_from_sea_level(101300, 68)
#'
sea_level_pressure_from_barometric <- function(p, h, g = 9.80665,
                                               M = 0.0289644, R0 = 8.31447,
                                               T0 = 288.15) {
  p * exp(-g * M * h / (R0 * T0))
}

#' @rdname sea_level_pressure_from_barometric
#' @export
barometric_pressure_from_sea_level <- function(p0, h, g = 9.80665,
                                               M = 0.0289644, R0 = 8.31447,
                                               T0 = 288.15) {
  p0 + p0 * (1 - exp(-g * M * h / (R0 * T0)))
}


#' Approximate declinations for Barrow Strait
#'
#' @param date_time A POSIXct for which declenation should be calculated
#'
#' @return A numeric vector of approximated declinations
#' @export
#'
#' @examples
#' # location of observatory
#' lon <- -91.25105
#' lat <- 74.60635
#'
#' # from start of deployment to the the end of the model's validity
#' date_time <- seq(
#'   as.POSIXct("2019-01-01 00:00:00", tz = "UTC"),
#'   as.POSIXct("2025-12-31 11:59:00", tz = "UTC"),
#'   by = "day"
#' )
#' dec_model <- oce::magneticField(
#'   rep(lon, length(date_time)),
#'   rep(lat, length(date_time)),
#'   date_time
#' )
#' t <- as.numeric(date_time, origin = "1970-01-01 00:00:00")
#' lm_approx <- lm(dec_model$declination ~ poly(t, 2, raw = TRUE))
#'
#' # take a look at the max error we can expect
#' max(residuals(lm_approx))
#'
#' plot(t, dec_model$declination, type = "l")
#' lines(t, predict(lm_approx), col = "red")
#'
#' # used to generate the values in this function
#' as.character(coef(lm_approx))
#'
barrow_strait_declination <- function(date_time) {
  t <- as.numeric(date_time, origin = "1970-01-01 00:00:00")
  -119.131004244733 + 8.48874014258752e-08 * t -1.61908017786388e-17 * t ^ 2
}

heading_normalize <- function(heading) {
  (heading + 360) %% 360
}

uv_from_heading <- function(true_heading) {
  tibble::tibble(
    u = cos((90 - true_heading) * pi / 180),
    v = sin((90 - true_heading) * pi / 180)
  )
}

heading_from_uv <- function(uv) {
  result <- ifelse(
    uv$v >= 0,
    atan(uv$u / uv$v) * 180 / pi,
    180 + atan(uv$u / uv$v) * 180 / pi
  )

  (result + 360) %% 360
}

heading_mean <- function(true_heading, na.rm = FALSE) {
  if (na.rm) {
    true_heading <- true_heading[!is.na(true_heading)]
  }

  uv <- uv_from_heading(true_heading)
  heading_from_uv(lapply(uv, sum))
}

heading_diff <- function(h1, h2) {
  common <- vctrs::vec_recycle_common(h1 = h1, h2 = h2)
  uv1 <- uv_from_heading(common$h1)
  uv2 <- uv_from_heading(common$h2)

  # get the chord lengths
  uv_chord <- Map("-", uv1, uv2)
  chord_len <- sqrt(uv_chord$u ^ 2 + uv_chord$v ^ 2)
  angle_chord <- 2 * asin(chord_len / 2) * 180 / pi

  # apply sign of cross-product for direction
  # in this case the sign aligns with negative values counterclockwise
  # on the compass and positive values clockwise on the compass
  direction <- sign(uv1$u * uv2$v - uv1$v * uv2$u)
  direction[direction == 0] <- 1

  angle_chord * direction
}

heading_sd <- function(true_heading, na.rm = FALSE) {
  if (na.rm) {
    true_heading <- true_heading[!is.na(true_heading)]
  }

  uv <- uv_from_heading(true_heading)

  # need the mean as a unit vector
  uv_mean <- lapply(uv, sum)
  uv_mean_len <- sqrt(uv_mean$u ^ 2 + uv_mean$v ^ 2)
  uv_mean$u <- uv_mean$u / uv_mean_len
  uv_mean$v <- uv_mean$v / uv_mean_len

  # ...to get the chord lengths
  uv_chord <- Map("-", uv, uv_mean)
  chord_len <- sqrt(uv_chord$u ^ 2 + uv_chord$v ^ 2)
  angle_chord <- 2 * asin(chord_len / 2) * 180 / pi

  sqrt(sum(angle_chord ^ 2) / (length(true_heading) - 1))
}

# Units: S/m, Deg C, dbar -> psu
salinity_from_cond_temp_pres <- function(cond, temp, pres) {
  # gsw expects mS/cm, ITS-90 deg C, dbar
  gsw::gsw_SP_from_C(cond * 10, temp, pres)
}

# Units: psu, Deg C, dbar -> m/s
sound_speed_from_psal_temp_pres <- function(psal, temp, pres,
                                            longitude = -91.251, latitude = 74.605) {
  absolute_sal <- gsw::gsw_SA_from_SP(psal, pres, longitude = longitude, latitude = latitude)
  conservative_temp <- gsw::gsw_CT_from_t(absolute_sal, temp, pres)
  gsw::gsw_sound_speed(absolute_sal, conservative_temp, pres)
}
