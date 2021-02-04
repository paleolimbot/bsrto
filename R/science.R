
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

heading_mean <- function(true_heading) {
  uv <- uv_from_heading(true_heading)
  heading_from_uv(lapply(uv, sum))
}

heading_sd <- function(true_heading) {
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

