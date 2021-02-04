
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

