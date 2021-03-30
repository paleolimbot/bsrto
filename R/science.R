
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


#' Approximate declination for Barrow Strait
#'
#' @param date_time A POSIXct for which declination should be calculated
#'
#' @return A numeric vector of approximated declination
#' @export
#'
#' @examples
#' barrow_strait_declination(Sys.time())
#'
barrow_strait_declination <- function(date_time) {
  headings::hdg_decl(
    lon = -91.25105,
    lat = 74.60635,
    year = headings::mm_decimal_year(date_time),
    height = 0,
    height_ref = "geoid",
    model = "IGRF13"
  )
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

# note that positive rotation here is rotation clockwise
rotate_about_origin <- function(coords, rotation_deg) {
  rotation <- rotation_deg * pi / 180
  coords <- as.matrix(coords)
  n_coord <- nrow(coords)
  stopifnot(
    is.numeric(coords),
    ncol(coords) == 2
  )

  coords_3d <- cbind(coords, rep(1, n_coord))

  if (length(rotation) == 1) {
    affine <- rbind(
      c(cos(rotation), -sin(rotation), 0),
      c(sin(rotation), cos(rotation), 0),
      c(0, 0, 1)
    )

    coords_3d <- coords_3d %*% affine
  } else {
    for (i in seq_along(rotation_deg)) {
      affine <- rbind(
        c(cos(rotation[i]), -sin(rotation[i]), 0),
        c(sin(rotation[i]), cos(rotation[i]), 0),
        c(0, 0, 1)
      )

      coords_3d[i, ] <- coords_3d[i, , drop = FALSE] %*% affine
    }
  }

  coords_3d[, 1:2, drop = FALSE]
}


