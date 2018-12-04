#' Coefficient of Direct Normal Irradiance reduction
#'
#' This function calculates the coefficient of reduction in Direct Normal Irradiance load due to angle of incidence. For example, a coefficient of 1 is obtained when the sun is perpendicular to the surface.
#'
#' @param type \code{character}, specifying surface type. All values must be either \code{"roof"} or \code{"facade"}
#' @param facade_az Facade azimuth, in decimal degrees from North. Only relevant for \code{type="facade"}
#' @param solar_pos A matrix with two columns representing sun position(s); first column is the solar azimuth (in decimal degrees from North), second column is sun elevation (in decimal degrees); rows represent different positions (e.g. at different times of day)
#'
#' @return Numeric vector of coefficients, to be multiplied by the direct beam radiation values. The vector length is the same as the length of the longest input (see \strong{Note} below)
#'
#' @note All four arguments are recycled to match each other's length. For example, you may specify a single \code{type} value of \code{"roof"} or \code{"facade"} and a single \code{facade_az} value, but multiple \code{sun_az} and \code{sun_elev} values, for calculating the coefficients for a single location given different positions of the sun, etc.
#' @export
#'
#' @examples
#' # Basic usage
#' coefDirect(type = "facade", facade_az = 180, solar_pos = matrix(c(210, 30), ncol = 2))
#'
#' # Demonstration - Direct beam radiation coefficient on 'facades'
#' sun_az = seq(270, 90, by = -5)
#' sun_elev = seq(0, 90, by = 5)
#' solar_pos = expand.grid(sun_az = sun_az, sun_elev = sun_elev)
#' solar_pos$coef = coefDirect(type = "facade", facade_az = 180, solar_pos = as.matrix(solar_pos))[1, ]
#' coef = reshape2::acast(solar_pos, sun_az ~ sun_elev, value.var = "coef")
#' image(
#'   180 - sun_az, sun_elev, coef,
#'   col = rev(heat.colors(10)),
#'   breaks = seq(0, 1, 0.1),
#'   asp = 1,
#'   xlab = "Facade azimuth - Sun azimuth (deg)",
#'   ylab = "Sun elevation (deg)",
#'   main = "Facade - Coefficient of Direct Normal Irradiance"
#' )
#' contour(180 - sun_az, sun_elev, coef, add = TRUE)
#'
#' # Demonstration - Direct beam radiation coefficient on 'roofs'
#' solar_pos$coef = coefDirect(type = "roof", facade_az = 180, solar_pos = as.matrix(solar_pos))[1, ]
#' coef = reshape2::acast(solar_pos, sun_az ~ sun_elev, value.var = "coef")
#' image(
#'   180 - sun_az, sun_elev, coef,
#'   col = rev(heat.colors(10)),
#'   breaks = seq(0, 1, 0.1),
#'   asp = 1,
#'   xlab = "Facade azimuth - Sun azimuth (deg)",
#'   ylab = "Sun elevation (deg)",
#'   main = "Roof - Coefficient of Direct Normal Irradiance"
#' )
#' contour(180 - sun_az, sun_elev, coef, add = TRUE)

coefDirect = function(type, facade_az, solar_pos) {

  # Match 'type' and 'facade_az' length
  stopifnot(length(type) == 1 || length(type) == length(facade_az))
  if(length(type) == 1) type = rep(type, length(facade_az))

  # Discard 'solar_pos'
  colnames(solar_pos) = NULL

  # Result
  result = matrix(nrow = length(facade_az), ncol = nrow(solar_pos))

  # Loop over times
  for(i in 1:ncol(result)) {

    # Recycle values
    dat = data.frame(
      type,
      facade_az,
      sun_az = solar_pos[i, 1],
      sun_elev = solar_pos[i, 2],
      coef = NA,
      stringsAsFactors = FALSE,
      row.names = NULL
      )

    # Azimuth difference
    dat$az_diff =
      ifelse(
        abs(dat$sun_az - dat$facade_az) > 180,
        360 - abs(dat$sun_az - dat$facade_az),
        abs(dat$sun_az - dat$facade_az)
      )

    # For 'roof' azimuth difference is 0
    dat$az_diff = ifelse(
      dat$type == "roof",
      0,
      dat$az_diff
    )

    # Elevation difference
    dat$elev_diff = ifelse(
      dat$type == "roof",
      90 - dat$sun_elev,
      dat$sun_elev
    )

    # To radians
    dat$az_diff_r = deg2rad(dat$az_diff)
    dat$elev_diff_r = deg2rad(dat$elev_diff)

    # Coefficient
    dat$coef = cos(dat$az_diff_r) * cos(dat$elev_diff_r)

    # Special case - Sun below horizon
    dat$coef[dat$sun_elev < 0] = 0

    # Special case - Sun "behind" facade or in zenith
    dat$coef[dat$type == "facade" & (dat$az_diff >= 90 | dat$sun_elev == 90)] = 0

    # fill 'result' column
    result[, i] = dat$coef

  }

  # Return coefficients
  return(result)

}


