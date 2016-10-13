#' Shift features by azimuth and distance.
#'
#' @param object The object to be shifted
#' @param az Shift azimuth, in decimal degrees
#' @param dist Shift distance, in \code{object} projection units
#'
#' @return The shifted \code{object}.
#'
#' @examples
#' data(build)
#' build_shifted = shiftAz(build, az = 45, dist = 2.5)
#' plot(build)
#' plot(build_shifted, add = TRUE, border = "red")
#'
#' @export

shiftAz = function(object, az, dist) {

  az_rad = deg2rad(90 - az)
  raster::shift(object, x = dist * cos(az_rad) , y = dist * sin(az_rad))

}
