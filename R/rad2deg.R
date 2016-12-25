#' Radians to degrees
#'
#' @param rad Angle in radians
#'
#' @return \code{numeric} Angle in degrees
#' @export
#'
#' @examples
#' rad2deg(2*pi) == 360

rad2deg = function(rad) {(rad * 180) / (pi)}
