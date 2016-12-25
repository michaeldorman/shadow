#' Degrees to radians
#'
#' @param deg Angle in degrees
#'
#' @return \code{numeric} Angle in radians
#' @export
#'
#' @examples
#' deg2rad(360) == 2*pi

deg2rad = function(deg) {(deg * pi) / (180)}
