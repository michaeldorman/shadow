#' \code{shadow}: R Package for Geometric Shade Calculations
#'
#' Functions for calculating:\itemize{
#' \item{Shade height at a single point}
#' \item{Shaded proportion of a building facade}
#' \item{A polygonal layer of shade footprints on the ground}
#' \item{Sky View Factor (SVF) value at a single point}
#' }
#' Typical inputs include a polygonal layer of buildings outline along with the height of each building, sun azimuth and sun elevation. The package also provides functions for related preliminary calculations: converting polygons to line segments, finding segment azimuth, shifting segments by azimuth and distance, and constructing the footprint of a line of sight between an observer and the sun.
#'
#' @docType package
#'
#' @name shadow
#'
#' @import sp
#' @import raster
#' @importFrom methods as
NULL
