#' \code{shadow}: R Package for Geometric Shade Calculations
#'
#' Main functions for calculating:\itemize{
#' \item \code{shadowHeight}, Shadow height at individual points or continuous surface
#' \item \code{shadowFootprint}, Polygonal layer of shadow footprints on the ground
#' \item \code{SVF}, Sky View Factor (SVF) value at individual points or continuous surface
#' }
#' Typical inputs for these functions include:\itemize{
#' \item \code{location}, Queried location(s)
#' \item \code{obstacles}, A polygonal layer of obstacles (e.g. buildings) outline, with height attributes \code{obstacles_height_field}
#' \item \code{solar_pos}, Solar position (i.e. sun azimuth and elevation angles)
#' }
#' The package also provides functions for related preliminary calculations, such as:\itemize{
#' \item \code{toSeg}, Converting polygons to line segments
#' \item \code{classifyAz}, Finding segment azimuth
#' \item \code{shiftAz}, Shifting segments by azimuth and distance
#' \item \code{ray}, Constructing a line between two points
#' }
#'
#' @docType package
#'
#' @name shadow
#'
#' @import sp
#' @import raster
#' @importFrom methods as

NULL
