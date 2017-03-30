#' Polygonal layer of four buildings in Rishon
#'
#' A \code{SpatialPolygonsDataFrame} object representing the outlines of four buildings located in Rishon-Le-Zion. The attribute \code{BLDG_HT} contains building height, in meters.
#'
#' @format A \code{SpatialPolygonsDataFrame} with 4 features and 2 attributes:
#' \describe{
#'   \item{build_id}{Building ID}
#'   \item{BLDG_HT}{Building height, in meters}
#' }

"rishon"

#' Polygonal layer of three buildings in Boston
#'
#' A \code{SpatialPolygonsDataFrame} object representing the outlines of three buildings located in Central Boston. The attribute \code{height_m} contains building height, in meters.
#'
#' @format A \code{SpatialPolygonsDataFrame} with 10 features and 4 attributes:
#' \describe{
#'   \item{objectid}{Building Part ID}
#'   \item{build_id}{Building ID}
#'   \item{part_floor}{Number of floors for Part}
#'   \item{height_m}{Building height, in meters}
#' }

"build"

#' Polygonal layer of a building block in Boston
#'
#' A \code{SpatialPolygons} object representing the boundaries of a building block in Central Boston.
#'
#' @format A \code{SpatialPolygons} with a single feature.

"block"

#' Polygonal layer of a park in Boston
#'
#' A \code{SpatialPolygons} object representing the boundaries of a park in Central Boston.
#'
#' @format A \code{SpatialPolygons} with a single feature.

"park"

#' Polygonal layer of sidewalks in Boston
#'
#' A \code{SpatialLinesDataFrame} object representing sidewalks in Central Boston.
#'
#' @format A \code{SpatialLinesDataFrame} with 78 features.

"sidewalk"



