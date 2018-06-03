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

#' Typical Meteorological Year (TMY) solar radiation in Tel-Aviv
#'
#' A table with hourly solar radiation estimates for a typical meteorological year in Tel-Aviv. \itemize{
#' \item{\code{time} Time, as \code{character} in the \code{"\%Y-\%m-\%d \%H:\%M:\%S"} format, e.g. \code{"2000-01-01 06:00:00"}}, referring to local time
#' \item{\code{sun_az} Sun azimuth, in decimal degrees from North}
#' \item{\code{sun_elev} Sun elevation, in decimal degrees}
#' \item{\code{solar_normal} Direct Normal Irradiance, in Wh/m^2}
#' \item{\code{solar_diffuse} Diffuse Horizontal Irradiance, in Wh/m^2}
#' }
#'
#' @format A \code{data.frame} with 8760 rows and 5 columns.
#'
#' @references
#' https://energyplus.net/weather-location/europe_wmo_region_6/ISR//ISR_Tel.Aviv-Bet.Dagan.401790_MSI

"tmy"



