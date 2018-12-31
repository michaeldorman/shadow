#' Polygonal layer of four buildings in Rishon
#'
#' A \code{SpatialPolygonsDataFrame} object representing the outlines of four buildings located in Rishon-Le-Zion. The attribute \code{BLDG_HT} contains building height, in meters.
#'
#' @format A \code{SpatialPolygonsDataFrame} with 4 features and 2 attributes:
#' \describe{
#'   \item{build_id}{Building ID}
#'   \item{BLDG_HT}{Building height, in meters}
#' }

"build"

#' Polygonal layer of three buildings in Boston
#'
#' A \code{SpatialPolygonsDataFrame} object representing the outlines of three buildings located in Central Boston. The attribute \code{height_m} contains building height, in meters.
#'
#' @format A \code{SpatialPolygonsDataFrame} with 10 features and 4 attributes:
#' \describe{
#'   \item{objectid}{Building part ID}
#'   \item{build_id}{Building ID}
#'   \item{part_floor}{Number of floors for part}
#'   \item{height_m}{Building height, in meters}
#' }

"boston_build"

#' Polygonal layer of 376 buildings in Beer-Sheva
#'
#' A \code{SpatialPolygonsDataFrame} object representing the outlines of 367 buildings in the Ramot neighborhood, Beer-Sheva. The attribute \code{height_m} contains building height, in meters.
#'
#' @format A \code{SpatialPolygonsDataFrame} with 10 features and 4 attributes:
#' \describe{
#'   \item{build_id}{Building ID}
#'   \item{floors}{Number of floors for building}
#'   \item{apartments}{Number of apartments}
#'   \item{height_m}{Building height, in meters}
#'   \item{elev}{Elevation above sea level of building base, in meters}
#' }

"beersheva_build"

#' Polygonal layer of a building block in Boston
#'
#' A \code{SpatialPolygons} object representing the boundaries of a building block in Central Boston.
#'
#' @format A \code{SpatialPolygons} with a single feature.

"boston_block"

#' Polygonal layer of a park in Boston
#'
#' A \code{SpatialPolygons} object representing the boundaries of a park in Central Boston.
#'
#' @format A \code{SpatialPolygons} with a single feature.

"boston_park"

#' Polygonal layer of sidewalks in Boston
#'
#' A \code{SpatialLinesDataFrame} object representing sidewalks in Central Boston.
#'
#' @format A \code{SpatialLinesDataFrame} with 78 features.

"boston_sidewalk"

#' Typical Meteorological Year (TMY) solar radiation in Tel-Aviv
#'
#' A table with hourly solar radiation estimates for a typical meteorological year in Tel-Aviv. \itemize{
#' \item{\code{time} Time, as \code{character} in the \code{"\%Y-\%m-\%d \%H:\%M:\%S"} format, e.g. \code{"2000-01-01 06:00:00"}}, referring to local time
#' \item{\code{sun_az} Sun azimuth, in decimal degrees from North}
#' \item{\code{sun_elev} Sun elevation, in decimal degrees}
#' \item{\code{solar_normal} Direct Normal Irradiance, in Wh/m^2}
#' \item{\code{solar_diffuse} Diffuse Horizontal Irradiance, in Wh/m^2}
#' \item{\code{dbt} Dry-bulb temperature, in Celsius degrees}
#' \item{\code{ws} Wind speed, in m/s}
#' }
#'
#' @format A \code{data.frame} with 8760 rows and 7 columns.
#'
#' @references
#' https://energyplus.net/weather-location/europe_wmo_region_6/ISR//ISR_Tel.Aviv-Bet.Dagan.401790_MSI

"tmy"

#' Typical Meteorological Year (TMY) solar radiation in Beer-Sheva
#'
#' A table with hourly solar radiation estimates for a typical meteorological year in Beer-Sheva. \itemize{
#' \item{\code{time} Time, as \code{character} in the \code{"\%Y-\%m-\%d \%H:\%M:\%S"} format, e.g. \code{"2000-01-01 06:00:00"}}, referring to local time
#' \item{\code{sun_az} Sun azimuth, in decimal degrees from North}
#' \item{\code{sun_elev} Sun elevation, in decimal degrees}
#' \item{\code{solar_normal} Direct Normal Irradiance, in Wh/m^2}
#' \item{\code{solar_diffuse} Diffuse Horizontal Irradiance, in Wh/m^2}
#' \item{\code{dbt} Dry-bulb temperature, in Celsius degrees}
#' \item{\code{ws} Wind speed, in m/s}
#' }
#'
#' @format A \code{data.frame} with 8760 rows and 7 columns.
#'
#' @references
#' https://energyplus.net/weather-location/europe_wmo_region_6/ISR//ISR_Beer.Sheva.401900_MSI

"tmy2"

#' DEM of Ramot neighborhood, Beer-Sheva
#'
#' Digital Elevation Model (DEM) of Ramot neighborhood, Beer-Sheva. Raster values represent elevation above sea level, in meters.
#'
#' @format A \code{RasterLayer} representing a grid of 1974 raster cells, each cell is a 30*30 meters rectangle. Data source is the Shuttle Radar Topography Mission (SRTM) 1 Arc-Second Global dataset.
#'
#' @references
#' https://lta.cr.usgs.gov/SRTM1Arc

"beersheva_elev"



