#' Calculate solar position(s) for location and time
#'
#' This is a wrapper function around \code{suntools::solarpos}, adapted for accepting location as a \code{Spatial*} layer or a \code{Raster}. The function calculates layer centroid, transforms it to lon-lat, then calls \code{suntools::solarpos} to calculate solar position(s) for that point at the given time(s)
#'
#' @param	location	A \code{Spatial*} or a \code{Raster} object
#' @param	time	A \code{SpatialLines*} or a \code{SpatialPolygons*} object
#' @return	A \code{matrix} with two columns representing sun position(s); first column is the solar azimuth (in decimal degrees from North), second column is sun elevation (in decimal degrees); rows represent different times corresponding to \code{time}
#'
#' @examples
#' time = as.POSIXct("2004-12-24 13:30:00", tz = "Asia/Jerusalem")
#' proj4string(build) = CRS("EPSG:32636")
#' solarpos2(build, time)
#'
#' @importFrom suntools solarpos
#' @export

solarpos2 = function(location, time) {

  # Checks
  stopifnot(methods::is(location, "Spatial") | methods::is(location, "Raster"))
  .checkTime(time)

  if(methods::is(location, "Raster")) location = raster::rasterToPoints(location, spatial = TRUE)

  # Find centroid
#  location_ctr = rgeos::gCentroid(location)
  location_ctr = sf::st_as_sf(sf::st_centroid(sf::st_union(sf::st_cast(sf::st_geometry(sf::st_as_sf(location), "POINT")))))

  # Transform to lon-lat
  location_ctr = sf::st_transform(location_ctr, "OGC:CRS84")

  # Calculate solar position
  suntools::solarpos(location_ctr, time)

}
