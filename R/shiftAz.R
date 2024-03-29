#' Shift features by azimuth and distance
#'
#' @param object The object to be shifted.
#' @param az Shift azimuth, in decimal degrees.
#' @param dist Shift distance, in \code{object} projection units.
#'
#' @return The shifted \code{object}.
#'
#' @examples
#' s = c(270, 90, 180, 0)
#' build_shifted = shiftAz(build, az = s, dist = 2.5)
#' plot(build)
#' plot(build_shifted, add = TRUE, border = "red")
#' # raster::text(rgeos::gCentroid(build, byid = TRUE), s)
#' raster::text(as(sf::st_centroid(sf::st_geometry(sf::st_as_sf(build))), "Spatial"), s)
#'
#' @export

shiftAz = function(object, az, dist) {

  # Check lengths of 'az' and 'dist'
  object_features = length(geometry(object))
  if(
    !(length(az) == 1 | length(az) == object_features)
    ) {
    stop("'az' needs to be of length 1 or same length as 'object'")
  }
  if(
    !(length(dist) == 1 | length(dist) == object_features)
    ) {
    stop("'az' needs to be of length 1 or same length as 'object'")
  }

  # Recycle if necessary
  if(length(az == 1)) az = rep(az, length(object))
  if(length(dist == 1)) dist = rep(dist, length(object))

  az_rad = deg2rad(90 - az)

  object_shifted = list()

  for(i in 1:length(object)) {

    object_shifted[[i]] = raster::shift(object[i, ], dist[i] * cos(az_rad[i]), dist[i] * sin(az_rad[i]))

    # Workaround for 'raster::shift' behavior of setting 'coord' dimnames
    if(class(object) %in% c("SpatialLines", "SpatialLinesDataFrame")) {
      for(j in 1:length(object_shifted[[i]]@lines[[1]]@Lines)) {
        dimnames(object_shifted[[i]]@lines[[1]]@Lines[[j]]@coords) = NULL
      }
    }
    if(class(object) %in% c("SpatialPolygons", "SpatialPolygonsDataFrame")) {
      for(j in 1:length(object_shifted[[i]]@polygons[[1]]@Polygons)) {
        dimnames(object_shifted[[i]]@polygons[[1]]@Polygons[[j]]@coords) = NULL
      }
    }

  }

  if(class(object) %in% c("SpatialLines", "SpatialLinesDataFrame", "SpatialPolygons", "SpatialPolygonsDataFrame")) {
    object_shifted = mapply(spChFIDs, object_shifted, row.names(object))
  }

  object_shifted = do.call(rbind, object_shifted)

}

