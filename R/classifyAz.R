#' Classify azimuth of line segments
#'
#' @param sl A \code{SpatialLines*} object
#'
#' @return A \code{numeric} vector with the segment azimuth values (in decimal degrees)
#'
#' @examples
#' build_seg = toSeg(build[1, ])
#' az = classifyAz(build_seg)
#' plot(build_seg, col = rainbow(4)[cut(az, c(0, 90, 180, 270, 360))])
#' raster::text(
#'   # rgeos::gCentroid(build_seg, byid = TRUE),
#'   as(sf::st_centroid(sf::st_as_sf(build_seg)), "Spatial"),
#'   round(az)
#' )
#'
#' @export

classifyAz = function(sl) {

  # If input is not SpatialLines
  stopifnot(class(sl) %in% c("SpatialLines", "SpatialLinesDataFrame"))

  # Empty vector for holding results
  result = rep(NA, length(sl))

  # For each feature
  for(i in 1:length(sl)) {

    # Select one
    s = sl[i, ]
    m = s@lines[[1]]@Lines[[1]]@coords

    # If feature is not a segment
    if(!nrow(m) == 2)
      stop(
        "Input contains features which are not segments.
        Consider using function 'polToSeg' first."
        )

    # Calculate segment direction
    x1 = m[1, 1]
    y1 = m[1, 2]
    x2 = m[2, 1]
    y2 = m[2, 2]

    if(x2 >= x1 & y2 > y1) {
      az = 360 - (180 / pi) * atan((y2-y1) / (x2-x1))
      }
    if(x2 >= x1 & y2 <= y1) {
      az = (180 / pi) * atan((y1-y2) / (x2-x1))
      }
    if(x2 < x1 & y2 <= y1) {
      az = 180 - (180 / pi) * atan((y1-y2) / (x1-x2))
      }
    if(x2 < x1 & y2 > y1) {
      az = 180 + (180 / pi) * atan((y2-y1) / (x1-x2))
      }

    result[i] = az

  }

  return(result)

}
