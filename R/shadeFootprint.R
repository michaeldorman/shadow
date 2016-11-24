#' Shade footprint on the ground
#'
#' Creates a vector layer of shade footprint on the ground, given sun position and extruded obstacles (usually a buildings layer).
#' The calculation method was inspired by the of Morel Weisthal's MSc thesis at Ben-Gurion University.
#'
#' @param build A \code{SpatialPolygonsDataFrame} object specifying the buildings outline.
#' @param height_field The name of the column with building height in \code{build}
#' @param sun_az Sun azimuth, in decimal degrees.
#' @param sun_elev Sun elevation, in decimal degrees.
#' @param b Buffer size for shade footprints of individual segments of a given polygon; used to eliminate minor internal holes in the resulting shade polygon.
#'
#' @references
#' Weisthal, M. (2014). Assessment of potential energy savings in Israel through climate-aware residential building design (Doctoral dissertation, Ben-Gurion University of the Negev).
#' \url{http://aranne5.bgu.ac.il/others/WeisthalMorel.pdf}
#'
#' @examples
#' data(build)
#' footprint = shadeFootprint(build, "BLDG_HT", 310, 30)
#' plot(footprint, col = adjustcolor("lightgrey", alpha.f = 0.5))
#' plot(build, add = TRUE, col = "darkgrey")
#'
#' @export

shadeFootprint = function(build, height_field, sun_az, sun_elev, b = 0.01) {

  # Check input classes
  if(class(build) != "SpatialPolygonsDataFrame")
    stop("'build' is not 'SpatialPolygonsDataFrame'")

  # Check projected
  if(!is.projected(build))
    stop("'seg' and/or 'build' not in projected CRS")

  # Check that height fields exist
  if(!build_height_field %in% names(build))
    stop("'build_height_field' not found in attribute table of 'build'")

  # Check that 'sun_az' and 'sun_elev' are of length 1
  if(length(sun_az) != 1 | !is.numeric(sun_az))
    stop("'sun_az' should be a numeric vector of length 1")
  if(length(sun_elev) != 1 | !is.numeric(sun_elev))
    stop("'sun_az' should be a numeric vector of length 1")

  # Check 'sun_az' and 'sun_elev' values
  if(sun_az < 0 | sun_az > 360)
    stop("'sun_az' should be a number in [0-360]")
  if(sun_elev < 0 | sun_elev > 90)
    stop("'sun_az' should be a number in [0-90]")

  # Container for footprints of individual 'build' features
  footprint_final = list()

  for(i in 1:length(build)) {

    # Calculate shift distance
    dist = build@data[i, height_field] / tan(shadow:::deg2rad(sun_elev))

    # Shift segments
    seg = shadow::toSeg(build[i, ])
    seg_shifted = shadow::shiftAz(seg, az = sun_az, dist = dist)

    # Container for footprints of individual segments of one 'build' feature
    footprint = list()

    for(j in 1:length(seg)) {

      f = sp::rbind.SpatialLines(seg[j, ], seg_shifted[j, ], makeUniqueIDs = TRUE)
      f = rgeos::gConvexHull(f)
      footprint[[j]] = f

    }

    # Bind footprings of individual segments of one 'build' feature
    footprint$makeUniqueIDs = TRUE
    footprint = do.call(sp::rbind.SpatialPolygons, footprint)
    footprint = rgeos::gUnaryUnion(footprint)
    footprint = rgeos::gBuffer(footprint, width = b)
    footprint_final[[i]] = rgeos::gUnion(footprint, build[i, ])

    }

  # Bind footprints of individual 'build' features
  footprint_final$makeUniqueIDs = TRUE
  footprint_final = do.call(sp::rbind.SpatialPolygons, footprint_final)
  footprint_final = SpatialPolygonsDataFrame(footprint_final, build@data, match.ID = FALSE)

  return(footprint_final)

}









