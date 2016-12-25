#' Shade footprint on the ground
#'
#' Creates a polygonal layer of shade footprints on the ground, given sun position and extruded obstacles (usually a buildings layer).
#' The calculation method was inspired by Morel Weisthal's MSc thesis at Ben-Gurion University.
#'
#' @param build A \code{SpatialPolygonsDataFrame} object specifying the buildings outline.
#' @param height_field The name of the column with building height in \code{buildings}
#' @param solar_pos A matrix with the solar azimuth (in degrees from North), and elevation
#' @param b Buffer size for shade footprints of individual segments of a given polygon; used to eliminate minor internal holes in the resulting shade polygon.
#'
#' @return A \code{SpatialPolygonsDataFrame} object representing shade footprint plus buildings outline.
#'
#' @references
#' Weisthal, M. (2014). Assessment of potential energy savings in Israel through climate-aware residential building design (Doctoral dissertation, Ben-Gurion University of the Negev).
#' \url{http://aranne5.bgu.ac.il/others/WeisthalMorel.pdf}
#'
#' @examples
#' data(build)
#' location = rgeos::gCentroid(build)
#' time = as.POSIXct("2004-12-24 13:30:00", tz = "Asia/Jerusalem")
#' solar_pos = maptools::solarpos(
#'   matrix(c(34.7767978098526, 31.9665936050395), ncol = 2),
#'   time
#'   )
#' footprint = shadeFootprint(build, "BLDG_HT", solar_pos)
#' plot(footprint, col = adjustcolor("lightgrey", alpha.f = 0.5))
#' plot(build, add = TRUE, col = "darkgrey")
#'
#' @export

shadeFootprint = function(build, height_field, solar_pos, b = 0.01) {

  # Check input classes
  if(class(build) != "SpatialPolygonsDataFrame")
    stop("'build' is not 'SpatialPolygonsDataFrame'")

  # Check projected
  if(!is.projected(build))
    stop("'build' not in projected CRS")

  # Check that height fields exist
  if(!height_field %in% names(build))
    stop("'height_field' not found in attribute table of 'build'")

  # Check 'solar_pos'
  if(class(solar_pos) != "matrix")
    stop("'solar_pos' must be a 'matrix' object")
  if(nrow(solar_pos) != 1)
    stop("'solar_pos' must have exacly one row")
  if(ncol(solar_pos) != 2)
    stop("'solar_pos' must have exacly two columns")
  if(any(solar_pos[, 1] < 0) | any(solar_pos[, 1] > 360))
    stop("Sun azimuth should be a number in [0, 360]")
  if(any(solar_pos[, 2] <= 0) | any(solar_pos[, 2] > 90))
    stop("Sun elevation should be a number in (0, 90]")

  # Container for footprints of individual 'build' features
  footprint_final = list()

  for(i in 1:length(build)) {

    # Calculate shift distance
    dist = build@data[i, height_field] / tan(deg2rad(solar_pos[1,2]))

    # Shift segments
    seg = shadow::toSeg(build[i, ])
    seg_shifted = shadow::shiftAz(seg, az = solar_pos[1,1], dist = -dist)

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









