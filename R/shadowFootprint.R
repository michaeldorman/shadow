#' Shadow footprint on the ground
#'
#' Creates a polygonal layer of shadow footprints on the ground, taking into account:\itemize{
#' \item{Obstacles outline (\code{obstacles}), given by a polygonal layer with a height attribute (\code{obstacles_height_field})}
#' \item{Sun position (\code{solar_pos}), given by azimuth and elevation angles}
#' }
#' The calculation method was inspired by Morel Weisthal's MSc thesis at the Ben-Gurion University of the Negev.
#'
#' @param obstacles A \code{SpatialPolygonsDataFrame} object specifying the obstacles outline
#' @param obstacles_height_field Name of attribute in \code{obstacles} with extrusion height for each feature
#' @param solar_pos A \code{matrix} with one row and two columns; first column is the solar azimuth (in decimal degrees from North), second column is sun elevation (in decimal degrees)
#' @param time When \code{solar_pos} is unspecified, \code{time} can be passed to automatically calculate \code{solar_pos} based on the time and the centroid of \code{obstacles}, using function \code{suntools::solarpos}. In such case \code{obstacles} must have a defined CRS (not \code{NA}). The \code{time} value must be a \code{POSIXct} or \code{POSIXlt} object

#' @param b Buffer size for shadow footprints of individual segments of a given polygon; used to eliminate minor internal holes in the resulting shadow polygon.
#'
#' @return A \code{SpatialPolygonsDataFrame} object representing shadow footprint, plus buildings outline. Object length is the same as that of the input \code{obstacles}, with an individual footprint feature for each obstacle.
#'
#' @references
#' Weisthal, M. (2014). Assessment of potential energy savings in Israel through climate-aware residential building design (MSc Thesis, Ben-Gurion University of the Negev).
#' \url{https://www.dropbox.com/s/bztnh1fi9znmswj/Thesis_Morel_Weisthal.pdf?dl=1}
#'
#' @examples
#' time = as.POSIXct("2004-12-24 13:30:00", tz = "Asia/Jerusalem")
#' proj4string(build) = CRS("+init=epsg:32636")
#' location_geo = matrix(c(34.7767978098526, 31.9665936050395), ncol = 2)
#' solar_pos = suntools::solarpos(location_geo, time)
#' footprint1 =               ## Using 'solar_pos'
#'   shadowFootprint(
#'     obstacles = build,
#'     obstacles_height_field = "BLDG_HT",
#'     solar_pos = solar_pos
#'     )
#' footprint2 =               ## Using 'time'
#'   shadowFootprint(
#'     obstacles = build,
#'     obstacles_height_field = "BLDG_HT",
#'     time = time
#'     )
#' all.equal(footprint1, footprint2)
#' footprint = footprint1
#' plot(footprint, col = adjustcolor("lightgrey", alpha.f = 0.5))
#' plot(build, add = TRUE, col = "darkgrey")
#'
#' @export
#' @name shadowFootprint

NULL

setGeneric("shadowFootprint", function(
  obstacles,
  obstacles_height_field,
  ...
) {
  standardGeneric("shadowFootprint")
})

#' @rdname shadowFootprint

setMethod(

  f = "shadowFootprint",

  signature = c(
    obstacles = "SpatialPolygonsDataFrame"
  ),

  function(
    obstacles,
    obstacles_height_field,
    solar_pos = solarpos2(obstacles, time),
    time = NULL,
    b = 0.01
    ) {

    # Check inputs
    .checkSolarPos(solar_pos, length1 = TRUE)
    .checkObstacles(obstacles, obstacles_height_field)

    # Container for footprints of individual 'obstacles' features
    footprint_final = list()

    for(i in 1:length(obstacles)) {

      # Calculate shift distance
      dist = obstacles@data[i, obstacles_height_field] / tan(deg2rad(solar_pos[1,2]))

      # Polygon to line segments
      seg = shadow::toSeg(obstacles[i, ])

      # Discard zero-length segments
      seg = seg[rgeos::gLength(seg, byid = TRUE) > 0, ]

      # Shift segments
      seg_shifted = shadow::shiftAz(seg, az = solar_pos[1, 1], dist = -dist)

      # Container for footprints of individual segments of one 'obstacles' feature
      footprint = list()

      for(j in 1:length(seg)) {

        f = sp::rbind.SpatialLines(seg[j, ], seg_shifted[j, ], makeUniqueIDs = TRUE)
        f = rgeos::gConvexHull(f)
        footprint[[j]] = f

      }

      # Bind footprings of individual segments of one 'obstacles' feature
      footprint$makeUniqueIDs = TRUE
      footprint = do.call(sp::rbind.SpatialPolygons, footprint)
      footprint = rgeos::gUnaryUnion(footprint)
      footprint = rgeos::gBuffer(footprint, width = b)
      footprint_final[[i]] = rgeos::gUnion(footprint, obstacles[i, ])

      }

    # Bind footprints of individual 'obstacles' features
    footprint_final$makeUniqueIDs = TRUE
    footprint_final = do.call(sp::rbind.SpatialPolygons, footprint_final)
    footprint_final = SpatialPolygonsDataFrame(footprint_final, obstacles@data, match.ID = FALSE)

    return(footprint_final)

  }
)

