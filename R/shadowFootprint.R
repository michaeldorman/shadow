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
#' @param solar_pos A matrix with one row and two columns; first column is the solar azimuth (in decimal degrees from North), second column is sun elevation (in decimal degrees)
#' @param b Buffer size for shadow footprints of individual segments of a given polygon; used to eliminate minor internal holes in the resulting shadow polygon.
#' @param messages Whether a message regarding distance units of the CRS should be displayed
#'
#' @return A \code{SpatialPolygonsDataFrame} object representing shadow footprint, plus buildings outline. Object length is the same as that of the input \code{obstacles}, with an individual footprint feature for each obstacle.
#'
#' @references
#' Weisthal, M. (2014). Assessment of potential energy savings in Israel through climate-aware residential building design (MSc Thesis, Ben-Gurion University of the Negev).
#' \url{http://aranne5.bgu.ac.il/others/WeisthalMorel.pdf}
#'
#' @examples
#' data(rishon)
#' location = rgeos::gCentroid(rishon)
#' time = as.POSIXct("2004-12-24 13:30:00", tz = "Asia/Jerusalem")
#' solar_pos = maptools::solarpos(
#'   matrix(c(34.7767978098526, 31.9665936050395), ncol = 2),
#'   time
#'   )
#' footprint =
#'   shadowFootprint(
#'     obstacles = rishon,
#'     obstacles_height_field = "BLDG_HT",
#'     solar_pos = solar_pos
#'     )
#' plot(footprint, col = adjustcolor("lightgrey", alpha.f = 0.5))
#' plot(rishon, add = TRUE, col = "darkgrey")
#'
#' @export
#' @name shadowFootprint

NULL

setGeneric("shadowFootprint", function(
  # surface,
  obstacles,
  obstacles_height_field,
  solar_pos,
  b = 0.01,
  messages = FALSE
) {
  standardGeneric("shadowFootprint")
})

#' @rdname shadowFootprint

setMethod(

  f = "shadowFootprint",

  signature = c(
    # surface = "missing",
    obstacles = "SpatialPolygonsDataFrame"
  ),

  function(
    # surface,
    obstacles,
    obstacles_height_field,
    solar_pos,
    b = 0.01,
    messages = FALSE
    ) {

    # Check inputs
    .checkSolarPos(solar_pos, length1 = TRUE)
    .checkObstacles(obstacles, obstacles_height_field, messages)

    # Container for footprints of individual 'obstacles' features
    footprint_final = list()

    for(i in 1:length(obstacles)) {

      # Calculate shift distance
      dist = obstacles@data[i, obstacles_height_field] / tan(deg2rad(solar_pos[1,2]))

      # Shift segments
      seg = shadow::toSeg(obstacles[i, ])
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








