#' Line between two points
#'
#' The function connects two points into a line segment.
#'
#' @param from A \code{SpatialPoints*} object specifying origin.
#' @param to A \code{SpatialPoints*} object specifying destination.
#'
#' @return A \code{SpatialLines} object.
#'
#' @examples
#' ctr = rgeos::gCentroid(build)
#' angles = seq(0, 359, 20)
#' sun = mapply(
#'   shadow:::.sunLocation,
#'   sun_az = angles,
#'   MoreArgs = list(
#'     location = ctr,
#'     sun_elev = 10)
#' )
#' rays = mapply(ray, MoreArgs = list(from = ctr), to = sun)
#' rays$makeUniqueIDs = TRUE
#' rays = do.call(rbind, rays)
#' plot(rays)
#' sun = do.call(rbind, sun)
#' text(sun, as.character(angles))
#'
#' @export

ray = function(from, to) {

  sp::SpatialLines(
    list(
      sp::Lines(
        list(
          sp::Line(
            rbind(
              sp::coordinates(from)[, 1:2],
              sp::coordinates(to)[, 1:2]
            )
          )
        ),
        ID = "A"
      )
    ),
    proj4string = sp::CRS(sp::proj4string(from))
  )

}
