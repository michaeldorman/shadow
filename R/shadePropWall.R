#' Shaded proportion of building walls
#'
#' @param seg A \code{SpatialLinesDataFrame} object where each feature is a single segment representing a wall.
#' @param seg_height_field The name of the column with wall height in \code{seg}
#' @param build A \code{SpatialPolygonsDataFrame} object specifying the buildings outline
#' @param height_field The name of the column with building height in \code{build}
#' @param sun_az Sun azimuth, in decimal degrees.
#' @param sun_elev Sun elevation, in decimal degrees.
#' @param shift_dist The distance for shifting the examined locations away from wall to avoid self-shading. Default is 1 cm.
#' @return
#' @examples
#' data(build)
#' seg = toSeg(build[1, ])
#' plot(seg, col = sample(rainbow(length(seg))))
#' shadePropWall (
#'   seg = seg,
#'   seg_height_field = "BLDG_HT",
#'   build = build,
#'   build_height_field = "BLDG_HT",
#'   sun_az = 30,
#'   sun_elev = 20,
#'   sample_dist = 1,
#'   shift_dist = 0.01
#' )

#' raster::text(rgeos::gCentroid(seg, byid = TRUE), 1:length(seg))
#'
#' @export

shadePropWall = function(
  seg,
  seg_height_field,
  build,
  build_height_field,
  sun_az,
  sun_elev,
  sample_dist = 1,
  shift_dist = 0.01
  ) {

  # Check that input is 'SpatialPointsDataFrame'
  stopifnot(class(seg) == "SpatialLinesDataFrame")

  # Check that height field exists
  # ...

  # Check that 'sun_az' and 'sun_elev' are of length 1
  # ...

  # Shift walls
  seg_az = classifyAz(seg)
  seg_shifted = shiftAz(seg, az = seg_az$az, dist = shift_dist)

  # Create regular points grid along walls
  wall_pnt = list()
  for(i in 1:length(seg_shifted)) {
    wall = seg_shifted[i, ]
    wall_sample =
      sp::spsample(
        wall,
        n = round(rgeos::gLength(wall) / sample_dist),
        type = "regular"
      )
    x = SpatialPointsDataFrame(
      wall_sample,
      data = cbind(
        wall@data
      )[rep(1, length(wall_sample)), ]
    )
    x$sample_id = 1:length(x)
    wall_pnt = c(wall_pnt, x)
  }
  wall_pnt = do.call(rbind, wall_pnt)
  wall_pnt$grid_id = 1:length(wall_pnt)

  # Calculate shade height for each point
  for(i in 1:length(grid)) {
    wall_pnt$shade_height[i] = shadeHeight(
      wall_pnt[i, ],
      build,
      build_height_field,
      sun_az,
      sun_elev
      )
  }

  # Calculate shade proportion
  wall_pnt$prop = wall_pnt@data[, seg_height_field]

  return(wall_pnt$prop)

}
