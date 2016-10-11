#' Shade height calculation considering sun position and buildings outlines.
#'
#' This function calculates shade height at a given point (\code{location}),
#' taking into account:\itemize{
#' \item{Buildings outline, given by a polygonal layer including a height attribute}
#' \item{Sun position, given by elevation and azimuth angles}
#' }
#'
#' @param location A \code{SpatialPoints*} object specifying the location for which to calculate shade height
#' @param build A \code{SpatialPolygonsDataFrame} object specifying the buildings outline
#' @param height_field The name of the column with building height in \code{build}
#' @param sun_az Sun azimuth, in decimal degrees.
#' @param sun_elev Sun elevation, in decimal degrees.
#' @param b Buffer size when joining intersection points with building outlines, to determine intersection height.
#'
#' @return Shade height, in meters.
#'
#' @examples
#'
#' library(sp)
#' library(rgeos)
#' library(raster)
#'
#' # Single location
#' ctr = gCentroid(build)
#' plot(build)
#' plot(ctr, add = TRUE)
#' location = ctr
#' build = build
#' height_field = "BLDG_HT"
#' sun_az = 30
#' sun_elev = 20
#' sun = sunLocation(location = location, sun_az = sun_az, sun_elev = sun_elev)
#' sun_ray = ray(from = location, to = sun)
#' build_outline = as(build_outline, "SpatialLinesDataFrame")
#' inter = gIntersection(build_outline, sun_ray)
#' plot(sun_ray, add = TRUE, col = "yellow")
#' plot(inter, add = TRUE, col = "red")
#' shadeHeight(location, build, height_field, sun_az, sun_elev)
#'
#' # Grid
#' ext = as(extent(build), "SpatialPolygons")
#' r = raster(ext, res = 1)
#' proj4string(r) = proj4string(build)
#' grid = rasterToPoints(r, spatial = TRUE)
#' grid = SpatialPointsDataFrame(grid, data.frame(grid_id = 1:length(grid)))
#' build = build
#' height_field = "BLDG_HT"
#' sun_az = 70
#' sun_elev = 30
#' for(i in 1:length(grid)) {
#'   grid$shade_height[i] =
#'     shadeHeight(grid[i, ], build, height_field, sun_az, sun_elev)
#' }
#' shade = as(grid, "SpatialPixels")
#' shade = raster(shade)
#' proj4string(shade) = proj4string(build)
#' shade = rasterize(grid, shade, field = "shade_height")
#' plot(shade, col = grey(seq(0.9, 0.2, -0.01)))
#' contour(shade, add = TRUE)
#' plot(build, add = TRUE, border = "red")
#'
#' @export

shadeHeight = function(location, build, height_field, sun_az, sun_elev, b = 0.1) {

  # Buildings outline to 'lines'
  build_outline = as(build, "SpatialLinesDataFrame")

  # If sun above the horizon
  if(sun_elev > 0) {

    # Sun position
    sun = sunLocation(location = location, sun_az = sun_az, sun_elev = sun_elev)

    # 'Line of sight' between sun and grid point
    sun_ray = ray(from = location, to = sun)

    ## Intersections with buildings outline
    inter = gIntersection(build_outline, sun_ray)

    # If there are any intersections
    if(!is.null(inter)) {

      # If some of the intersections are are lines
      if(class(inter) == "SpatialCollections") {

        lin = inter@lineobj
        inter = inter@pointobj

        for(lin_i in 1:length(lin)) {

          lin_pnt =
            lin[lin_i, ] %>%
            coordinates %>%
            "[["(1) %>%
            "[["(1) %>%
            SpatialPoints(proj4string = CRS(proj4string(grid)))
          inter = rbind(inter, lin_pnt)

        }

      }

      # Set row names
      row.names(inter) = 1:length(inter)

      # Extract building data for each intersection
      inter =
        SpatialPointsDataFrame(
          inter,
          over(inter, gBuffer(build_outline, byid = TRUE, width = b), fn = max)
        )

      # Distance between examined location and intersections
      inter$dist = gDistance(inter, location, byid = TRUE)[1, ]

      # Shade height calculation
      inter$shade_fall = inter$dist * tan(deg2rad(sun_elev))
      inter$shade_height = inter@data[, height_field] - inter$shade_fall
      inter$shade_height[inter$shade_height < 0] = 0
      shade_height = max(inter$shade_height)

    } else shade_height = 0

  } else shade_height = Inf

  # If point is on a building then shade height is at least the building height
  if(rgeos::gIntersects(location, build))
    shade_height = max(
      c(
        shade_height,
        over(location, build)[, height_field]
        )
      )

  return(shade_height)

}





