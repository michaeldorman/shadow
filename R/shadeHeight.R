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
#' location = rgeos::gCentroid(build)
#' location_geo = sp::spTransform(location, "+proj=longlat +datum=WGS84")
#' time = as.POSIXct("2004-12-24 13:30:00", tz = "Asia/Jerusalem")
#' solar_position = maptools::solarpos(location_geo, time)
#' sun_az = solar_position[1, 1]
#' sun_elev = solar_position[1, 2]
#' plot(build, main = time)
#' plot(location, add = TRUE)
#' sun = shadow:::sunLocation(location = location, sun_az = sun_az, sun_elev = sun_elev)
#' sun_ray = ray(from = location, to = sun)
#' build_outline = as(build, "SpatialLinesDataFrame")
#' inter = gIntersection(build_outline, sun_ray)
#' plot(sun_ray, add = TRUE, col = "yellow")
#' plot(inter, add = TRUE, col = "red")
#' shadeHeight(location, build, "BLDG_HT", sun_az, sun_elev)
#'
#' # Grid
#' ext = as(extent(build), "SpatialPolygons")
#' r = raster(ext, res = 3)
#' proj4string(r) = proj4string(build)
#' grid = rasterToPoints(r, spatial = TRUE)
#' grid = SpatialPointsDataFrame(grid, data.frame(grid_id = 1:length(grid)))
#' height_field = "BLDG_HT"
#' for(i in 1:length(grid)) {
#'   grid$shade_height[i] =
#'     shadeHeight(grid[i, ], build, height_field, sun_az, sun_elev)
#' }
#' shade = as(grid, "SpatialPixelsDataFrame")
#' shade = raster(shade, layer = "shade_height")
#' plot(shade, col = grey(seq(0.9, 0.2, -0.01)), main = time)
#' contour(shade, add = TRUE)
#' plot(build, add = TRUE, border = "red")
#'
#' @export

shadeHeight = function(location, build, height_field, sun_az, sun_elev, b = 0.1) {

  # Buildings outline to 'lines'
  build_outline = as(build, "SpatialLinesDataFrame")

  # If sun above the horizon
  if(sun_elev <= 0) shade_height = Inf else { # There is sunlight

    # Sun position
    sun = shadow:::sunLocation(location = location, sun_az = sun_az, sun_elev = sun_elev)

    # 'Line of sight' between sun and grid point
    sun_ray = shadow::ray(from = location, to = sun)

    ## Intersections with buildings outline
    inter = rgeos::gIntersection(build_outline, sun_ray)

    # No intersections means there is no shade
    if(is.null(inter)) shade_height = 0 else {

      # If some of the intersections are lines
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
          sp::over(inter, rgeos::gBuffer(build_outline, byid = TRUE, width = b), fn = max)
        )

      # Distance between examined location and intersections
      inter$dist = rgeos::gDistance(inter, location, byid = TRUE)[1, ]

      # Shade height calculation
      inter$shade_fall = inter$dist * tan(shadow:::deg2rad(sun_elev))
      inter$shade_height = inter@data[, height_field] - inter$shade_fall
      shade_height = max(inter$shade_height)

      # Non-positive shade height means no shade
      if(shade_height <= 0) shade_height = 0

    }

  }

  # If point is on a building and shade height is lower than building
  # Then there is no shade
  if(rgeos::gIntersects(location, build)) {
    build_height = over(location, build)[, height_field]
    if(shade_height <= build_height)
      shade_height = build_height
  }

  return(shade_height)

}





