#' Shade height calculation considering sun position and buildings outlines.
#'
#' This function calculates shade height at a given point (\code{location}),
#' taking into account:\itemize{
#' \item{Buildings outline, given by a polygonal layer including a height attribute}
#' \item{Sun position, given by elevation and azimuth angles}
#' }
#' @note For a correct geometric calculation, make sure that:\itemize{
#' \item{The layers \code{location} and \code{build} are projected}
#' \item{The values in \code{height_field} of \code{build} are given in the same distance units as the CRS (e.g. meters when using UTM)}
#'}
#'
#' @param location A \code{SpatialPoints*} object specifying the location for which to calculate shade height
#' @param build A \code{SpatialPolygonsDataFrame} object specifying the buildings outline.
#' @param height_field The name of the column with building height in \code{build}
#' @param solar_pos A matrix with the solar azimuth (in degrees from North), and elevation
#' @param b Buffer size when joining intersection points with building outlines, to determine intersection height.
#' @param messages Whether a message regarding distance units of the CRS should be displayed.
#'
#' @return Shade height, in meters.
#'
#' @examples
#' # Single location
#' location = rgeos::gCentroid(build)
#' location_geo = sp::spTransform(location, "+proj=longlat +datum=WGS84")
#' time = as.POSIXct("2004-12-24 13:30:00", tz = "Asia/Jerusalem")
#' solar_pos = maptools::solarpos(location_geo, time)
#' plot(build, main = time)
#' plot(location, add = TRUE)
#' sun = shadow:::.sunLocation(location = location, sun_az = solar_pos[1,1], sun_elev = solar_pos[1,2])
#' sun_ray = ray(from = location, to = sun)
#' build_outline = as(build, "SpatialLinesDataFrame")
#' inter = rgeos::gIntersection(build_outline, sun_ray)
#' plot(sun_ray, add = TRUE, col = "yellow")
#' plot(inter, add = TRUE, col = "red")
#' shadeHeight(location, build, "BLDG_HT", solar_pos)
#'
#' # Grid
#' ext = as(raster::extent(build), "SpatialPolygons")
#' r = raster::raster(ext, res = 3)
#' proj4string(r) = proj4string(build)
#' grid = raster::rasterToPoints(r, spatial = TRUE)
#' grid = sp::SpatialPointsDataFrame(grid, data.frame(grid_id = 1:length(grid)))
#' height_field = "BLDG_HT"
#' for(i in 1:length(grid)) {
#'   grid$shade_height[i] =
#'     shadeHeight(grid[i, ], build, height_field, solar_pos, messages = FALSE)
#' }
#' shade = as(grid, "SpatialPixelsDataFrame")
#' shade = raster::raster(shade, layer = "shade_height")
#' plot(shade, col = grey(seq(0.9, 0.2, -0.01)), main = time)
#' raster::contour(shade, add = TRUE)
#' plot(build, add = TRUE, border = "red")
#'
#' @export

shadeHeight = function(
  location,
  build,
  height_field,
  solar_pos,
  b = 0.1,
  messages = TRUE
  ) {

  # Check that 'location' is of length 1
  if(length(location) != 1)
    stop("'location' should be of length 1")

  # Check projected
  if(!sp::is.projected(location) | !sp::is.projected(build))
    stop("'build' and/or 'location' not in projected CRS")

  # Check classes of 'build' and 'location'
  if(!class(location) %in% c("SpatialPoints", "SpatialPointsDataFrame"))
    stop("'location' is not 'SpatialPoints*'")
  if(class(build) != "SpatialPolygonsDataFrame")
    stop("'build' is not 'SpatialPolygonsDataFrame'")

  # Check that height fields exist
  if(!height_field %in% names(build))
    stop("'height_field' not found in attribute table of 'build'")

  # Check 'solar_pos'
  if(class(solar_pos) != "matrix")
    stop("'solar_pos' must be a 'matrix' object")
  if(ncol(solar_pos) != 2)
    stop("'solar_pos' must have exacly two columns")
  if(any(solar_pos[, 1] < 0) | any(solar_pos[, 1] > 360))
    stop("Sun azimuth should be a number in [0, 360]")
  if(any(solar_pos[, 2] < -90) | any(solar_pos[, 2] > 90))
    stop("Sun elevation should be a number in [-90, 90]")

  # Print units assumption
  if(messages) {
    message(
        paste0(
          "Assuming ", height_field, " given in ",
          gsub(" .*", "",
               gsub(".*\\+units=", "", sp::proj4string(build))
               )
          )
      )
  }

  # Buildings outline to 'lines' *** DEPENDS ON PACKAGE 'sp' ***
  build_outline = as(build, "SpatialLinesDataFrame")

  result = rep(NA, nrow(solar_pos))

  for(p in 1:nrow(solar_pos)) {

  # If sun above the horizon
    if(solar_pos[p, 2] <= 0) shade_height = Inf else { # There is sunlight

      # Sun position
      sun = .sunLocation(location = location, sun_az = solar_pos[p, 1], sun_elev = solar_pos[p, 2])

      # 'Line of sight' between sun and grid point
      sun_ray = shadow::ray(from = location, to = sun)

      # Intersections with buildings outline
      inter = rgeos::gIntersection(build_outline, sun_ray)

      # No intersections means there is no shade
      if(is.null(inter)) shade_height = NA else {

        # If some of the intersections are lines
        if(class(inter) == "SpatialCollections") {

          lin = inter@lineobj
          inter = inter@pointobj

          for(lin_i in 1:length(lin)) {

            lin_pnt = lin[lin_i, ]
            lin_pnt = sp::coordinates(lin_pnt)[[1]][[1]]
            lin_pnt = sp::SpatialPoints(
              lin_pnt,
              proj4string = sp::CRS(sp::proj4string(grid))
              )
            inter = sp::rbind.SpatialPoints(inter, lin_pnt)

          }

        }

        # Set row names
        row.names(inter) = 1:length(inter)

        # Extract building data for each intersection
        inter =
          SpatialPointsDataFrame(
            inter,
            sp::over(
              inter,
              rgeos::gBuffer(
                build_outline,
                byid = TRUE,
                width = b
                ),
              fn = max)
          )

        # Distance between examined location and intersections
        inter$dist = rgeos::gDistance(inter, location, byid = TRUE)[1, ]

        # Shade height calculation
        inter$shade_fall = inter$dist * tan(.deg2rad(solar_pos[p, 2]))
        inter$shade_height = inter@data[, height_field] - inter$shade_fall
        shade_height = max(inter$shade_height)

        # Non-positive shade height means no shade
        if(shade_height <= 0) shade_height = NA

      }

    }

    # If point is on a building and shade height is lower than building
    # Then there is no shade
    if(rgeos::gIntersects(location, build)) {
      build_height = sp::over(location, build)[, height_field]
      if(shade_height <= build_height)
        shade_height = NA
    }

    result[p] = shade_height

  }

  return(result)

}





