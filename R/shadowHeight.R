#' Shadow height calculation considering sun position and obstacles
#'
#' This function calculates shadow height at given points or complete grid (\code{location}),
#' taking into account:\itemize{
#' \item{Obstacles outline (\code{obstacles}), given by a polygonal layer with a height attribute (\code{obstacles_height_field})}
#' \item{Sun position (\code{solar_pos}), given by azimuth and elevation angles}
#' }
#' @note For a correct geometric calculation, make sure that:\itemize{
#' \item{The layers \code{location} and \code{obstacles} are projected and in same CRS}
#' \item{The values in \code{obstacles_height_field} of \code{build} are given in the same distance units as the CRS (e.g. meters when using UTM)}
#'}
#'
#' @param location A \code{SpatialPoints*} or \code{Raster*} object, specifying the location(s) for which to calculate shadow height
#' @param obstacles A \code{SpatialPolygonsDataFrame} object specifying the obstacles outline
#' @param obstacles_height_field Name of attribute in \code{obstacles} with extrusion height for each feature
#' @param solar_pos A matrix with two columns representing sun position(s); first column is the solar azimuth (in degrees from North), second column is sun elevation (in degrees); rows represent different positions (e.g. at different times of day)
#' @param b Buffer size when joining intersection points with building outlines, to determine intersection height
#' @param messages Whether a message regarding distance units of the CRS should be displayed
#' @param parallel Number of parallel processes or a predefined socket cluster. With \code{parallel = 1} uses ordinary, non-parallel processing. The parallel processing is done with the \code{parallel} package
#'
#' @return Shadow height, in CRS units (e.g. meters). \code{NA} value means no shadow, \code{Inf} means complete shadow (i.e. sun below horizon).
#' \itemize{
#' \item{If input \code{location} is a \code{SpatialPoints*}, then returned object is a \code{matrix} with rows representing spatial locations (\code{location} features) and columns representing solar positions (\code{solar_pos} rows).}
#' \item{If input \code{location} is a \code{Raster*}, then returned object is a \code{RasterLayer} or \code{RasterStack} representing shadow height surface for a single sun position or multiple sun positions, respectively.}
#' }
#'
#' @examples
#' # Single location
#' location = rgeos::gCentroid(rishon)
#'
#' \dontrun{
#' location_geo = spTransform(location, "+proj=longlat +datum=WGS84")
#' }
#' location_geo = matrix(c(34.7767978098526, 31.9665936050395), ncol = 2)
#' time = as.POSIXct("2004-12-24 13:30:00", tz = "Asia/Jerusalem")
#' solar_pos = maptools::solarpos(location_geo, time)
#' plot(rishon, main = time)
#' plot(location, add = TRUE)
#' sun = shadow:::.sunLocation(location = location, sun_az = solar_pos[1,1], sun_elev = solar_pos[1,2])
#' sun_ray = ray(from = location, to = sun)
#' rishon_outline = as(rishon, "SpatialLinesDataFrame")
#' inter = rgeos::gIntersection(rishon_outline, sun_ray)
#' plot(sun_ray, add = TRUE, col = "yellow")
#' plot(inter, add = TRUE, col = "red")
#' shadowHeight(
#'   location = location,
#'   obstacles = rishon,
#'   obstacles_height_field = "BLDG_HT",
#'   solar_pos = solar_pos
#' )
#'
#' \dontrun{
#'
#' # Two locations and three times
#' location0 = rgeos::gCentroid(rishon)
#' location1 = raster::shift(location0, 0, -15)
#' location2 = raster::shift(location0, -10, 20)
#' locations = rbind(location1, location2)
#' times = seq(from = time, by = "1 hour", length.out = 3)
#' solar_pos = maptools::solarpos(location_geo, times)
#' shadowHeight(
#'   location = locations,
#'   obstacles = rishon,
#'   obstacles_height_field = "BLDG_HT",
#'   solar_pos = solar_pos
#' )
#'
#' # Grid
#' ext = as(raster::extent(rishon), "SpatialPolygons")
#' r = raster::raster(ext, res = 3)
#' proj4string(r) = proj4string(rishon)
#' shadow = shadowHeight(
#'     location = r,
#'     obstacles = rishon,
#'     obstacles_height_field = "BLDG_HT",
#'     solar_pos = solar_pos,
#'     messages = FALSE,
#'     parallel = 3
#'   )
#' plot(shadow[[1]], col = grey(seq(0.9, 0.2, -0.01)), main = time)
#' raster::contour(shadow[[1]], add = TRUE)
#' plot(rishon, add = TRUE, border = "red")
#'
#' }
#' @export
#' @name shadowHeight

NULL

setGeneric("shadowHeight", function(
  location,
  # surface,
  obstacles,
  obstacles_height_field,
  solar_pos,
  ...
) {
  standardGeneric("shadowHeight")
})

#' @export
#' @rdname shadowHeight

setMethod(

  f = "shadowHeight",

  signature = c(
    location = "SpatialPoints"#,
    # surface = "missing",
    # obstacles = "SpatialPolygonsDataFrame"
    ),

function(
  location,
  # surface,
  obstacles,
  obstacles_height_field,
  solar_pos,
  b = 0.01,
  messages = TRUE,
  parallel = getOption("mc.cores")
  ) {

  # Checks
  .checkLocation(location, length1 = FALSE)
  .checkSolarPos(solar_pos, length1 = FALSE)
  .checkObstacles(obstacles, obstacles_height_field, messages)

  # Buildings outline to 'lines' *** DEPENDS ON PACKAGE 'sp' ***
  obstacles_outline = as(obstacles, "SpatialLinesDataFrame")

  # Results matrix
  result = matrix(
    NA,
    nrow = length(location), # Rows represent *space*
    ncol = nrow(solar_pos) # Columns represent *time*
    )

  # Iteration over locations and times
  if (is.null(parallel)) parallel = 1
  hasClus = inherits(parallel, "cluster")

  # 'for' loop
  if(parallel == 1) {

  for(row in 1:nrow(result)) { # Locations

    for(col in 1:nrow(solar_pos)) { # Times

      result[row, col] =
        .shadowHeightPnt(
          location = location[row, ],
          # surface,
          obstacles = obstacles,
          obstacles_height_field = obstacles_height_field,
          solar_pos = solar_pos[col, , drop = FALSE],
          obstacles_outline = obstacles_outline,
          b = 0.01
        )

      }

  }

  } else {

    # Parallel across space
    location_df = sp::SpatialPointsDataFrame(
      location,
      data.frame(id = 1:length(location))
    )
    location_split = split(location_df, location_df$id)

    if(hasClus || parallel > 1) {

      for(col in 1:nrow(solar_pos)) { # Times

        if(.Platform$OS.type == "unix" && !hasClus) {

        tmp = parallel::mclapply(
          X = location_split,
          FUN = .shadowHeightPnt,
          # surface,
          obstacles = obstacles,
          obstacles_height_field = obstacles_height_field,
          solar_pos = solar_pos[col, , drop = FALSE],
          obstacles_outline = obstacles_outline,
          b = 0.01, mc.cores = parallel
        )

        result[, col] = simplify2array(tmp)

        } else {

          if(!hasClus) {
            parallel = parallel::makeCluster(parallel)
          }

          tmp = parallel::parLapply(
            parallel,
            X = location_split,
            fun = .shadowHeightPnt,
            # surface,
            obstacles = obstacles,
            obstacles_height_field = obstacles_height_field,
            solar_pos = solar_pos[col, , drop = FALSE],
            obstacles_outline = obstacles_outline,
            b = 0.01
          )

          result[, col] = simplify2array(tmp)

          if(!hasClus)
            parallel::stopCluster(parallel)
        }

      }

      }

    }

  return(result)

}
)

#' @export
#' @rdname shadowHeight

setMethod(

  f = "shadowHeight",

  signature = c(
    location = "Raster"#,
    # surface = "missing",
    # obstacles = "SpatialPolygonsDataFrame"
  ),

  function(
    location,
    # surface,
    obstacles,
    obstacles_height_field,
    solar_pos,
    b = 0.01,
    messages = TRUE,
    parallel = getOption("mc.cores")
  ) {

    pnt = raster::rasterToPoints(location, spatial = TRUE)

    heights = shadowHeight(
      location = pnt,
      obstacles = obstacles,
      obstacles_height_field = obstacles_height_field,
      solar_pos = solar_pos,
      b = b,
      messages = messages,
      parallel = parallel
      )

    template = location
    result = raster::stack()

    for(i in 1:ncol(heights)) {
      template[] = heights[, i]
      result = raster::stack(result, template)
    }

    if(raster::nlayers(result) == 1)
      return(result[[1]]) else
        (return(result))

  }

)

















