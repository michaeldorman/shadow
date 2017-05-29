#' Sky View Factor (SVF) calculation
#'
#' Calculates the Sky View Factor (SVF) at given points or complete grid (\code{location}), taking into account obstacles outline (\code{obstacles}) given by a polygonal layer with a height attribute (\code{obstacles_height_field}).
#'
#' @param location A \code{SpatialPoints*} or \code{Raster*} object, specifying the location(s) for which to calculate SVF
#' @param obstacles A \code{SpatialPolygonsDataFrame} object specifying the obstacles outline
#' @param obstacles_height_field Name of attribute in \code{obstacles} with extrusion height for each feature
#' @param res_angle Circular sampling resolution, in decimal degrees. Default is 5 degrees, i.e. 0, 5, 10... 355.
#' @param b Buffer size when joining intersection points with building outlines, to determine intersection height
#' @param messages Whether a message regarding distance units of the CRS should be displayed
#' @param parallel Whether to use a parallel computation (function \code{mclapply}). Default is \code{FALSE}
#' @param mc.cores The number of cores to use, when \code{parallel} is set to \code{TRUE}
#'
#' @return A numeric value between 0 (sky completely obstructed) and 1 (sky completely visible).
#'\itemize{
#' \item{If input \code{location} is a \code{SpatialPoints*}, then returned object is a \code{vector} elements representing spatial locations (\code{location} features).}
#' \item{If input \code{location} is a \code{Raster*}, then returned object is a \code{RasterLayer} representing the SVF surface.}
#' }
#'
#' @examples
#' # Individual locations
#' data(rishon)
#' location0 = rgeos::gCentroid(rishon)
#' location1 = raster::shift(location0, 0, -15)
#' location2 = raster::shift(location0, -10, 20)
#' locations = rbind(location1, location2)
#' svfs = SVF(
#'   location = locations,
#'   obstacles = rishon,
#'   obstacles_height_field = "BLDG_HT",
#'   parallel = FALSE
#' )
#' plot(rishon)
#' plot(locations, add = TRUE)
#' raster::text(locations, round(svfs, 2), col = "red", pos = 3)
#'
#' \dontrun{
#'
#' # Grid
#' ext = as(raster::extent(rishon), "SpatialPolygons")
#' r = raster::raster(ext, res = 5)
#' proj4string(r) = proj4string(rishon)
#' pnt = raster::rasterToPoints(r, spatial = TRUE)
#' svfs = SVF(
#'     location = r,
#'     obstacles = rishon,
#'     obstacles_height_field = "BLDG_HT",
#'     parallel = TRUE,
#'     mc.cores = 3
#'   )
#' plot(svfs, col = grey(seq(0.9, 0.2, -0.01)))
#' raster::contour(svfs, add = TRUE)
#' plot(rishon, add = TRUE, border = "red")
#'
#' }
#'
#' @export
#' @name SVF

NULL

setGeneric("SVF", function(
  location,
  # surface,
  obstacles,
  obstacles_height_field,
  ...
) {
  standardGeneric("SVF")
})

#' @export
#' @rdname SVF

setMethod(

  f = "SVF",

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
  res_angle = 5,
  b = 0.01,
  messages = TRUE,
  parallel = FALSE,
  mc.cores = NULL
  ) {

  # Checks
  .checkLocation(location, length1 = FALSE)
  .checkObstacles(obstacles, obstacles_height_field, messages)

  # Buildings outline to 'lines' *** DEPENDS ON PACKAGE 'sp' ***
  obstacles_outline = as(obstacles, "SpatialLinesDataFrame")

  # Results vector
  result = rep(NA, length(location)) # Elements represent *space*

  # Iteration over locations and times

  # 'for' loop
  if(!parallel) {

    for(i in 1:length(result)) {

      result[i] = .SVFPnt(
        location = location[i, ],
        obstacles = obstacles,
        obstacles_height_field = obstacles_height_field,
        res_angle = res_angle,
        b = b
      )

    }

  }

  # Parallel
  if(parallel) {

    location_df = sp::SpatialPointsDataFrame(
      location,
      data.frame(id = 1:length(location))
      )
    location_split = split(location_df, location_df$id)

    result = parallel::mclapply(
      location_split,
      .SVFPnt,
      obstacles = obstacles,
      obstacles_height_field = obstacles_height_field,
      res_angle = res_angle,
      b = b,
      mc.cores = mc.cores
      )

    result = simplify2array(result)

  }

  return(result)

  }

)

#' @export
#' @rdname SVF

setMethod(

  f = "SVF",

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
    res_angle = 5,
    b = 0.01,
    messages = TRUE,
    parallel = FALSE,
    mc.cores = NULL
  ) {

    # Keep first raster layer only
    if(raster::nlayers(location) > 1) location = location[[1]]

    # Convert raster to points
    pnt = raster::rasterToPoints(location, spatial = TRUE)

    location[] = SVF(
      location = pnt,
      obstacles = obstacles,
      obstacles_height_field = obstacles_height_field,
      res_angle = res_angle,
      b = b,
      messages = messages,
      parallel = parallel,
      mc.cores = mc.cores
    )

    return(location)

  }

)

#' @export
#' @name SVF















