#' Sky View Factor (SVF) calculation
#'
#' Calculates the Sky View Factor (SVF) at given points or complete grid (\code{location}), taking into account obstacles outline (\code{obstacles}) given by a polygonal layer with a height attribute (\code{obstacles_height_field}).
#'
#' @param location A \code{SpatialPoints*} or \code{Raster*} object, specifying the location(s) for which to calculate SVF. If \code{location} is \code{SpatialPoints*}, then it can have 2 or 3 dimensions. In the latter case the 3rd dimension is assumed to be elevation above ground (in CRS units). If \code{location} is \code{RasterLayer} then SVF is calculated for ground locations represented by cell centers (raster values are ignored).
#' @param obstacles A \code{SpatialPolygonsDataFrame} object specifying the obstacles outline
#' @param obstacles_height_field Name of attribute in \code{obstacles} with extrusion height for each feature
#' @param res_angle Circular sampling resolution, in decimal degrees. Default is 5 degrees, i.e. 0, 5, 10... 355.
#' @param b Buffer size when joining intersection points with building outlines, to determine intersection height
#' @param parallel Number of parallel processes or a predefined socket cluster. With \code{parallel=1} uses ordinary, non-parallel processing. Parallel processing is done with the \code{parallel} package
#'
#' @return A numeric value between 0 (sky completely obstructed) and 1 (sky completely visible).
#'\itemize{
#' \item{If input \code{location} is a \code{SpatialPoints*}, then returned object is a \code{vector} where each element representing the SVF for each point in \code{location}}
#' \item{If input \code{location} is a \code{Raster*}, then returned object is a \code{RasterLayer} where cell values express SVF for each ground location}
#' }
#'
#' @note
#' SVF calculation for each view direction follows the following equation -
#' \deqn{1 - (sin(\beta))^2}
#' Where \eqn{\beta} is the highest elevation angle (see equation 3 in Gal & Unger 2014).
#'
#' @references
#' Erell, E., Pearlmutter, D., & Williamson, T. (2012). Urban microclimate: designing the spaces between buildings. Routledge.
#'
#' Gal, T., & Unger, J. (2014). A new software tool for SVF calculations using building and tree-crown databases. Urban Climate, 10, 594-606.
#'
#'
#' @examples
#' ## Individual locations
#' data(rishon)
#' location0 = rgeos::gCentroid(rishon)
#' location1 = raster::shift(location0, 0, -15)
#' location2 = raster::shift(location0, -10, 20)
#' locations = rbind(location1, location2)
#' svfs = SVF(
#'   location = locations,
#'   obstacles = rishon,
#'   obstacles_height_field = "BLDG_HT"
#' )
#' plot(rishon)
#' plot(locations, add = TRUE)
#' raster::text(locations, round(svfs, 2), col = "red", pos = 3)
#'
#' \dontrun{
#'
#' ## Grid
#' ext = as(raster::extent(rishon), "SpatialPolygons")
#' r = raster::raster(ext, res = 5)
#' proj4string(r) = proj4string(rishon)
#' pnt = raster::rasterToPoints(r, spatial = TRUE)
#' svfs = SVF(
#'     location = r,
#'     obstacles = rishon,
#'     obstacles_height_field = "BLDG_HT",
#'     parallel = 3
#'   )
#' plot(svfs, col = grey(seq(0.9, 0.2, -0.01)))
#' raster::contour(svfs, add = TRUE)
#' plot(rishon, add = TRUE, border = "red")
#'
#' ## 3D points
#' ctr = rgeos::gCentroid(rishon)
#' heights = seq(0, 28, 1)
#' loc3d = data.frame(
#'     x = coordinates(ctr)[, 1],
#'     y = coordinates(ctr)[, 2],
#'     z = heights
#' )
#' coordinates(loc3d) = ~ x + y + z
#' proj4string(loc3d) = proj4string(rishon)
#' svfs = SVF(
#'     location = loc3d,
#'     obstacles = rishon,
#'     obstacles_height_field = "BLDG_HT",
#'     parallel = 3
#' )
#' plot(heights, svfs, type = "b", xlab = "Elevation (m)", ylab = "SVF", ylim = c(0, 1))
#' abline(v = rishon$BLDG_HT, col = "red")
#'
#' ## Example from Erell et al. 2012 (p. 19 Fig. 1.2)
#'
#' # Geometry
#' pol1 = rgeos::readWKT("POLYGON ((0 100, 1 100, 1 0, 0 0, 0 100))")
#' pol2 = rgeos::readWKT("POLYGON ((2 100, 3 100, 3 0, 2 0, 2 100))")
#' pol = sp::rbind.SpatialPolygons(pol1, pol2, makeUniqueIDs = TRUE)
#' pol = sp::SpatialPolygonsDataFrame(pol, data.frame(h = c(1, 1)), match.ID = FALSE)
#' pnt = rgeos::readWKT("POINT (1.5 50)")
#' plot(pol, col = "grey", xlim = c(0, 3), ylim = c(45, 55))
#' plot(pnt, add = TRUE, col = "red")
#'
#' # Fig. 1.2 reproduction
#' h = seq(0, 2, 0.1)
#' svf = rep(NA, length(h))
#' for(i in 1:length(h)) {
#'   pol$h = h[i]
#'   svf[i] = SVF(location = pnt, obstacles = pol, obstacles_height_field = "h", res_angle = 1)
#' }
#' plot(h, svf, type = "b", ylim = c(0, 1))
#'
#' # Comparison with SVF values from the book
#' test = c(1, 0.9805806757, 0.9284766909, 0.8574929257, 0.7808688094,
#' 0.7071067812, 0.6401843997, 0.5812381937, 0.52999894, 0.4856429312,
#' 0.4472135955, 0.4138029443, 0.3846153846, 0.3589790793, 0.336336397,
#' 0.316227766, 0.2982749931, 0.282166324, 0.2676438638, 0.2544932993,
#' 0.242535625)
#' range(test - svf)
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

##################################################################################
# 'SVF' method for points

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
  parallel = getOption("mc.cores")
  ) {

  # Checks
  .checkLocation(location, length1 = FALSE)
  .checkObstacles(obstacles, obstacles_height_field)

  # Buildings outline to 'lines' *** DEPENDS ON PACKAGE 'sp' ***
  obstacles_outline = as(obstacles, "SpatialLinesDataFrame")

  # Iteration over locations

  # Parallel
  if(is.null(parallel)) parallel = 1
  hasClus = inherits(parallel, "cluster")

  # 'for' loop
  if(parallel == 1) {

    # Results vector
    result = rep(NA, length(location))

    for(i in 1:length(result)) {

      result[i] = .SVFPnt(
        location = location[i, ],
        obstacles = obstacles,
        obstacles_height_field = obstacles_height_field,
        res_angle = res_angle,
        b = b
      )

    }

  } else {

    location_df = sp::SpatialPointsDataFrame(
      location,
      data.frame(id = 1:length(location))
    )
    location_split = split(location_df, location_df$id)

    if(hasClus || parallel > 1) {

      if(.Platform$OS.type == "unix" && !hasClus) {

        result = parallel::mclapply(
          location_split,
          .SVFPnt,
          obstacles = obstacles,
          obstacles_height_field = obstacles_height_field,
          res_angle = res_angle,
          b = b,
          mc.cores = parallel
          )

        result = simplify2array(result)

      } else {

        if(!hasClus) {
          parallel = parallel::makeCluster(parallel)
        }

        result = parallel::parLapply(
          parallel,
          X = location_split,
          fun = .SVFPnt,
          obstacles = obstacles,
          obstacles_height_field = obstacles_height_field,
          res_angle = res_angle,
          b = b
        )

        result = simplify2array(result)

        if(!hasClus)
          parallel::stopCluster(parallel)

      }

    }

  }

  return(result)

  }

)

#' @export
#' @rdname SVF

##################################################################################
# 'SVF' method for raster

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
    parallel = getOption("mc.cores")
  ) {

    # Keep first raster layer only
    if(raster::nlayers(location) > 1) location = location[[1]]

    # Convert raster to points
    pnt = raster::rasterToPoints(location, spatial = TRUE)

    # Run the 'SpatialPoints' method for each point on raster
    location[] = SVF(
      location = pnt,
      obstacles = obstacles,
      obstacles_height_field = obstacles_height_field,
      res_angle = res_angle,
      b = b,
      parallel = parallel
    )

    return(location)

  }

)

#' @export
#' @name SVF

##################################################################################













