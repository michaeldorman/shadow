#' Shadow height calculation considering sun position and obstacles
#'
#' This function calculates shadow height at given points or complete grid (\code{location}),
#' taking into account:\itemize{
#' \item{Obstacles outline (\code{obstacles}), given by a polygonal layer with a height attribute (\code{obstacles_height_field})}
#' \item{Sun position (\code{solar_pos}), given by azimuth and elevation angles}
#' }
#'
#' @param location A \code{SpatialPoints*} or \code{Raster*} object, specifying the location(s) for which to calculate shadow height
#' @param obstacles A \code{SpatialPolygonsDataFrame} object specifying the obstacles outline
#' @param obstacles_height_field Name of attribute in \code{obstacles} with extrusion height for each feature
#' @param solar_pos A \code{matrix} with two columns representing sun position(s); first column is the solar azimuth (in decimal degrees from North), second column is sun elevation (in decimal degrees); rows represent different positions (e.g. at different times of day)
#' @param time When \code{solar_pos} is unspecified, \code{time} can be passed to automatically calculate \code{solar_pos} based on the time and the centroid of \code{location}, using function \code{suntools::solarpos}. In such case \code{location} must have a defined CRS (not \code{NA}). The \code{time} value must be a \code{POSIXct} or \code{POSIXlt} object
#' @param b Buffer size when joining intersection points with building outlines, to determine intersection height
#' @param parallel Number of parallel processes or a predefined socket cluster. With \code{parallel=1} uses ordinary, non-parallel processing. Parallel processing is done with the \code{parallel} package
#' @param filter_footprint Should the points be filtered using \code{shadowFootprint} before calculating shadow height? This can make the calculation faster when there are many point which are not shaded
#'
#' @return
#' Returned object is either a numeric \code{matrix} or a \code{Raster*} -
#' \itemize{
#' \item{If input \code{location} is a \code{SpatialPoints*}, then returned object is a \code{matrix}, where rows represent spatial locations (\code{location} features), columns represent solar positions (\code{solar_pos} rows) and values represent shadow height}
#' \item{If input \code{location} is a \code{Raster*}, then returned object is a \code{RasterLayer} or \code{RasterStack} where layers represent solar positions (\code{solar_pos} rows) and pixel values represent shadow height}
#' }
#' In both cases the numeric values express shadow height -
#' \itemize{
#' \item{\code{NA} value means no shadow}
#' \item{A \strong{valid number} expresses shadow height, in CRS units (e.g. meters)}
#' \item{\code{Inf} means complete shadow (i.e. sun below horizon)}
#' }
#'
#' @note For a correct geometric calculation, make sure that:\itemize{
#' \item{The layers \code{location} and \code{obstacles} are projected and in same CRS}
#' \item{The values in \code{obstacles_height_field} of \code{obstacles} are given in the same distance units as the CRS (e.g. meters when using UTM)}
#'}
#'
#' @examples
#' # Single location
#' location = rgeos::gCentroid(build)
#' location_geo = matrix(c(34.7767978098526, 31.9665936050395), ncol = 2)
#' time = as.POSIXct("2004-12-24 13:30:00", tz = "Asia/Jerusalem")
#' solar_pos = suntools::solarpos(location_geo, time)
#' plot(build, main = time)
#' plot(location, add = TRUE)
#' sun = shadow:::.sunLocation(location = location, sun_az = solar_pos[1,1], sun_elev = solar_pos[1,2])
#' sun_ray = ray(from = location, to = sun)
#' build_outline = as(build, "SpatialLinesDataFrame")
#' inter = rgeos::gIntersection(build_outline, sun_ray)
#' plot(sun_ray, add = TRUE, col = "yellow")
#' plot(inter, add = TRUE, col = "red")
#' shadowHeight(
#'   location = location,
#'   obstacles = build,
#'   obstacles_height_field = "BLDG_HT",
#'   solar_pos = solar_pos
#' )
#'
#' # Automatically calculating 'solar_pos' using 'time'
#' proj4string(build) = CRS("+init=epsg:32636")
#' proj4string(location) = CRS("+init=epsg:32636")
#' shadowHeight(
#'   location = location,
#'   obstacles = build,
#'   obstacles_height_field = "BLDG_HT",
#'   time = time
#' )
#'
#' \dontrun{
#'
#' # Two points - three times
#' location0 = rgeos::gCentroid(build)
#' location1 = raster::shift(location0, 0, -15)
#' location2 = raster::shift(location0, -10, 20)
#' locations = rbind(location1, location2)
#' time = as.POSIXct("2004-12-24 13:30:00", tz = "Asia/Jerusalem")
#' times = seq(from = time, by = "1 hour", length.out = 3)
#' shadowHeight(                            ## Using 'solar_pos'
#'   location = locations,
#'   obstacles = build,
#'   obstacles_height_field = "BLDG_HT",
#'   solar_pos = suntools::solarpos(location_geo, times)
#' )
#' shadowHeight(                            ## Using 'time'
#'   location = locations,
#'   obstacles = build,
#'   obstacles_height_field = "BLDG_HT",
#'   time = times
#' )
#'
#' # Grid - three times
#' time = as.POSIXct("2004-12-24 13:30:00", tz = "Asia/Jerusalem")
#' times = seq(from = time, by = "1 hour", length.out = 3)
#' ext = as(raster::extent(build), "SpatialPolygons")
#' r = raster::raster(ext, res = 2)
#' proj4string(r) = proj4string(build)
#' x = Sys.time()
#' shadow1 = shadowHeight(
#'   location = r,
#'   obstacles = build,
#'   obstacles_height_field = "BLDG_HT",
#'   time = times,
#'   parallel = 3
#' )
#' y = Sys.time()
#' y - x
#' x = Sys.time()
#' shadow2 = shadowHeight(
#'   location = r,
#'   obstacles = build,
#'   obstacles_height_field = "BLDG_HT",
#'   solar_pos = solarpos2(r, times),
#'   parallel = 3
#' )
#' y = Sys.time()
#' y - x
#' shadow = shadow1
#' opar = par(mfrow = c(1, 3))
#' for(i in 1:raster::nlayers(shadow)) {
#'   plot(shadow[[i]], col = grey(seq(0.9, 0.2, -0.01)), main = raster::getZ(shadow)[i])
#'   raster::contour(shadow[[i]], add = TRUE)
#'   plot(build, border = "red", add = TRUE)
#' }
#' par(opar)
#'
#' }
#' @export
#' @name shadowHeight

NULL

setGeneric("shadowHeight", function(
  location,
  obstacles,
  obstacles_height_field,
  ...
) {
  standardGeneric("shadowHeight")
})

#' @export
#' @rdname shadowHeight

##################################################################################
# 'shadowHeight' method for points

setMethod(

  f = "shadowHeight",

  signature = c(location = "SpatialPoints"),

  function(
    location,
    obstacles,
    obstacles_height_field,
    solar_pos = solarpos2(location, time),
    time = NULL,
    b = 0.01,
    parallel = getOption("mc.cores"),
    filter_footprint = FALSE
    ) {

    # Checks
    .checkLocation(location, length1 = FALSE)
    .checkObstacles(obstacles, obstacles_height_field)
    .checkSolarPos(solar_pos, length1 = FALSE)

    # Obstacles outline to 'lines' *** DEPENDS ON PACKAGE 'sp' ***
    obstacles_outline = as(obstacles, "SpatialLinesDataFrame")

    # Results matrix
    result = matrix(
      NA,
      nrow = length(location),  ## Rows represent *space*
      ncol = nrow(solar_pos)    ## Columns represent *time*
      )

    # Iteration over locations and times
    if (is.null(parallel)) parallel = 1
    hasClus = inherits(parallel, "cluster")

    # 'for' loop
    if(parallel == 1) {

    for(col in 1:ncol(result)) { # Times

      # Filter unshaded area based on 'shadowFootprint'
      if(filter_footprint) {
        footprint = shadowFootprint(
          obstacles = obstacles,
          obstacles_height_field = obstacles_height_field,
          solar_pos = solar_pos[col, , drop = FALSE]
        )
        intersection_w_footprint = rgeos::gIntersects(location, footprint, byid = TRUE)[1, ]
      }

      for(row in 1:nrow(result)) { # Locations

        result[row, col] =
          .shadowHeightPnt(
            location = location[row, ],
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

          # Filter unshaded area based on 'shadowFootprint'
          if(filter_footprint) {
            footprint = shadowFootprint(
              obstacles = obstacles,
              obstacles_height_field = obstacles_height_field,
              solar_pos = solar_pos[col, , drop = FALSE]
            )
            intersection_w_footprint = rgeos::gIntersects(location, footprint, byid = TRUE)[1, ]
          }

          if(.Platform$OS.type == "unix" && !hasClus) {

            if(filter_footprint) {
              evaluated_locations = location_split[intersection_w_footprint]
            } else {
              evaluated_locations = location_split
            }

          tmp = parallel::mclapply(
            X = evaluated_locations,
            FUN = .shadowHeightPnt,
            # surface,
            obstacles = obstacles,
            obstacles_height_field = obstacles_height_field,
            solar_pos = solar_pos[col, , drop = FALSE],
            obstacles_outline = obstacles_outline,
            b = 0.01,
            mc.cores = parallel
          )

          if(filter_footprint) {
            result[intersection_w_footprint, col] = simplify2array(tmp)
          } else {
            result[, col] = simplify2array(tmp)
          }

          } else {

            if(!hasClus) {
              parallel = parallel::makeCluster(parallel)
            }

            if(filter_footprint) {
              evaluated_locations = location_split[intersection_w_footprint]
            } else {
              evaluated_locations = location_split
            }

            tmp = parallel::parLapply(
              parallel,
              X = evaluated_locations,
              fun = .shadowHeightPnt,
              obstacles = obstacles,
              obstacles_height_field = obstacles_height_field,
              solar_pos = solar_pos[col, , drop = FALSE],
              obstacles_outline = obstacles_outline,
              b = 0.01
            )

            if(filter_footprint) {
              result[intersection_w_footprint, col] = simplify2array(tmp)
            } else {
              result[, col] = simplify2array(tmp)
            }

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

##################################################################################
# 'shadowHeight' method for raster

setMethod(

  f = "shadowHeight",

  signature = c(location = "Raster"),

  function(
    location,
    obstacles,
    obstacles_height_field,
    solar_pos = solarpos2(pnt, time),
    time = NULL,
    b = 0.01,
    parallel = getOption("mc.cores"),
    filter_footprint = FALSE
  ) {

    pnt = raster::rasterToPoints(location, spatial = TRUE)

    heights = shadowHeight(
      location = pnt,
      obstacles = obstacles,
      obstacles_height_field = obstacles_height_field,
      solar_pos = solar_pos,
      time = time,
      b = b,
      parallel = parallel,
      filter_footprint = filter_footprint
    )

    template = location
    result = raster::stack()

    for(i in 1:ncol(heights)) {
      template[] = heights[, i]
      result = raster::stack(result, template)
    }

    if(!is.null(time)) {
      result = raster::setZ(result, time)
    }

    if(raster::nlayers(result) == 1)
      return(result[[1]]) else
        (return(result))

  }

)

##################################################################################
















