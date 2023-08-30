#' Estimation of Direct and Diffuse Radiation Load on Extruded Polygon Surfaces
#'
#' This is a wrapper function for calculating total diffuse, direct and total radiation load per unit area on extruded polygon surfaces. The function operates on obstacle geometry and a set of sun positions with associated meteorological estimates for direct and diffuse radiation (see Details below).
#'
#' Input arguments for this function comprise the following:\itemize{
#' \item{An extruded polygon obstacles layer (\code{obstacles} and \code{obstacles_height_field}) inducing shading on the queried \code{grid}}
#' \item{A grid of 3D points (\code{grid}) where radiation is to be estimated. May be created from the 'obstacles' layer, or a subset of it, using function \code{\link{surfaceGrid}}. For instance, in the code example (see below) radiation is estimated on a grid covering just one of four buildings in the \code{build} layer (the first building), but all four buildings are taken into account for evaluating self- and mutual-shading by the buildings.}
#' \item{Solar positions matrix (\code{solar_pos})}
#' \item{Direct and diffuse radiation meteorological estimate vectors (\code{solar_normal} and \code{solar_diffuse})}
#'}
#'
#' Given these inputs, the function goes through the following steps:\itemize{
#' \item{Determining whether each grid point is shaded, at each solar position, using \code{\link{inShadow}}}
#' \item{Calculating the coefficient of Direct Normal Irradiance reduction, using \code{\link{coefDirect}}}
#' \item{Summing direct radiation considering (1) mutual shading, (2) direst radiation coefficient and (3) direct radiation estimates}
#' \item{Calculating the Sky View Factor (SVF) for each point, using \code{\link{SVF}}}
#' \item{Summing diffuse radiation load considering (1) SVF and (2) diffuse radiation estimates}
#' \item{Summing total (direct + diffuse) radiation load}
#' }
#'
#' @param grid A 3D \code{SpatialPointsDataFrame} layer, such as returned by function \code{\link{surfaceGrid}}, specifying the locations where radiation is to be estimated. The layer must include an attribute named \code{type}, with possible values being \code{"roof"} or \code{"facade"}, expressing surface orientation per 3D point. The layer must also include an attribute named \code{facade_az}, specifying facade azimuth (only for \code{"facade"} points, for \code{"roof"} points the value should be \code{NA}). The \code{type} and \code{facade_az} attributes are automatically created when creating the grid with the \code{\link{surfaceGrid}} function
#' @param obstacles A \code{SpatialPolygonsDataFrame} object specifying the obstacles outline, inducing self- and mutual-shading on the grid points
#' @param obstacles_height_field Name of attribute in \code{obstacles} with extrusion height for each feature
#' @param solar_pos A \code{matrix} with two columns representing sun position(s); first column is the solar azimuth (in decimal degrees from North), second column is sun elevation (in decimal degrees); rows represent different sun positions corresponding to the \code{solar_normal} and the \code{solar_diffuse} estimates. For example, if \code{solar_normal} and \code{solar_diffuse} refer to hourly measurements in a Typical Meteorological Year (TMY) dataset, then \code{solar_pos} needs to contain the corresponding hourly sun positions
#' @param time When \code{solar_pos} is unspecified, \code{time} can be passed to automatically calculate \code{solar_pos} based on the time and the centroid of \code{obstacles}, using function \code{suntools::solarpos}. In such case \code{obstacles} must have a defined CRS (not \code{NA}). The \code{time} value must be a \code{POSIXct} or \code{POSIXlt} object
#' @param solar_normal Direct Normal Irradiance (e.g. in Wh/m^2), at sun positions corresponding to \code{solar_pos}. Must be a vector with the same number of elements as the number of rows in \code{solar_pos}
#' @param solar_diffuse Diffuse Horizontal Irradiance (e.g. in Wh/m^2), at sun positions corresponding to \code{solar_pos}. Must be a vector with the same number of elements as the number of rows in \code{solar_pos}
#' @param radius Effective search radius (in CRS units) for considering obstacles when calculating shadow and SVF. The default is to use a global search, i.e. \code{radius=Inf}. Using a smaller radius can be used to speed up the computation when working on large areas. Note that the search radius is not specific per grid point; instead, a buffer is applied on all grid points combined, then "dissolving" the individual buffers, so that exactly the same obstacles apply to all grid points
#' @param returnList Logical, determines whether to return summed radiation over the entire period per 3D point (default, \code{FALSE}), or to return a list with all radiation values per time step (\code{TRUE})
#' @param parallel Number of parallel processes or a predefined socket cluster. With \code{parallel=1} uses ordinary, non-parallel processing. Parallel processing is done with the \code{parallel} package
#'
#' @return If \code{returnList=FALSE} (the default), then returned object is a \code{data.frame}, with rows corresponding to \code{grid} points and four columns corresponding to the following estimates:\itemize{
#' \item{\code{svf}} Computed Sky View Factor (see function \code{\link{SVF}})
#' \item{\code{direct}} Total direct radiation for each grid point
#' \item{\code{diffuse}} Total diffuse radiation for each grid point
#' \item{\code{total}} Total radiation (direct + diffuse) for each grid point
#' }
#' Each row of the \code{data.frame} gives summed radiation values for the entire time period in \code{solar_pos}, \code{solar_normal} and \code{solar_diffuse}
#'
#' If \code{returnList=TRUE} then returned object is a \code{list} with two elements:\itemize{
#' \item{\code{direct}} Total direct radiation for each grid point
#' \item{\code{diffuse}} Total diffuse radiation for each grid point
#' }
#' Each of the elements is a \code{matrix} with rows corresponding to \code{grid} points and columns corresponding to time steps in \code{solar_pos}, \code{solar_normal} and \code{solar_diffuse}
#' @export
#'
#' @examples
#'
#' # Create surface grid
#' grid = surfaceGrid(
#'   obstacles = build[1, ],
#'   obstacles_height_field = "BLDG_HT",
#'   res = 2
#' )
#'
#' solar_pos = tmy[, c("sun_az", "sun_elev")]
#' solar_pos = as.matrix(solar_pos)
#'
#' # Summed 10-hour radiation estimates for two 3D points
#' rad1 = radiation(
#'   grid = grid[1:2, ],
#'   obstacles = build,
#'   obstacles_height_field = "BLDG_HT",
#'   solar_pos = solar_pos[8:17, , drop = FALSE],
#'   solar_normal = tmy$solar_normal[8:17],
#'   solar_diffuse = tmy$solar_diffuse[8:17],
#'   returnList = TRUE
#' )
#' rad1
#'
#' \dontrun{
#'
#' # Same, using 'time' instead of 'solar_pos'
#'
#' rad2 = radiation(
#'   grid = grid[1:2, ],
#'   obstacles = build,
#'   obstacles_height_field = "BLDG_HT",
#'   time = as.POSIXct(tmy$time[8:17], tz = "Asia/Jerusalem"),
#'   solar_normal = tmy$solar_normal[8:17],
#'   solar_diffuse = tmy$solar_diffuse[8:17],
#'   returnList = TRUE
#' )
#' rad2
#'
#' # Differences due to the fact that 'tmy' data come with their own
#' # solar positions, not exactly matching those calulated using 'suntools::solarpos'
#' rad1$direct - rad2$direct
#' rad1$diffuse - rad2$diffuse
#'
#' }
#'
#' \dontrun{
#'
#' ### Warning! The calculation below takes some time.
#'
#' # Annual radiation estimates for entire surface of one building
#' rad = radiation(
#'   grid = grid,
#'   obstacles = build,
#'   obstacles_height_field = "BLDG_HT",
#'   solar_pos = solar_pos,
#'   solar_normal = tmy$solar_normal,
#'   solar_diffuse = tmy$solar_diffuse,
#'   parallel = 3
#' )
#'
#' # 3D plot of the results
#' library(plot3D)
#' opar = par(mfrow=c(1, 3))
#'
#' scatter3D(
#'   x = coordinates(grid)[, 1],
#'   y = coordinates(grid)[, 2],
#'   z = coordinates(grid)[, 3],
#'   colvar = rad$direct / 1000,
#'   scale = FALSE,
#'   theta = 55,
#'   pch = 20,
#'   cex = 1.35,
#'   clab = expression(paste("kWh / ", m^2)),
#'   main = "Direct radiation"
#' )
#' scatter3D(
#'   x = coordinates(grid)[, 1],
#'   y = coordinates(grid)[, 2],
#'   z = coordinates(grid)[, 3],
#'   colvar = rad$diffuse / 1000,
#'   scale = FALSE,
#'   theta = 55,
#'   pch = 20,
#'   cex = 1.35,
#'   clab = expression(paste("kWh / ", m^2)),
#'   main = "Diffuse radiation"
#' )
#' scatter3D(
#'   x = coordinates(grid)[, 1],
#'   y = coordinates(grid)[, 2],
#'   z = coordinates(grid)[, 3],
#'   colvar = rad$total / 1000,
#'   scale = FALSE,
#'   theta = 55,
#'   pch = 20,
#'   cex = 1.35,
#'   clab = expression(paste("kWh / ", m^2)),
#'   main = "Total radiation"
#' )
#'
#' par(opar)
#'}

radiation = function(
  grid,
  obstacles,
  obstacles_height_field,
  solar_pos = solarpos2(obstacles, time),
  time = NULL,
  solar_normal,
  solar_diffuse,
  radius = Inf,
  returnList = FALSE,
  parallel = getOption("mc.cores")
) {

  # Check inputs
  .checkGrid(grid)
  .checkSolarRad(solar_normal, solar_diffuse, solar_pos)

  # Remove obstacles outside of search radius
  if(radius < Inf) {
    b = rgeos::gBuffer(grid, width = radius)
    obstacles = obstacles[b, ]
  }

  # Calculate radiation
  result = .radiationGrid(
    grid,
    obstacles,
    obstacles_height_field,
    solar_pos,
    solar_normal,
    solar_diffuse,
    returnList,
    parallel
  )


  # Return result
  result

}
