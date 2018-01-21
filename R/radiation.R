#' Estimation of Total Direct and Diffuse Radiation Load on Extruded Polygon Surfaces
#'
#' This is a wrapper function for calculating summed diffuse, direct and total radiation load per unit area on extruded polygon surfaces.
#'
#' Input arguments comprise:\itemize{
#'
#' \item{An obstacles layer (\code{obstacles} and \code{obstacles_height_field}) inducing shading on the queried location}
#'
#' \item{A 3D point grid (\code{grid} where radiation is to be estimated. May be created from the 'obstacles' layer or a subset of it using \strong{\code{\link{surfaceGrid}}}}. For instance, in the code example (see below) radiation is estimated on a grid covering just one of four buildings in the \code{rishon} layer (the first building), but all four buildings are taken into account for evaluating shading.
#'
#' \item{Solar positions (\code{sloar_pos})}
#'
#' \item{Direct and diffuse radiation meteorological estimates (\code{solar_normal} and \code{solar_diffuse})}
#'
#'}
#'
#' Given these inputs, the function goest through the following steps:\itemize{
#'
#' \item{Determining whether each grid point is shaded at each time, using \strong{\code{\link{inShadow}}}}
#'
#' \item{Calculating the coefficient of Direct Normal Irradiance reduction, using \strong{\code{\link{coefDirect}}}}
#'
#' \item{Summing direct radiation considering (1) mutual shading, (2) direst radiation coefficient and (3) direct radiation estimates}
#'
#' \item{Calculating the Sky View Factor (SVF) for each point, using \strong{\code{\link{SVF}}}}
#'
#' \item{Summing diffuse radiation load considering (1) SVF and (2) diffuse radiation estimates}
#'
#' \item{Calculating total (direct + diffuse) radiation load}
#'
#' }
#'
#' @param grid A 3D \code{SpatialPointsDataFrame} layer, such as returned by function \strong{\code{\link{surfaceGrid}}}, specifying the locations where radiation is to be estimated. The layer must include an attribute named \code{type}, with possible values \code{"roof"} and \code{"facade"} specifying surface orientation, and an attribute named \code{facade_az} specifying facade azimuth for facades. These two attributes are automatically created when creating the grid with \code{\link{surfaceGrid}}
#' @param obstacles A \code{SpatialPolygonsDataFrame} object specifying the obstacles outline
#' @param obstacles_height_field Name of attribute in \code{obstacles} with extrusion height for each feature
#' @param solar_pos A matrix with two columns representing sun position(s); first column is the solar azimuth (in decimal degrees from North), second column is sun elevation (in decimal degrees); rows represent different sun positions corresponding to the \code{solar_normal} and the \code{solar_diffuse} estimates. For example, if \code{solar_normal} and \code{solar_diffuse} refer to hourly measurements in a Typical Meteorological Year (TMY) dataset then \code{solar_pos} needs to contain the corresponding hourly sun positions, in which case the returned value will represent total annual radiation load (see example below)
#' @param solar_normal Direct Normal Irradiance (e.g. in Wh/m^2)
#' @param solar_diffuse Diffuse Horizontal Irradiance (e.g. in Wh/m^2)
#' @param parallel Number of parallel processes or a predefined socket cluster. With \code{parallel=1} uses ordinary, non-parallel processing. Parallel processing is done with the \code{parallel} package
#'
#' @return a \code{data.frame} with three columns:\itemize{
#' \item{\code{svf}} Computed Sky View Factor (see function \code{\link{SVF}})
#' \item{\code{direct}} Total direct radiation per grid point
#' \item{\code{diffuse}} Total diffuse radiation per grid point
#' \item{\code{total}} Total
#' }
#' @export
#'
#' @examples
#'
#'# Create surface grid
#' grid = surfaceGrid(
#'   obstacles = rishon[1, ],
#'   obstacles_height_field = "BLDG_HT",
#'   res = 2
#' )
#'
#' solar_pos = tmy[, c("sun_az", "sun_elev")]
#' solar_pos = as.matrix(solar_pos)
#'
#' # One point and one day
#' rad = radiation(
#'   grid = grid[1, ],
#'   obstacles = rishon,
#'   obstacles_height_field = "BLDG_HT",
#'   solar_pos = solar_pos[8:17, , drop = FALSE],
#'   solar_normal = tmy$solar_normal[8:17],
#'   solar_diffuse = tmy$solar_diffuse[8:17]
#' )
#' rad
#'
#' \dontrun{
#'
#' ### Warning! The calculation below takes some time.
#'
#' # All grid points and annual summary
#' rad = radiation(
#'   grid = grid,
#'   obstacles = rishon,
#'   obstacles_height_field = "BLDG_HT",
#'   solar_pos = solar_pos,
#'   solar_normal = tmy$solar_normal,
#'   solar_diffuse = tmy$solar_diffuse,
#'   parallel = 3
#' )
#'
#' # 3D plot of results
#' library(plot3D)
#' opar = par(mfrow=c(1, 3))
#'
#' scatter3D(
#'   x = coordinates(grid)[, 1],
#'   y = coordinates(grid)[, 2],
#'   z = coordinates(grid)[, 3],
#'   colvar = rad$direct / 1000,
#'   pch = 20,
#'   cex = 1,
#'   clab = c("Rad\n(kWh m^-2 yr^-1)"),
#'   main = "Direct"
#' )
#'
#' scatter3D(
#'   x = coordinates(grid)[, 1],
#'   y = coordinates(grid)[, 2],
#'   z = coordinates(grid)[, 3],
#'   colvar = rad$diffuse / 1000,
#'   pch = 20,
#'   cex = 1,
#'   clab = c("Rad\n(kWh m^-2 yr^-1))"),
#'   main = "Diffuse"
#' )
#'
#' scatter3D(
#'   x = coordinates(grid)[, 1],
#'   y = coordinates(grid)[, 2],
#'   z = coordinates(grid)[, 3],
#'   colvar = rad$total / 1000,
#'   pch = 20,
#'   cex = 1,
#'   clab = c("Rad\n(kWh m^-2 yr^-1))"),
#'   main = "Total"
#' )
#'
#' par(opar)
#'}


radiation = function(
  grid,
  obstacles,
  obstacles_height_field,
  solar_pos,
  solar_normal,
  solar_diffuse,
  parallel = getOption("mc.cores")
) {

  # Check inputs
  # .checkObstacles(obstacles, obstacles_height_field) ### Alread checked by 'inShadow'
  # .checkSolarPos(solar_pos, length1 = FALSE) ### Alread checked by 'inShadow'
  .checkGrid(grid)
  .checkSolarRad(solar_normal, solar_diffuse, solar_pos)

  # Determine shadow
  s = inShadow(
    location = grid,
    obstacles = obstacles,
    obstacles_height_field = obstacles_height_field,
    solar_pos = solar_pos,
    parallel = parallel
  )

  # Calculate 'direct' radiation coefficient
  coef = coefDirect(
    type = grid$type,
    facade_az = grid$facade_az,
    solar_pos = solar_pos
  )

  # Multiply by 'direct' radiation load
  direct = sweep(
    x = coef,
    MARGIN = 2,
    STATS = solar_normal,
    FUN = "*"
  )

  # Sum 'direct' radiation
  direct_sum = rowSums(direct)

  # Calculate 'SVF'
  grid$svf = SVF(
    location = grid,
    obstacles = rishon,
    obstacles_height_field = "BLDG_HT",
    parallel = parallel
  )

  # Calculate 'diffuse' radiation
  diffuse = outer(grid$svf, solar_diffuse)

  # Sum 'diffuse' radiation
  diffuse_sum = rowSums(diffuse)

  # Sum 'direct' + 'diffuse'
  total_sum = direct_sum + diffuse_sum

  # Return result
  data.frame(
    svf = grid$svf,
    direct = direct_sum,
    diffuse = diffuse_sum,
    total = total_sum
  )

}