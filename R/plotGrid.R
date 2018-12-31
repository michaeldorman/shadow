#' Interactive plot for 3D spatial points
#'
#' This is a wrapper around \code{scatterplot3js} from package \code{threejs}. The function adjusts the x, y and z axes so that 1:1:1 proportion are kept and z=0 corresponds to ground level.
#'
#' @param grid A three-dimensional \code{SpatialPoints*} object
#' @param color Point color, either a single value or vector corresponding to the number of points. The default values draws "facade" and "roof" points in different colors, assuming these classes appear in a column named \code{type}, as returned by function \code{\link{surfaceGrid}}
#' @param size Point radius, default is \code{0.1}
#' @param ... Additional parameters passed to \code{scatterplot3js}
#'
#' @return An htmlwidget object that is displayed using the object's show or print method. If you don't see your widget plot, try printing it with the print function. (Same as for \code{threejs::scatterplot3js})
#'
#' @examples
#' \dontrun{
#' grid = surfaceGrid(
#'   obstacles = build,
#'   obstacles_height_field = "BLDG_HT",
#'   res = 1,
#'   offset = 0.01
#' )
#' plotGrid(grid)
#' }
#'
#' @export

plotGrid = function(grid, color = c("grey", "red")[as.factor(grid$type)], size = 0.2, ...) {

  # Check 'threejs' package availability
  if(!requireNamespace("threejs"))
    stop("You need to install the 'threejs' package to be able to use this function")

  # Check if 'pnt' is '3D point' object
  if(!class(grid) %in% c("SpatialPoints", "SpatialPointsDataFrame"))
    stop("'grid' is not 'SpatialPoints*'")
  if(dimensions(grid) != 3)
    stop("'grid' is not three-dimensional")

  # Get 3D point coordinates
  coords = coordinates(grid)

  # Determine X, Y and Z range
  x_range = range(coords[, 1])
  y_range = range(coords[, 2])
  z_range = range(coords[, 3])
  x_range_diff = diff(x_range)
  y_range_diff = diff(y_range)
  z_range_diff = diff(z_range)
  x_mid = mean(x_range)
  y_mid = mean(y_range)
  z_mid = mean(z_range)
  max_range = max(c(x_range_diff, y_range_diff, z_range_diff))

  # Plot
  threejs::scatterplot3js(
    x = coords[, 1],
    y = coords[, 2],
    z = coords[, 3],
    xlim = c(x_mid - max_range / 2, x_mid + max_range / 2),
    ylim = c(y_mid - max_range / 2, y_mid + max_range / 2),
    zlim = c(0, z_mid + max_range),
    num.ticks = c(6, 6, 6),
    x.ticklabs = rep("", 6),
    y.ticklabs = rep("", 6),
    color = color,
    size = size,
    ...
    )

}

