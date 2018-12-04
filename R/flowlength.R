#' Calculate flow length
#'
#' Calculates flow length for each pixel in a Digital Elevation Model.
#'
#' @param	elev	A numeric \code{matrix} representing a Digital Elevation Model (DEM)
#' @param	veg	A logical \code{matrix} representing vegetation presence. \code{TRUE} values represent vegetated cells where flow is absorbed (i.e. sinks), \code{FALSE} values represent cells where flow is unobstructed
#' @param	res	\code{numeric} vector of length 1, specifying DEM resolution. Default is 1, i.e. pixel size is 1*1
#' @return	A numeric \code{matrix} where each cell value is flow length, in \code{res} units
#'
#' @references
#' The algorithm is described in:
#'
#' Mayor, A. G., Bautista, S., Small, E. E., Dixon, M., & Bellot, J. (2008). Measurement of the connectivity of runoff source areas as determined by vegetation pattern and topography: A tool for assessing potential water and soil losses in drylands. Water Resources Research, 44(10).
#'
#' @examples
#' # Example from Fig. 2 in Mayor et al. 2008
#'
#' elev = rbind(
#'   c(8, 8, 8, 8, 9, 8, 9),
#'   c(7, 7, 7, 7, 9, 7, 7),
#'   c(6, 6, 6, 6, 6, 5, 7),
#'   c(4, 5, 5, 3, 5, 4, 7),
#'   c(4, 5, 4, 5, 4, 6, 5),
#'   c(3, 3, 3, 3, 2, 3, 3),
#'   c(2, 2, 2, 3, 4, 1, 3)
#' )
#' veg = rbind(
#'   c(TRUE,  TRUE,  TRUE,  TRUE,  FALSE, FALSE, TRUE),
#'   c(TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  FALSE, FALSE),
#'   c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
#'   c(FALSE, TRUE,  FALSE, FALSE, FALSE, FALSE, TRUE),
#'   c(TRUE,  TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE),
#'   c(TRUE,  TRUE,  TRUE,  FALSE, FALSE, FALSE, FALSE),
#'   c(FALSE, TRUE,  TRUE,  FALSE, FALSE, TRUE,  TRUE)
#' )
#'
#' # Calculate flow length
#' f = flowlength(elev, veg)
#'
#' # Plot
#' library(raster)
#' r = raster(f)
#' pnt = rasterToPoints(r)
#' plot(r, main = "Flow length")
#' text(pnt[, 1:2], as.character(round(pnt[, 3], 1)))
#'
#' @export

# Function to calculate 'flowlength' matrix
flowlength = function(elev, veg, res = 1) {

  # Add "padding"
  elev = rbind(rep(Inf, ncol(elev)), elev, rep(Inf, ncol(elev)))
  elev = cbind(rep(Inf, nrow(elev)), elev, rep(Inf, nrow(elev)))
  veg = rbind(rep(TRUE, ncol(veg)), veg, rep(TRUE, ncol(veg)))
  veg = cbind(rep(TRUE, nrow(veg)), veg, rep(TRUE, nrow(veg)))

  # Progress bar
  pb = utils::txtProgressBar(min = 2, max = nrow(elev)-1, style = 3)

  # Calculate 'flowlength' matrix
  result = matrix(NA, nrow = nrow(elev), ncol = ncol(elev))
  for(i in 2:(nrow(elev)-1)) {
    for(j in 2:(ncol(elev)-1)) {
      result[i, j] = flowlength1(elev, veg, c(i, j), res)
      utils::setTxtProgressBar(pb, i)
    }
  }

  # Remove "padding"
  result = result[2:(nrow(result)-1), 2:(ncol(result)-1)]

  # Return
  cat("\n")
  return(result)

}

# Function to get values of 8 neighboring matrix cells, *non-border* cells only
getNeighbors = function(m, pos) {
  v = c(
    m[pos[1]-1, pos[2]-1],
    m[pos[1]-1, pos[2]  ],
    m[pos[1]-1, pos[2]+1],
    m[pos[1],   pos[2]-1],
    m[pos[1],   pos[2]+1],
    m[pos[1]+1, pos[2]-1],
    m[pos[1]+1, pos[2]  ],
    m[pos[1]+1, pos[2]+1]
  )
  return(v)
}

# Function to calculate 'flowlength' for one cell, *non-border* cells only
flowlength1 = function(elev, veg, pos, res) {

  # Check
  stopifnot(all(dim(elev) == dim(veg)))

  # Distances
  dists = c(
    sqrt(2), 1, sqrt(2),
    1,          1,
    sqrt(2), 1, sqrt(2)
  )
  dists = dists * res

  # Flow length counter
  fl = 0

  # Visited cells
  visited = matrix(FALSE, nrow = nrow(elev), ncol = ncol(elev))
  visited[c(1, nrow(visited)), ] = TRUE
  visited[, c(1, ncol(visited))] = TRUE

  while(TRUE) {

    # Break if visited or vegetated
    if(visited[pos[1], pos[2]]) break
    if(veg[pos[1], pos[2]]) break

    # Mark current cell
    visited[pos[1], pos[2]] = TRUE

    # Find neighbors
    nElev = getNeighbors(elev, pos)
    nVeg = getNeighbors(veg, pos)

    # Find drops
    drops = elev[pos[1], pos[2]] - nElev
    dropsNorm = drops / dists
    dropsNorm[dropsNorm < 0] = NA

    # Break if nowhere to go
    if(all(is.na(dropsNorm))) break

    # Pick maximal & non-vegetated drop
    dropsNorm[dropsNorm != max(dropsNorm, na.rm = TRUE)] = NA

    # Places to go to
    goto = length(dropsNorm[!is.na(dropsNorm)])

    # Randomly choose one in case of ties
    if(goto > 1) {
      toDelete = sample(which(!is.na(dropsNorm)), goto - 1)
      dropsNorm[toDelete] = NA
    }

    # Change position to next cell
    i = which(!is.na(dropsNorm))
    if(i == 1) {pos[1] = pos[1]-1; pos[2] = pos[2]-1}
    if(i == 2) {pos[1] = pos[1]-1}
    if(i == 3) {pos[1] = pos[1]-1; pos[2] = pos[2]+1}
    if(i == 4) {pos[2] = pos[2]-1}
    if(i == 5) {pos[2] = pos[2]+1}
    if(i == 6) {pos[1] = pos[1]+1; pos[2] = pos[2]-1}
    if(i == 7) {pos[1] = pos[1]+1}
    if(i == 8) {pos[1] = pos[1]+1; pos[2] = pos[2]+1}

    # Increment length
    fl = fl + sqrt(drops[i]^2 + dists[i]^2)

  }

  return(fl)

}
