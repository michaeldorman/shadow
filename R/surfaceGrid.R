#' Create grid of 3D points covering the 'facades' and 'roofs' of obstacles
#'
#' The function creates a grid of 3D points covering the given obstacles at specified resolution. Such a grid can later on be used to quantify the shaded / non-shaded proportion of the obstacles surface area.
#'
#' @param obstacles A \code{SpatialPolygonsDataFrame} object specifying the obstacles outline
#' @param obstacles_height_field Name of attribute in \code{obstacles} with extrusion height for each feature
#' @param res Required grid resolution, in CRS units
#' @param offset Offset between grid points and facade (horizontal distance) or between grid points and roof (vertical distance).
#' @note The reason for introducing an offset is to avoid ambiguity as for whether the grid points are "inside" or "outside" of the obstacle. With an offset all grid points are "outside" of the building and thus not intersecting it. \code{offset} should be given in CRS units; default is 0.01.
#'
#' @return A 3D \code{SpatialPointsDataFrame} layer, including all attributes of the original obstacles each surface point corresponds to, followed by six new attributes:\itemize{
#' \item{\code{obs_id} Unique consecutive ID for each feature in \code{obstacles}}
#' \item{\code{type} Either \code{"facade"} or \code{"roof"}}
#' \item{\code{seg_id} Unique consecutive ID for each facade segment (only for 'facade' points)}
#' \item{\code{xy_id} Unique consecutive ID for each ground location (only for 'facade' points)}
#' \item{\code{facade_az} The azimuth of the corresponding facade, in decimal degrees (only for 'facade' points)}
#' }
#'
#' @seealso Function \code{\link{plotGrid}} to visualize grid.
#'
#' @examples
#' grid = surfaceGrid(
#'   obstacles = build,
#'   obstacles_height_field = "BLDG_HT",
#'   res = 2
#' )
#' plot(grid)
#' plot(grid, pch = 1, lwd = 0.1, col = "black", add = TRUE)
#'
#' # When 'res/2' is larger then height, facade will be left unsampled
#' build_small = build
#' build_small$BLDG_HT = 1
#' grid = surfaceGrid(
#'   obstacles = build_small,
#'   obstacles_height_field = "BLDG_HT",
#'   res = 2
#' )
#' plot(grid)
#' plot(grid, pch = 1, lwd = 0.1, col = "black", add = TRUE)
#' table(grid$type)
#'
#' grid = surfaceGrid(
#'   obstacles = build_small,
#'   obstacles_height_field = "BLDG_HT",
#'   res = 2.00001  # res/2 > h
#' )
#' plot(grid)
#' plot(grid, pch = 1, lwd = 0.1, col = "black", add = TRUE)
#' table(grid$type)
#'
#' # When input already contains 'obs_id', 'type', 'seg_id', 'xy_id', 'facade_az' or 'ZZZ'
#' build2 = build
#' build2$ZZZ = 1
#' grid = surfaceGrid(
#'   obstacles = build2,
#'   obstacles_height_field = "BLDG_HT",
#'   res = 2
#' )
#'
#' @export

surfaceGrid = function(obstacles, obstacles_height_field, res, offset = 0.01) {

  # Check inputs
  .checkObstacles(obstacles, obstacles_height_field)

  # Check column names
  pnt_names = c("obs_id", "type", "seg_id", "xy_id", "facade_az")
  if(any(names(obstacles) %in% pnt_names))
    stop("'obstacles' cannot contain any of the following reserved column names: 'obs_id', 'type', 'seg_id', 'xy_id' and 'facade_az'")
  if(any(names(obstacles) == "ZZZ")) {
    obstacles$ZZZ = NULL
    warning("'ZZZ' column removed from 'obstacles' since it is a reserved column name")
  }

  #######################################
  # Facade sample points

  # Obstacle ID
  obstacles$obs_id = 1:nrow(obstacles)

  # Obstacles outline to 'lines' *** DEPENDS ON PACKAGE 'sp' ***
  obstacles_outline = as(obstacles, "SpatialLinesDataFrame")

  # Obstacles outline union
  obstacles_outline = rgeos::gLineMerge(raster::disaggregate(obstacles_outline))

  # Point sample along facades
  facade_sample =
    spsample(
      obstacles_outline,
      n = round(rgeos::gLength(obstacles_outline)) / res,
      type = "regular"
    )

  # Obstacles outline to segments
  seg = toSeg(obstacles)
  seg$facade_az = classifyAz(seg)
  seg$seg_id = 1:nrow(seg)

  # Segments buffer
  seg_b = rgeos::gBuffer(seg, width = offset, byid = TRUE)

  # Classify 'facade_az' per facade sample point
  facade_sample =
    SpatialPointsDataFrame(
      facade_sample,
      over(facade_sample, seg_b)
    )

  # Shift facade sample points 'away' from facades
  facade_sample = shiftAz(facade_sample, az = facade_sample$facade_az, dist = offset)

  facade_pnt = list()
  for(i in unique(facade_sample$obs_id)) {
    tmp = facade_sample[facade_sample$obs_id == i, ]
    tmp$xy_id = 1:nrow(tmp)
    current_build_height = obstacles[[obstacles_height_field]][obstacles$obs_id == i]
    if(res / 2 <= current_build_height) {
      sampled_heights = seq(
        from = res / 2,
        to = current_build_height,
        by = res
      )
      for(h in sampled_heights) {
        x = SpatialPointsDataFrame(
          tmp,
          data = cbind(
            tmp@data,
            ZZZ = h
          )
        )
        x$xy_id = 1:length(x)
        facade_pnt = c(facade_pnt, x)
      }
    }
  }
  facade_pnt = do.call(rbind, facade_pnt)

  if(!is.null(facade_pnt)) {

    # Rearrange columns
    facade_pnt$type = "facade"
    other_names = setdiff(names(facade_pnt), pnt_names)
    facade_pnt = facade_pnt[, c(other_names, pnt_names)]

  }

  #######################################
  # Roof sample points

  roof_pnt = spsample(obstacles, cellsize = res, type = "regular")
  roof_pnt = SpatialPointsDataFrame(
    roof_pnt,
    data.frame(type = rep("roof", length(roof_pnt)), stringsAsFactors = FALSE)
  )
  roof_pnt$seg_id = NA
  roof_pnt$xy_id = NA
  roof_pnt$facade_az = NA
  roof_pnt@data = cbind(roof_pnt@data, over(roof_pnt, obstacles))
  roof_pnt$ZZZ = roof_pnt[[obstacles_height_field]] + offset

  # Rearrange columns
  other_names = setdiff(names(roof_pnt), pnt_names)
  roof_pnt = roof_pnt[, c(other_names, pnt_names)]

  # Combine
  if(!is.null(facade_pnt)) {
    combined_pnt = rbind(roof_pnt, facade_pnt)
  } else {
    combined_pnt = roof_pnt
  }

  #######################################
  # Remove points encompassed "within" obstacle volume

  max_height = over(combined_pnt, obstacles[, obstacles_height_field], fn = max)
  max_height = max_height[[obstacles_height_field]]
  max_height[is.na(max_height)] = 0  # For points which do not intersect with 'obstacles'
  external_point = combined_pnt$ZZZ > max_height
  combined_pnt = combined_pnt[external_point, ]

  #######################################
  # To 3D

  coords = coordinates(combined_pnt)
  coords = cbind(coords, h = combined_pnt$ZZZ)
  combined_pnt = SpatialPointsDataFrame(
    coords = coords,
    data = combined_pnt@data,
    proj4string = CRS(proj4string(obstacles))
  )

  # Remove 'height' attribute
  combined_pnt$ZZZ = NULL

  return(combined_pnt)

}




















