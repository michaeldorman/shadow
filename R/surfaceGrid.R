#' Create grid of 3D points covering the 'facades' and 'roofs' of obstacles
#'
#' The function creates a grid of 3D points covering the given obstacles at specified resolution. Such a grid can later on be used to quantify the shaded / non-shaded proportion of the obstacles surface area.
#'
#' @param obstacles A \code{SpatialPolygonsDataFrame} object specifying the obstacles outline
#' @param obstacles_height_field Name of attribute in \code{obstacles} with extrusion height for each feature
#' @param res Required grid resolution, in CRS units.
#' @param offset Offset between grid points and facade (horizontal distance) or between grid points and roof (vertical distance).
#' @note The reason for introducing an offset is to avoid ambiguity as for whether the grid points are "inside" or "outside" of the obstacle. With an offset all grid points are "outside" of the building and thus not intersecting it. \code{offset} should be given in CRS units; default is 0.01.
#'
#' @return A 3D \code{SpatialPointsDataFrame} layer, including all attributes of the original building to whose surface each point corresponds to, as well as new attributes:\itemize{
#' \item{\code{type} Either \code{"facade"} or \code{"roof"}}
#' \item{\code{seg_id} Facade segment unique ID (only for 'facade' points)}
#' \item{\code{xy_id} Facade ground location unique ID (only for 'facade' points)}
#' \item{\code{az} The azimuth of the corresponding facade, in decimal degrees (only for 'facade' points)}
#' }
#'
#' @seealso Function \code{\link{plotGrid}} to visualize grid.
#'
#' @examples
#' grid = surfaceGrid(
#'   obstacles = rishon,
#'   obstacles_height_field = "BLDG_HT",
#'   res = 2,
#'   offset = 0.01
#' )
#'
#' @export

surfaceGrid = function(obstacles, obstacles_height_field, res, offset = 0.01) {

  #######################################
  # Facade sample points

  # Obstacle ID
  obstacles$obs_id = 1:nrow(obstacles)

  # Obstacles outline to 'lines' *** DEPENDS ON PACKAGE 'sp' ***
  obstacles_outline = as(obstacles, "SpatialLinesDataFrame")

  # Obstacles outline union
  obstacles_outline = rgeos::gLineMerge(obstacles_outline)

  # Point sample along facades
  facade_sample =
    spsample(
      obstacles_outline,
      n = round(rgeos::gLength(obstacles_outline)) / res,
      type = "regular"
    )

  # Obstacles outline to segments
  seg = toSeg(obstacles)
  seg$az = classifyAz(seg)
  seg$seg_id = 1:nrow(seg)

  # Segments buffer
  seg_b = rgeos::gBuffer(seg, width = offset, byid = TRUE)

  # Classify 'az' per facade sample point
  facade_sample =
    SpatialPointsDataFrame(
      facade_sample,
      over(facade_sample, seg_b)
    )

  # Shift facade sample points 'away' from facades
  facade_sample = shiftAz(facade_sample, az = facade_sample$az, dist = offset)

  facade_pnt = list()
  for(i in unique(facade_sample$obs_id)) {
    tmp = facade_sample[facade_sample$obs_id == i, ]
    tmp$xy_id = 1:nrow(tmp)
    sampled_heights = seq(
      from = res / 2,
      to = obstacles[[obstacles_height_field]][obstacles$obs_id == i],
      by = res
    )
    for(h in sampled_heights) {
      x = SpatialPointsDataFrame(
        tmp,
        data = cbind(
          tmp@data,
          height = h
        )
      )
      x$xy_id = 1:length(x)
      facade_pnt = c(facade_pnt, x)
    }
  }
  facade_pnt = do.call(rbind, facade_pnt)

  # Rearrange columns
  facade_pnt$type = "facade"
  pnt_names = c("type", "seg_id", "xy_id", "az", "height")
  other_names = setdiff(names(facade_pnt), pnt_names)
  facade_pnt = facade_pnt[, c(pnt_names, other_names)]

  # # Shift segments 'away' from obstacles
  # seg_shifted = list()
  # for(i in 1:nrow(seg)) {
  #   seg_shifted[[i]] = shiftAz(seg[i, ], az = seg$az[i], dist = offset)
  # }
  # seg_shifted = mapply(spChFIDs, seg_shifted, row.names(seg))
  # seg_shifted = do.call(rbind, seg_shifted)
  #
  # # Calculate sample points
  # facade_pnt = list()
  # for(i in 1:nrow(seg_shifted)) {
  #   facade = seg_shifted[i, ]
  #   facade$seg_id = i
  #   facade_sample =
  #     spsample(
  #       facade,
  #       n = round(rgeos::gLength(facade)) / res,
  #       type = "regular"
  #     )
  #   sampled_heights = seq(
  #     from = res / 2,
  #     to = facade[[obstacles_height_field]],
  #     by = res
  #     )
  #   for(h in sampled_heights) {
  #     x = SpatialPointsDataFrame(
  #       facade_sample,
  #       data = cbind(
  #         facade@data,
  #         height = h
  #       )[rep(1, length(facade_sample)), ]
  #     )
  #     x$xy_id = 1:length(x)
  #     facade_pnt = c(facade_pnt, x)
  #   }
  # }
  # facade_pnt = do.call(rbind, facade_pnt)
  #
  # # Rearrange columns
  # facade_pnt$type = "facade"
  # pnt_names = c("type", "seg_id", "xy_id", "az", "height")
  # other_names = setdiff(names(facade_pnt), pnt_names)
  # facade_pnt = facade_pnt[, c(pnt_names, other_names)]

  #######################################
  # Roof sample points

  roof_pnt = spsample(obstacles, cellsize = res, type = "regular")
  roof_pnt = SpatialPointsDataFrame(
    roof_pnt,
    data.frame(type = rep("roof", length(roof_pnt)), stringsAsFactors = FALSE)
  )
  roof_pnt$seg_id = NA
  roof_pnt$xy_id = NA
  roof_pnt$az = NA
  roof_pnt@data = cbind(roof_pnt@data, over(roof_pnt, obstacles))
  roof_pnt$height = roof_pnt[[obstacles_height_field]] + offset

  # Rearrange columns
  facade_pnt = facade_pnt[, c(pnt_names, other_names)]

  # Combine
  combined_pnt = rbind(roof_pnt, facade_pnt)

  #######################################
  # Remove points encompassed "within" obstacle volume
  max_height = over(combined_pnt, obstacles[, obstacles_height_field], fn = max)
  max_height = max_height[[obstacles_height_field]]
  max_height[is.na(max_height)] = 0 # For points which do not intersect with 'obstacles'
  external_point = combined_pnt$height > max_height
  combined_pnt = combined_pnt[external_point, ]

  #######################################
  # To 3D
  coords = coordinates(combined_pnt)
  coords = cbind(coords, h = combined_pnt$height)
  combined_pnt = SpatialPointsDataFrame(
    coords = coords,
    data = combined_pnt@data,
    proj4string = CRS(proj4string(obstacles))
  )

  return(combined_pnt)

}




















