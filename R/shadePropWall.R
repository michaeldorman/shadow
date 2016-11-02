#' Shaded proportion of building walls
#'
#' Calculates the shaded proportion of a segment (usually representing a building wall),
#' given the segment vertical height as well as the outlines and heights of obstacles
#' (usually buildings).
#'
#' @param seg A \code{SpatialLinesDataFrame} object where each feature is a single segment representing a wall.
#' @param seg_height_field The name of the column with wall height in \code{seg}
#' @param build A \code{SpatialPolygonsDataFrame} object specifying the buildings outline
#' @param height_field The name of the column with building height in \code{build}
#' @param sun_az Sun azimuth, in decimal degrees.
#' @param sun_elev Sun elevation, in decimal degrees.
#' @param shift_dist The distance for shifting the examined locations away from wall to avoid self-shading. Default is 1 cm.
#'
#' @return Proportion of shaded area of the given segment.
#'
#' @examples
#' data(build)
#' seg = shadow::toSeg(build[2, ])
#'
#' # Calculate shade proportion for walls of 2nd feature from 'build'
#' props = shadePropWall(
#'   seg = seg,
#'   seg_height_field = "BLDG_HT",
#'   build = build,
#'   build_height_field = "BLDG_HT",
#'   sun_az = 330,
#'   sun_elev = 10,
#'   sample_dist = 1,
#'   shift_dist = 0.01
#' )
#'
#' # Plot
#' cols = grey(seq(1, 0.5, length.out = 100))
#' props = round(props, 2) * 100
#' plot(build)
#' plot(seg, add = TRUE, col = cols[props], lwd = 2)
#' raster::text(rgeos::gCentroid(seg, byid = TRUE), props, cex = 0.75)
#'
#' @export

shadePropWall = function(
  seg,
  seg_height_field,
  build,
  build_height_field,
  sun_az,
  sun_elev,
  sample_dist = 1,
  shift_dist = 0.01
  ) {

  # Check input classes
  if(class(seg) != "SpatialLinesDataFrame")
    stop("'seg' is not 'SpatialLinesDataFrame'")
  if(class(build) != "SpatialPolygonsDataFrame")
    stop("'build' is not 'SpatialPolygonsDataFrame'")

  # Check that height fields exist
  if(!seg_height_field %in% names(seg))
    stop("'seg_height_field' not found in attribute table of 'seg'")
  if(!build_height_field %in% names(build))
    stop("'seg_height_field' not found in attribute table of 'seg'")

  # Check that 'sun_az' and 'sun_elev' are of length 1
  if(length(sun_az) != 1 | !is.numeric(sun_az))
    stop("'sun_az' should be a numeric vector of length 1")
  if(length(sun_elev) != 1 | !is.numeric(sun_elev))
    stop("'sun_az' should be a numeric vector of length 1")

  # Shift walls
  seg_az = classifyAz(seg)$az
  seg_shifted = shiftAz(seg, az = seg_az, dist = shift_dist)

  # Create new field to hold wall shade proportion values
  shade_prop = rep(NA, length(seg))

  # For each wall...
  for(i in 1:length(seg_shifted)) {

    # Sample points along given wall
    wall_sample =
      sp::spsample(
        seg_shifted[i, ],
        n = round(rgeos::gLength(wall) / sample_dist),
        type = "regular"
      )

    # Calculate shade height for each point
    shade_heights = rep(NA, length(wall_sample))

    # For each point along wall...
    for(j in 1:length(wall_sample)) {

      shade_heights[j] = shadeHeight(
        wall_sample[j, ],
        build,
        build_height_field,
        sun_az,
        sun_elev
      )

    }

    # If shade height is above wall height reduce back to wall height
    shade_heights[shade_heights > seg@data[i, build_height_field]] =
      seg@data[i, build_height_field]

    # Calculate wall shade proportion
    shade_prop[i] = mean(shade_heights / seg@data[i, build_height_field])

  }

  # If wall is facing away from sun then shade proportion is 1
  az_diff =
    ifelse(
      abs(sun_az - seg_az) > 180,
      360 - abs(sun_az - seg_az),
      abs(sun_az - seg_az)
    )
  shade_prop[az_diff >= 90] = 1

  # If sun below horizon or directly above shade proportion is 1
  shade_prop[sun_elev == 90 | sun_elev < 0] = 1

  return(shade_prop)

}














