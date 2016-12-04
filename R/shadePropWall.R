#' Shaded proportion of building walls
#'
#' Calculates the shaded proportion of a segment (usually representing a building wall),
#' given the segment vertical height as well as the outlines and heights of obstacles
#' (usually buildings).
#'
#' @note
#' For a correct geometric calculation, make sure that:\itemize{
#' \item{The layers \code{location} and \code{build} are projected}
#' \item{The values in \code{height_field} of \code{build} are given in the same distance units as the CRS (e.g. meters when using UTM)}
#' }
#' @param seg A \code{SpatialLinesDataFrame} object where each feature is a single segment representing a wall.
#' @param seg_height_field The name of the column with wall height in \code{seg}
#' @param build A \code{SpatialPolygonsDataFrame} object specifying the buildings outline
#' @param build_height_field The name of the column with building height in \code{build}
#' @param solar_pos A matrix with the solar azimuth (in degrees from North), and elevation
#' @param sample_dist Distance between sampling points along wall
#' @param shift_dist The distance for shifting the examined locations away from wall to avoid self-shading. Default is 1 cm.
#' @param messages Whether a message regarding distance units of the CRS should be displayed.
#'
#' @return Proportion of shaded area of the given segment.
#'
#' @examples
#' data(build)
#' time = as.POSIXct("2004-12-24 13:30:00", tz = "Asia/Jerusalem")
#' solar_pos = maptools::solarpos(
#'   matrix(c(34.7767978098526, 31.9665936050395), ncol = 2),
#'   time
#' )
#' seg = shadow::toSeg(build[2, ])[5:10, ]
#'
#' # Calculate shade proportion for walls 5-10 of 2nd building
#' props = shadePropWall(
#'   seg = seg,
#'   seg_height_field = "BLDG_HT",
#'   build = build,
#'   build_height_field = "BLDG_HT",
#'   solar_pos = solar_pos,
#'   sample_dist = 1,
#'   shift_dist = 0.01
#' )
#'
#' # Plot
#' plot(build)
#' plot(seg, add = TRUE, col = "red", lwd = 2)
#' raster::text(rgeos::gCentroid(seg, byid = TRUE), round(props, 2), cex = 0.75)
#'
#' @export

shadePropWall = function(
  seg,
  seg_height_field,
  build,
  build_height_field,
  solar_pos,
  sample_dist = 1,
  shift_dist = 0.01,
  messages = TRUE
  ) {

  # Check classes of 'seg' and 'build'
  if(class(seg) != "SpatialLinesDataFrame")
    stop("'seg' is not 'SpatialLinesDataFrame'")
  if(class(build) != "SpatialPolygonsDataFrame")
    stop("'build' is not 'SpatialPolygonsDataFrame'")

  # Check projected
  if(!sp::is.projected(seg) | !sp::is.projected(build))
    stop("'seg' and/or 'build' not in projected CRS")

  # Check that height fields exist
  if(!seg_height_field %in% names(seg))
    stop("'seg_height_field' not found in attribute table of 'seg'")
  if(!build_height_field %in% names(build))
    stop("'build_height_field' not found in attribute table of 'build'")

  # Check 'solar_pos'
  if(class(solar_pos) != "matrix")
    stop("'solar_pos' must be a 'matrix' object")
  if(ncol(solar_pos) != 2)
    stop("'solar_pos' must have exacly two columns")
  if(any(solar_pos[, 1] < 0) | any(solar_pos[, 1] > 360))
    stop("Sun azimuth should be a number in [0, 360]")
  if(any(solar_pos[, 2] < -90) | any(solar_pos[, 2] > 90))
    stop("Sun elevation should be a number in [-90, 90]")

  if(messages) {
    message(
      paste0(
        "Assuming ", build_height_field, "and ", seg_height_field, " given in ",
        gsub(" .*", "",
             gsub(".*\\+units=", "", proj4string(build))
        )
      )
    )
  }

  # Shift walls
  seg_az = shadow::classifyAz(seg)$az
  seg_shifted = shadow::shiftAz(seg, az = seg_az, dist = shift_dist)

  result = matrix(NA, nrow = length(seg_shifted), ncol = nrow(solar_pos))

  for(p in 1:nrow(solar_pos)) {

    # Create new field to hold wall shade proportion values
    shade_prop = rep(NA, length(seg))

    # For each wall...
    for(i in 1:length(seg_shifted)) {

      # Sample points along given wall
      wall_sample =
        sp::spsample(
          seg_shifted[i, ],
          n = round(rgeos::gLength(seg) / sample_dist),
          type = "regular"
        )

      # Calculate shade height for each point
      shade_heights = rep(NA, length(wall_sample))

      # For each point along wall...
      for(j in 1:length(wall_sample)) {

        shade_heights[j] = shadow::shadeHeight(
          location = wall_sample[j, ],
          build = build,
          height_field = build_height_field,
          solar_pos = solar_pos,
          messages = FALSE
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
        abs(solar_pos[p, 1] - seg_az) > 180,
        360 - abs(solar_pos[p, 1] - seg_az),
        abs(solar_pos[p, 1] - seg_az)
      )
    shade_prop[az_diff >= 90] = 1

    # If sun below horizon or directly above shade proportion is 1
    shade_prop[solar_pos[p, 2] == 90 | solar_pos[p, 2] < 0] = 1

    result[, p] = shade_prop

  }

  return(result)

}














