#' Create an image of wall shading
#'
#' The function divides a given wall to a grid of rectangular 'cells', then finds whether each cell is shaded (at least partially) or not, by repeatedly calling \code{shadeHeight} on regular sample points along the wall facade.
#'
#' @param seg A \code{SpatialLinesDataFrame} object representing the wall footprint
#' @param seg_height_field The name of the column with wall height in \code{seg}
#' @param build A \code{SpatialPolygonsDataFrame} object specifying the buildings outline.
#' @param build_height_field The name of the column with building height in \code{build}
#' @param solar_pos A matrix with two columns: solar azimuth (in degrees from North), and elevation
#' @param sample_dist Horizontal sampling distance of \code{seg} for creating the shade image
#' @param shift_dist The distance for shifting the examined locations away from wall to avoid self-shading. Default is 1 cm.
#' @param messages Whether a message regarding distance units of the CRS should be displayed.
#'
#' @return A \code{data.frame} representing the shade image of \code{seg}, with the following columns:\itemize{
#' \item{\code{solar_pos_row} The corresponding sun position (i.e. the respective row in the \code{solar_pos} matrix}
#' \item{\code{width} The horizontal distance along wall facade, from left to right, with 0 representing the left 'side' of the wall (when the viewer stands in front of it)}
#' \item{\code{height_upper}, \code{height_ctr}, \code{height_lower}}
#' }
#'
#' @export
#'
#' @examples
#'
#' data(build)
#' time = as.POSIXct("2004-12-24 12:30:00", tz = "Asia/Jerusalem")
#' solar_pos = maptools::solarpos(
#'   matrix(c(34.7767978098526, 31.9665936050395), ncol = 2),
#'   time
#' )
#' seg = shadow::toSeg(build[2, ])[5, ]
#'
#' # Show wall position on a map
#' plot(build)
#' plot(seg, add = TRUE, col = "red", lwd = 3)
#'
#' # Calculate wall 'image'
#' img = shadeImageWall(
#'   seg = seg,
#'   seg_height_field = "BLDG_HT",
#'   build = build,
#'   build_height_field = "BLDG_HT",
#'   solar_pos = solar_pos,
#'   sample_dist = 1,
#'   shift_dist = 0.01
#' )
#'
#' # Plot wall image
#' z = reshape2::acast(img, width ~ height_ctr, value.var = "shade")
#' image(
#'   x = sort(unique(tmp$width)),
#'   y = sort(unique(tmp$height_ctr)),
#'   z = z,
#'   asp = 1, axes = FALSE, frame.plot = FALSE,
#'   xlab = "Ground distance (m)", ylab = "Height (m)"
#' )
#' rect(
#'   xleft = min(tmp$width) - max(diff(sort(tmp$width)))/2,
#'   ybottom = min(tmp$height_ctr) - max(diff(sort(tmp$width)))/2,
#'   xright = max(tmp$width) + max(diff(sort(tmp$width)))/2,
#'   ytop = max(tmp$height_ctr) + max(diff(sort(tmp$width)))/2
#' )
#' axis(side = 1, labels = TRUE)
#' axis(side = 2, labels = TRUE)


shadeImageWall = function(
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

  # Shift wall
  seg_az = shadow::classifyAz(seg)$az
  seg_shifted = shadow::shiftAz(seg, az = seg_az, dist = shift_dist)

  # Sample points along given wall
  wall_sample =
    sp::spsample(
      seg_shifted,
      n = round(rgeos::gLength(seg) / sample_dist),
      type = "regular"
    )

  # Coordinate of 'left' corner of wall
  corner = as(seg, "SpatialPoints")
  corner = corner[length(corner), ]

  # Distance along wall (from point furthest clockwise)
  d = rgeos::gDistance(
    corner,
    wall_sample, byid = TRUE
    )[, 1]
  wall_sample = SpatialPointsDataFrame(wall_sample, data.frame(width = d))

  # Reorder from lefth to right
  wall_sample = wall_sample[order(wall_sample$width), ]

  result = NULL

  for(p in 1:nrow(solar_pos)) {

    # Calculate shade height for each point
    wall_sample$shade_height = NA

    # For each point along wall...
    for(j in 1:length(wall_sample)) {

      wall_sample$shade_height[j] = shadow::shadeHeight(
        location = wall_sample[j, ],
        build = build,
        height_field = build_height_field,
        solar_pos = solar_pos[p, , drop = FALSE],
        messages = FALSE
      )

    }

    template = expand.grid(
      width = wall_sample$width,
      height_upper = seq(1, max(seg@data[, seg_height_field]), 1)
    )
    template$height_ctr = template$height_upper - 0.5
    template$height_lower = template$height_upper - 1
    dat = merge(template, wall_sample@data, "width", all.x = TRUE, sort = FALSE)

    # 'NA' shade_height means no shade
    dat$shade = ifelse(
      is.na(dat$shade_height),
      FALSE,
      dat$shade_height > dat$height_lower
      )

    dat$solar_pos_row = p

    dat = dat[, c("solar_pos_row", "width", "height_ctr", "shade")]

    result = rbind(result, dat)

    }

  return(result)

  }





