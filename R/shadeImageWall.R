
#' Title
#'
#' @param seg_height_field
#' @param build
#' @param build_height_field
#' @param solar_pos
#' @param sample_dist
#' @param shift_dist
#' @param messages
#'
#' @return
#' @export
#'
#' @examples
#'
#' # data(build)
#' time = as.POSIXct("2004-12-24 12:30:00", tz = "Asia/Jerusalem")
#' solar_pos = maptools::solarpos(
#'   matrix(c(34.7767978098526, 31.9665936050395), ncol = 2),
#'   time
#' )
#' seg = shadow::toSeg(build[2, ])[5, ]
#'
#' plot(build)
#' plot(seg, add = TRUE, col = "red")
#'
#' seg = seg
#'   seg_height_field = "BLDG_HT"
#'   build = build
#'   build_height_field = "BLDG_HT"
#'   solar_pos = solar_pos
#'   sample_dist = 1
#'   shift_dist = 0.01
#'
#' shadeImageWall(
#'   seg,
#'   seg_height_field = "BLDG_HT",
#'   build = build,
#'   build_height_field = "BLDG_HT",
#'   solar_pos = solar_pos,
#'   sample_dist = 1,
#'   shift_dist = 0.01
#' )


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

  # Shift walls
  seg_az = shadow::classifyAz(seg)$az
  seg_shifted = shadow::shiftAz(seg, az = seg_az, dist = shift_dist)

  result = NULL

  for(p in 1:nrow(solar_pos)) {

    # Create new field to hold wall shade proportion values
    # shade_prop = rep(NA, length(seg))

    # i = 1

    # For each wall...
    # for(i in 1:length(seg_shifted)) {

      # Sample points along given wall
      wall_sample =
        sp::spsample(
          seg_shifted,
          n = round(rgeos::gLength(seg) / sample_dist),
          type = "regular"
        )

      # Distance along wall (from point furthest clockwise)
      d = rgeos::gDistance(wall_sample[length(wall_sample), ], wall_sample, byid = TRUE)[, 1]
      wall_sample = SpatialPointsDataFrame(wall_sample, data.frame(width = d))

      # Calculate shade height for each point
      wall_sample$shade_height = NA

      # For each point along wall...
      for(j in 1:length(wall_sample)) {

        wall_sample$shade_height[j] = shadow::shadeHeight(
          location = wall_sample[j, ],
          build = build,
          height_field = build_height_field,
          solar_pos = solar_pos,
          messages = FALSE
        )

      }

      # If shade height is above wall height reduce back to wall height
      # shade_heights[shade_heights > seg@data[i, build_height_field]] =
      #   seg@data[i, build_height_field]

      # Calculate wall shade proportion
      # shade_prop[i] = mean(shade_heights / seg@data[i, build_height_field])

      # Reorder from lefth to right
      wall_sample = wall_sample[order(wall_sample$width), ]

      template = expand.grid(
        width = wall_sample$width,
        height_upper = seq(1, max(seg@data[, seg_height_field]), 1)
      )
      template$height_ctr = template$height_upper - 0.5
      template$height_lower = template$height_upper - 1
      dat = plyr::join(template, wall_sample@data, "width")

      # 'NA' shade_height means no shade
      dat$shade = ifelse(is.na(dat$shade_height), FALSE, dat$shade_height > dat$height_lower)

      dat$solar_pos_row = p

      dat = dat[, c("solar_pos_row", "width", "height_upper", "height_ctr", "height_lower", "shade")]

      result = rbind(result, dat)

    }

  #   # If wall is facing away from sun then shade proportion is 1
  #   az_diff =
  #     ifelse(
  #       abs(solar_pos[p, 1] - seg_az) > 180,
  #       360 - abs(solar_pos[p, 1] - seg_az),
  #       abs(solar_pos[p, 1] - seg_az)
  #     )
  #   shade_prop[az_diff >= 90] = 1
  #
  #   # If sun below horizon or directly above shade proportion is 1
  #   shade_prop[solar_pos[p, 2] == 90 | solar_pos[p, 2] < 0] = 1
  #
  #   result[, p] = shade_prop
  #
  # }

  return(result)

}

# # Image
# tmp = dat
# z = reshape2::acast(dat, width ~ height_ctr, value.var = "shade")
# image(
#   x = tmp$width %>% unique %>% sort,
#   y = tmp$height_ctr %>% unique %>% sort,
#   z = z,
#   asp = 1, axes = FALSE, frame.plot = FALSE,
#   xlab = "Ground distance (m)", ylab = "Height (m)"
#   # zlim = c(0, max(final$rad_annual))
#   # col = cols, main = wall
# )
# rect(
#   xleft = min(tmp$width) - max(diff(sort(tmp$width)))/2,
#   ybottom = min(tmp$height_ctr) - max(diff(sort(tmp$width)))/2,
#   xright = max(tmp$width) + max(diff(sort(tmp$width)))/2,
#   ytop = max(tmp$height_ctr) + max(diff(sort(tmp$width)))/2
# )
# axis(side = 1, labels = TRUE)
# axis(side = 2, labels = TRUE)
# contour(
#   x = tmp$width %>% unique %>% sort,
#   y = tmp$height_ctr %>% unique %>% sort,
#   z,
#   add = TRUE#,
#   # levels = seq(0, max(tmp$rad_annual), length.out = 10)
# )


