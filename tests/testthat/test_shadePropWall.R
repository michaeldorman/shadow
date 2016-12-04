# library(shadow)
#
# context("shadePropWall")
#
# test_that("Wall shade proportion calculation is correct", {
#   expect_equal({
#     set.seed(1)
#     data(build)
#     time = as.POSIXct("2004-12-24 13:30:00", tz = "Asia/Jerusalem")
#     solar_pos = maptools::solarpos(
#       matrix(c(34.7767978098526, 31.9665936050395), ncol = 2),
#       time
#     )
#     seg = shadow::toSeg(build[2, ])[5, ]
#     props = shadePropWall(
#       seg = seg,
#       seg_height_field = "BLDG_HT",
#       build = build,
#       build_height_field = "BLDG_HT",
#       solar_pos = solar_pos,
#       sample_dist = 1,
#       shift_dist = 0.01
#     )
#   },
#   matrix(0.879576892919472, nrow = 1, ncol = 1)
#   )
#   }
# )
