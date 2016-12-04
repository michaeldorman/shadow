library(shadow)

context("shadeFootprint")

test_that("Shade footprint calculation is correct", {
  expect_equal({
    data(build)
    location = rgeos::gCentroid(build)
    time = as.POSIXct("2004-12-24 13:30:00", tz = "Asia/Jerusalem")
    solar_pos = maptools::solarpos(
      matrix(c(34.7767978098526, 31.9665936050395), ncol = 2),
      time
      )
    footprint = shadeFootprint(build, "BLDG_HT", solar_pos)
    rgeos::gArea(footprint)
  },
  6513.44739346873
  )
  }
)
