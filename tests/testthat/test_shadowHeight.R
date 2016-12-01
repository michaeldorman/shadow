library(shadow)

context("Shade height")

test_that("Shade height calculation is correct", {
  expect_equal({
    data(build)
    location = rgeos::gCentroid(build)
    location_geo = sp::spTransform(location, "+proj=longlat +datum=WGS84")
    time = as.POSIXct("2004-12-24 13:30:00", tz = "Asia/Jerusalem")
    time = seq(time, by = "3 hour", length.out = 3)
    solar_pos = maptools::solarpos(location_geo, time)
    shadeHeight(location, build, "BLDG_HT", solar_pos)
  },
  c(19.8645079285858, 22.3721120166259, Inf) # Vector of sun positions
)
  expect_equal({
    data(build)
    location = rgeos::gCentroid(build)
    location_geo = sp::spTransform(location, "+proj=longlat +datum=WGS84")
    time = as.POSIXct("2004-12-24 23:30:00", tz = "Asia/Jerusalem")
    solar_pos = maptools::solarpos(location_geo, time)
    shadeHeight(location, build, "BLDG_HT", solar_pos)
  },
  Inf # Night = Infinite shade height at night
  )
  expect_equal({
    data(build)
    location = rgeos::gCentroid(build)
    solar_pos = matrix(c(0, 80), ncol = 2)
    shadeHeight(location, build, "BLDG_HT", solar_pos)
  },
  NA # No shade at noon
  )
}
)
