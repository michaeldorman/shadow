library(shadow)

context("Shade height")

test_that("Shade height calculation is correct", {
  expect_equal({
    data(build)
    location = rgeos::gCentroid(build)
    location_geo = sp::spTransform(location, "+proj=longlat +datum=WGS84")
    time = as.POSIXct("2004-12-24 13:30:00", tz = "Asia/Jerusalem")
    solar_position = maptools::solarpos(location_geo, time)
    sun_az = solar_position[1, 1]
    sun_elev = solar_position[1, 2]
    shadeHeight(location, build, "BLDG_HT", sun_az, sun_elev)
  },
  19.8645079285858
)
  expect_equal({
    data(build)
    location = rgeos::gCentroid(build)
    location_geo = sp::spTransform(location, "+proj=longlat +datum=WGS84")
    time = as.POSIXct("2004-12-24 23:30:00", tz = "Asia/Jerusalem")
    solar_position = maptools::solarpos(location_geo, time)
    sun_az = solar_position[1, 1]
    sun_elev = solar_position[1, 2]
    shadeHeight(location, build, "BLDG_HT", sun_az, sun_elev)
  },
  Inf # Night = Infinite shade height
  )
  expect_equal({
    data(build)
    location = rgeos::gCentroid(build)
    location_geo = sp::spTransform(location, "+proj=longlat +datum=WGS84")
    sun_az = 0
    sun_elev = 90
    shadeHeight(location, build, "BLDG_HT", sun_az, sun_elev)
  },
  NA # Noon = no shade
  )
}
)
