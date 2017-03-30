library(shadow)

context("shadowFootprint")

test_that("Shade footprint calculation is correct", {
  expect_equal({
    data(rishon)
    location = rgeos::gCentroid(rishon)
    time = as.POSIXct("2004-12-24 13:30:00", tz = "Asia/Jerusalem")
    solar_pos = maptools::solarpos(
      matrix(c(34.7767978098526, 31.9665936050395), ncol = 2),
      time
      )
    footprint = shadowFootprint(
      obstacles = rishon,
      obstacles_height_field = "BLDG_HT",
      solar_pos = solar_pos
      )
    rgeos::gArea(footprint)
  },
  6513.44739346873
  )
  }
)
