library(shadow)

context("SVF")

test_that("SVF calculation is correct", {
  expect_equal({
    data(rishon)
    location0 = rgeos::gCentroid(rishon)
    SVF(
      location = location0,
      obstacles = rishon,
      obstacles_height_field = "BLDG_HT"
      )
  },
  0.439093305222458
)
  expect_equal({
    data(rishon)
    location0 = rgeos::gCentroid(rishon)
    location1 = raster::shift(location0, 0, -15)
    SVF(
      location = location1,
      obstacles = rishon,
      obstacles_height_field = "BLDG_HT"
    )
  },
  0.189066910951481
  )
  expect_equal({
    data(rishon)
    location0 = rgeos::gCentroid(rishon)
    location2 = raster::shift(location0, -10, 20)
    SVF(
      location = location2,
      obstacles = rishon,
      obstacles_height_field = "BLDG_HT"
    )
  },
  0.770651683412361
  )
}
)
