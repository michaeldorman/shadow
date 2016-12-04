library(shadow)

context("SVF")

test_that("SVF calculation is correct", {
  expect_equal({
    data(build)
    location0 = rgeos::gCentroid(build)
    svf = SVF(location0, build, "BLDG_HT")
  },
  0.439093305222458
)
  expect_equal({
    data(build)
    location0 = rgeos::gCentroid(build)
    location1 = raster::shift(location0, 0, -15)
    SVF(location1, build, "BLDG_HT")
  },
  0.189066910951481
  )
  expect_equal({
    data(build)
    location0 = rgeos::gCentroid(build)
    location2 = raster::shift(location0, -10, 20)
    SVF(location2, build, "BLDG_HT")
  },
  0.770651683412361
  )
}
)
