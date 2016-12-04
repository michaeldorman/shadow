library(shadow)

context("shadeHeight")

test_that("Shade height calculation is correct", {
  expect_equal({
    data(build)
    location = rgeos::gCentroid(build)
    solar_pos = matrix(
      c(208.733303840646, 241.006416412884, 262.037856636259,
        28.7994405393304, 1.81958332207186, -34.5606455413366),
      ncol = 2)
    shadeHeight(location, build, "BLDG_HT", solar_pos)
  },
  c(19.8645079285858, 22.3721120166259, Inf) # Vector of sun positions
)
  expect_equal({
    data(build)
    location = rgeos::gCentroid(build)
    solar_pos = matrix(c(343.665362102935, -81.0986528138936), ncol = 2)
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
