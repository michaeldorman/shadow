library(shadow)

context("shadowHeight")

test_that("Shade height calculation is correct", {
  expect_equal({
    data(build)
#    location = rgeos::gCentroid(build)
    location = as(sf::st_geometry(sf::st_centroid(sf::st_union(sf::st_as_sf(build)))), "Spatial")
    solar_pos = matrix(
      c(208.733303840646, 241.006416412884, 262.037856636259,
        28.7994405393304, 1.81958332207186, -34.5606455413366),
      ncol = 2)
    shadowHeight(
      location = location,
      obstacles = build,
      obstacles_height_field = "BLDG_HT",
      solar_pos = solar_pos
      )
  },
  matrix(
    c(19.8645079285858, 22.3721120166259, Inf), # Matrix of sun positions
    nrow = 1,
    ncol = 3
    )
)

  expect_equal({
    data(build)
#    location = rgeos::gCentroid(build)
    location = as(sf::st_geometry(sf::st_centroid(sf::st_union(sf::st_as_sf(build)))), "Spatial")
    solar_pos = matrix(c(343.665362102935, -81.0986528138936), ncol = 2)
    shadowHeight(
      location = location,
      obstacles = build,
      obstacles_height_field = "BLDG_HT",
      solar_pos = solar_pos
      )
  },
  matrix(Inf, nrow = 1, ncol = 1) # Night = Infinite shade height at night
  )
  expect_equal({
    data(build)
#    location = rgeos::gCentroid(build)
    location = as(sf::st_geometry(sf::st_centroid(sf::st_union(sf::st_as_sf(build)))), "Spatial")
    solar_pos = matrix(c(0, 80), ncol = 2)
    shadowHeight(
      location = location,
      obstacles = build,
      obstacles_height_field = "BLDG_HT",
      solar_pos = solar_pos
      )
  },
  matrix(NA, nrow = 1, ncol = 1) # No shade at noon
  )
}
)
