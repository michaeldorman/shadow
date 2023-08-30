library(shadow)

context("SVF")

test_that("SVF calculation is correct", {
  expect_equal({
    data(build)
#    location0 = rgeos::gCentroid(build)
    location0 = as(sf::st_geometry(sf::st_centroid(sf::st_union(sf::st_as_sf(build)))), "Spatial")
    SVF(
      location = location0,
      obstacles = build,
      obstacles_height_field = "BLDG_HT"
      )
  },
  0.39597205126485
)
  expect_equal({
    data(build)
#    location0 = rgeos::gCentroid(build)
    location0 = as(sf::st_geometry(sf::st_centroid(sf::st_union(sf::st_as_sf(build)))), "Spatial")
    location1 = raster::shift(location0, 0, -15)
    SVF(
      location = location1,
      obstacles = build,
      obstacles_height_field = "BLDG_HT"
    )
  },
  0.139491688314422
  )
  expect_equal({
    data(build)
#    location0 = rgeos::gCentroid(build)
    location0 = as(sf::st_geometry(sf::st_centroid(sf::st_union(sf::st_as_sf(build)))), "Spatial")
    location2 = raster::shift(location0, -10, 20)
    SVF(
      location = location2,
      obstacles = build,
      obstacles_height_field = "BLDG_HT"
    )
  },
  0.770505253636759
  )
}
)
