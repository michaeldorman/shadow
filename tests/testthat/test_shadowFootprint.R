library(shadow)

context("shadowFootprint")

test_that("Shade footprint calculation is correct", {
  expect_equal({
    data(build)
#    location = rgeos::gCentroid(build)
    location = as(sf::st_geometry(sf::st_centroid(sf::st_union(sf::st_as_sf(build)))), "Spatial")
    time = as.POSIXct("2004-12-24 13:30:00", tz = "Asia/Jerusalem")
    solar_pos = suntools::solarpos(
      matrix(c(34.7767978098526, 31.9665936050395), ncol = 2),
      time
      )
    footprint = shadowFootprint(
      obstacles = build,
      obstacles_height_field = "BLDG_HT",
      solar_pos = solar_pos
      )
#    rgeos::gArea(footprint)
     sum(sf::st_area(sf::st_as_sf(footprint)))
    
  },
  6513.44739346873
  )
  }
)
