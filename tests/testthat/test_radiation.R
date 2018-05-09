library(shadow)

context("radiation")

test_that("radiation calculation is correct", {
  expect_equal({

    data(rishon)
    data(tmy)

    # grid
    grid = new(
      "SpatialPointsDataFrame",
      data = structure(
        list(
          build_id = c("407", "407", "407"),
          BLDG_HT = c(21.38,
                      21.38, 21.38),
          obs_id = c(1L, 1L, 1L),
          type = c("facade", "roof",
                   "facade"),
          seg_id = c(6L, NA, 21L),
          xy_id = c(18L, NA, 50L),
          facade_az = c(321.427363755109, NA, 136.951204869441)
        ),
        .Names = c(
          "build_id",
          "BLDG_HT",
          "obs_id",
          "type",
          "seg_id",
          "xy_id",
          "facade_az"
        ),
        row.names = c(400L, 15L, 687L),
        class = "data.frame"
      )
      ,
      coords.nrs = numeric(0)
      ,
      coords = structure(
        c(
          667855.102115946,
          667862.288856074,
          667871.369943073,
          3538112.62815535,
          3538101.47268511,
          3538095.63665798,
          11,
          21.39,
          21
        ),
        .Dim = c(3L, 3L),
        .Dimnames = list(NULL, c("x1", "x2", "h"))
      )
      ,
      bbox = structure(
        c(
          667855.102115946,
          3538095.63665798,
          11,
          667871.369943073,
          3538112.62815535,
          21.39
        ),
        .Dim = c(3L, 2L),
        .Dimnames = list(c("x1", "x2", "h"), c("min", "max"))
      )
      ,
      proj4string = new("CRS", projargs = "+proj=utm +zone=36 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
    )

    radiation(
      grid = grid,
      obstacles = rishon,
      obstacles_height_field = "BLDG_HT",
      solar_pos = solar_pos[8:17, , drop = FALSE],
      solar_normal = tmy$solar_normal[8:17],
      solar_diffuse = tmy$solar_diffuse[8:17]
    )
  },

  structure(
    list(
      svf = c(0.484475008535678, 0.998328963274761,0.491760588247644),
      direct = c(21.0822885075299, 2755.36321510678,2913.69184551694),
      diffuse = c(250.473579412946, 516.136074013052,254.240224124032),
      total = c(271.555867920476, 3271.49928911984,3167.93206964097)
    ),
    .Names = c("svf", "direct", "diffuse", "total"),
    row.names = c(NA,-3L),
    class = "data.frame"
  )

)

}
)
