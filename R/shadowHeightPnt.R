.shadowHeightPnt = function(
  location,
  # surface,
  obstacles,
  obstacles_height_field,
  solar_pos,
  obstacles_outline,
  b = 0.01
) {

  # If sun above the horizon
  if(solar_pos[, 2] <= 0) shade_height = Inf else { # There is sunlight

    # Sun position
    sun = .sunLocation(
      location = location,
      sun_az = solar_pos[, 1],
      sun_elev = solar_pos[, 2]
    )

    # 'Line of sight' between sun and grid point
    sun_ray = shadow::ray(from = location, to = sun)

    # Intersections with buildings outline
    inter = rgeos::gIntersection(obstacles_outline, sun_ray)

    # No intersections means there is no shade
    if(is.null(inter)) shade_height = NA else {

      # If some of the intersections are lines
      if(class(inter) == "SpatialCollections") {

        lin = inter@lineobj
        inter = inter@pointobj

        for(lin_i in 1:length(lin)) {

          lin_pnt = lin[lin_i, ]
          lin_pnt = sp::coordinates(lin_pnt)[[1]][[1]]
          lin_pnt = sp::SpatialPoints(
            lin_pnt,
            proj4string = sp::CRS(sp::proj4string(inter))
          )
          inter = sp::rbind.SpatialPoints(inter, lin_pnt)

        }

      }

      # Set row names
      row.names(inter) = 1:length(inter)

      # Extract building data for each intersection
      inter =
        SpatialPointsDataFrame(
          inter,
          sp::over(
            inter,
            rgeos::gBuffer(
              obstacles_outline[, obstacles_height_field],
              byid = TRUE,
              width = b
            ),
            fn = max)
        )

      # Distance between examined location and intersections
      inter$dist = rgeos::gDistance(inter, location, byid = TRUE)[1, ]

      # Shade height calculation
      inter$shade_fall = inter$dist * tan(deg2rad(solar_pos[, 2]))
      inter$shade_height =
        inter@data[, obstacles_height_field] - inter$shade_fall
      shade_height = max(inter$shade_height)

      # Assign NA when there is no shade

      # (1) If point is on a building & shade_height < building_height
      if(rgeos::gIntersects(location, obstacles)) {
        build_height = sp::over(location, obstacles)[, obstacles_height_field]
        if(shade_height <= build_height)
          shade_height = NA
      } else # (2) If point is on ground & shade_height < 0
        if(shade_height <= 0) shade_height = NA

    }

  }

  return(shade_height)

}

