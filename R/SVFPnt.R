.SVFPnt = function(
  location,
  obstacles,
  obstacles_height_field,
  res_angle = 5,
  b = 0.01
) {

  # Create rays
  angles = seq(0, 359.9999, res_angle)
  sun = mapply(
    .sunLocation,
    sun_az = angles,
    MoreArgs = list(
      location = location,
      sun_elev = 0
    )
  )
  rays = mapply(shadow::ray, MoreArgs = list(from = location), to = sun)
  rays$makeUniqueIDs = TRUE
  rays = do.call(sp::rbind.SpatialLines, rays)

  # Buildings outline to 'lines' *** DEPENDS ON PACKAGE 'sp' ***
  obstacles_outline = as(obstacles, "SpatialLinesDataFrame")

  # If point is on a building then SVF='NA'
  if(rgeos::gIntersects(location, obstacles)) svf_final = NA else {

    svf = rep(NA, length(rays))

    for(i in 1:length(rays)) {

      # 'Line of sight' between sun and location
      ray1 = rays[i, ]

      # Intersections with buildings outline
      inter = rgeos::gIntersection(obstacles_outline, ray1)

      # No intersections means SVF=1
      if(is.null(inter)) svf[i] = 1 else {

        # If some of the intersections are lines
        if(class(inter) == "SpatialCollections") {

          lin = inter@lineobj
          inter = inter@pointobj

          for(lin_i in 1:length(lin)) {

            lin_pnt = lin[lin_i, ]
            lin_pnt = coordinates(lin_pnt)[[1]][[1]]
            lin_pnt = sp::SpatialPoints(
              lin_pnt,
              proj4string = CRS(proj4string(inter))
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
                obstacles_outline,
                byid = TRUE,
                width = b
              ),
              fn = max)
          )

        # Distance between examined location and intersections
        inter$dist = rgeos::gDistance(inter, location, byid = TRUE)[1, ]

        # Maximal angle of obstruction calculation
        inter$angle = rad2deg(
          atan(inter@data[, obstacles_height_field] / inter$dist)
        )
        inter$svf = 1 - inter$angle / 90
        svf[i] = min(inter$svf)

      }

    }

    svf_final = mean(svf)

  }

  return(svf_final)

}
