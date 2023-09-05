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

  # # Add 'surface' outline to obstacles
  # surface = rgeos::gBuffer(location, width = 1000)
  # surface = SpatialPolygonsDataFrame(
  #   Sr = surface,
  #   data = data.frame(height = 0),
  #   match.ID = FALSE
  # )
  # names(surface) = obstacles_height_field
  # surface = rbind(obstacles[, obstacles_height_field], surface)

  # 'obstacles' / 'surface' outline to 'lines' *** DEPENDS ON PACKAGE 'sp' ***
  # surface_outline = as(surface, "SpatialLinesDataFrame")
  obstacles_outline = as(obstacles, "SpatialLinesDataFrame")

  if(

    # 2D mode - if point is *on* a building then 'SVF=NA'
    (
      dimensions(location) == 2 & # 2D
#        rgeos::gIntersects(location, obstacles) # Intersects with 'obstacles'
        any(unlist(sf::st_intersects(sf::st_as_sf(location), sf::st_as_sf(obstacles))))
    ) ||

    # 3D mode - if point is *inside* building then 'SVF=NA'
    dimensions(location) == 3 & ( # 3D
#      rgeos::gIntersects(location, obstacles) && # Intersects with 'obstacles'
      any(unlist(sf::st_intersects(sf::st_as_sf(location), sf::st_as_sf(obstacles)))) &&
      coordinates(location)[, 3] < obstacles[location, ]@data[, obstacles_height_field] # Location z < Obstacle h
      )
    ) svf_final = NA else {

    svf = rep(NA, length(rays))

    for(i in 1:length(rays)) {

      # 'Line of sight' between sun and location
      ray1 = rays[i, ]

      # if(dimensions(location) == 2) {

        # 2D mode - Intersections with 'obstacles' outline
#        inter = rgeos::gIntersection(obstacles_outline, ray1)
        inter = sf::st_intersection(sf::st_as_sf(obstacles_outline), sf::st_as_sf(ray1))
      # }

      # if(dimensions(location) == 3) {
      #
      #   # 3D mode - Intersections with 'surface' outline
      #   inter = rgeos::gIntersection(surface_outline, ray1)
      #
      # }

      # No intersections means 'SVF=1'
      if(nrow(inter) == 0L) svf[i] = 1 else {

        # If some of the intersections are lines then convert to points
#        if(is(inter, "SpatialCollections")) {
#          lin = inter@lineobj
#          inter = inter@pointobj
#          for(lin_i in 1:length(lin)) {
#            lin_pnt = lin[lin_i, ]
#            lin_pnt = coordinates(lin_pnt)[[1]][[1]]
#            lin_pnt = sp::SpatialPoints(
#              lin_pnt,
#              proj4string = CRS(proj4string(inter))
#            )
#            inter = sp::rbind.SpatialPoints(inter, lin_pnt)
#          }
        if(length(grep("GEOMETRY", sf::st_geometry_type(inter))) > 0) {

          lin = sf::st_cast(sf::st_collection_extract(inter, "LINESTRING"), "POINT")
          inter = sf::st_collection_extract(inter, "POINT")

          inter = as(c(st_geometry(inter), st_geometry(lin)), "Spatial")
        }

        # Set row names
        row.names(inter) = 1:nrow(inter)

        # Extract 'obstacles' / 'surface' data for each intersection
        # if(dimensions(location) == 2) outline = obstacles_outline
        # if(dimensions(location) == 3) outline = surface_outline
        inter =
          SpatialPointsDataFrame(
            inter,
            sp::over(
              inter,
#              rgeos::gBuffer(obstacles_outline, byid = TRUE, width = b),
              as(sf::st_buffer(sf::st_as_sf(obstacles_outline), dist = b), 
                "Spatial"),
            fn = max)
          )

        # Distance between examined location and intersections
#        inter$dist = rgeos::gDistance(inter, location, byid = TRUE)[1, ]
        inter$dist = sf::st_distance(sf::st_as_sf(inter), sf::st_as_sf(location))

        # Maximal angle of obstruction calculation

        # 2D mode - height difference equal to obstacles height
        if(dimensions(location) == 2) {
          inter$height_diff = inter@data[, obstacles_height_field]
        }

        # 3D mode - location height *subtracted* from obstacles height
        if(dimensions(location) == 3) {
          inter$height_diff = max(
            inter@data[, obstacles_height_field] - coordinates(location)[, 3],
            0 # If location z > obstacles h then 'height_diff=0'
            )
        }

        inter$angle = rad2deg(atan(inter$height_diff / inter$dist))
        # inter$svf = 1 - inter$angle / 90
        inter$svf = 1 - sin(deg2rad(inter$angle))^2 # Gal & Unger 2014
        svf[i] = min(inter$svf)

      }

    }

    svf_final = mean(svf)

  }

  return(svf_final)

}
