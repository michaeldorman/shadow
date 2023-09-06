#' @importFrom sf st_intersection
#' @importFrom sf st_intersects
#' @importFrom sf st_distance
#' @importFrom sf st_buffer
#' @importFrom sf st_collection_extract
#' @importFrom sf st_geometry_type
#' @importFrom sf st_cast
#' @importFrom sf st_as_sf
#' @importFrom methods as


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
#    inter = rgeos::gIntersection(obstacles_outline, sun_ray)
    inter = sf::st_intersection(sf::st_geometry(sf::st_as_sf(location)), sf::st_geometry(sf::st_as_sf(sun)))

    # No intersections means there is no shade
#    if(is.null(inter)) shade_height = NA else {
    if(length(inter) == 0L) shade_height = NA else {

      # If some of the intersections are lines
#      if(is(inter, "SpatialCollections")) {
      if(length(grep("GEOMETRY", sf::st_geometry_type(inter))) > 0) {

#        lin = inter@lineobj
        lin = sf::st_cast(sf::st_collection_extract(inter, "LINESTRING"), "POINT")
#        inter = inter@pointobj
        inter = sf::st_collection_extract(inter, "POINT")

#        for(lin_i in 1:length(lin)) {
#
#          lin_pnt = lin[lin_i, ]
#          lin_pnt = sp::coordinates(lin_pnt)[[1]][[1]]
#          lin_pnt = sp::SpatialPoints(
#            lin_pnt,
#            proj4string = sp::CRS(sp::proj4string(inter))
#          )
#          inter = sp::rbind.SpatialPoints(inter, lin_pnt)
#       }

        inter = as(c(st_geometry(inter), st_geometry(lin)), "Spatial")

      }

      # Set row names
      row.names(inter) = 1:nrow(inter)

      # Extract building data for each intersection
      inter =
        SpatialPointsDataFrame(
          inter,
          sp::over(
            inter,
#            rgeos::gBuffer(
#              obstacles_outline[, obstacles_height_field],
#              byid = TRUE,
#              width = b
#            ),
            as(sf::st_buffer(
              sf::st_as_sf(obstacles_outline[, obstacles_height_field]), 
              dist = b), 
              "Spatial"),
            fn = max)
        )

      # Distance between examined location and intersections
#      inter$dist = rgeos::gDistance(inter, location, byid = TRUE)[1, ]
      inter$dist = sf::st_distance(sf::st_as_sf(inter), sf::st_as_sf(location))
      # Shadow height calculation
      inter$shade_fall = inter$dist * tan(deg2rad(solar_pos[, 2]))
      inter$shade_height =
        inter@data[, obstacles_height_field] - inter$shade_fall
      shade_height = max(inter$shade_height)

      # Assign NA when there is no shadow

      # (1) If point is on a building & shadow_height < building_height
#      if(rgeos::gIntersects(location, obstacles)) {
      if(sf::st_intersects(
        sf::st_union(
          sf::st_as_sf(location)
        ),
        sf::st_union(
          sf::st_as_sf(obstacles)
        ))) {
        build_height = sp::over(location, obstacles)[, obstacles_height_field]
        if(shade_height <= build_height)
          shade_height = NA
      } else # (2) If point is on ground & shadow_height < 0
        if(shade_height <= 0) shade_height = NA

    }

  }

  return(shade_height)

}

