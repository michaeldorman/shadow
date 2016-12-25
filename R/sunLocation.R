

.sunLocation = function(location, sun_az, sun_elev) {

  # Conditions
  stopifnot(class(location) %in% c("SpatialPoints", "SpatialPointsDataFrame"))

  # Average earth-sun distance in meters
  dist_m = 149.6 * 10^9
  # d = 149597870700 * (1 - 0.01672 * cos(deg2rad(0.9856 * (day - 4))))

  # To radians
  az_rad = deg2rad(90 - sun_az)
  sun_elev = deg2rad(sun_elev)

  # Sun height
  height = data.frame(height = dist_m * tan(sun_elev))

  # Attribute table
  if(class(location) == "SpatialPointsDataFrame")
    data = cbind(location@data, height) else
      data = height

  # Sun location
  sp::SpatialPointsDataFrame(
    coords = raster::shift(
      location,
      x = dist_m * cos(az_rad),
      y = dist_m * sin(az_rad)
      ),
    data = data
  )

}










