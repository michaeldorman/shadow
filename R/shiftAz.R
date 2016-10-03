shiftAz = function(object, az, dist) {

  az = 90 - az
  az_rad = (az * pi) / (180)
  raster::shift(object, x = dist * cos(az_rad) , y = dist * sin(az_rad))

}
