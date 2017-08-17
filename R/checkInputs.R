##################################################################################

.checkLocation = function(location, length1 = TRUE) {

  # Check that 'location' is of length 1
  if(length1 & length(location) != 1)
    stop("'location' should be of length 1")

  # Check projected
  if(!sp::is.projected(location))
    stop("'location' not in projected CRS")

  if(!class(location) %in% c("SpatialPoints", "SpatialPointsDataFrame"))
    stop("'location' is not 'SpatialPoints*'")

}

##################################################################################

.checkSolarPos = function(solar_pos, length1) {

  # Matrix
  if(class(solar_pos) != "matrix")
    stop("'solar_pos' must be a 'matrix' object")

  # 1-row
  if(length1 & nrow(solar_pos) > 1)
    stop("'solar_pos' must have a single row (sun position)")

  # 2-column
  if(ncol(solar_pos) != 2)
    stop("'solar_pos' must have two columns (azimuth + elevation)")

  # Azimuth in [0,360]
  if(any(solar_pos[, 1] < 0) | any(solar_pos[, 1] > 360))
    stop("Sun azimuth should be a number in [0, 360]")

  # Elevation in [-90,90]
  if(any(solar_pos[, 2] < -90) | any(solar_pos[, 2] > 90))
    stop("Sun elevation should be a number in [-90, 90]")

}

##################################################################################

.checkObstacles = function(obstacles, obstacles_height_field, messages) {

  # Check projected
  if(!sp::is.projected(obstacles))
    stop("'obstacles' not in projected CRS")

  # Check class
  if(class(obstacles) != "SpatialPolygonsDataFrame")
    stop("'obstacles' is not 'SpatialPolygonsDataFrame'")

  # Check that height fields exist
  if(!obstacles_height_field %in% names(obstacles))
    stop("'obstacles_height_field' not found in attribute table of 'obstacles'")

  # Check that height values are posivive
  if(any(obstacles@data[, obstacles_height_field] < 0))
    stop("'obstacles_height_field' cannot contain negative values")

  # Print units assumption
  if(messages) {
    message(
      paste0(
        "Assuming ", obstacles_height_field, " given in ",
        gsub(" .*", "",
             gsub(".*\\+units=", "", sp::proj4string(obstacles))
        )
      )
    )
  }
}

##################################################################################
