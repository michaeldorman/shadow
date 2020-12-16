################################################################

.checkTime = function(time) {
  if(!any(class(time) %in% "POSIXt"))
    stop("'time' must be of class 'POSIXct' or 'POSIXlt'")
}

################################################################

.checkLocation = function(location, length1 = TRUE) {

  # Check that 'location' is of length 1
  if(length1 & length(location) != 1)
    stop("'location' should be of length 1")

  # Check projected
  if(!is.na(proj4string(location)) & !is.projected(location))
    stop("'location' not in projected CRS")

  if(!class(location) %in% c("SpatialPoints", "SpatialPointsDataFrame"))
    stop("'location' is not 'SpatialPoints*'")

}

################################################################

.checkSolarPos = function(solar_pos, length1) {

  # Matrix
  if(!is(solar_pos, "matrix"))
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

################################################################

.checkObstacles = function(obstacles, obstacles_height_field) {

  # Check projected
  if(!is.na(proj4string(obstacles)) & !is.projected(obstacles))
    stop("'obstacles' not in projected CRS")

  # Check class
  if(class(obstacles) != "SpatialPolygonsDataFrame")
    stop("'obstacles' is not 'SpatialPolygonsDataFrame'")

  # Check that height fields exist
  if(!obstacles_height_field %in% names(obstacles))
    stop("'obstacles_height_field' not found in attribute table of 'obstacles'")

  # Check that height values are positive
  if(any(obstacles@data[, obstacles_height_field] < 0))
    stop("'obstacles_height_field' cannot contain negative values")

}

################################################################

.checkSolarRad = function(solar_normal, solar_diffuse, solar_pos) {

  if(!is.vector(solar_normal) | !is.numeric(solar_normal))
    stop("'solar_normal' must be a numeric vector")

  if(!is.vector(solar_diffuse) | !is.numeric(solar_diffuse))
    stop("'solar_diffuse' must be a numeric vector")

  if(!all.equal(length(solar_normal), length(solar_diffuse), nrow(solar_pos)))
    stop("'solar_normal', 'solar_diffuse' and 'solar_pos' must have same length")

}

################################################################

.checkGrid = function(grid) {

  if(class(grid) != c("SpatialPointsDataFrame"))
    stop("'grid' must be 'SpatialPointsDataFrame'")

  if(dimensions(grid) != 3)
    stop("'grid' must be three-dimensional")

  if(!"type" %in% colnames(grid@data))
    stop("'grid' must contain an attribute named 'type'")

  if(!"facade_az" %in% colnames(grid@data))
    stop("'grid' must contain an attribute named 'facade_az'")

  # Azimuth in [0,360]
  if(any(!is.na(grid$facade_az) & (grid$facade_az < 0 | grid$facade_az > 360)))
    stop("'grid$facade_az' should be numeric in [0, 360]")

  if(any(!grid$type %in% c("roof", "facade")))
    stop("All values of 'type' attribute must be 'roof' or 'facade'")

}

################################################################

.checkLinePoly = function(x) {

  if(!class(x) %in% c(
    "SpatialLines",
    "SpatialLinesDataFrame",
    "SpatialPolygons",
    "SpatialPolygonsDataFrame")
  ) {

    stop("Input must be 'SpatialLines*' or 'SpatialPolygons*' object")

  }

}

################################################################
