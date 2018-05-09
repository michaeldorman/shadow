#' Logical shadow calculation (is given point shaded?) for 3D points considering sun position and obstacles
#'
#' This function determines whether each given point in a set of 3D points (\code{location}), is shaded or not taking into account:\itemize{
#' \item{Obstacles outline (\code{obstacles}), given by a polygonal layer with a height attribute (\code{obstacles_height_field})}
#' \item{Sun position (\code{solar_pos}), given by azimuth and elevation angles}
#' }
#' Alternatively, the function determines whether each point is in shadow based on a raster representing shadow height \code{shadowHeightRaster}, in which case \code{obstacles}, \code{obstacles_height_field} and \code{solar_pos} are left unspecified.
#'
#' @param location A \code{SpatialPoints*} or \code{Raster*} object, specifying the location(s) for which to calculate shadow height
#' @param shadowHeightRaster Raster representing shadow height
#' @param obstacles A \code{SpatialPolygonsDataFrame} object specifying the obstacles outline
#' @param obstacles_height_field Name of attribute in \code{obstacles} with extrusion height for each feature
#' @param solar_pos A matrix with two columns representing sun position(s); first column is the solar azimuth (in degrees from North), second column is sun elevation (in degrees); rows represent different positions (e.g. at different times of day)
#' @param ... Other parameters passed to \strong{\code{\link{shadowHeight}}}, such as \code{parallel}
#'
#' @return
#' Returned object is either a logical \code{matrix} or a \code{Raster*} with logical values -
#' \itemize{
#' \item{If input \code{location} is a \code{SpatialPoints*}, then returned object is a \code{matrix} where rows represent spatial locations (\code{location} features), columns represent solar positions (\code{solar_pos} rows) and values represent shadow state}
#' \item{If input \code{location} is a \code{Raster*}, then returned object is a \code{RasterLayer} or \code{RasterStack}, where raster layers represent solar positions (\code{solar_pos} rows) and pixel values represent shadow state}
#' }
#' In both cases the logical values express shadow state -
#' \itemize{
#' \item{\code{TRUE} means the location is in shadow}
#' \item{\code{FALSE} means the location is not in shadow}
#' \item{\code{NA} means the location 3D-intersects an obstacle}
#' }
#'
#' @note For a correct geometric calculation, make sure that:\itemize{
#' \item{The layers \code{location} and \code{obstacles} are projected and in same CRS}
#' \item{The values in \code{obstacles_height_field} of \code{obstacles} are given in the same distance units as the CRS (e.g. meters when using UTM)}
#'}
#'
#' @examples
#'
#' # Method for 3D points - Manually defined
#'
#' opar = par(mfrow = c(1, 3))
#'
#' # Ground level
#' location = sp::spsample(
#'   rgeos::gBuffer(rgeos::gEnvelope(rishon), width = 40),
#'   n = 200,
#'   type = "regular"
#' )
#' solar_pos = as.matrix(tmy[9, c("sun_az", "sun_elev")])
#' s = inShadow(
#'   location = location,
#'   obstacles = rishon,
#'   obstacles_height_field = "BLDG_HT",
#'   solar_pos = solar_pos
#' )
#' plot(location, col = ifelse(s[, 1], "grey", "yellow"), main = "h=0")
#' plot(rishon, add = TRUE)
#'
#' # 15 meters above ground level
#' coords = coordinates(location)
#' coords = cbind(coords, z = 15)
#' location1 = SpatialPoints(coords, proj4string = CRS(proj4string(location)))
#' solar_pos = as.matrix(tmy[9, c("sun_az", "sun_elev")])
#' s = inShadow(
#'   location = location1,
#'   obstacles = rishon,
#'   obstacles_height_field = "BLDG_HT",
#'   solar_pos = solar_pos
#' )
#' plot(location, col = ifelse(s[, 1], "grey", "yellow"), main = "h=15")
#' plot(rishon, add = TRUE)
#'
#' # 30 meters above ground level
#' coords = coordinates(location)
#' coords = cbind(coords, z = 30)
#' location2 = SpatialPoints(coords, proj4string = CRS(proj4string(location)))
#' solar_pos = as.matrix(tmy[9, c("sun_az", "sun_elev")])
#' s = inShadow(
#'   location = location2,
#'   obstacles = rishon,
#'   obstacles_height_field = "BLDG_HT",
#'   solar_pos = solar_pos
#' )
#' plot(location, col = ifelse(s[, 1], "grey", "yellow"), main = "h=30")
#' plot(rishon, add = TRUE)
#'
#' par(opar)
#'
#' # Shadow on a grid covering obstacles surface
#' \dontrun{
#'
#' # Method for 3D points - Covering building surface
#'
#' obstacles = rishon[c(2, 4), ]
#' location = surfaceGrid(
#'   obstacles = obstacles,
#'   obstacles_height_field = "BLDG_HT",
#'   res = 2,
#'   offset = 0.01
#' )
#' solar_pos = tmy[c(9, 16), c("sun_az", "sun_elev")]
#' solar_pos = as.matrix(solar_pos)
#' s = inShadow(
#'   location = location,
#'   obstacles = obstacles,
#'   obstacles_height_field = "BLDG_HT",
#'   solar_pos = solar_pos
#' )
#' location$shadow = s[, 1]
#' plotGrid(location, color = c("yellow", "grey")[as.factor(location$shadow)], size = 0.5)
#' location$shadow = s[, 2]
#' plotGrid(location, color = c("yellow", "grey")[as.factor(location$shadow)], size = 0.5)
#'
#' # Method for ground locations raster
#'
#' ext = as(raster::extent(rishon) + 20, "SpatialPolygons")
#' location = raster::raster(ext, res = 2)
#' proj4string(location) = proj4string(rishon)
#' obstacles = rishon[c(2, 4), ]
#' solar_pos = tmy[c(9, 16), c("sun_az", "sun_elev")]
#' solar_pos = as.matrix(solar_pos)
#' s = inShadow(
#'   location = location,
#'   obstacles = obstacles,
#'   obstacles_height_field = "BLDG_HT",
#'   solar_pos = solar_pos
#' )
#' plot(s)
#'
#' # Method for pre-calculated shadow height raster
#'
#' ext = as(raster::extent(rishon), "SpatialPolygons")
#' r = raster::raster(ext, res = 1)
#' proj4string(r) = proj4string(rishon)
#' r[] = rep(seq(30, 0, length.out = ncol(r)), times = nrow(r))
#' location = surfaceGrid(
#'   obstacles = rishon[c(2, 4), ],
#'   obstacles_height_field = "BLDG_HT",
#'   res = 2,
#'   offset = 0.01
#' )
#' s = inShadow(
#'   location = location,
#'   shadowHeightRaster = r
#' )
#' location$shadow = s[, 1]
#' r_pnt = raster::as.data.frame(r, xy = TRUE)
#' coordinates(r_pnt) = names(r_pnt)
#' proj4string(r_pnt) = proj4string(r)
#' r_pnt = SpatialPointsDataFrame(
#'   r_pnt,
#'   data.frame(
#'     shadow = rep(TRUE, length(r_pnt)),
#'     stringsAsFactors = FALSE
#'     )
#'  )
#' pnt = rbind(location[, "shadow"], r_pnt)
#'
#' plotGrid(pnt, color = c("yellow", "grey")[as.factor(pnt$shadow)], size = 0.5)
#' }
#'
#' @export
#' @name inShadow

NULL

setGeneric("inShadow", function(
  location,
  shadowHeightRaster,
  obstacles,
  obstacles_height_field,
  solar_pos,
  ...
) {
  standardGeneric("inShadow")
})

#' @export
#' @rdname inShadow

##################################################################################
# 'inShadow' method for Points + shadowHeightRaster

setMethod(

  f = "inShadow",

  signature = c(
    location = "SpatialPoints",
    shadowHeightRaster = "Raster",
    obstacles = "missing",
    obstacles_height_field = "missing",
    solar_pos = "missing"
  ),

  function(
    location,
    shadowHeightRaster,
    obstacles,
    obstacles_height_field,
    solar_pos
  ) {

    # Convert to 3D if necessary
    if(dimensions(location) == 2) {
      if(class(location) == "SpatialPoints") {
        coords = coordinates(location)
        coords = cbind(coords, z = rep(0, nrow(coords)))
        location = sp::SpatialPoints(
          coords = coords,
          proj4string = sp::CRS(sp::proj4string(location))
          )
      }
      if(class(location) == "SpatialPointsDataFrame") {
        coords = coordinates(location)
        coords = cbind(coords, z = rep(0, nrow(coords)))
        location = sp::SpatialPointsDataFrame(
          coords = coords,
          data = location@data,
          proj4string = sp::CRS(sp::proj4string(location)),
          match.ID = FALSE
          )
      }
    }

    location_2d = location
    location_2d@coords = location_2d@coords[, 1:2, drop = FALSE]

    # Locations z-index
    h = coordinates(location)[, 3]

    # Results matrix
    result = matrix(
      NA,
      nrow = length(location), # Rows represent *space*
      ncol = raster::nlayers(shadowHeightRaster) # Columns represent *time*
    )

    # Progress bar
    pb = utils::txtProgressBar(min = 0, max = ncol(result), initial = 0, style = 3)

    for(col in 1:ncol(result)) { # Times

      # Raster-based shadow height
      shadow_height = raster::extract(shadowHeightRaster[[col]], location_2d)

      # Comparison
      result[, col] = !(shadow_height < h | is.na(shadow_height))

      # Progress
      utils::setTxtProgressBar(pb, col)

    }

    return(result)

    }

)

#' @export
#' @rdname inShadow

##################################################################################
# 'inShadow' method for Points

setMethod(

  f = "inShadow",

  signature = c(
    location = "SpatialPoints",
    shadowHeightRaster = "missing",
    obstacles = "SpatialPolygonsDataFrame",
    obstacles_height_field = "character",
    solar_pos = "matrix"
  ),

  function(
    location,
    shadowHeightRaster,
    obstacles,
    obstacles_height_field,
    solar_pos,
    ...
  ) {

    # Convert to 3D if necessary
    if(dimensions(location) == 2) {
      if(class(location) == "SpatialPoints") {
        coords = coordinates(location)
        coords = cbind(coords, z = rep(0, nrow(coords)))
        location = sp::SpatialPoints(
          coords = coords,
          proj4string = sp::CRS(sp::proj4string(location))
        )
      }
      if(class(location) == "SpatialPointsDataFrame") {
        coords = coordinates(location)
        coords = cbind(coords, z = rep(0, nrow(coords)))
        location = sp::SpatialPointsDataFrame(
          coords = coords,
          data = location@data,
          proj4string = sp::CRS(sp::proj4string(location)),
          match.ID = FALSE
        )
      }
    }

    # Unique ground locations
    coord = as.data.frame(coordinates(location))
    coord_unique = coord[!duplicated(coord[, 1:2]), 1:2]

    # Point layer of unique ground locations
    pnt_unique = coord_unique
    coordinates(pnt_unique) = names(pnt_unique)[1:2]
    proj4string(pnt_unique) = proj4string(location)

    # Results matrix
    result = matrix(
      NA,
      nrow = length(location), # Rows represent *space*
      ncol = nrow(solar_pos) # Columns represent *time*
    )

    # Progress bar
    pb = utils::txtProgressBar(min = 0, max = ncol(result), initial = 0, style = 3)

    for(col in 1:ncol(result)) { # Times

      coord_unique$shadow_height =
        shadowHeight(
          location = pnt_unique,
          obstacles = obstacles,
          obstacles_height_field = obstacles_height_field,
          solar_pos = solar_pos[col, , drop = FALSE],
          ...
        )[, 1]

      coord = plyr::join(
        coord,
        coord_unique,
        names(coord)[1:2]
        )

      result[, col] = !(coord$shadow_height < coord[, 3] | is.na(coord$shadow_height))

      # If point is *inside* building (Location z < Obstacle h) then 'inShadow=NA'
      obstacle_h = over(location, obstacles)[[obstacles_height_field]]
      location_z = coordinates(location)[, 3]
      low = !is.na(obstacle_h) & location_z < obstacle_h
      result[, col][low] = NA

      coord$shadow_height = NULL

      # Progress
      utils::setTxtProgressBar(pb, col)

    }

    return(result)

    }

)

#' @export
#' @rdname inShadow

##################################################################################
# 'inShadow' method for Raster

setMethod(

  f = "inShadow",

  signature = c(
    location = "Raster",
    shadowHeightRaster = "missing",
    obstacles = "SpatialPolygonsDataFrame",
    obstacles_height_field = "character",
    solar_pos = "matrix"
  ),

  function(
    location,
    shadowHeightRaster,
    obstacles,
    obstacles_height_field,
    solar_pos,
    ...
  ) {

    pnt = raster::rasterToPoints(location, spatial = TRUE)

    s = inShadow(
      location = pnt,
      obstacles = obstacles,
      obstacles_height_field = obstacles_height_field,
      solar_pos = solar_pos,
      ...
    )

    template = location
    result = raster::stack()

    for(i in 1:ncol(s)) {
      template[] = s[, i]
      result = raster::stack(result, template)
    }

    if(raster::nlayers(result) == 1)
      return(result[[1]]) else
        (return(result))

  }

)

#' @export
#' @rdname inShadow



















