#' Logical shadow calculation (is given point shaded?) for 3D points considering sun position and obstacles
#'
#' This function determines whether each given point in a set of 3D points (\code{location}), is shaded or not
#' taking into account:\itemize{
#' \item{Obstacles outline (\code{obstacles}), given by a polygonal layer with a height attribute (\code{obstacles_height_field})}
#' \item{Sun position (\code{solar_pos}), given by azimuth and elevation angles}
#' }
#' @note For a correct geometric calculation, make sure that:\itemize{
#' \item{The layers \code{location} and \code{obstacles} are projected and in same CRS}
#' \item{The values in \code{obstacles_height_field} of \code{obstacles} are given in the same distance units as the CRS (e.g. meters when using UTM)}
#'}
#'
#' @param location A \code{SpatialPoints*} or \code{Raster*} object, specifying the location(s) for which to calculate shadow height
#' @param shadowHeightRaster Raster representing shadow height
#' @param obstacles A \code{SpatialPolygonsDataFrame} object specifying the obstacles outline
#' @param obstacles_height_field Name of attribute in \code{obstacles} with extrusion height for each feature
#' @param solar_pos A matrix with two columns representing sun position(s); first column is the solar azimuth (in degrees from North), second column is sun elevation (in degrees); rows represent different positions (e.g. at different times of day)
#' @param ... Other parameters passed to \code{shadowHeight}

#'
#' @return Logical \code{matrix} with rows representing spatial locations (\code{location} features) and columns representing solar positions (\code{solar_pos} rows)
#' @examples
#'
#' \dontrun{
#' # Method for obstacles and sun position
#' obstacles = rishon[c(2, 4), ]
#' location = surfaceGrid(
#'   obstacles = obstacles,
#'   obstacles_height_field = "BLDG_HT",
#'   res = 2,
#'   offset = 0.01
#' )
#' solar_pos = matrix(c(88.83113, 46.724), ncol = 2)
#' s = inShadow(
#'   location = location,
#'   obstacles = obstacles,
#'   obstacles_height_field = "BLDG_HT",
#'   solar_pos = solar_pos
#' )
#' location$shadow = s[, 1]
#' plotGrid(location, color = c("yellow", "grey")[as.factor(location$shadow)], size = 0.5)
#' }
#'
#' # Method for pre-calculated shadow height raster
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
#' \dontrun{
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

    location_2d = location
    location_2d@coords = location_2d@coords[, 1:2]

    # Locations z-index
    h = coordinates(location)[, 3]

    # Results matrix
    result = matrix(
      NA,
      nrow = length(location), # Rows represent *space*
      ncol = raster::nlayers(shadowHeightRaster) # Columns represent *time*
    )

    for(col in 1:ncol(result)) { # Times

      # Raster-based shadow height
      shadow_height = raster::extract(shadowHeightRaster[[col]], location_2d)

      # Comparison
      result[, col] = !(shadow_height < h | is.na(shadow_height))

    }

    return(result)

    }

)

#' @export
#' @rdname inShadow

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

      result[, col] = !(coord$shadow_height < coord$h | is.na(coord$shadow_height))

    }

    return(result)

    }

)
