#' Sky View Factor (SVF) calculation
#'
#' Calculates the Sky View Factor (SVF) at a given location, given extruded obstacles (usually a buildings layer).
#'
#' @param location A \code{SpatialPoints*} object specifying the location for which to calculate shade height
#' @param build A \code{SpatialPolygonsDataFrame} object specifying the buildings outline.
#' @param height_field The name of the column with building height in \code{build}
#' @param res Circular sampling resolution, in decimal degrees. Default is 5 degrees, i.e. 0, 5, 10... 355.
#' @param b Buffer size when joining intersection points with building outlines, to determine intersection height.
#'
#' @return A numeric value between 0 (sky completely obstructed) and 1 (sky completely visible).
#'
#' @examples
#' data(build)
#'
#' location0 = rgeos::gCentroid(build)
#' svf = SVF(location0, build, "BLDG_HT")
#' location1 = raster::shift(location0, 0, -15)
#' svf1 =  SVF(location1, build, "BLDG_HT")
#' location2 = raster::shift(location0, -10, 20)
#' svf2 =  SVF(location2, build, "BLDG_HT")
#'
#' plot(build)
#' plot(location0, add = TRUE)
#' plot(location1, add = TRUE)
#' plot(location2, add = TRUE)
#' raster::text(location0, round(svf, 2), pos = 3)
#' raster::text(location1, round(svf1, 2), pos = 4)
#' raster::text(location2, round(svf2, 2), pos = 3)
#'
#' @export

SVF = function(location, build, height_field, res = 5, b = 0.01) {

  # Check that 'location' is of length 1
  if(length(location) != 1)
    stop("'location' should be of length 1")

  # Check projected
  if(!is.projected(location) | !is.projected(build))
    stop("'build' and/or 'location' not in projected CRS")

  # Stop if class conditions not met
  if(!class(location) %in% c("SpatialPoints", "SpatialPointsDataFrame"))
    stop("'location' is not 'SpatialPoints*'")
  if(class(build) != "SpatialPolygonsDataFrame")
    stop("'build' is not 'SpatialPolygonsDataFrame'")

  # Check that height fields exist
  if(!height_field %in% names(build))
    stop("'height_field' not found in attribute table of 'build'")

  # Print units assumption
  message(
    paste0(
      "Assuming ", height_field, " given in ",
      gsub(" .*", "",
           gsub(".*\\+units=", "", proj4string(build))
      )
    )
  )

  # Create rays
  angles = seq(0, 359.9999, res)
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

  # Check view obstruction
  # Buildings outline to 'lines' *** DEPENDS ON PACKAGE 'sp' ***
  build_outline = as(build, "SpatialLinesDataFrame")

  # If point is on a building then SVF='NA'
  if(rgeos::gIntersects(location, build)) svf_final = NA else {

    svf = rep(NA, length(rays))

    for(i in 1:length(rays)) {

      # 'Line of sight' between sun and location
      ray1 = rays[i, ]

      # Intersections with buildings outline
      inter = rgeos::gIntersection(build_outline, ray1)

      # No intersections means SVF=1
      if(is.null(inter)) svf[i] = 1 else {

        # If some of the intersections are lines
        if(class(inter) == "SpatialCollections") {

          lin = inter@lineobj
          inter = inter@pointobj

          for(lin_i in 1:length(lin)) {

            lin_pnt =
              lin[lin_i, ] %>%
              coordinates %>%
              "[["(1) %>%
              "[["(1) %>%
              sp::SpatialPoints(proj4string = CRS(proj4string(grid)))
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
                build_outline,
                byid = TRUE,
                width = b
              ),
              fn = max)
          )

        # Distance between examined location and intersections
        inter$dist = rgeos::gDistance(inter, location, byid = TRUE)[1, ]

        # Maximal angle of obstruction calculation
        inter$angle = .rad2deg(
          atan(inter@data[, height_field] / inter$dist)
          )
        inter$svf = 1 - inter$angle / 90
        svf[i] = min(inter$svf)

      }

    }

    svf_final = mean(svf)

  }

  return(svf_final)

}



















