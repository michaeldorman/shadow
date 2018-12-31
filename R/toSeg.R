#' Split polygons or lines to segments
#'
#' Split lines or polygons to separate segments.
#'
#' @param	x	A \code{SpatialLines*} or a \code{SpatialPolygons*} object
#' @return	A \code{SpatialLines} object where each segment is represented by a separate feature
#'
#' @references
#' This function uses a modified version of code from the following 'r-sig-geo' post by Roger Bivand:
#' \url{https://stat.ethz.ch/pipermail/r-sig-geo/2013-April/017998.html}
#'
#' @examples
#' seg = toSeg(build[1, ])
#' plot(seg, col = sample(rainbow(length(seg))))
#' raster::text(rgeos::gCentroid(seg, byid = TRUE), 1:length(seg))
#'
#' # Other data structures
#' toSeg(geometry(build)) # SpatialPolygons
#' toSeg(boston_sidewalk) # SpatialLinesDataFrame
#' toSeg(geometry(boston_sidewalk)) # SpatialLinesDataFrame
#'
#' @export

toSeg = function(x) {

  # Check 'x' class
  .checkLinePoly(x)

  seg = list()

  for(f in 1:length(x)) {

    # Select one polygon
    dat = x[f, ]

    # Convert to 'SpatialLines' *** Depends on package 'sp' ***
    dat_l = as(dat, "SpatialLines")

    # Split to line segments
    cSl = sp::coordinates(dat_l)
    in_nrows = lapply(cSl, function(x) sapply(x, nrow))
    outn = sapply(in_nrows, function(y) sum(y - 1))
    res = vector(mode = "list", length = outn)
    i = 1
    for (j in seq(along=cSl)) {
      for (k in seq(along=cSl[[j]])) {
        for (l in 1:(nrow(cSl[[j]][[k]])-1)) {
          res[[i]] = cSl[[j]][[k]][l:(l+1),]
          i = i + 1
        }
      }
    }
    res1 = vector(mode = "list", length = outn)
    for (i in seq(along = res))
      res1[[i]] = sp::Lines(list(sp::Line(res[[i]])), paste(f, i))
    seg1 = sp::SpatialLines(res1, proj4string = sp::CRS(sp::proj4string(x)))

    # Add polygon attribute table entry to each segment
    if(class(x) %in% c("SpatialLinesDataFrame", "SpatialPolygonsDataFrame")) {
      attr_table =
          x@data[rep(f, length(seg1)), , drop = FALSE]
      rownames(attr_table) = paste(f, 1:length(res))
      seg1 = sp::SpatialLinesDataFrame(
        seg1,
        data = attr_table,
        match.ID = FALSE
        )
    }

    seg[[f]] = seg1

  }

  seg = do.call(rbind, seg)

  seg

}
