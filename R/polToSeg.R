#'  Polygons or lines to segments
#'
#'  Split lines or polygons to separate segments.
#'
#'	@param	x	A \code{SpatialLines*} or a \code{SpatialPolygons*} object.
#'	@return	A \code{SpatialLines} object where each segment is represented by a separate feature.
#'	@references
#'	The function is based on code from the following 'r-sig-geo' post by Roger Bivand:
#'
#'	\url{https://stat.ethz.ch/pipermail/r-sig-geo/2013-April/017998.html}
#'	@examples
#'	# Create sample SpatialPoygons named 'SpP'
#'	Sr1 = Polygon(cbind(c(2,4,4,1,2),c(2,3,5,4,2)))
#'	Sr2 = Polygon(cbind(c(5,4,2,5),c(2,3,2,2)))
#'	Sr3 = Polygon(cbind(c(4,4,5,10,4),c(5,3,2,5,5)))
#'	Sr4 = Polygon(cbind(c(5,6,6,5,5),c(4,4,3,3,4)), hole = TRUE)
#'	Srs1 = Polygons(list(Sr1), "s1")
#'	Srs2 = Polygons(list(Sr2), "s2")
#'	Srs3 = Polygons(list(Sr3, Sr4), "s3/4")
#'	SpP = SpatialPolygons(list(Srs1,Srs2,Srs3), 1:3)
#'	# Convert 'SpP' to segments
#'	seg = polToSeg(SpP)
#'	plot(seg, col = sample(rainbow(15)))
#'	text(gCentroid(seg, byid = TRUE), add = TRUE)
#'

polToSeg = function(x) {

  seg = list()

  for(f in 1:length(x)) {

    # Select one polygon
    dat = x[f, ]

    # Convert to 'SpatialLines'
    dat_l = as(dat, "SpatialLines")

    # Split to line segments
    cSl = coordinates(dat_l)
    cSl
    in_nrows = lapply(cSl, function(x) sapply(x, nrow))
    outn = sapply(in_nrows, function(y) sum(y-1))
    res = vector(mode="list", length=outn)
    i = 1
    for (j in seq(along=cSl)) {
      for (k in seq(along=cSl[[j]])) {
        for (l in 1:(nrow(cSl[[j]][[k]])-1)) {
          res[[i]] = cSl[[j]][[k]][l:(l+1),]
          i = i + 1
        }
      }
    }
    res1 = vector(mode="list", length=outn)
    for (i in seq(along=res))
      res1[[i]] = Lines(list(Line(res[[i]])), paste(f, i))
    seg1 = SpatialLines(res1, proj4string = CRS(proj4string(x)))

    # Add polygon attribute table entry to each segment
    if(class(x) %in% c("SpatialLinesDataFrame", "SpatialPolygonsDataFrame")) {
      attr_table =
        cbind(
          x@data[rep(f, length(seg1)), ],
          wall = 1:length(seg1)
        )
      rownames(attr_table) = paste(f, 1:length(res))
      seg1 = SpatialLinesDataFrame(seg1, data = attr_table, match.ID = FALSE)
    }

    seg[[f]] = seg1

  }

  seg = do.call(rbind, seg)

  seg

}
