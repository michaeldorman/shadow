% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classifyAz.R
\name{classifyAz}
\alias{classifyAz}
\title{Classify azimuth of line segments}
\usage{
classifyAz(sl)
}
\arguments{
\item{sl}{A \code{SpatialLines*} object}
}
\value{
A \code{numeric} vector with the segment azimuth values (in decimal degrees)
}
\description{
Classify azimuth of line segments
}
\examples{
build_seg = toSeg(build[1, ])
az = classifyAz(build_seg)
plot(build_seg, col = rainbow(4)[cut(az, c(0, 90, 180, 270, 360))])
raster::text(
  # rgeos::gCentroid(build_seg, byid = TRUE),
  as(sf::st_centroid(sf::st_geometry(sf::st_as_sf(build_seg))), "Spatial"),
  round(az)
)

}
