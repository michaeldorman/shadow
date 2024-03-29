% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shadowFootprint.R
\name{shadowFootprint}
\alias{shadowFootprint}
\alias{shadowFootprint,SpatialPolygonsDataFrame-method}
\title{Shadow footprint on the ground}
\usage{
\S4method{shadowFootprint}{SpatialPolygonsDataFrame}(
  obstacles,
  obstacles_height_field,
  solar_pos = solarpos2(obstacles, time),
  time = NULL,
  b = 0.01
)
}
\arguments{
\item{obstacles}{A \code{SpatialPolygonsDataFrame} object specifying the obstacles outline}

\item{obstacles_height_field}{Name of attribute in \code{obstacles} with extrusion height for each feature}

\item{solar_pos}{A \code{matrix} with one row and two columns; first column is the solar azimuth (in decimal degrees from North), second column is sun elevation (in decimal degrees)}

\item{time}{When \code{solar_pos} is unspecified, \code{time} can be passed to automatically calculate \code{solar_pos} based on the time and the centroid of \code{obstacles}, using function \code{suntools::solarpos}. In such case \code{obstacles} must have a defined CRS (not \code{NA}). The \code{time} value must be a \code{POSIXct} or \code{POSIXlt} object}

\item{b}{Buffer size for shadow footprints of individual segments of a given polygon; used to eliminate minor internal holes in the resulting shadow polygon.}
}
\value{
A \code{SpatialPolygonsDataFrame} object representing shadow footprint, plus buildings outline. Object length is the same as that of the input \code{obstacles}, with an individual footprint feature for each obstacle.
}
\description{
Creates a polygonal layer of shadow footprints on the ground, taking into account:\itemize{
\item{Obstacles outline (\code{obstacles}), given by a polygonal layer with a height attribute (\code{obstacles_height_field})}
\item{Sun position (\code{solar_pos}), given by azimuth and elevation angles}
}
The calculation method was inspired by Morel Weisthal's MSc thesis at the Ben-Gurion University of the Negev.
}
\examples{
time = as.POSIXct("2004-12-24 13:30:00", tz = "Asia/Jerusalem")
proj4string(build) = CRS("EPSG:32636")
location_geo = matrix(c(34.7767978098526, 31.9665936050395), ncol = 2)
solar_pos = suntools::solarpos(location_geo, time)
footprint1 =               ## Using 'solar_pos'
  shadowFootprint(
    obstacles = build,
    obstacles_height_field = "BLDG_HT",
    solar_pos = solar_pos
    )
footprint2 =               ## Using 'time'
  shadowFootprint(
    obstacles = build,
    obstacles_height_field = "BLDG_HT",
    time = time
    )
all.equal(footprint1, footprint2)
footprint = footprint1
plot(footprint, col = adjustcolor("lightgrey", alpha.f = 0.5))
plot(build, add = TRUE, col = "darkgrey")

}
\references{
Weisthal, M. (2014). Assessment of potential energy savings in Israel through climate-aware residential building design (MSc Thesis, Ben-Gurion University of the Negev).
\url{https://www.dropbox.com/s/bztnh1fi9znmswj/Thesis_Morel_Weisthal.pdf?dl=1}
}
