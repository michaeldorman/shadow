% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{tmy}
\alias{tmy}
\title{Typical Meteorological Year (TMY) solar radiation in Tel-Aviv}
\format{
A \code{data.frame} with 8760 rows and 7 columns.
}
\usage{
tmy
}
\description{
A table with hourly solar radiation estimates for a typical meteorological year in Tel-Aviv. \itemize{
\item{\code{time} Time, as \code{character} in the \code{"\%Y-\%m-\%d \%H:\%M:\%S"} format, e.g. \code{"2000-01-01 06:00:00"}}, referring to local time
\item{\code{sun_az} Sun azimuth, in decimal degrees from North}
\item{\code{sun_elev} Sun elevation, in decimal degrees}
\item{\code{solar_normal} Direct Normal Irradiance, in Wh/m^2}
\item{\code{solar_diffuse} Diffuse Horizontal Irradiance, in Wh/m^2}
\item{\code{dbt} Dry-bulb temperature, in Celsius degrees}
\item{\code{ws} Wind speed, in m/s}
}
}
\examples{
head(tmy)
}
\references{
\url{https://energyplus.net/weather-location/europe_wmo_region_6/ISR/ISR_Tel.Aviv-Bet.Dagan.401790_MSI}
}
\keyword{datasets}
