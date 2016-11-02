#' Find sun azimuth and elevation angles
#'
#' The function returns sun azimuth and elevation given time and spatial location.
#'
#' @param timeGMT Time, a \code{POSIXct} object.
#' @param lon Longitude.
#' @param lat Latitude.
#' @return A list with two components, \code{elevation} and \code{azimuth}, both in decimal degrees.
#'
#' @details
#' This function is a modified version of the following 'stackoverflow' forum post by
#' Josh O'Brien:
#' \url{http://stackoverflow.com/questions/8708048/position-of-the-sun-given-time-of-day-latitude-and-longitude}
#'
#' @references
#' Michalsky, J.J. 1988. The Astronomical Almanac's algorithm for approximate solar position (1950-2050). Solar Energy. 40(3):227-235.
#'
#' Michalsky, J.J. 1989. Errata. Solar Energy. 43(5):323.
#'
#' Spencer, J.W. 1989. Comments on "The Astronomical Almanac's Algorithm for Approximate Solar Position (1950-2050)". Solar Energy. 42(4):353.
#'
#' Walraven, R. 1978. Calculating the position of the sun. Solar Energy. 20:393-397.
#'
#' @examples
#'
#' time_local = as.POSIXct("1999-01-01 12:00:00", tz = "Asia/Jerusalem")
#' time_GMT = as.POSIXct(format.POSIXct(time_local, tz = "GMT"), tz = "GMT")
#' sunElevAz(time_GMT, lat = 31.974447, lon = 34.791708)
#'
#' @export

sunElevAz <- function(timeGMT, lon, lat) {

  # Extract timestamp components
  year = as.numeric(format(time_GMT, "%Y"))
  month = as.numeric(format(time_GMT, "%m"))
  day = as.numeric(format(time_GMT, "%d"))
  hour = as.numeric(format(time_GMT, "%H"))
  min = as.numeric(format(time_GMT, "%M"))
  sec = as.numeric(format(time_GMT, "%S"))

  twopi <- 2 * pi
  deg2rad <- pi / 180

  # Get day of the year, e.g. Feb 1 = 32, Mar 1 = 61 on leap years
  month.days <- c(0,31,28,31,30,31,30,31,31,30,31,30)
  day <- day + cumsum(month.days)[month]
  leapdays <- year %% 4 == 0 & (year %% 400 == 0 | year %% 100 != 0) &
    day >= 60 & !(month==2 & day==60)
  day[leapdays] <- day[leapdays] + 1

  # Get Julian date - 2400000
  hour <- hour + min / 60 + sec / 3600 # hour plus fraction
  delta <- year - 1949
  leap <- trunc(delta / 4) # former leapyears
  jd <- 32916.5 + delta * 365 + leap + day + hour / 24

  # The input to the Atronomer's almanach is the difference between
  # the Julian date and JD 2451545.0 (noon, 1 January 2000)
  time <- jd - 51545.

  # Ecliptic coordinates

  # Mean longitude
  mnlong <- 280.460 + .9856474 * time
  mnlong <- mnlong %% 360
  mnlong[mnlong < 0] <- mnlong[mnlong < 0] + 360

  # Mean anomaly
  mnanom <- 357.528 + .9856003 * time
  mnanom <- mnanom %% 360
  mnanom[mnanom < 0] <- mnanom[mnanom < 0] + 360
  mnanom <- mnanom * deg2rad

  # Ecliptic longitude and obliquity of ecliptic
  eclong <- mnlong + 1.915 * sin(mnanom) + 0.020 * sin(2 * mnanom)
  eclong <- eclong %% 360
  eclong[eclong < 0] <- eclong[eclong < 0] + 360
  oblqec <- 23.439 - 0.0000004 * time
  eclong <- eclong * deg2rad
  oblqec <- oblqec * deg2rad

  # Celestial coordinates
  # Right ascension and declination
  num <- cos(oblqec) * sin(eclong)
  den <- cos(eclong)
  ra <- atan(num / den)
  ra[den < 0] <- ra[den < 0] + pi
  ra[den >= 0 & num < 0] <- ra[den >= 0 & num < 0] + twopi
  dec <- asin(sin(oblqec) * sin(eclong))

  # Local coordinates
  # Greenwich mean sidereal time
  gmst <- 6.697375 + .0657098242 * time + hour
  gmst <- gmst %% 24
  gmst[gmst < 0] <- gmst[gmst < 0] + 24.

  # Local mean sidereal time
  lmst <- gmst + lon / 15.
  lmst <- lmst %% 24.
  lmst[lmst < 0] <- lmst[lmst < 0] + 24.
  lmst <- lmst * 15. * deg2rad

  # Hour angle
  ha <- lmst - ra
  ha[ha < -pi] <- ha[ha < -pi] + twopi
  ha[ha > pi] <- ha[ha > pi] - twopi

  # Latitude to radians
  lat <- lat * deg2rad

  # Azimuth and elevation
  el <- asin(sin(dec) * sin(lat) + cos(dec) * cos(lat) * cos(ha))
  az <- asin(-cos(dec) * sin(ha) / cos(el))

  # For logic and names, see Spencer, J.W. 1989. Solar Energy. 42(4):353
  cosAzPos <- (0 <= sin(dec) - sin(el) * sin(lat))
  sinAzNeg <- (sin(az) < 0)
  az[cosAzPos & sinAzNeg] <- az[cosAzPos & sinAzNeg] + twopi
  az[!cosAzPos] <- pi - az[!cosAzPos]

  # if (0 < sin(dec) - sin(el) * sin(lat)) {
  #     if(sin(az) < 0) az <- az + twopi
  # } else {
  #     az <- pi - az
  # }


  el <- el / deg2rad
  az <- az / deg2rad
  lat <- lat / deg2rad

  return(list(elevation=el, azimuth=az))
}



# # Test NOAA
# testPts <- data.frame(lat = c(-41,-3,3, 41),
#                       lon = c(0, 0, 0, 0))
#
# # Sun's position as returned by the NOAA Solar Calculator,
# NOAA <- data.frame(elevNOAA = c(72.44, 69.57, 63.57, 25.6),
#                    azNOAA = c(359.09, 180.79, 180.62, 180.3))
#
# # Sun's position as returned by sunElevAz()
# sunPos <- sunElevAz(year = 2012,
#                       month = 12,
#                       day = 22,
#                       hour = 12,
#                       min = 0,
#                       sec = 0,
#                       lat = testPts$lat,
#                       lon = testPts$lon)
#
# cbind(testPts, NOAA, sunPos)











