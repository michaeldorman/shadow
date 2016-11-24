#'	Local time to GMT
#'
#'  The function transforms a \code{POSIXct} object in any given time zone to GMT.
#'
#'	@param	time	Time, a \code{POSIXct} object.
#'	@return	A a \code{POSIXct} object, in GMT.
#'	@examples
#'  time = as.POSIXct("1999-01-01 12:00:00", tz = "Asia/Jerusalem")
#'  toGMT(time)
#'  @export

toGMT = function(time) {
  as.POSIXct(format.POSIXct(time, tz = "GMT"), tz = "GMT")
}
