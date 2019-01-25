
#' Parse a date-time vector
#' @param x vector of date-times
ss_parse_datetime <- function(x) {
  x <- as.POSIXct(strptime(x, "%Y-%m-%dT%H:%M:%OS"), tz = "US/Mountain")
  lubridate::with_tz(x, tzone = Sys.timezone())
}