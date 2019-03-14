#' Get ShipStation users
#' @export
ss_get_users <- function() {
  x <- ss_api("users")
  ss_parse_response(x)
}