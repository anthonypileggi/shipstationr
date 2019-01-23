
#' Call the ShipStation API
#' @param order_number Order Number (integer/vector)
#' @param start_date first day of data (Date/scalar)
#' @param end_date last day of data (Date/scalar)
#' @export
ss_get_shipments <- function(order_number = NULL, start_date = Sys.Date() - 1, end_date = Sys.Date() - 1) {

  my_json <- list(
    orderNumber = order_number,
    createDateStart = start_date,
    createDateEnd = end_date,
    pageSize = 500,
    page = 0
  )

  # query api, iterating over all pages
  go <- TRUE
  out <- list()
  while (go) {
    my_json$page <- my_json$page + 1
    x <- httr::GET(
      "https://ssapi.shipstation.com/shipments",
      httr::authenticate(Sys.getenv("SHIPSTATION_API_KEY"), Sys.getenv("SHIPSTATION_API_SECRET")),
      query = my_json
    )
    xx <- httr::content(x)
    out <- c(out, xx$shipments)
    if (xx$page > 1)
      message("Collecting page ", xx$page, "/", xx$pages)
    if (xx$page >= xx$pages)
      go <- FALSE
  }

  # clean-up api response
  dplyr::mutate(
    ss_parse_response(out),
    createDate = as.POSIXct(strptime(createDate, "%Y-%m-%dT%H:%M:%OS"), tz = "US/Mountain"),
    createDate = lubridate::with_tz(createDate, tzone = Sys.timezone()),
    #voidDate = as.POSIXct(strptime(voidDate, "%Y-%m-%dT%H:%M:%OS")),
    shipDate = as.Date(shipDate)
  )
}


#' Call the ShipStation Orders API
#' @param order_number Order Number (integer/vector)
#' @param start_date first day of data (Date/scalar)
#' @param end_date last day of data (Date/scalar)
#' @export
ss_get_orders <- function(order_number = NULL, start_date = Sys.Date() - 1, end_date = Sys.Date()) {

  # TODO: adjust start/end_date so align with {local} timezone
  my_json <- list(
    orderNumber = order_number,
    orderDateStart = paste(start_date - 1, "21:00:00"),
    orderDateEnd = paste(end_date, "20:59:59"),
    pageSize = 500,
    page = 0
  )

  # query api, iterating over all pages
  go <- TRUE
  out <- list()
  while (go) {
    my_json$page <- my_json$page + 1
    x <- httr::GET(
      "https://ssapi.shipstation.com/orders",
      httr::authenticate(Sys.getenv("SHIPSTATION_API_KEY"), Sys.getenv("SHIPSTATION_API_SECRET")),
      query = my_json
    )
    xx <- httr::content(x)
    out <- c(out, xx$orders)
    if (xx$page > 1)
      message("Collecting page ", xx$page, "/", xx$pages)
    if (xx$page >= xx$pages)
      go <- FALSE
  }

  # clean-up api response
  dplyr::mutate(
    ss_parse_response(out),
    orderDate = as.POSIXct(strptime(orderDate, "%Y-%m-%dT%H:%M:%OS"), tz = "US/Pacific"),
    orderDate = lubridate::with_tz(orderDate, tzone = Sys.timezone()),
    createDate = as.POSIXct(strptime(createDate, "%Y-%m-%dT%H:%M:%OS"), tz = "US/Pacific"),
    createDate = lubridate::with_tz(createDate, tzone = Sys.timezone()),
    shipDate = as.Date(shipDate)
  )
}




#' Get ShipStation users
ss_get_users <- function() {
  x <- httr::GET(
    "https://ssapi.shipstation.com/users",
    httr::authenticate(Sys.getenv("SHIPSTATION_API_KEY"), Sys.getenv("SHIPSTATION_API_SECRET")),
    query = list()
  )
  xx <- httr::content(x)
  ss_parse_response(xx)
}


#' Parse a nested list returned by the API as a tibble
#' @param response a nested list
#' @export
ss_parse_response <- function(response) {
  purrr::map_df(
    response,
    function(r) {
      vars <- names(r)[purrr::map_lgl(r, ~length(.x) == 1)]   # TODO: this ignores entries with > 1 length (e.g., multiple suppliers)
      tibble::as_tibble(r[vars])
    }
  )
}