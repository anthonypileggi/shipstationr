
#' Call the ShipStation API
#' @param order_number Order Number (integer/vector)
#' @param start_date first day of data (Date/scalar)
#' @param end_date last day of data (Date/scalar)
#' @importFrom magrittr "%>%"
#' @export
ss_get_shipments <- function(order_number = NULL, start_date = Sys.Date() - 1, end_date = Sys.Date() - 1) {
  
  # query api, iterating over all pages
  go <- TRUE
  pg <- 0
  out <- list()
  while (go) {
    pg <- pg + 1
    x <- ss_api("shipments",
        orderNumber = order_number,
        createDateStart = paste(start_date - 1, "21:00:00"),     # TODO: adjust start/end_date so align with {local} timezone
        createDateEnd = paste(end_date, "20:59:59"),
        pageSize = 500,
        page = pg
      )
    out <- c(out, x$shipments)
    if (x$page > 1)
      message("Collecting page ", x$page, "/", x$pages)
    if (x$page >= x$pages)
      go <- FALSE
  }
  
  # clean-up api response
  dplyr::mutate(
    ss_parse_response(out),
    createDate = ss_parse_datetime(createDate),
    #voidDate = as.POSIXct(strptime(voidDate, "%Y-%m-%dT%H:%M:%OS")),
    shipDate = as.Date(shipDate)
  )
}
