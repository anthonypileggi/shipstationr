#' Call the ShipStation Orders API
#' @param order_number Order Number (integer/vector)
#' @param start_date first day of data (Date/scalar)
#' @param end_date last day of data (Date/scalar)
#' @export
ss_get_orders <- function(order_number = NULL,
                          start_date = Sys.Date() - 1,
                          end_date = Sys.Date()) {

  if (!is.null(order_number)) {
    start_date <- as.Date("2000-01-01")
    end_date <- Sys.Date() + 10
  }

  # query api, iterating over all pages
  go <- TRUE
  pg <- 0
  out <- list()
  while (go) {
    pg <- pg + 1
    x <- ss_api("orders",
      orderNumber = order_number,
      orderDateStart = paste(start_date - 1, "21:00:00"),     # TODO: adjust start/end_date so align with {local} timezone
      orderDateEnd = paste(end_date, "20:59:59"),
      pageSize = 500,
      page = pg
    )
    out <- c(out, x$orders)
    if (x$page > 1)
      message("Collecting page ", x$page, "/", x$pages)
    if (x$page >= x$pages)
      go <- FALSE
  }

  # TODO: convert 'items' to nested tibble

  # check if response object is empty
  if (x$total == 0) {
    message("No orders found.")
    return(invisible(NULL))
  }

  # clean-up api response
  dplyr::mutate_at(
    ss_parse_response(out),
    dplyr::vars(orderDate, orderDate, createDate, paymentDate, modifyDate),
    ss_parse_datetime
  )
}
