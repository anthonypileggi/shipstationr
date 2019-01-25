
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
        createDateStart = start_date,
        createDateEnd = end_date,
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
  purrr::map_df(
    out,
    function(r) {
      vars <- names(r)[purrr::map_lgl(r, ~length(.x) == 1)]        # TODO: this ignores entries with > 1 length (e.g., multiple suppliers)
      tmp <- tibble::as_tibble(r[vars])
      for (v in names(r)[purrr::map_lgl(r, is.list)] ) {   # format list objects as tibbles
        vv <- names(r[[v]])[purrr::map_lgl(r[[v]], ~length(.x) == 1)]
        tmp[[v]] <- list(dplyr::as_tibble(r[[v]][vv]))
      }
      tmp
    }
  ) %>%
  dplyr::mutate(
    #ss_parse_response(out),
    createDate = ss_parse_datetime(createDate),
    #voidDate = as.POSIXct(strptime(voidDate, "%Y-%m-%dT%H:%M:%OS")),
    shipDate = as.Date(shipDate)
  )
}
