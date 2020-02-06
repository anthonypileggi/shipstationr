
#' Get shipping rates for ALL carriers for a given order
#' @param order_number order number (numeric/scalar)
#' @param carriers carriers (character/vector)
#' @export
ss_get_rates <- function(order_number = "3023035",
                         carriers = c("stamps_com", "ups")) {
  p <- ss_get_orders(order_number = order_number)
  out <-
    purrr::map_df(
      carriers,
      ~ss_get_carrier_rates(
        carrier = .x,
        fromPostalCode = "18901",
        toPostalCode = stringr::str_split(p$shipTo[[1]]$postalCode, "-")[[1]][1],
        toCountry = p$shipTo[[1]]$country,
        weight = list(value = p$weight[[1]]$value, units = p$weight[[1]]$units),
        packageCode = "package",
        # TODO: only add dimensions when available!!!
        dimensions = list(
          length = p$dimensions[[1]]$length,
          width = p$dimensions[[1]]$width,
          height = p$dimensions[[1]]$height,
          units = "inches"
          )
      )
    ) %>%
  dplyr::arrange(out, shipmentCost)
}


#' Get shipping rates for specific carriers
#' @param ... payload passed on to API
#' @export
ss_get_carrier_rates <- function(carrier, ...) {
  # TODO: try/catch any errors due to carrier restrictions
  out <- tryCatch(
    purrr::map_df(
      ss_api("shipments/getrates", httr::POST, carrierCode = carrier, ...),
      ~.x
    ),
    error = function(e) {
      message("Cannot get rates for `", carrier, "`!\n")
      return(NULL)
    }
  )
  return(out)
}


