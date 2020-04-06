
#' Get shipping rates for ALL carriers for a given order
#' @param order_number order number (numeric/scalar)
#' @param carriers carriers (character/vector)
#' @param speed shipping speed (character/scalar)
#' @export
ss_get_rates <- function(order_number = "3023035",
                         carriers = c("stamps_com", "ups", "fedex", "amazon_shipping", "amazon_shipping_us"),
                         speed = c("standard", "2day", "1day")) {

  # set shipping speed restrictions (serviceCode)
  speed <- match.arg(speed)

  # TODO: set package restrictions (if has weight/dimensions)
  # TODO: set carrier restrictions (based on marketplace)
  # TODO: set ground/air restrictions (liquids/batteries)

  p <- ss_get_orders(order_number = order_number)

  out <-
    purrr::map_df(
      carriers,
      ~ss_get_carrier_rates(
        carrier = .x,
        fromPostalCode = "18901",
        toPostalCode = stringr::str_split(p$shipTo[[1]]$postalCode, "-")[[1]][1],
        toCountry = p$shipTo[[1]]$country,
        #weight = list(value = p$weight[[1]]$value, units = p$weight[[1]]$units),
        weight = as.list(p$weight[[1]]),
        serviceCode = dplyr::case_when(
          .x == "amazon_shipping_us" ~ "amazon_shipping_standard",
          TRUE ~ NA_character_
        ),
        packageCode = dplyr::case_when(
          .x == "amazon_shipping_us" ~ "package",
          TRUE ~ NA_character_
        ),
        dimensions = as.list(p$dimensions[[1]]),
        confirmation = dplyr::case_when(
          .x == "amazon_shipping_us" ~ "none",
          TRUE ~ NA_character_
        )
      )
    )
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


