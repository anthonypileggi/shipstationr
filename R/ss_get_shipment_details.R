#' Get recent shipment info/details
#' @importFrom magrittr "%>%"
#' @export
ss_get_shipment_details <- function(start_date = Sys.Date() - 7, end_date = Sys.Date() - 1) {

  orders <- ss_get_orders(start_date = start_date - 5, end_date = end_date + 2)

  shipments <- ss_get_shipments(start_date = start_date, end_date = end_date)
  users <- ss_get_users()

  shipments %>%
    dplyr::left_join(users, by = "userId") %>%
    dplyr::select(orderKey, name, createDate, shipDate, shipmentCost, shipTo, serviceCode) %>%
    dplyr::arrange(desc(shipmentCost)) %>%
    dplyr::left_join(
      dplyr::select(orders, orderKey, items),
      by = "orderKey"
    ) %>%
    dplyr::mutate(
      orderTotal = purrr::map_dbl(items, ~sum(.x$quantity * .x$unitPrice)),
      weight = purrr::map_dbl(items, ~sum(ifelse(.x$units == "ounces", .x$value / 16, .x$value))),
      shipTo = purrr::map_chr(shipTo, ~paste(.x$state, .x$country, sep = ", ")),
      Sku = purrr::map(items, ~paste0(.x$sku, " (", .x$quantity, ")")),
      Sku = purrr::map_chr(Sku, ~paste0(sort(.x), collapse = ", ")),
      is_priority = serviceCode %in% c("ups_2nd_day_air")
    ) %>%
    dplyr::select(-items)
}