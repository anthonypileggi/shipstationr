#' Estimate product weights based on shipping weights for recent single-item orders
#' @param start_date first day of data (Date/scalar)
#' @param end_date last day of data (Date/scalar)
#' @importFrom magrittr "%>%"
#' @export
ss_get_single_item_shipping_weights <- function(start_date = Sys.Date() - 7, end_date = Sys.Date() - 1) {
  
  # TODO: add 'dimensions'
  
  # get total weights (lbs) for all recent shipments
  shipments <- ss_get_shipments(start_date = start_date, end_date = end_date) %>%
    dplyr::select(orderNumber, weight, shipmentCost, packageCode) %>%
    tidyr::unnest() %>%
    dplyr::mutate(
      value = dplyr::case_when(
        units == "ounces" ~ value / 16,
        TRUE ~ value
      )
    ) %>%
    dplyr::select(orderNumber, ship_weight = value, ship_cost = shipmentCost, packageCode)

  # identify shipment contents and keep only single-item orders
  ss_get_orders(start_date = start_date - 1, end_date = end_date + 1) %>%
    dplyr::mutate(n_items = purrr::map_int(items, nrow)) %>%
    dplyr::filter(
      n_items == 1, 
      orderStatus == "shipped",
      orderNumber %in% shipments$orderNumber
      ) %>%
    dplyr::select(orderNumber, items) %>%
    tidyr::unnest() %>%
    dplyr::mutate(
      value = dplyr::case_when(
        units == "ounces" ~ value / 16,
        TRUE ~ value
      )
    ) %>%
    dplyr::filter(quantity == 1) %>%                               # only count qty=1 orders (TODO: factor in qty>1 orders)
    dplyr::select(orderNumber, sku, ca_weight = value) %>%
    dplyr::left_join(shipments, by = "orderNumber")
}