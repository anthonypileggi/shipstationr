#' Estimate product dimensions based on shipping LxWxH for recent single-item orders
#' @param start_date first day of data (Date/scalar)
#' @param end_date last day of data (Date/scalar)
#' @importFrom magrittr "%>%"
#' @export
ss_get_single_item_shipping_info <- function(start_date = Sys.Date() - 7, end_date = Sys.Date() - 1) {

  # get dimensions/weights/packageType for all recent shipments
  shipments <- ss_get_shipments(start_date = start_date, end_date = end_date) %>%
    dplyr::filter(!voided) %>%
    dplyr::select(orderNumber, carrierCode, serviceCode, weight, dimensions, packageCode, shipmentCost)

  df_wt <- shipments %>%
    tidyr::unnest(weight) %>%
    dplyr::mutate(
      value = dplyr::case_when(
        units == "ounces" ~ value / 16,
        TRUE ~ value
      )
    ) %>%
    dplyr::select(orderNumber, carrierCode, serviceCode, packageCode, weight = value, cost = shipmentCost)
  df_dims <- shipments %>%
    tidyr::unnest(dimensions) %>%
    dplyr::filter(units == "inches") %>%
    dplyr::select(orderNumber, length, width, height)
  df <- dplyr::left_join(df_wt, df_dims, by = "orderNumber")

  # identify shipment contents and keep only single-item orders
  ss_get_orders(start_date = start_date - 1, end_date = end_date + 1) %>%
    dplyr::mutate(n_items = purrr::map_int(items, nrow)) %>%
    dplyr::filter(
      n_items == 1,
      orderStatus == "shipped",
      orderNumber %in% df$orderNumber
    ) %>%
    dplyr::select(orderNumber, items) %>%
    tidyr::unnest(items) %>%
    dplyr::mutate(
      value = dplyr::case_when(
        units == "ounces" ~ value / 16,
        TRUE ~ value
      )
    ) %>%
    dplyr::filter(quantity == 1) %>%                               # only count qty=1 orders (TODO: factor in qty>1 orders)
    dplyr::select(orderNumber, Sku = sku) %>%
    dplyr::inner_join(df, by = "orderNumber")
}