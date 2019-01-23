

#' Get a status report for the order-->pick-->ship process
get_operations_status <- function(start_date = Sys.Date() - 7, end_date = Sys.Date()) {


  # Order shipment status (via ShipStation)
  # TODO: fix date inputs on 'ss_get_shipments' to align with EST timezone
  shipments <- ss_get_shipments(start_date = start_date, end_date = end_date)
  # should this be 'ss_get_orders'?

  ## Order pick status (via SkuVault)
  x <- skuvaultr::sv_get_sales(start_date = start_date, end_date = end_date)

  # Compute Packing status
  left_join(shipments, x, by = c("orderNumber" = "ChannelId")) %>%
    mutate(
      ship_date = dplyr::case_when(
        lubridate::wday(createDate) == 6 & lubridate::hour(createDate) >= 14 ~ lubridate::date(createDate) + 3,    # friday afternoon
        lubridate::wday(createDate) == 7 ~ lubridate::date(createDate) + 2,                                        # saturday
        lubridate::wday(createDate) == 1 ~ lubridate::date(createDate) + 1,                                        # sunday
        lubridate::hour(createDate) < 14 ~ lubridate::date(createDate),
        lubridate::hour(createDate) >= 14 ~ lubridate::date(createDate) + 1
      ),
      ship_step = dplyr::case_when(
        orderStatus == "shipped" ~ "shipped",
        orderStatus == "cancelled" ~ "cancelled",
        Status == "Completed" & orderStatus == "awaiting_shipment" ~ "packed",
        Status == "ReadyToShip" ~ "ordered"
      )
    ) %>%
    group_by(ship_date, ship_step) %>%
    summarize(orders = n())  %>%
    tidyr::spread(ship_step, orders, fill = 0)
}