
#' Mark orders as shipped and add tracking info
#' @param orderid OrderId
#' @param carrient shipping carrier
#' @param tracking tracking number
#' @param ship_date date shipped
#' @param notify_customer should customer be notified? (logical/scalar)
#' @param notify_channel should channel be notified? (logical/scalar)
#' @export
ss_mark_as_shipped <- function(orderid,
                               carrier,
                               tracking,
                               ship_date,
                               notify_customer = FALSE,
                               notify_channel = TRUE) {

  # load order infomartion
  message(paste("Loading info for order #", orderid))
  order <- ss_get_orders(orderid)

  # check if order was found
  if (is.null(order) || nrow(order) != 1) {
    message(paste0("Could not find an order matching # ", orderid, "."))
    return(invisible())
  }

  # confirm it is not yet shipped (or has no tracking)
  if (order$orderStatus != "awaiting_shipment") {
    message(paste("Order #", orderid, "does not need to be shipped!"))
    return(invisible())
  }

  # mark as shipped
  ss_api(
    path = "orders/markasshipped",
    fun = httr::POST,
    orderId = order$orderId,
    carrierCode = carrier,
    shipDate = format(ship_date, "%Y-%m-%d"),
    trackingNumber = tracking,
    notifyCustomer = notify_customer,
    notifySalesChannel = notify_channel
  )

}