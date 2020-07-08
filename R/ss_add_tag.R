#' Add tag to an order in ShipStation
#' @param order_number order number (character/scalar)
#' @param tag_id tag id (numeric/scalar)
#' @export
ss_add_tag <- function(order_number, tag_id = NULL) {
  
  if (is.null(tag_id))
    stop("You must specify the tag to use!")
  
  # get orderId for the specified order
  order <- shipstationr::ss_get_orders(order_number = order_number)
  
  if (nrow(order) == 1) {
    out <- 
      shipstationr::ss_api(
        path = "orders/addtag", 
        fun = httr::POST,
        orderId = order$orderId,
        tagId = 95540
      )
  } else {
    out <- NULL
  }
  
  return(invisible(out))
}

