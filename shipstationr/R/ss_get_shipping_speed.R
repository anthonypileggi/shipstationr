#' Get shipping speed (overall; user-specific)
#' @param start_date first day of data (Date/scalar)
#' @param end_date last day of data (Date/scalar)
#' @importFrom magrittr "%>%"
#' @export
ss_get_shipping_speed <- function(start_date = Sys.Date(), end_date = Sys.Date()) {
  
  users <- ss_get_users()
  shipments <- ss_get_shipments(start_date = start_date, end_date = end_date) %>%
    dplyr::arrange(createDate)
  
  # overall
  out1 <- shipments %>%
    dplyr::mutate(
      diff = as.numeric(difftime(createDate, lag(createDate), units = "secs")),
      diff = ifelse(diff > 60*30, NA, diff)
    ) %>%
    dplyr::summarize(
      name = "Total",
      shipments = dplyr::n(),
      avg = mean(diff, na.rm = TRUE),
      recent = mean(tail(diff, 20), na.rm = TRUE)
    ) 
  
  # by user
  out2 <- shipments %>%
    dplyr::left_join(users, by = "userId") %>%
    dplyr::group_by(name) %>%
    dplyr::mutate(
      diff = as.numeric(difftime(createDate, lag(createDate), units = "secs")),
      diff = ifelse(diff > 60*30, NA, diff)                                      # ignore lags over 30 minutes (breaktime?)
    ) %>%
    dplyr::summarize(
      shipments = dplyr::n(),
      avg = mean(diff, na.rm = TRUE),
      recent = mean(tail(diff, 10), na.rm = TRUE)
    )
  
  dplyr::bind_rows(out2, out1)
}
