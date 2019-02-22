#' Get shipping speed (overall; user-specific)
#' @param start_date first day of data (Date/scalar)
#' @param end_date last day of data (Date/scalar)
#' @param show_plot show time-series plot (logical/scalar)
#' @importFrom magrittr "%>%"
#' @export
ss_get_shipping_speed <- function(start_date = Sys.Date(), 
                                  end_date = Sys.Date(),
                                  show_plot = TRUE) {
  
  users <- ss_get_users()
  shipments <- ss_get_shipments(start_date = start_date, end_date = end_date) %>%
    dplyr::arrange(createDate)
  
  # overall
  out1 <- shipments %>%
    dplyr::mutate(
      diff = as.numeric(difftime(createDate, dplyr::lag(createDate), units = "secs")),
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
      diff = as.numeric(difftime(createDate, dplyr::lag(createDate), units = "secs")),
      diff = ifelse(diff > 60*30, NA, diff)                                      # ignore lags over 30 minutes (breaktime?)
    ) %>%
    dplyr::summarize(
      shipments = dplyr::n(),
      avg = mean(diff, na.rm = TRUE),
      recent = mean(tail(diff, 10), na.rm = TRUE)
    )
  
  # plot time-series
  if (show_plot) {
    require(ggplot2)
    g1 <- shipments %>%
      dplyr::left_join(users, by = "userId") %>%
      dplyr::group_by(lubridate::date(createDate), name) %>%
      dplyr::mutate(
        count = dplyr::row_number()
      ) %>%
      ggplot(aes(x = createDate, y = count, group = name, color = name)) +
      geom_point() +
      labs(x = NULL, y = "Total Shipped", color = NULL) +
      theme_bw()
    print(g1)
  }
  
  dplyr::bind_rows(out2, out1)
}
