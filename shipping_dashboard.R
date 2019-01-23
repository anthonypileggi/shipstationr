
## Shipstation Dashboard
require(tidyverse)

shipments <- ss_get_shipments(start_date = Sys.Date() - 7, end_date = Sys.Date())
users <- ss_get_users()

## Picking Orders
x <- sv_get_sales(start_date = Sys.Date() - 7, end_date = Sys.Date())


# Orders by Date/Status
orders <- ss_get_orders(start_date = Sys.Date() - 7, end_date = Sys.Date())
orders %>%
  mutate(date = as.Date(orderDate)) %>%
  group_by(date, orderStatus) %>%
  summarize(
    orders = n()
  ) %>%
  tidyr::spread(orderStatus, orders, fill = 0)

# Split into our 'shipping day' (2pm previous day - 2pm current day)
# TODO: update for weekends
# TODO: probably just should get 'actual_shipped' from 'shipments' endpoint
orders %>%
  mutate(ship_date = dplyr::case_when(
    lubridate::wday(createDate) == 6 & lubridate::hour(createDate) >= 14 ~ as.Date(createDate) + 3,    # friday afternoon
    lubridate::wday(createDate) == 7 ~ as.Date(createDate) + 2,                                        # saturday
    lubridate::wday(createDate) == 1 ~ as.Date(createDate) + 1,                                        # sunday
    lubridate::hour(createDate) < 14 ~ as.Date(createDate),
    lubridate::hour(createDate) >= 14 ~ as.Date(createDate) + 1
  )) %>%
  group_by(ship_date, orderStatus) %>%
  summarize(
    orders = n()
  ) %>%
  tidyr::spread(orderStatus, orders, fill = 0)


# packing status
left_join(orders, x, by = c("orderNumber" = "ChannelId")) %>%
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



# General ----------------------------

# Shipment counts by date/user
shipments %>%
  left_join(users, by = "userId") %>%
  group_by(date = shipDate, name) %>%
  summarize(
    count = n()
  ) %>%
  tidyr::spread(name, count, fill = 0)


# Shipping Speed Today --------------

# Overall
shipments %>%
  filter(as.Date(createDate) == Sys.Date()) %>%
  mutate(diff = createDate - lag(createDate)) %>%
  summarize(
    avg_time_bw_shipments = mean(diff, na.rm = TRUE)
  )


# By User
shipments %>%
  #filter(as.Date(createDate) == Sys.Date()) %>%
  left_join(users, by = "userId") %>%
  mutate(date = )
  group_by(name, as.Date(createDate)) %>%
  mutate(
    diff = as.numeric(difftime(createDate, lag(createDate), units = "secs")),
    diff = ifelse(diff > 60*30, NA, diff)                                      # ignore lags over 30 minutes (breaktime?)
    ) %>%
  summarize(
    shipments = n(),
    avg_time_to_ship = mean(diff, na.rm = TRUE)
  ) %>%
  mutate_if(is.numeric, round, 1)


# Orders ----------------------
