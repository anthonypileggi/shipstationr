#' Get ShipStation order tags
#' @export
ss_get_tags <- function() {
  purrr::map_df(
    shipstationr::ss_api("accounts/listtags"), 
    dplyr::as_tibble
  )
}