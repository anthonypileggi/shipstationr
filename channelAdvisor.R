## CHANNEL ADVISOR API FUNCTIONS

# TODO: Create a new product listing

#' Refresh an access token
#' @param access_token dev access token
ca_refresh_token <- function() {
  response <- httr::POST(
    "https://api.channeladvisor.com/oauth2/token", 
    httr::authenticate(Sys.getenv("CA_APP_ID"), Sys.getenv("CA_SECRET")), 
    query = list(
      client_id = Sys.getenv("CA_APP_ID"),
      grant_type = "soap",
      developer_key = Sys.getenv("CA_DEV_KEY"),
      password = Sys.getenv("CA_DEV_PASSWORD"),
      account_id = Sys.getenv("CA_ACCOUNT_ID")
      #grant_type = "refresh_token",
      #refresh_token = Sys.getenv("CA_REFRESH_TOKEN")
      )
    )
  
}


#' Return product info (possibly by SKU)
#' @param sku product SKU
#' @param access_token dev access token
#' @export
ca_get_product_info <- function(sku = NULL, access_token = Sys.getenv("CA_ACCESS_TOKEN")) {
  q <- list(access_token = access_token)
  if (!is.null(sku))
    q <- c(q, "$filter" = paste0("Sku eq '", sku, "'"))
  x <- httr::GET("https://api.channeladvisor.com/v1/Products", query = q)
  xx <- httr::content(x)
  # TODO: paginate
  #   USE the next link in xx[["@odata.nextLink"]]
  purrr::map_df(purrr::map(xx$value, ~replace(.x, lengths(.) == 0, NA)), dplyr::as_tibble)
}
