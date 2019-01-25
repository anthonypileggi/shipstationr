#' Make a ShipStation API call
#' @param path path to api endpoint
#' @export
ss_api <- function(path, ...) {
  
  # setup creds
  if (nchar(Sys.getenv("SHIPSTATION_API_KEY")) == 0 | nchar(Sys.getenv("SHIPSTATION_API_SECRET")) == 0)
    stop("You must set environment variables: SHIPSTATION_API_KEY, SHIPSTATION_API_SECRET", call. = FALSE)
  
  # call api
  url <- httr::modify_url("https://ssapi.shipstation.com", path = path)
  auth <- httr::authenticate(Sys.getenv("SHIPSTATION_API_KEY"), Sys.getenv("SHIPSTATION_API_SECRET"))
  response <- httr::GET(url, auth, query = list(...))

  # check status code
  if (httr::status_code(response) != 200) {
    stop(
      sprintf(
        "ShipStation API request failed [%s]\n%s\n<%s>",
        httr::status_code(response)
      ),
      call. = FALSE
    )
  }
  
  httr::content(response)
}