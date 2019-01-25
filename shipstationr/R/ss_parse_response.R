#' Parse a nested list returned by the API as a tibble
#' @param response a nested list
#' @export
ss_parse_response <- function(response) {
  
  purrr::map_df(
    response,
    function(r) {
      vars <- names(r)[purrr::map_lgl(r, ~length(.x) == 1)]        # TODO: this ignores entries with > 1 length (e.g., multiple suppliers)
      tmp <- tibble::as_tibble(r[vars])
      for (v in names(r)[purrr::map_lgl(r, is.list)] ) {   # format list objects as tibbles
        vv <- names(r[[v]])[purrr::map_lgl(r[[v]], ~length(.x) == 1)]
        tmp[[v]] <- list(dplyr::as_tibble(r[[v]][vv]))
      }
      tmp
    }
  )
  
  # purrr::map_df(
  #   response,
  #   function(r) {
  #     #vars <- names(r)[purrr::map_lgl(r, ~length(.x) == 1)]   # TODO: this ignores entries with > 1 length (e.g., multiple suppliers)
  #     #tmp <- tibble::as_tibble(r[vars])
  #     is_list <- purrr::map_lgl(r, is.list)
  #     vars <- names(r)[!is_list]
  #     list_vars <- names(r)[is_list]
  #     tmp <- tibble::as_tibble(purrr::flatten(r[vars]))     # remove NULL columns
  #     purrr::map(
  #       list_vars,
  #       function(v) {
  #         if (v == "items") {
  #           xt <- purrr::map_df(r[[v]], rlang::flatten)  
  #         } else {
  #           xt <- dplyr::as_tibble(purrr::flatten(r[[v]]))
  #         }
  #         tmp[[v]] <- list(xt)
  #         # if (v %in% names(r)) {
  #         #   items <- purrr::map_df(r$items, purrr::flatten)
  #         #   tmp$items <- list(items)
  #         # }
  #       }
  #     )
  #     tmp
  #   }
  # )
}