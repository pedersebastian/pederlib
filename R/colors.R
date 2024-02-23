#' Scrapes values from a coolors.co palette
#'
#' @param url coolors.co link
#' @param cat cat
#'
#' @return vector of colors
#' @export
#'
#' @examples
#' x <- "https://coolors.co/96adc8-d7ffab-fcff6c-d89d6a-6d454c"
#' coolors_scrape(x, cat = TRUE)
coolors_scrape <- function(url, cat = FALSE) {
  if (!is.character(url)) {
    rlang::abort("Input must be character")
  }
  rs <-
    url %>%
    stringr::str_extract(pattern = "([^/]+$)") %>%
    stringr::str_split(pattern = "-") %>%
    unlist() %>%
    paste0("#", .)

  if (cat) {
    rs <-
      paste0(rs, ", ")
    return(cat(rs))
  }
  return(rs)
}
