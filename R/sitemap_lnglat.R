# sitemap
# parse lat/lng
###
#' Parsing lat and lng
#'
#' @description extract latitude and longitude for geo_field like GEO3212(31.3112,-88.22)
#' @param tbl a tbl
#' @param geo_variable variable navn with the lng and lat
#' @param first the first value is latitude or longitude
#'
#' @return a tbl
#' @export
parse_geo <- function(tbl, geo_variable, first = "latitude") {
  if (!is.character(first)) {
    rlang::abort("Variable first must be character")
  }
  first <- str_to_lower(first)
  if (first %in% c("latitude", "lat")) {
    first <- "latitude"
  } else if (first %in% c("longitude", "lng", "long", "lon")) {
    first <- "longitude"
  } else {
    rlang::abort("Not a valid", body = "Must be latitude or longitude")
  }


  rs <- tbl %>%
    dplyr::mutate(
      new_geo = stringr::str_remove_all({{ geo_variable }}, "[:alpha:]"),
      new_geo = stringr::str_replace_all(new_geo, "[\r\n]", ""),
      new_geo = stringr::str_replace_all(new_geo, "^.*\\(", ""),
      longitude = readr::parse_number(new_geo),
      latitude = stringr::str_remove(new_geo, ".*\\s"),
      latitude = readr::parse_number(latitude),
      dplyr::across(c(latitude, longitude), ~ ifelse(stringr::str_detect({{ geo_variable }}, "\\(") == FALSE, NA, .)),
      dplyr::across(c(latitude, longitude), ~ ifelse(. == 0, NA, .))
    ) %>%
    dplyr::select(-new_geo)

  if (first == "longitude") {
    rs <- rs %>%
      dplyr::rename(
        long = latitude,
        lat = longitude
      ) %>%
      dplyr::rename(longitude = long, latitude = latitude)
  }

  return(rs)
}
######## 3SITEMAP

#' parsing sitemaps
#'
#' @param url sitemap.xml url
#'
#' @return a tibble
#' @export
sitemap <- function(url) {
  rs <-
    xml2::read_xml(url) %>%
    as.character() %>%
    dplyr::as_tibble() %>%
    tidyr::separate_rows(value, sep = "\\n") %>%
    dplyr::filter(stringr::str_detect(value, "<loc>|<lastmod>")) %>%
    dplyr::mutate(
      value = stringr::str_squish(value),
      type = ifelse(stringr::str_detect(value, "^<loc>"), "link", "last_mod")
    ) %>%
    dplyr::group_by(type) %>%
    dplyr::mutate(
      id = dplyr::row_number(),
      value = stringr::str_remove_all(value, "<loc>|</loc>|<lastmod>|</lastmod>")
    ) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = type, values_from = value)

  return(rs)
}
