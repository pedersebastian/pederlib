
#' Theme Peder
#'
#' @description
#' Based on theme_minimal\n
#' Font is `BentonSans Regular`, and must be installed first\n
#' title, subtitle are centered
#'
#' @param base_size b
#' @param strip_text_size b
#' @param strip_text_margin b
#' @param subtitle_size b
#' @param subtitle_margin b
#' @param plot_title_size b
#' @param plot_title_margin b
#' @param ... x
#'
#' @return
#' @export
#'
#' @examples
theme_pedr <- function(base_size = 11,
                       strip_text_size = 12,
                       strip_text_margin = 5,
                       subtitle_size = 13,
                       subtitle_margin = 10,
                       plot_title_size = 16,
                       plot_title_margin = 10,
                       strip_color = "gray90",
                       ...) {
  ret <- ggplot2::theme_minimal(
    base_family = "BentonSans Regular",
    base_size = base_size, ...
  )
  ret$strip.text <- ggplot2::element_text(
    hjust = 0.5, size = strip_text_size,
    margin = ggplot2::margin(b = strip_text_margin),
    family = "BentonSans Regular"
  )
  ret$strip.background <- ggplot2::element_rect(
    fill = strip_color,
    color = NA,
    size = NULL,
    linetype = NULL
  )
  ret$plot.subtitle <- ggplot2::element_text(
    hjust = 0.5, size = subtitle_size,
    margin = ggplot2::margin(b = subtitle_margin),
    family = "BentonSans Regular",
    color = "gray10"
  )
  ret$plot.title <- ggplot2::element_text(
    hjust = 0.5, size = plot_title_size,
    margin = ggplot2::margin(b = plot_title_margin),
    family = "BentonSans Bold"
  )

  ret
}

#############################################################################
#' Title
#'
#' @description #theme_center
#' Based on theme_minimal
#' title, subtitle are centered

#'
#' @param base_size x
#' @param strip_text_size x
#' @param strip_text_margin x
#' @param subtitle_size x
#' @param subtitle_margin x
#' @param plot_title_size x
#' @param plot_title_margin x
#' @param ... x
#'
#' @return
#' @export
#'
#' @examples
#' library(ggplot2)
#' data(faithful)
#' faithful %>%
#'   ggplot(aes(eruptions)) +
#'   geom_histogram() +
#'   scale_x_continuous(labels = komma()) +
#'   theme_center() +
#'   labs(title = "This is a title", subtitle = "This is a subtitle")
theme_center <- function(base_size = 11,
                         strip_text_size = 12,
                         strip_text_margin = 5,
                         subtitle_size = 13,
                         subtitle_margin = 10,
                         plot_title_size = 16,
                         plot_title_margin = 10,
                         strip_color = "gray90",
                         ...) {
  ret <- ggplot2::theme_minimal(
    base_family = NULL,
    base_size = base_size, ...
  )
  ret$strip.text <- ggplot2::element_text(
    hjust = 0.5,
    size = strip_text_size,
    margin = ggplot2::margin(b = strip_text_margin),
    family = NULL
  )

  ret$strip.background <- ggplot2::element_rect(
    fill = strip_color,
    color = NA,
    size = NULL,
    linetype = NULL
  )
  ret$plot.subtitle <- ggplot2::element_text(
    hjust = 0.5, size = subtitle_size,
    margin = ggplot2::margin(b = subtitle_margin),
    family = NULL,
    color = "gray10"
  )
  ret$plot.title <- ggplot2::element_text(
    hjust = 0.5, size = plot_title_size,
    margin = ggplot2::margin(b = plot_title_margin),
    family = NULL
  )

  ret
}

#########################

#' Title
#'
#' @param ... passed on to scales::comma_format
#'
#' @return
#' @export
komma <- function(...) {
  scales::comma_format(decimal.mark = ",", big.mark = ".", ...)
}

#' Title
#'
#' @param ... passed on to scales::percent_format
#'
#' @return
#' @export
prosent <- function(...) {
  scales::percent_format(decimal.mark = ",", big.mark = ".", ...)
}
