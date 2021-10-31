

##################################

###  theme_pedr
# *** Based on theme_minimal
# *** Font is `BentonSans Regular`, and must be installed first
# *** title, subtitle are centered

##theme_center
# *** Based on theme_minimal
# *** title, subtitle are centered

##library(ggplot2)
# faithfuld %>%
#    ggplot(aes(density)) + geom_histogram()+ scale_x_continuous(labels = komma()) + theme_pedr()+ labs(title = "This is a title", subtitle = "This is a subtitle")


theme_pedr <- function(base_size = 11,
                       strip_text_size = 12,
                       strip_text_margin = 5,
                       subtitle_size = 13,
                       subtitle_margin = 10,
                       plot_title_size = 16,
                       plot_title_margin = 10,
                       ...) {
  ret <- ggplot2::theme_minimal(base_family = "BentonSans Regular",
                                base_size = base_size, ...)
  ret$strip.text <- ggplot2::element_text(
    hjust = 0.5, size = strip_text_size,
    margin = ggplot2::margin(b = strip_text_margin),
    family = "BentonSans Regular"
  )
  ret$strip.background <-  ggplot2::element_rect(
    fill = "gray80",
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
theme_center <- function(base_size = 11,
                         strip_text_size = 12,
                         strip_text_margin = 5,
                         subtitle_size = 13,
                         subtitle_margin = 10,
                         plot_title_size = 16,
                         plot_title_margin = 10,
                         ...) {
  ret <- ggplot2::theme_minimal(base_family = NULL,
                                base_size = base_size, ...)
  ret$strip.text <- ggplot2::element_text(
    hjust = 0.5, size = strip_text_size,
    margin = ggplot2::margin(b = strip_text_margin),
    family = NULL
  )

  ret$strip.background <-  ggplot2::element_rect(
    fill = "gray80",
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

komma <- function(...) {
  scales::comma_format(decimal.mark = ",", ...)
}

prosent <- function(...) {
  scales::percent_format(decimal.mark = ",", ...)
}




