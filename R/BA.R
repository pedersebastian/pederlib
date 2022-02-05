#' @importFrom stats lm
#' @importFrom broom tidy glance augment
#' @importFrom tibble tibble add_column
#' @importFrom magrittr %>%
#' @import ggplot2


### Ny Bland-altman plot ( my version of deepankardatta/blandr package
min_BA <-function (statistics.results,
                   method1name = "Method 1",
                   method2name = "Method 2",
                   plotTitle = "Bland-Altman plot for comparison of 2 methods",
                   ciDisplay = TRUE,
                   ciShading = TRUE,
                   normalLow = FALSE,
                   normalHigh = FALSE,
                   overlapping = FALSE,
                   x.plot.mode = "means",
                   y.plot.mode = "difference",
                   plotProportionalBias = FALSE,
                   plotProportionalBias.se = TRUE,
                   assume.differences.are.normal = TRUE,
                   bias_color = "#06d6a0",
                   ULoA_color = "#ef476f",
                   LLoA_color = "#118ab2")
{
  require(ggplot2)
  x.axis <- statistics.results$means
  if (y.plot.mode == "proportion") {
    y.axis <- statistics.results$proportion
  }
  else {
    y.axis <- statistics.results$differences
  }
  plot.data <- data.frame(x.axis, y.axis)
  colnames(plot.data)[1] <- "x.axis"
  colnames(plot.data)[2] <- "y.axis"
  ba.plot <- ggplot(plot.data, aes(x = plot.data$x.axis, y = plot.data$y.axis)) +
              geom_point() +
              theme(plot.title = element_text(hjust = 0.5)) +
              geom_hline(yintercept = 0, linetype = 1) +
              geom_hline(yintercept = statistics.results$bias, linetype = 2) +
              geom_hline(yintercept = statistics.results$bias + (statistics.results$biasStdDev * statistics.results$sig.level.convert.to.z), linetype = 2) +
              geom_hline(yintercept = statistics.results$bias - (statistics.results$biasStdDev * statistics.results$sig.level.convert.to.z), linetype = 2) +
              ggtitle(plotTitle) + xlab("Means")

  if (y.plot.mode == "proportion") {
    ba.plot <- ba.plot + ylab("Difference / Average %")
  }
  else {
    ba.plot <- ba.plot + ylab("Differences")
  }
  if (ciDisplay == TRUE) {
    ba.plot <- ba.plot + geom_hline(yintercept = statistics.results$biasUpperCI,
                                    linetype = 3) +
      geom_hline(yintercept = statistics.results$biasLowerCI,
                 linetype = 3) +
      geom_hline(yintercept = statistics.results$upperLOA_upperCI, linetype = 3) +
      geom_hline(yintercept = statistics.results$upperLOA_lowerCI, linetype = 3) +
      geom_hline(yintercept = statistics.results$lowerLOA_upperCI, linetype = 3) +
      geom_hline(yintercept = statistics.results$lowerLOA_lowerCI, linetype = 3)

    if (ciShading == TRUE) {
      ba.plot <- ba.plot +
        annotate("rect",
                 xmin = -Inf,
                 xmax = Inf,
                 ymin = statistics.results$biasLowerCI,
                 ymax = statistics.results$biasUpperCI,
                 fill = bias_color,
                 alpha = 0.35) +
        annotate("rect",
                 xmin = -Inf,
                 xmax = Inf,
                 ymin = statistics.results$upperLOA_lowerCI,
                 ymax = statistics.results$upperLOA_upperCI,
                 fill = ULoA_color,
                 alpha = 0.45) +
        annotate("rect",
                 xmin = -Inf,
                 xmax = Inf,
                 ymin = statistics.results$lowerLOA_lowerCI,
                 ymax = statistics.results$lowerLOA_upperCI,
                 fill = LLoA_color,
                 alpha = 0.45)
    }
  }
  if (normalLow != FALSE) {
    if (is.numeric(normalLow) == TRUE) {
      ba.plot <- ba.plot + geom_vline(xintercept = normalLow,
                                      linetype = 4, col = 6)
    }
  }
  if (normalHigh != FALSE) {
    if (is.numeric(normalHigh) == TRUE) {
      ba.plot <- ba.plot + geom_vline(xintercept = normalHigh,
                                      linetype = 4, col = 6)
    }
  }
  if (overlapping == TRUE) {
    ba.plot <- ba.plot + geom_count()
  }
  if (plotProportionalBias == TRUE) {
    if (plotProportionalBias.se != TRUE && plotProportionalBias.se !=
        FALSE) {
      plotProportionalBias.se <- TRUE
    }
    ba.plot <- ba.plot + ggplot2::geom_smooth(method = "lm",
                                              se = plotProportionalBias.se)
  }
  return(ba.plot)
}


##############
tidy.blandr <- function(x,
                        conf.int = FALSE,
                        regression_equation = FALSE) {


  rs <- tibble::tibble(bias = x$bias,
                       bias_sd = x$biasStdDev,
                       bias_se = x$biasSEM,
                       upper_LoA = x$upperLOA,
                       lower_LoA = x$lowerLOA,
                       regression_fixed_slope = x$regression.fixed.slope,
                       regression_fixed_intercept = x$regression.fixed.intercept
  )

  if (conf.int) {
    rs <-   rs %>%
      tibble::add_column(
        bias_conf_low = x$biasLowerCI, .after = "bias") %>%
      tibble::add_column(
        bias_conf_high = x$biasUpperCI, .after = "bias_conf_low") %>%
      tibble::add_column(
        upper_LoA_conf_low = x$upperLOA_lowerCI, .after = "upper_LoA")%>%
      tibble::add_column(
        upper_LoA_conf_high = x$upperLOA_upperCI, .after = "upper_LoA_conf_low") %>%
      tibble::add_column(
        lower_LoA_conf_low = x$lowerLOA_lowerCI, .after = "lower_LoA") %>%
      tibble::add_column(
        lower_LoA_conf_high = x$lowerLOA_upperCI, .after = "lower_LoA_conf_low"
      )

    if (regression_equation) {
      rs <- rs %>%
        tibble::add_column(
          regression_equation = x$regression.equation, .before = regression_fixed_slope
        )

    }

  }

  return(rs)

}

#### Augment

# Example
# rs %>%
#   augment(new_data = NULL)

augment.blandr <- function(x, new_data = NULL) {

  if (!is.null(new_data)) {
    rlang::abort(
      "Blandr cannot be augmented on new data",
      body = "Please leave new_data as NULL",
    )
  }
  rs <-  tibble::tibble(
    method_1 = x$method1,
    method_2 = x$method2,
    differences = x$differences,
    mean = x$means,
    proportion = x$proportion
  )
  return(rs)

}

################# Glance
#Example
#glance(rs)

glance <- function(x,...) {
  model <- lm(x$differences ~ x$means)

  rs <- broom::glance(model, ...)

  rlang::inform("Caution:",
                body = "Glanced is based on the lm-object of differences ~ means")
  return(rs)
}
