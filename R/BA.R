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
