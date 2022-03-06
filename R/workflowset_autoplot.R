

# wfs_autoplot <- function(trained_workflowset, metric = "roc_auc", n = 1, all_metrics = TRUE, rank_by_metric = FALSE, ...) {
#   minimize <- c("mn_log_loss", "rmse", "mae", "mape", "mase")
#
#   p_df <-
#     collect_metrics(trained_workflowset) %>%
#     separate(wflow_id, into = c("recipe", "model_type"), sep = "_", remove = F, extra = "merge") %>%
#     mutate(
#       metric_name = str_to_upper(.metric),
#       metric_name = case_when(
#         metric_name == "ROC_AUC" ~ "Area Under the ROC-curve (ROC AUC)",
#         metric_name == "RMSE" ~ "Root Mean Square Error (RMSE)",
#         metric_name == "SPEC" ~ "Specificity",
#         metric_name == "SENS" ~ "Sensitivity",
#         metric_name == "ACCURACY" ~ "Accuracy",
#         metric_name == "MN_LOG_LOSS" ~ "Mean log loss",
#         metric_name == "RSQ" ~ "R-Squared",
#         metric_name == "MAE" ~ "Mean absolutt error",
#         metric_name == "MASE" ~ "Mean absolutt squared error",
#         TRUE ~ metric_name
#       ),
#       metric_name = str_replace_all(metric_name, "\\_", " ")
#     )
#
#
#
#   p_df_min <- p_df %>%
#     filter(.metric %in% minimize) %>%
#     group_by(wflow_id, .metric) %>%
#     slice_min(order_by = mean, n = n, with_ties = F) %>%
#     group_by(.metric) %>%
#     arrange(mean) %>%
#     group_by(.metric) %>%
#     mutate(
#       workflow_rank = row_number(),
#       dir = "min"
#     ) %>%
#     ungroup()
#
#
#   p_df_max <- p_df %>%
#     filter(!.metric %in% minimize) %>%
#     group_by(wflow_id, .metric) %>%
#     slice_max(order_by = mean, n = n, with_ties = F) %>%
#     group_by(.metric) %>%
#     arrange(-mean) %>%
#     mutate(
#       workflow_rank = row_number(),
#       dir = "max"
#     ) %>%
#     ungroup()
#
#   if (rank_by_metric == TRUE) {
#     p_df_joined <- p_df %>%
#       group_by(wflow_id, .metric) %>%
#       slice_max(order_by = mean, n = n, with_ties = F) %>%
#       group_by(.metric) %>%
#       arrange(-mean) %>%
#       mutate(workflow_rank = row_number()) %>%
#       ungroup()
#
#     x_axis <- glue::glue("Workflowrank based on {metric}")
#   } else {
#     p_df_joined <- bind_rows(p_df_min, p_df_max)
#
#     x_axis <- glue::glue("Workflowrank based on each different metric")
#   }
#
#
#
#
#   if (all_metrics) {
#     fct_wr <- facet_wrap(vars(metric_name), scales = "free")
#
#     facet <- list(fct_wr)
#     y_axis <- list(labs(y = NULL))
#   } else {
#     p_df_joined <- p_df_joined %>%
#       filter(.metric == metric)
#     facet <- list()
#     y_axis <- list(labs(y = glue::glue("{p_df_joined$metric_name}")))
#   }
#
#
#
#   p <- p_df_joined %>%
#     ggplot(aes(x = workflow_rank, y = mean, shape = recipe)) +
#     geom_errorbar(aes(ymin = mean - std_err, ymax = mean + std_err), color = "gray50", lty = 2, width = 0.4) +
#     geom_point(size = 3, aes(color = model)) +
#     labs(
#       title = "Performance Comparison of Workflow Sets",
#       x = x_axis,
#       color = "Model Types",
#       shape = "Recipes"
#     ) +
#     scale_x_continuous(breaks = scales::pretty_breaks(), limits = c(0, NA)) +
#     scale_y_continuous(labels = pederlib::komma()) +
#     facet +
#     y_axis
#
#   return(p)
# }
