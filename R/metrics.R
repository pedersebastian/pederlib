#Metrics

########
# Metrics for regression and classification

#' Title
#'
#' @param mode Classification of regression (C or R)
#' @param ...  passed on the yardstick::metric_set()
#'
#' @return metric_set ibject
#' @export
#'
#' @examples
#' mset()
mset <- function(mode = "C", ...) {
  arguments <- match.call()
  arguments <- as.list(arguments)

  if (is.null(arguments$mode)) {
    mode <- "C"
  } else {
    mode <- arguments$mode
  }

  mode <- stringr::str_to_upper(mode)


  if (!mode %in% c("C", "R")) {
    rlang::inform(message = "Mode must be classification(C) or regression(R) for standard setup\nYou can still specify your metric using ...\n\n", body = ":)")
    metrics <- yardstick::metric_set(...)
  } else if (mode == "R") {
    metrics <- yardstick::metric_set(
      ...,
      yardstick::rmse,
      yardstick::rsq,
      yardstick::mae,
      yardstick::mase,
    )
  } else {
    metrics <- yardstick::metric_set(
      ...,
      yardstick::roc_auc,
      yardstick::accuracy,
      yardstick::sensitivity,
      yardstick::specificity,
      yardstick::mn_log_loss,
    )
  }
  metrics
}
#####################

####### Custom metrics

## RMSLE - root mean squared log error
## code from Julia Silge :)

rmsle_vec <- function(truth, estimate, na_rm = TRUE, ...) {
  rmsle_impl <- function(truth, estimate) {
    sqrt(mean((log(truth + 1) - log(estimate + 1))^2))
  }

  yardstick::metric_vec_template(
    metric_impl = rmsle_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
    ...
  )
}

#' root mean square log error
#'
#' @param data data
#' @param ... needs truth and estimate
#'
#' @return rmsle
#' @export
rmsle <- function(data, ...) {
  UseMethod("rmsle")
}

rmsle <- yardstick::new_numeric_metric(rmsle, direction = "minimize")

rmsle.data.frame <- function(data, truth, estimate, na_rm = TRUE, ...) {
  yardstick::metric_summarizer(
    metric_nm = "rmsle",
    metric_fn = rmsle_vec,
    data = data,
    truth = !!rlang::enquo(truth),
    estimate = !!rlang::enquo(estimate),
    na_rm = na_rm,
    ...
  )
}
###############
# Geometric mean of sensitivity and specificity



#' Title
#'
#' @param data data
#' @param ... ...
#'
#' @return geometric
#' @export
g_mean <- function(data, ...) {
  UseMethod("g_mean")
}

g_mean <- yardstick::new_class_metric(g_mean, direction = "maximize")

g_mean.data.frame <- function(data,
                              truth,
                              estimate,
                              estimator = NULL,
                              na_rm = TRUE,
                              event_level = "first",
                              ...) {
  yardstick::metric_summarizer(
    metric_nm = "g_mean",
    metric_fn = g_mean_vec,
    data = data,
    truth = !!rlang::enquo(truth),
    estimate = !!rlang::enquo(estimate),
    estimator = estimator,
    na_rm = na_rm,
    event_level = event_level,
    ...
  )
}



g_mean_vec <- function(truth,
                       estimate,
                       estimator = NULL,
                       na_rm = TRUE,
                       event_level = "first",
                       ...) {
  estimator <- yardstick::finalize_estimator(truth, estimator, metric_class = "g_mean")

  g_mean_impl <- function(truth, estimate) {
    xtab <- table(estimate, truth)

    g_mean_estimator_impl(xtab, estimator, event_level)
  }

  yardstick::metric_vec_template(
    metric_impl = g_mean_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "factor",
    estimator = estimator,
    ...
  )
}

# This function switches between binary and multiclass implementations
g_mean_estimator_impl <- function(data, estimator, event_level) {
  if (estimator == "binary") {
    g_mean_binary(data, event_level)
  } else {
    # Encapsulates the macro, macro weighted, and micro cases
    wt <- yardstick::get_weights(data, estimator)
    res <- g_mean_multiclass(data, estimator)
    weighted.mean(res, wt)
  }
}

g_mean_binary <- function(data, event_level) {
  relevant_sens <- pos_val2(data, event_level)
  numer_sens <- sum(data[relevant_sens, relevant_sens])
  denom_sens <- sum(data[, relevant_sens])

  sens <- numer_sens / denom_sens

  negative_spec <- neg_val2(data, event_level)

  numer_spec <- sum(data[negative_spec, negative_spec])
  denom_spec <- sum(data[, negative_spec])

  spec <- numer_spec / denom_spec

  g_mean <- sqrt(spec * sens)
  g_mean
}

g_mean_multiclass <- function(data, estimator) {
  numer_sens <- diag(data)
  denom_sens <- colSums(data)
  numer_sens <- sum(numer_sens, na.rm = TRUE)
  denom_sens <- sum(denom_sens, na.rm = TRUE)
  sens <- numer_sens / denom_sens

  n_spec <- sum(data)

  tp_spec <- diag(data)
  tpfp_spec <- rowSums(data)
  tpfn_spec <- colSums(data)
  tn_spec <- n_spec - (tpfp_spec + tpfn_spec - tp_spec)
  fp_spec <- tpfp_spec - tp_spec

  numer_spec <- tn_spec
  denom_spec <- tn_spec + fp_spec

  spec <- numer_spec / denom_spec


  g_mean <- sqrt(spec * sens)
  g_mean
}

